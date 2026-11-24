package taxdelta.model

class SaezZucman2019Calculators[C <: SaezZucman2019Context & Singleton](val context : C):

  import context.*

  /** The change in another instrument's rate for a group, relative to baseline (zero under the baseline itself).
   *  Crowd-out factors use changes rather than levels, so broad taxes that already exist at baseline
   *  (e.g. an existing VAT in the general model) don't shrink the observed rates of other taxes. */
  private def changeFromBaseline( r : Instrument.Result, counterfactual : Option[Counterfactual] ) : Double =
    r.taxRateUnder(counterfactual) - r.baselineTaxRate

  object Rebaser extends AverageIncomeRebaser:
    override def apply(g : Group, model : Model) = g.averageIncome + (model.parametersDouble(FiscalCapitalGainsTaxableShare) * model.dataDouble(FiscalCapitalGains)(g))
    override def requires : Set[InputLabel] = Set(FiscalCapitalGains,FiscalCapitalGainsTaxableShare)

  object CorporateTaxInstrument extends Instrument.Unified:
    private def computeTaxRate(
      changeInNationalIncomeTaxRateForGroup               : Double,
      changeInValueAddedTaxRateForGroup                   : Double,
      corporateTaxCurrentDomesticProfits                  : Double,
      corporateTaxCurrentDomesticReceipts                 : Double,
      corporateTaxCurrentForeignReceipts                  : Double,
      corporateTaxForeignReceiptsUnderStrongerEnforcement : Double,
      corporateTaxDeployStrongerEnforcement               : Boolean,
      corporateTaxProposedEffectiveRate                   : Double,
      corporateTaxShareOfIncome                           : Double
    ) : Double =
      val currentTotalReceipts   = corporateTaxCurrentDomesticReceipts + corporateTaxCurrentForeignReceipts
      val potentialTotalReceipts =
        val foreignReceipts = if corporateTaxDeployStrongerEnforcement then corporateTaxForeignReceiptsUnderStrongerEnforcement else corporateTaxCurrentForeignReceipts
        (corporateTaxCurrentDomesticProfits * corporateTaxProposedEffectiveRate) + foreignReceipts
      val crowdOutFactor = (1 - changeInNationalIncomeTaxRateForGroup - changeInValueAddedTaxRateForGroup) // new broad taxes shrink the base; existing ones are already in the data
      val proposalAdjustmentFactor = potentialTotalReceipts / currentTotalReceipts
      crowdOutFactor * proposalAdjustmentFactor * corporateTaxShareOfIncome
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val changeInNationalIncomeTaxRateForGroup               = changeFromBaseline( prev(NationalIncomeTax)( g ), counterfactual )
      val changeInValueAddedTaxRateForGroup                   = changeFromBaseline( prev(ValueAddedTax)( g ), counterfactual )
      val corporateTaxCurrentDomesticProfits                  = model.parametersDouble(CorporateTaxCurrentDomesticProfits)
      val corporateTaxCurrentDomesticReceipts                 = model.parametersDouble(CorporateTaxCurrentDomesticReceipts)
      val corporateTaxCurrentForeignReceipts                  = model.parametersDouble(CorporateTaxCurrentForeignReceipts)
      val corporateTaxForeignReceiptsUnderStrongerEnforcement = model.parametersDouble(CorporateTaxForeignReceiptsUnderStrongerEnforcement)
      val corporateTaxDeployStrongerEnforcement               = model.orPrefer( counterfactual )(CorporateTaxDeployStrongerEnforcement)
      val corporateTaxProposedEffectiveRate                   = model.orPrefer( counterfactual )(CorporateTaxProposedEffectiveRate)
      val corporateTaxShareOfIncome                           = model.dataDouble(CorporateTaxShareOfIncome)( g )
      computeTaxRate(
        changeInNationalIncomeTaxRateForGroup,
        changeInValueAddedTaxRateForGroup,
        corporateTaxCurrentDomesticProfits,
        corporateTaxCurrentDomesticReceipts,
        corporateTaxCurrentForeignReceipts,
        corporateTaxForeignReceiptsUnderStrongerEnforcement,
        corporateTaxDeployStrongerEnforcement,
        corporateTaxProposedEffectiveRate,
        corporateTaxShareOfIncome
      )
    override val requires = Set(
      CorporateTaxCurrentDomesticProfits,
      CorporateTaxCurrentDomesticReceipts,
      CorporateTaxCurrentForeignReceipts,
      CorporateTaxForeignReceiptsUnderStrongerEnforcement,
      CorporateTaxDeployStrongerEnforcement,
      CorporateTaxProposedEffectiveRate,
      CorporateTaxShareOfIncome
    )
    override val depends  = Set(NationalIncomeTax,ValueAddedTax)

  object EstateTaxInstrument extends Instrument.Unified:
    private def computeTaxRate( estateTaxShareOfIncome : Double, estateTaxCurrentStatutoryTopRate : Double, estateTaxProposedTopRate : Double, estateTaxCurrentAvoidanceRate : Double, estateTaxAnticipatedAvoidanceRate : Double ) : Double =
      val adjustmentFactorNewTopRate       = estateTaxProposedTopRate / estateTaxCurrentStatutoryTopRate
      val adjustmentFactorNewAvoidanceRate = (1 - estateTaxAnticipatedAvoidanceRate) / (1 - estateTaxCurrentAvoidanceRate)
      estateTaxShareOfIncome * adjustmentFactorNewTopRate * adjustmentFactorNewAvoidanceRate
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val estateTaxShareOfIncome = model.dataDouble(EstateTaxShareOfIncome)(g)
      val estateTaxCurrentStatutoryTopRate = model.parametersDouble(EstateTaxCurrentStatutoryTopRate)
      val estateTaxProposedTopRate = model.orPrefer( counterfactual )( EstateTaxProposedTopRate )
      val estateTaxCurrentAvoidanceRate = model.parametersDouble(EstateTaxCurrentAvoidanceRate)
      val estateTaxAnticipatedAvoidanceRate = model.orPrefer( counterfactual )( EstateTaxAnticipatedAvoidanceRate )
      computeTaxRate( estateTaxShareOfIncome, estateTaxCurrentStatutoryTopRate, estateTaxProposedTopRate, estateTaxCurrentAvoidanceRate, estateTaxAnticipatedAvoidanceRate )
    override val requires = Set(EstateTaxShareOfIncome,EstateTaxCurrentStatutoryTopRate,EstateTaxProposedTopRate,EstateTaxCurrentAvoidanceRate,EstateTaxAnticipatedAvoidanceRate)

  object HealthInsuranceTaxInstrument extends Instrument.Unified:
    private def computeTaxRate( countPrivateHealthPremiumsAsTax : Boolean, employerSponsoredPrivateHealthPremiumsAsTaxRate : Double, reductionOfEmployerSponsoredPrivateHealthPremiums : Double ) : Double =
      if countPrivateHealthPremiumsAsTax then
        (1 - reductionOfEmployerSponsoredPrivateHealthPremiums) * employerSponsoredPrivateHealthPremiumsAsTaxRate
      else
        0
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val countPrivateHealthPremiumsAsTax = model.parametersBoolean(CountPrivateHealthPremiumsAsTax)
      val employerSponsoredPrivateHealthPremiumsAsTaxRate = model.dataDouble(EmployersHealthInsuranceContributionShareOfIncome)(g)
      val reductionOfEmployerSponsoredPrivateHealthPremiums = model.orPrefer(counterfactual)(ReductionOfEmployerSponsoredPrivateHealthPremiums)
      computeTaxRate( countPrivateHealthPremiumsAsTax, employerSponsoredPrivateHealthPremiumsAsTaxRate, reductionOfEmployerSponsoredPrivateHealthPremiums )
    override val requires = Set(CountPrivateHealthPremiumsAsTax,EmployersHealthInsuranceContributionShareOfIncome,ReductionOfEmployerSponsoredPrivateHealthPremiums)

  object NationalIncomeTaxInstrument extends Instrument.Unified:
    private def computeTaxRate( proposedNationalIncomeTaxRate : Double, nationalIncomeTaxBase : Double, rebasedAverageIncome : Double ) : Double =
      (proposedNationalIncomeTaxRate * nationalIncomeTaxBase) / rebasedAverageIncome
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val nitRate = model.orPrefer(counterfactual)(NationalIncomeTaxRate)
      computeTaxRate( nitRate, model.dataDouble(NationalIncomeTaxBase)(g), model.rebasedAverageIncome(g) )
    override val requires = Set(NationalIncomeTaxBase,NationalIncomeTaxRate)

  object PayrollTaxInstrument extends Instrument.Unified:
    private def computeTaxRate(
      employerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax : Boolean,
      payrollTaxShareOfIncome : Double,
      employerShareOfPrivateHealthPremiums : Double,
      employerSponsoredPrivateHealthPremiumsAsTaxRate : Double
    ) : Double =
      val extra =
        if employerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax then
          (employerShareOfPrivateHealthPremiums * employerSponsoredPrivateHealthPremiumsAsTaxRate)
        else
          0
      payrollTaxShareOfIncome + extra
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val employerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax = model.orPrefer(counterfactual)(EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax)
      val payrollTaxShareOfIncome = model.dataDouble(PayrollTaxShareOfIncome)(g)
      val employerShareOfPrivateHealthPremiums = model.parametersDouble(EmployerShareOfPrivateHealthPremiums)
      val employerSponsoredPrivateHealthPremiumsAsTaxRate = model.dataDouble(EmployersHealthInsuranceContributionShareOfIncome)(g)
      computeTaxRate(
        employerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax,
        payrollTaxShareOfIncome,
        employerShareOfPrivateHealthPremiums,
        employerSponsoredPrivateHealthPremiumsAsTaxRate
      )
    override val requires = Set(EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax,PayrollTaxShareOfIncome,EmployerShareOfPrivateHealthPremiums,EmployersHealthInsuranceContributionShareOfIncome)


  // a bit degenerate
  object PropertyTaxInstrument extends Instrument.Unified:
    private def computeTaxRate( propertyTaxShareOfIncome : Double ) = propertyTaxShareOfIncome
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double = model.dataDouble(PropertyTaxShareOfIncome)( g )
    override val requires = Set(PropertyTaxShareOfIncome)

  // generally, to expand consumption taxes, prefer introducing a VAT.
  // rescaling sales-and-excise-taxes implies proportionally rescaling all of them,
  // i.e. if you increase the sales tax by 20%, you also increase tobacco and fuel taxes by the same
  //
  // this model also foresees "broad" taxes crowding out corporate and progressive individual income taxes.
  // that is set up for VAT and a potential flat "national income tax", but not for rescalings of sales
  // and excise taxes.
  object SalesAndExciseTaxInstrument extends Instrument.Unified:
    private def computeTaxRate(
      uniformlyRescaleSalesAndExciseTaxesFactor : Double,
      existingSalesAndExciseShareOfIncome       : Double
    ) : Double =  uniformlyRescaleSalesAndExciseTaxesFactor * existingSalesAndExciseShareOfIncome
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val salesAndExciseTaxReduction = model.orPrefer(counterfactual)(SalesAndExciseTaxReduction)
      val existingSalesAndExciseShareOfIncome = model.dataDouble(SalesTaxShareOfIncome)( g )
      computeTaxRate(1-salesAndExciseTaxReduction,existingSalesAndExciseShareOfIncome)
    override val requires = Set(SalesAndExciseTaxReduction,SalesTaxShareOfIncome)

  object ValueAddedTaxInstrument extends Instrument.Unified:
    private def groupSpecificWeightingOfTaxableSales( groupSpecificSalesTaxShareOfIncome : Double, averageSalesTaxShareOfIncome : Double, consumerIncidenceShare : Double) : Double =
      (consumerIncidenceShare * (groupSpecificSalesTaxShareOfIncome / averageSalesTaxShareOfIncome)) + ((1-consumerIncidenceShare) * 1)
    private def computeTaxRate( valueAddedTaxRate : Double, averageValueAddedTaxableConsumptionShareOfIncome : Double, groupSpecificSalesTaxShareOfIncome : Double, averageSalesTaxShareOfIncome : Double, consumerIncidenceShare : Double ) : Double =
      val taxRateOnVatPurchases = valueAddedTaxRate / (1+valueAddedTaxRate)
      val groupWeight = groupSpecificWeightingOfTaxableSales( groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, consumerIncidenceShare )
      taxRateOnVatPurchases * averageValueAddedTaxableConsumptionShareOfIncome * groupWeight
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val valueAddedTaxRate = model.orPrefer(counterfactual)(ValueAddedTaxRate)
      val averageValueAddedTaxableConsumptionShareOfIncome = model.parametersDouble(AverageValueAddedTaxableConsumptionShareOfIncome)
      val groupSpecificSalesTaxShareOfIncome = model.dataDouble(SalesTaxShareOfIncome)(g)
      val averageSalesTaxShareOfIncome = model.parametersDouble(AverageSalesTaxShareOfIncome)
      val consumerIncidenceShare = model.parametersDouble(ConsumerIncidenceShare)
      computeTaxRate( valueAddedTaxRate, averageValueAddedTaxableConsumptionShareOfIncome, groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, consumerIncidenceShare )
    override val requires = Set(ValueAddedTaxRate,AverageValueAddedTaxableConsumptionShareOfIncome,SalesTaxShareOfIncome,AverageSalesTaxShareOfIncome,ConsumerIncidenceShare)

  object WealthTaxInstrument extends Instrument.Unified:
    private def computeTaxRate( schedule : Schedule, excessCurve : ExcessCurve, evasionRate : Double, rebasedAverageIncome : Double ) : Double =
      val taxesDue = schedule.averageTax( excessCurve );
      val afterEvasion = (1 - evasionRate) * taxesDue;
      afterEvasion / rebasedAverageIncome
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val schedule = model.orPreferAs[Schedule](counterfactual)(WealthTaxSchedule)
      val evasionRate = model.orPrefer(counterfactual)(WealthTaxEvasionRate)
      computeTaxRate( schedule, model.dataOtherAs[ExcessCurve](WealthExcessCurve)(g), evasionRate, model.rebasedAverageIncome(g) )
    override val requires = Set(WealthTaxSchedule,WealthExcessCurve,WealthTaxEvasionRate)

  object IndividualIncomeTaxInstrument extends Instrument.Unified:
    private def computeTaxRate(
      baselineTaxSchedule                                               : Schedule,
      avgOrdinaryMarginalTaxRate                                        : Double,
      avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent             : Double,
      closelyHeldCorpTaxRetainedEarningsAtOrdinaryRates                 : Boolean,
      closelyHeldCorpTotalRetainedEarningsInDollars                     : Double,
      changeInNationalIncomeTaxRateForGroup                             : Double,
      changeInValueAddedTaxRateForGroup                                 : Double,
      corporateTaxProposedEffectiveRate                                 : Double,
      corporateTaxCurrentDomesticProfits                                : Double,
      corporateTaxCurrentDomesticReceipts                               : Double,
      corporateTaxCurrentForeignReceipts                                : Double,
      corporateTaxCurrentStateReceipts                                  : Double,
      corporateTaxForeignReceiptsUnderStrongerEnforcement               : Double,
      corporateTaxDeployStrongerEnforcement                             : Boolean,
      effectiveIncomeTaxShareOfIncome                                   : Double,
      fiscalCapitalGains                                                : Double,
      fiscalDividends                                                   : Double,
      fiscalIncomeIncludingCapitalGains                                 : Double,
      fiscalCapGainsAndDividendsTotal                                   : Double,
      incomeExcessCurve                                                 : ExcessCurve,
      individualIncomeTaxCapitalGainsTaxedAtOrdinaryRate                : Boolean,
      individualIncomeTaxDividendsTaxedAtOrdinaryRate                   : Boolean,
      individualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate : Boolean, // if true, obviates individualIncomeTaxDividendsTaxedAtOrdinaryRate, it'd be better to use a three-value enum for this
      replicateWorkbookForeignDoubleCount                               : Boolean,
      individualIncomeTaxSchedule                                       : Schedule,
      rebasedAverageIncomeForGroup                                      : Double
    ) : Double =
      val proposedFederalCorporateTaxRate =
        val foreignReceipts = if corporateTaxDeployStrongerEnforcement then corporateTaxForeignReceiptsUnderStrongerEnforcement else corporateTaxCurrentForeignReceipts

        // XXX: To reproduce the Saez-Zucman workbook. But this is probably a mistake we will want to eliminate later
        val probablyErroneousExtraCurrentForeignReceiptsTerm =
          if replicateWorkbookForeignDoubleCount then corporateTaxCurrentForeignReceipts else 0d

        val proposedTotalReceipts   = (corporateTaxCurrentDomesticProfits * corporateTaxProposedEffectiveRate) + foreignReceipts + probablyErroneousExtraCurrentForeignReceiptsTerm
        val proposedFederalReceipts = proposedTotalReceipts - corporateTaxCurrentStateReceipts   // state tax is held fixed and isn't creditable
        proposedFederalReceipts / corporateTaxCurrentDomesticProfits

      val currentFederalCorporateTaxRate =
        (corporateTaxCurrentDomesticReceipts + corporateTaxCurrentForeignReceipts - corporateTaxCurrentStateReceipts) / corporateTaxCurrentDomesticProfits

      val naiveChangeAtOrdinaryRates = individualIncomeTaxSchedule.averageTax(incomeExcessCurve) - baselineTaxSchedule.averageTax(incomeExcessCurve)
      val naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome = naiveChangeAtOrdinaryRates / fiscalIncomeIncludingCapitalGains

      // notation: xTaxSchedule(dollars) means the tax those dollars pay, sitting on top of the
      // person's other income, under schedule x. we approximate, with our data,
      //     oldTaxSchedule(dollars) ≈ mtr * dollars
      //     newTaxSchedule(dollars) ≈ (mtr + naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome) * dollars

      val additionDueToCapGainsPreference = // use a negative value for if exclusion
        if individualIncomeTaxCapitalGainsTaxedAtOrdinaryRate then
          // we have
          //
          //    newTaxSchedule(fiscalCapGains) - oldTaxSchedule(fiscalCapGains) // treating fiscalCapGains as marginal income
          //
          // we want
          //
          //    newTaxSchedule(fiscalCapGains) - min(0.20,mtr) * fiscalCapGains          // 0.20 is the hardcoded current-law preference rate, but not more than mtr
          //
          // want - have = newTaxSchedule(fiscalCapGains) - min(0.20,mtr) * fiscalCapGains - (newTaxSchedule(fiscalCapGains) - oldTaxSchedule(fiscalCapGains))
          //             = newTaxSchedule(fiscalCapGains) - min(0.20,mtr) * fiscalCapGains - newTaxSchedule(fiscalCapGains) + oldTaxSchedule(fiscalCapGains)
          //             = - min(0.20,mtr) * fiscalCapGains + oldTaxSchedule(fiscalCapGains)
          //             ≈ - min(0.20,mtr) * fiscalCapGains + (mtr * fiscalCapGains)
          //             ≈ (mtr - min(0.20,mtr)) * fiscalCapGains
          //             ≈ avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent * fiscalCapGains
          //
          avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent * fiscalCapitalGains
        else
          // we have
          //
          //    newTaxSchedule(fiscalCapGains) - oldTaxSchedule(fiscalCapGains) // treating fiscalCapGains as marginal income
          //
          // we want
          //
          //    0                                                               // should contribute nothing to the delta, 
          //                                                                    // because we're leaving its treatment alone,
          //                                                                    // and ay its preference rate, unaffected by
          //                                                                    // any change in brackets
          //                                                                    //
          //                                                                    // but newTaxSchedule(fiscalCapGains) - oldTaxSchedule(fiscalCapGains)
          // So...                                                              // would have mechanically applied effects of bracket changes!
          //
          // want - have = 0 - (newTaxSchedule(fiscalCapGains) - oldTaxSchedule(fiscalCapGains))
          //             = oldTaxSchedule(fiscalCapGains) - newTaxSchedule(fiscalCapGains)
          //             ≈ mtr * fiscalCapGains - (mtr + naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome) * fiscalCapGains
          //             = - naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome * fiscalCapGains
          //
          -naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome * fiscalCapitalGains
      val additionDueToDividendTreatment = // use a negative value for if exclusion
        if individualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate then
          //
          // we have fiscalDividends
          //
          // grossedUpDividends = fiscalDividends / (1 - currentFederalCorporateTaxRate)
          //
          // our tax brackets have computed, for the dividend dollars,
          //     newTaxSchedule(fiscalDividends) - oldTaxSchedule(fiscalDividends)
          //
          // but what we really want is
          //    the new personal tax on the grossed-up dividend,
          //      less the credit for corporate tax already paid, 
          //      less what the dividends actually pay today (min(0.20,mtr) * fiscalDividends) 
          //        (the preference rate, or less if the person's ordinary rate is below it)
          //     newTaxSchedule(grossedUpDividends) - proposedFederalCorporateTaxRate * grossedUpDividends - (min(0.20,mtr) * fiscalDividends)
          //
          // so we add want - have:
          //
          //   want = newTaxSchedule(grossedUpDividends) - (proposedFederalCorporateTaxRate * grossedUpDividends) - (min(0.20,mtr) * fiscalDividends)
          //   have = newTaxSchedule(fiscalDividends) - oldTaxSchedule(fiscalDividends)
          //
          //   want - have = newTaxSchedule(grossedUpDividends) - (proposedFederalCorporateTaxRate * grossedUpDividends) - (min(0.20,mtr) * fiscalDividends) - (newTaxSchedule(fiscalDividends) - oldTaxSchedule(fiscalDividends))
          //   want - have = newTaxSchedule(grossedUpDividends) - (proposedFederalCorporateTaxRate * grossedUpDividends) - (min(0.20,mtr) * fiscalDividends) - newTaxSchedule(fiscalDividends) + oldTaxSchedule(fiscalDividends)
          //
          //   rearrange:
          //     want - have = newTaxSchedule(grossedUpDividends) - newTaxSchedule(fiscalDividends) - (proposedFederalCorporateTaxRate * grossedUpDividends) - (min(0.20,mtr) * fiscalDividends) + oldTaxSchedule(fiscalDividends)
          //
          //   since we consider applying a tax schedule to an increment of dollars applying "at the top", at the margina rate, we simplify:
          //     want - have = newTaxSchedule(grossedUpDividends-fiscalDividends) - (proposedFederalCorporateTaxRate * grossedUpDividends) - (min(0.20,mtr) * fiscalDividends) + oldTaxSchedule(fiscalDividends)
          //
          //   and then replace with their approximate definitions (see above)
          //     want - have = (mtr + naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome) * (grossedUpDividends-fiscalDividends) - (proposedFederalCorporateTaxRate * grossedUpDividends) - (min(0.20,mtr) * fiscalDividends) + (mtr * fiscalDividends)
          //     want - have = (mtr + naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome) * (grossedUpDividends-fiscalDividends) - (proposedFederalCorporateTaxRate * grossedUpDividends) + ((mtr - min(0.20,mtr)) * fiscalDividends)
          //
          val grossedUpDividends          = fiscalDividends / (1 - currentFederalCorporateTaxRate)
          val grossUpIncrement            = grossedUpDividends - fiscalDividends
          (avgOrdinaryMarginalTaxRate + naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome) * grossUpIncrement - (proposedFederalCorporateTaxRate * grossedUpDividends) + ((avgOrdinaryMarginalTaxRate - math.min(0.20,avgOrdinaryMarginalTaxRate)) * fiscalDividends)
        else if individualIncomeTaxDividendsTaxedAtOrdinaryRate then
          // we have
          //
          //    newTaxSchedule(fiscalDividends) - oldTaxSchedule(fiscalDividends) // treating fiscalDividends as marginal income
          //
          // we want
          //
          //    newTaxSchedule(fiscalDividends) - min(0.20,mtr) * fiscalDividends          // 0.20 is the hardcoded current-law preference rate, but not more than mtr
          //
          // want - have = newTaxSchedule(fiscalDividends) - min(0.20,mtr) * fiscalDividends - (newTaxSchedule(fiscalDividends) - oldTaxSchedule(fiscalDividends))
          //             = newTaxSchedule(fiscalDividends) - min(0.20,mtr) * fiscalDividends - newTaxSchedule(fiscalDividends) + oldTaxSchedule(fiscalDividends)
          //             = - min(0.20,mtr) * fiscalDividends + oldTaxSchedule(fiscalDividends)
          //             ≈ - min(0.20,mtr) * fiscalDividends + (mtr * fiscalDividends)
          //             ≈ (mtr - min(0.20,mtr)) * fiscalDividends
          //             ≈ avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent * fiscalDividends
          //
          avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent * fiscalDividends
        else
          // we have
          //
          //    newTaxSchedule(fiscalDividends) - oldTaxSchedule(fiscalDividends) // treating fiscalDividends as marginal income
          //
          // we want
          //
          //    0                                                               // should contribute nothing to the delta, 
          //                                                                    // because we're leaving its treatment alone,
          //                                                                    // and ay its preference rate, unaffected by
          //                                                                    // any change in brackets
          //                                                                    //
          //                                                                    // but newTaxSchedule(fiscalDividends) - oldTaxSchedule(fiscalDividends)
          // So...                                                              // would have mechanically applied effects of bracket changes!
          //
          // want - have = 0 - (newTaxSchedule(fiscalDividends) - oldTaxSchedule(fiscalDividends))
          //             = oldTaxSchedule(fiscalDividends) - newTaxSchedule(fiscalDividends)
          //             ≈ mtr * fiscalDividends - (mtr + naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome) * fiscalDividends
          //             = - naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome * fiscalDividends
          //
          -naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome * fiscalDividends

      val additionDueToRetainedEarningsOfCloselyHeldCorps : Double =
        if closelyHeldCorpTaxRetainedEarningsAtOrdinaryRates then
          val retainedEarningsImputedToPersonInGroup =
            // Note: averageShareOfCapGainsPlusDividendsPerGroup would be numInGroup * (fiscalCapitalGains + fiscalDividends) / fiscalCapGainsAndDividendsTotal
            //       averageShareOfCapGainsPlusDividendsPerPersonInGroup is averageShareOfRetainedEarningsPerGroup / numInGroup = (fiscalCapitalGains + fiscalDividends) / fiscalCapGainsAndDividendsTotal
            //       We use share of cap-gains plus dividends as our proxy for share of retained earnings
            val averageShareOfCapGainsPlusDividendsPerPersonInGroup = (fiscalCapitalGains + fiscalDividends).toDouble / fiscalCapGainsAndDividendsTotal
            closelyHeldCorpTotalRetainedEarningsInDollars * averageShareOfCapGainsPlusDividendsPerPersonInGroup
          val pretaxRetainedEarningsImputedToPersonInGroup = retainedEarningsImputedToPersonInGroup / (1 - currentFederalCorporateTaxRate)
          (avgOrdinaryMarginalTaxRate + naiveChangeAtOrdinaryRatesPerDollarOfFiscalIncome - proposedFederalCorporateTaxRate) * pretaxRetainedEarningsImputedToPersonInGroup
        else
          0

      val correctedChange = naiveChangeAtOrdinaryRates + additionDueToCapGainsPreference + additionDueToDividendTreatment + additionDueToRetainedEarningsOfCloselyHeldCorps
      val correctedChangeAsShareOfIncome = correctedChange / rebasedAverageIncomeForGroup
      val crowdOutFactor = (1 - changeInNationalIncomeTaxRateForGroup - changeInValueAddedTaxRateForGroup) // new broad taxes shrink the base; existing ones are already in the data
      crowdOutFactor * math.max(effectiveIncomeTaxShareOfIncome + correctedChangeAsShareOfIncome, 0d)

    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val baselineTaxSchedule                                               = model.baseline.leversOtherAs[Schedule](IndividualIncomeTaxSchedule) // we don't treat this as a lever though, ans shift with counterfactual. we always want the baseline schedule here
      val avgOrdinaryMarginalTaxRate                                        = model.dataDouble(AvgOrdinaryMarginalTaxRate)( g )
      val avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent             = model.dataDouble(AvgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent)( g )
      val closelyHeldCorpTaxRetainedEarningsAtOrdinaryRates                 = model.orPrefer(counterfactual)(CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates)
      val closelyHeldCorpTotalRetainedEarnings                              = model.parametersDouble(CloselyHeldCorpTotalRetainedEarnings)
      val changeInNationalIncomeTaxRateForGroup                             = changeFromBaseline( prev(NationalIncomeTax)( g ), counterfactual )
      val changeInValueAddedTaxRateForGroup                                 = changeFromBaseline( prev(ValueAddedTax)( g ), counterfactual )
      val corporateTaxProposedEffectiveRate                                 = model.orPrefer(counterfactual)(CorporateTaxProposedEffectiveRate)
      val corporateTaxCurrentDomesticProfits                                = model.parametersDouble(CorporateTaxCurrentDomesticProfits)
      val corporateTaxCurrentDomesticReceipts                               = model.parametersDouble(CorporateTaxCurrentDomesticReceipts)
      val corporateTaxCurrentForeignReceipts                                = model.parametersDouble(CorporateTaxCurrentForeignReceipts)
      val corporateTaxForeignReceiptsUnderStrongerEnforcement               = model.parametersDouble(CorporateTaxForeignReceiptsUnderStrongerEnforcement)
      val corporateTaxDeployStrongerEnforcement                             = model.orPrefer(counterfactual)(CorporateTaxDeployStrongerEnforcement)
      val corporateTaxCurrentStateReceipts                                  = model.parametersDouble(CorporateTaxCurrentStateReceipts)
      val effectiveIncomeTaxShareOfIncome                                   = model.dataDouble(EffectiveIncomeTaxShareOfIncome)( g )
      val fiscalCapitalGains                                                = model.dataDouble(FiscalCapitalGains)( g )
      val fiscalDividends                                                   = model.dataDouble(FiscalDividends)( g )
      val fiscalIncomeIncludingCapitalGains                                 = model.dataDouble(FiscalIncomeIncludingCapitalGains)( g )
      val fiscalCapGainsAndDividendsTotal                                   = model.aggregationsDouble(TotalGainsAndDividends)
      val incomeExcessCurve                                                 = model.dataOtherAs[ExcessCurve](IncomeExcessCurve)( g )
      val individualIncomeTaxCapitalGainsTaxedAtOrdinaryRate                = model.orPrefer(counterfactual)(IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate)
      val individualIncomeTaxDividendsTaxedAtOrdinaryRate                   = model.orPrefer(counterfactual)(IndividualIncomeTaxDividendsTaxedAtOrdinaryRate) // ignored if IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate is true
      val individualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate = model.orPrefer(counterfactual)(IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate) // if true IndividualIncomeTaxDividendsTaxedAtOrdinaryRate is ignored
      val replicateWorkbookForeignDoubleCount                               = model.parametersBoolean(ReplicateWorkbookForeignDoubleCount)
      val individualIncomeTaxSchedule                                       = model.orPreferAs[Schedule](counterfactual)(IndividualIncomeTaxSchedule)
      val rebasedAverageIncomeForGroup                                      = model.rebasedAverageIncome( g )
      computeTaxRate(
        baselineTaxSchedule                                               = baselineTaxSchedule,
        avgOrdinaryMarginalTaxRate                                        = avgOrdinaryMarginalTaxRate,
        avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent             = avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent,
        closelyHeldCorpTaxRetainedEarningsAtOrdinaryRates                 = closelyHeldCorpTaxRetainedEarningsAtOrdinaryRates,
        closelyHeldCorpTotalRetainedEarningsInDollars                     = closelyHeldCorpTotalRetainedEarnings * 1_000_000_000,
        changeInNationalIncomeTaxRateForGroup                             = changeInNationalIncomeTaxRateForGroup,
        changeInValueAddedTaxRateForGroup                                 = changeInValueAddedTaxRateForGroup,
        corporateTaxProposedEffectiveRate                                 = corporateTaxProposedEffectiveRate,
        corporateTaxCurrentDomesticProfits                                = corporateTaxCurrentDomesticProfits,
        corporateTaxCurrentDomesticReceipts                               = corporateTaxCurrentDomesticReceipts,
        corporateTaxCurrentForeignReceipts                                = corporateTaxCurrentForeignReceipts,
        corporateTaxCurrentStateReceipts                                  = corporateTaxCurrentStateReceipts,
        corporateTaxForeignReceiptsUnderStrongerEnforcement               = corporateTaxForeignReceiptsUnderStrongerEnforcement,
        corporateTaxDeployStrongerEnforcement                             = corporateTaxDeployStrongerEnforcement,
        effectiveIncomeTaxShareOfIncome                                   = effectiveIncomeTaxShareOfIncome,
        fiscalCapitalGains                                                = fiscalCapitalGains.toDouble,
        fiscalDividends                                                   = fiscalDividends.toDouble,
        fiscalIncomeIncludingCapitalGains                                 = fiscalIncomeIncludingCapitalGains.toDouble,
        fiscalCapGainsAndDividendsTotal                                   = fiscalCapGainsAndDividendsTotal.toDouble,
        incomeExcessCurve                                                 = incomeExcessCurve,
        individualIncomeTaxCapitalGainsTaxedAtOrdinaryRate                = individualIncomeTaxCapitalGainsTaxedAtOrdinaryRate,
        individualIncomeTaxDividendsTaxedAtOrdinaryRate                   = individualIncomeTaxDividendsTaxedAtOrdinaryRate,
        individualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate = individualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate,
        replicateWorkbookForeignDoubleCount                               = replicateWorkbookForeignDoubleCount,
        individualIncomeTaxSchedule                                       = individualIncomeTaxSchedule,
        rebasedAverageIncomeForGroup                                      = rebasedAverageIncomeForGroup
      ) 
    override val requires = Set(
      IndividualIncomeTaxSchedule,
      AvgOrdinaryMarginalTaxRate,
      AvgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent,
      CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates,
      CloselyHeldCorpTotalRetainedEarnings,
      CorporateTaxProposedEffectiveRate,
      CorporateTaxCurrentDomesticProfits,
      CorporateTaxCurrentDomesticReceipts,
      CorporateTaxCurrentForeignReceipts,
      CorporateTaxForeignReceiptsUnderStrongerEnforcement,
      CorporateTaxDeployStrongerEnforcement,
      CorporateTaxCurrentStateReceipts,
      EffectiveIncomeTaxShareOfIncome,
      FiscalCapitalGains,
      FiscalDividends,
      FiscalIncomeIncludingCapitalGains,
      IncomeExcessCurve,
      IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate,
      IndividualIncomeTaxDividendsTaxedAtOrdinaryRate,
      IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate,
      ReplicateWorkbookForeignDoubleCount,
      IndividualIncomeTaxSchedule,
      TotalGainsAndDividends
    )
    override val depends  = Set(NationalIncomeTax,ValueAddedTax)


