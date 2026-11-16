package taxdelta.model

class SaezZucman2019Calculators[C <: SaezZucman2019Context & Singleton](val context : C):

  import context.*

  object Rebaser extends AverageIncomeRebaser:
    override def apply(g : Group, model : Model) = g.averageIncome + (model.parametersDouble(RealizedGainsTaxableShare) * model.dataInt(RealizedCapitalGains)(g))
    override def requires : Set[InputLabel] = Set(RealizedCapitalGains,RealizedGainsTaxableShare)

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

  object WealthTaxInstrument extends Instrument.Unified:
    private def computeTaxRate( schedule : Schedule, excessCurve : ExcessCurve, evasionRate : Double, rebasedAverageIncome : Double ) : Double =
      val taxesDue = schedule.averageTax( excessCurve );
      val afterEvasion = (1 - evasionRate) * taxesDue;
      afterEvasion / rebasedAverageIncome
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val schedule = model.orPrefer(counterfactual)(WealthTaxSchedule)
      val evasionRate = model.orPrefer(counterfactual)(WealthTaxEvasionRate)
      computeTaxRate( schedule, model.dataExcessCurve(WealthExcessCurve)(g), evasionRate, model.rebasedAverageIncome(g) )
    override val requires = Set(WealthTaxSchedule,WealthExcessCurve,WealthTaxEvasionRate)

  object NationalIncomeTaxInstrument extends Instrument.Unified:
    private def computeTaxRate( proposedNationalIncomeTaxRate : Double, nationalIncomeTaxBase : Double, rebasedAverageIncome : Double ) : Double =
      (proposedNationalIncomeTaxRate * nationalIncomeTaxBase) / rebasedAverageIncome
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val nitRate = model.orPrefer(counterfactual)(NationalIncomeTaxRate)
      computeTaxRate( nitRate, model.dataInt(NationalIncomeTaxBase)(g), model.rebasedAverageIncome(g) )
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
