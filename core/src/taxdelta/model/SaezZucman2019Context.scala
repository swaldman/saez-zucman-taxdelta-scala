package taxdelta.model

class SaezZucman2019Context( parameterOverrides : Map[String,Any] = Map.empty ) extends SaezZucman2019Labels:

  val defaultParametersBoolean = Map(
    CountPrivateHealthPremiumsAsTax     -> false,

    // for computing "integrated" dividends and retained-earnings,
    // the individual income tax instrument uses a proposedFederalCorporateTaxRate
    // that seems to double-count CorporateTaxCurrentForeignReceipts.
    // probably this is a (very small) mistake, but by default we keep it to match
    // values from the Saez-Zucman values
    ReplicateWorkbookForeignDoubleCount -> true,
  )
  val defaultParametersDouble = Map(
    AverageSalesTaxShareOfIncome                        -> 0.04675733843078d, // from calculatorbig, F137: calculated as the denom-weighted avg of per-group sales-tax-share-of-income
    AverageValueAddedTaxableConsumptionShareOfIncome    ->   0.4d,
    CloselyHeldCorpTotalRetainedEarnings                -> 166.6d, // billions
    ConsumerIncidenceShare                              ->  0.75d,
    CorporateTaxCurrentDomesticProfits                  ->  1308d, // billions
    CorporateTaxCurrentDomesticReceipts                 ->   210d, // billions, Federal + State
    CorporateTaxCurrentForeignReceipts                  ->     8d,
    CorporateTaxCurrentStateReceipts                    ->    60d, // billions
    CorporateTaxForeignReceiptsUnderStrongerEnforcement ->    96d,
    EmployerShareOfPrivateHealthPremiums                ->  0.55d,
    EstateTaxCurrentAvoidanceRate                       ->   0.6d,
    EstateTaxCurrentStatutoryTopRate                    ->   0.4d,
    FiscalCapitalGainsTaxableShare                      -> 0.366d,
  )

  val dataDouble = Map(
    AvgOrdinaryMarginalTaxRate                            -> SaezZucman2019DataBridge.avgOrdinaryMarginalTaxRate,
    AvgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent -> SaezZucman2019DataBridge.avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent,
    CorporateTaxShareOfIncome                             -> SaezZucman2019DataBridge.corporateTaxShareOfIncome,
    EffectiveIncomeTaxShareOfIncome                       -> SaezZucman2019DataBridge.effectiveIncomeTaxShareOfIncome,
    EmployersHealthInsuranceContributionShareOfIncome     -> SaezZucman2019DataBridge.healthTaxShareOfIncome,
    EstateTaxShareOfIncome                                -> SaezZucman2019DataBridge.estateTaxShareOfIncome,
    FiscalCapitalGains                                    -> SaezZucman2019DataBridge.fiscalCapitalGains,
    FiscalDividends                                       -> SaezZucman2019DataBridge.fiscalDividends,
    FiscalIncomeIncludingCapitalGains                     -> SaezZucman2019DataBridge.fiscalIncomeIncludingCapGains,
    NationalIncomeTaxBase                                 -> SaezZucman2019DataBridge.nationalIncomeTaxBase,
    NumberOfAdults                                        -> SaezZucman2019DataBridge.numberOfAdults,
    PayrollTaxShareOfIncome                               -> SaezZucman2019DataBridge.payrollTaxShareOfIncome,
    PropertyTaxShareOfIncome                              -> SaezZucman2019DataBridge.propertyTaxShareOfIncome,
    SalesTaxShareOfIncome                                 -> SaezZucman2019DataBridge.salesTaxShareOfIncome,
  )
  val dataOther = Map(
    IncomeExcessCurve -> SaezZucman2019DataBridge.incomeExcessCurves,

    // we match the model's imputation of health for high levels not in the data,
    // only for the top 400 group. see core.scala for details
    WealthExcessCurve -> SaezZucman2019DataBridge.wealthExcessCurves.map(imputeHigherBracketsTopGroupOnly) 
  )

  val calculators     = new SaezZucman2019Calculators[this.type](this)
  val counterfactuals = new SaezZucman2019Counterfactuals[this.type](this)

  // manage parameter overrides
  val overrideParametersBoolean = findOverrideParametersBoolean( parameterOverrides )
  val overrideParametersDouble  = findOverrideParametersDouble( parameterOverrides )
  val unparsedParameterOverrides = parameterOverrides.keySet -- (overrideParametersBoolean.keySet.map(_.toString) ++ overrideParametersDouble.keySet.map(_.toString))
  require(unparsedParameterOverrides.isEmpty, "The following keys of parameterOverrides were unrecognized, or else their values were not of their expected type: " + unparsedParameterOverrides)
  val parametersBoolean = defaultParametersBoolean ++ overrideParametersBoolean
  val parametersDouble  = defaultParametersDouble ++ overrideParametersDouble

  // note that we do need to build out baseline scenario after defining the
  // potentially overridden parameters, because our baseline depends upon
  // the final parameterization of the model
  val baseline = Baseline(
    leversBoolean = Map(
      CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates                 -> false,
      CorporateTaxDeployStrongerEnforcement                             -> false,
      EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax          -> false,
      IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate                -> false,
      IndividualIncomeTaxDividendsTaxedAtOrdinaryRate                   -> false,
      IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate -> false,
    ),
    leversDouble = Map(
      CorporateTaxProposedEffectiveRate                 -> parametersDouble(CorporateTaxCurrentDomesticReceipts) / parametersDouble(CorporateTaxCurrentDomesticProfits), // baseline rate, by definition
      EstateTaxAnticipatedAvoidanceRate                 -> parametersDouble(EstateTaxCurrentAvoidanceRate),    // no change at baseline
      EstateTaxProposedTopRate                          -> parametersDouble(EstateTaxCurrentStatutoryTopRate), // no change in baseline
      ReductionOfEmployerSponsoredPrivateHealthPremiums -> 0.0d,
      NationalIncomeTaxRate                             -> 0.0d,
      SalesAndExciseTaxReduction                        -> 0.0d,
      ValueAddedTaxRate                                 -> 0.0d,
      WealthTaxEvasionRate                              -> 0.15d,
    ),
    leversOther = Map(
      IndividualIncomeTaxSchedule -> Schedule( // current law schedule
        IndexedSeq(0d, 77_000, 165_000, 315_000, 400_000, 600_000, 1_000_000, 2_500_000, 5_000_000, 10_000_000, 25_000_000, 50_000_000),
        IndexedSeq(0.12, 0.22, 0.24, 0.32, 0.35, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37)
      ),
      WealthTaxSchedule -> Schedule.empty
    )
  )

  val mainModel = Model(
    groups            = SaezZucman2019DataBridge.groups,
    rebaser           = calculators.Rebaser,
    parametersBoolean = parametersBoolean,
    parametersDouble  = parametersDouble,
    dataDouble        = dataDouble,
    dataOther         = dataOther,
    baseline          = baseline,
    instruments       = List(
                          EstateTax           -> calculators.EstateTaxInstrument,
                          HealthInsuranceTax  -> calculators.HealthInsuranceTaxInstrument,
                          NationalIncomeTax   -> calculators.NationalIncomeTaxInstrument,
                          PayrollTax          -> calculators.PayrollTaxInstrument,
                          PropertyTax         -> calculators.PropertyTaxInstrument,
                          SalesAndExciseTax   -> calculators.SalesAndExciseTaxInstrument,
                          ValueAddedTax       -> calculators.ValueAddedTaxInstrument,
                          WealthTax           -> calculators.WealthTaxInstrument,
                          CorporateTax        -> calculators.CorporateTaxInstrument, // depends upon NationalIncomeTax and ValueAddedTax having been executed previously
                          IndividualIncomeTax -> calculators.IndividualIncomeTaxInstrument, // depends upon NationalIncomeTax and ValueAddedTax having been executed previously
                        )
  )

