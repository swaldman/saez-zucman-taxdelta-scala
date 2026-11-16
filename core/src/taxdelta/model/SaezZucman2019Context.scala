package taxdelta.model

class SaezZucman2019Context( parameterOverrides : Map[String,Any] = Map.empty ) extends SaezZucman2019Labels:

  val defaultParametersBoolean = Map(
    CountPrivateHealthPremiumsAsTax -> false
  )

  val defaultParametersDouble = Map(
    AverageSalesTaxShareOfIncome                     -> 0.04675733843078d, // from calculatorbig, F137: calculated as the denom-weighted avg of per-group sales-tax-share-of-income
    AverageValueAddedTaxableConsumptionShareOfIncome -> 0.4d,
    ConsumerIncidenceShare                           -> 0.75d,
    EmployerShareOfPrivateHealthPremiums             -> 0.55d,
    RealizedGainsTaxableShare                        -> 0.366d,
  )

  val dataDouble = Map(
    EmployersHealthInsuranceContributionShareOfIncome -> SaezZucman2019DataBridge.healthTaxSharesOfIncome,
    PayrollTaxShareOfIncome                           -> SaezZucman2019DataBridge.payrollTaxShareOfIncome,
    SalesTaxShareOfIncome                             -> SaezZucman2019DataBridge.salesTaxSharesOfIncome,
  )
  val dataInt = Map(
    NationalIncomeTaxBase -> SaezZucman2019DataBridge.nationalIncomeTaxBase,
    RealizedCapitalGains  -> SaezZucman2019DataBridge.realizedGains,
  )
  val dataExcessCurve = Map(
    // we match the model's imputation of health for high levels not in the data,
    // only for the top 400 group. see core.scala for details
    WealthExcessCurve -> SaezZucman2019DataBridge.wealthExcessCurves.map(imputeHigherBracketsTopGroupOnly) 
  )

  object Baseline:
    val leversBoolean =  Map(
      EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax -> false
    )
    val leversDouble = Map(
      ReductionOfEmployerSponsoredPrivateHealthPremiums                 -> 0.0d,
      NationalIncomeTaxRate                                             -> 0.0d,
      ValueAddedTaxRate                                                 -> 0.0d,
      WealthTaxEvasionRate                                              -> 0.15d,
    )
    val leversSchedule = Map(
      WealthTaxSchedule -> Schedule.empty
    )

  val WarrenCounterfactual =
    // the 0.0005 & 0.0010 terms are a financial transactions tax awkwardly
    // modeled as part of the wealth tax.
    val warrenWealthTaxSchedule = Schedule(
      IndexedSeq(     0,   1_000_000,      50_000_000,   1_000_000_000),
      IndexedSeq(0.0005,      0.0010,   0.0010 + 0.02,   0.0010 + 0.06)
    )
    val leversBoolean =  Map(
      EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax -> true
    )
    val leversDouble = Map(
      ReductionOfEmployerSponsoredPrivateHealthPremiums                 -> 1.00d, // single-payer!
      WealthTaxEvasionRate                                              -> 0.15d,
    )
    val leversSchedule = Map(
      WealthTaxSchedule -> warrenWealthTaxSchedule,
    )
    Counterfactual(
      leversBoolean = leversBoolean,
      leversDouble = leversDouble,
      leversSchedule = leversSchedule
    )

  val calculators = new SaezZucman2019Calculators[this.type](this)

  // manage parameter overrides
  val overrideParametersBoolean = findOverrideParametersBoolean( parameterOverrides )
  val overrideParametersDouble  = findOverrideParametersDouble( parameterOverrides )
  val unparsedParameterOverrides = parameterOverrides.keySet -- (overrideParametersBoolean.keySet.map(_.toString) ++ overrideParametersDouble.keySet.map(_.toString))
  require(unparsedParameterOverrides.isEmpty, "The following keys of parameterOverrides were unrecognized, or else their values were not of their expected type: " + unparsedParameterOverrides)

  val model = Model(
    groups            = SaezZucman2019DataBridge.groups,
    rebaser           = calculators.Rebaser,
    parametersBoolean = defaultParametersBoolean ++ overrideParametersBoolean,
    parametersDouble  = defaultParametersDouble ++ overrideParametersDouble,
    dataDouble        = dataDouble,
    dataInt           = dataInt,
    dataExcessCurve   = dataExcessCurve,
    leversBoolean     = Baseline.leversBoolean,
    leversDouble      = Baseline.leversDouble,
    leversSchedule    = Baseline.leversSchedule,
    instruments       = List(
                          HealthInsuranceTax -> calculators.HealthInsuranceTaxInstrument,
                          NationalIncomeTax  -> calculators.NationalIncomeTaxInstrument,
                          PayrollTax         -> calculators.PayrollTaxInstrument,
                          ValueAddedTax      -> calculators.ValueAddedTaxInstrument,
                          WealthTax          -> calculators.WealthTaxInstrument
                        )
  )
