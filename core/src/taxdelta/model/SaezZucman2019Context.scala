package taxdelta.model

class SaezZucman2019Context( parameterOverrides : Map[String,Any] = Map.empty ) extends SaezZucman2019Labels:

  val defaultParametersBoolean = Map(
    CountPrivateHealthPremiumsAsTax -> false
  )

  val defaultParametersDouble = Map(
    AverageSalesTaxShareOfIncome                        -> 0.04675733843078d, // from calculatorbig, F137: calculated as the denom-weighted avg of per-group sales-tax-share-of-income
    AverageValueAddedTaxableConsumptionShareOfIncome    ->   0.4d,
    ConsumerIncidenceShare                              ->  0.75d,
    CorporateTaxCurrentDomesticProfits                  ->  1308d, // billions
    CorporateTaxCurrentDomesticReceipts                 ->   210d, // billions
    CorporateTaxCurrentForeignReceipts                  ->     8d,
    CorporateTaxForeignReceiptsUnderStrongerEnforcement ->    96d,
    EmployerShareOfPrivateHealthPremiums                ->  0.55d,
    EstateTaxCurrentAvoidanceRate                       ->   0.6d,
    EstateTaxCurrentStatutoryTopRate                    ->   0.4d,
    RealizedGainsTaxableShare                           -> 0.366d,
  )

  val dataDouble = Map(
    CorporateTaxShareOfIncome                         -> SaezZucman2019DataBridge.corporateTaxSharesOfIncome,
    EmployersHealthInsuranceContributionShareOfIncome -> SaezZucman2019DataBridge.healthTaxSharesOfIncome,
    EstateTaxShareOfIncome                            -> SaezZucman2019DataBridge.estateTaxSharesOfIncome,
    PayrollTaxShareOfIncome                           -> SaezZucman2019DataBridge.payrollTaxSharesOfIncome,
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
  object Baseline:
    val leversBoolean =  Map(
      CorporateTaxDeployStrongerEnforcement                    -> false,
      EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax -> false
    )
    val leversDouble = Map(
      CorporateTaxProposedEffectiveRate                 -> parametersDouble(CorporateTaxCurrentDomesticReceipts) / parametersDouble(CorporateTaxCurrentDomesticProfits), // baseline rate, by definition
      EstateTaxAnticipatedAvoidanceRate                 -> parametersDouble(EstateTaxCurrentAvoidanceRate),    // no change at baseline
      EstateTaxProposedTopRate                          -> parametersDouble(EstateTaxCurrentStatutoryTopRate), // no change in baseline
      ReductionOfEmployerSponsoredPrivateHealthPremiums -> 0.0d,
      NationalIncomeTaxRate                             -> 0.0d,
      ValueAddedTaxRate                                 -> 0.0d,
      WealthTaxEvasionRate                              -> 0.15d,
    )
    val leversSchedule = Map(
      WealthTaxSchedule -> Schedule.empty
    )

  val model = Model(
    groups            = SaezZucman2019DataBridge.groups,
    rebaser           = calculators.Rebaser,
    parametersBoolean = parametersBoolean,
    parametersDouble  = parametersDouble,
    dataDouble        = dataDouble,
    dataInt           = dataInt,
    dataExcessCurve   = dataExcessCurve,
    leversBoolean     = Baseline.leversBoolean,
    leversDouble      = Baseline.leversDouble,
    leversSchedule    = Baseline.leversSchedule,
    instruments       = List(
                          EstateTax          -> calculators.EstateTaxInstrument,
                          HealthInsuranceTax -> calculators.HealthInsuranceTaxInstrument,
                          NationalIncomeTax  -> calculators.NationalIncomeTaxInstrument,
                          PayrollTax         -> calculators.PayrollTaxInstrument,
                          ValueAddedTax      -> calculators.ValueAddedTaxInstrument,
                          WealthTax          -> calculators.WealthTaxInstrument,
                          CorporateTax       -> calculators.CorporateTaxInstrument, // depends upon NationalIncomeTax and ValueAddedTax having been executed previously
                        )
  )
