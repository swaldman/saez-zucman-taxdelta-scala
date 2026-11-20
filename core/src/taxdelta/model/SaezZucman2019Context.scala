package taxdelta.model

// An reimplementation of Saez and Zucman taxjusticenow.org model, as provided here
//
//   https://eml.berkeley.edu/~saez/taxsimulator2.xlsx
//
// Inspired by, but includes modeling assumptions and choices independent from
//
//   Thomas Piketty, Emmanuel Saez and Stefanie Stantcheva (2014), "Optimal Taxation of Top Labor Incomes: A Tale of Three Elasticities," American Economic Journal: Economic Policy 6(1): 230–271. https://doi.org/10.1257/pol.6.1.230
//   https://www.aeaweb.org/articles?id=10.1257/pol.6.1.230
//
// (It attends to only two of its three elasticities.)

class SaezZucman2019Context( parameterOverrides : Map[String,Any] = Map.empty ) extends SaezZucman2019Labels:

  /* Basic parameters of the model */
  private lazy val _defaultParametersBoolean = Map(
    CountPrivateHealthPremiumsAsTax -> false,

    // for computing "integrated" dividends and retained-earnings,
    // the individual income tax instrument uses a proposedFederalCorporateTaxRate
    // that seems to double-count CorporateTaxCurrentForeignReceipts.
    // probably this is a (very small) mistake, but by default we keep it to match
    // values from the Saez-Zucman values
    ReplicateWorkbookForeignDoubleCount -> true,
  )
  private lazy val _defaultParametersDouble = Map(
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
  // we use defs returning private vals so we are overridable
  // but never generate the maps more than once
  def defaultParametersBoolean = _defaultParametersBoolean
  def defaultParametersDouble  = _defaultParametersDouble
  def defaultParametersOther   = Map.empty[LabelParameterOther,Any] // we don't use these, but subclasses might!

  /* Per-group data -- each value is a mapping of Groups to values */
  private lazy val _dataDouble = Map(
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
  private lazy val _dataOther = Map(
    IncomeExcessCurve -> SaezZucman2019DataBridge.incomeExcessCurves,

    // we match the model's imputation of health for high levels not in the data,
    // only for the top 400 group. see core.scala for details
    WealthExcessCurve -> SaezZucman2019DataBridge.wealthExcessCurves.map(imputeHigherBracketsTopGroupOnly) 
  )
  def dataDouble = _dataDouble
  def dataOther  = _dataOther

  /* Precomputed aggregations of the data */
  private lazy val _aggregationsDouble = Map(
    TotalGainsAndDividends -> SaezZucman2019DataBridge.totalGainsAndDividends
  )
  def aggregationsDouble = _aggregationsDouble

  /* The code that computes both the taxes and the income denominator they are defined against */
  lazy val calculators     = new SaezZucman2019Calculators[this.type](this)

  /* Preset counterfactuals to which we can apply the model and see changes from the baseline scenario */
  lazy val counterfactuals = new SaezZucman2019Counterfactuals[this.type](this)

  // manage parameter overrides
  val overrideParametersBoolean = findOverrideParametersBoolean( parameterOverrides )
  val overrideParametersDouble  = findOverrideParametersDouble( parameterOverrides )
  val overrideParametersOther   = findOverrideParametersOther( parameterOverrides )
  val overrideParametersUsed = overrideParametersBoolean.keySet.map(_.toString) ++ overrideParametersDouble.keySet.map(_.toString) ++ overrideParametersOther.keySet.map(_.toString)
  val unparsedParameterOverrides = parameterOverrides.keySet -- overrideParametersUsed
  require(unparsedParameterOverrides.isEmpty, "The following keys of parameterOverrides were unrecognized, or else their values were not of their expected type: " + unparsedParameterOverrides)

  lazy val parametersBoolean = defaultParametersBoolean ++ overrideParametersBoolean
  lazy val parametersDouble  = defaultParametersDouble ++ overrideParametersDouble
  lazy val parametersOther   = defaultParametersOther ++ overrideParametersOther

  /* Baseline scenario */

  // note that we do need to build out baseline scenario after defining the
  // potentially overridden parameters, because our baseline depends upon
  // the final parameterization of the model
  lazy val baseline = Baseline(
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

  lazy val mainModel = Model(
    groups             = SaezZucman2019DataBridge.groups,
    rebaser            = calculators.Rebaser,
    parametersBoolean  = parametersBoolean,
    parametersDouble   = parametersDouble,
    parametersOther    = parametersOther,
    dataDouble         = dataDouble,
    dataOther          = dataOther,
    aggregationsDouble = aggregationsDouble,
    baseline           = baseline,
    instruments        = List(
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




