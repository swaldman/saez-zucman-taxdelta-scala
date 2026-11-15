package taxdelta.model

object SaezZucman2019Context extends SaezZucman2019Labels:

  /*
   *  we only have data up to wealth > $1B, so we make some assumptions
   *  we assume 90% of wealth over $1B also over $2.5B,
   *  but only  60% of wealth over $1B is over $5B,
   *  and only  45% of wealth over $1B is also over $10B.
   */
  val ImputeHigherWealthBracketsTopGroupOnly = true
  val ImputedHigherBrackets = IndexedSeq[Double](2_500_000_000, 5_000_000_000, 10_000_000_000)
  val ImputationFactors     = IndexedSeq[Double](0.9d, 0.6d, 0.45d)
  val ImputationZeroes      = ImputationFactors.map( _ => 0.0d )

  //val parametersBoolean = Map(
  //)
  val parametersDouble = Map(
    AverageSalesTaxShareOfIncome                     -> 0.04675733843078d, // from calculatorbig, F137: calculated as the denom-weighted avg of per-group sales-tax-share-of-income
    AverageValueAddedTaxableConsumptionShareOfIncome -> 0.4d,
    ConsumerIncidenceShare                           -> 0.75d,
    RealizedGainsTaxableShare                        -> 0.366d,
  )

  def imputeHigherBrackets( topGroupOnly : Boolean )( tup : Tuple2[Group,ExcessCurve.Default] ) : Tuple2[Group,ExcessCurve.Default] =
    val (k,v) = tup
    if topGroupOnly then
      k match
        case tg : TopGroup => (k, v.imputeHigherBrackets(ImputedHigherBrackets,ImputationFactors))
        case _             => (k, v.imputeHigherBrackets(ImputedHigherBrackets,ImputationZeroes))
    else
      (k, v.imputeHigherBrackets(ImputedHigherBrackets,ImputationFactors) )

  val dataDouble = Map(
    SalesTaxShareOfIncome -> SaezZucman2019DataBridge.salesTaxSharesOfIncome,
  )
  val dataInt = Map(
    NationalIncomeTaxBase -> SaezZucman2019DataBridge.nationalIncomeTaxBase,
    RealizedCapitalGains  -> SaezZucman2019DataBridge.realizedGains,
  )
  val dataExcessCurve = Map(
    WealthExcessCurve -> SaezZucman2019DataBridge.wealthExcessCurves.map(imputeHigherBrackets(ImputeHigherWealthBracketsTopGroupOnly)) // a bit awkward, accessing the parameter map pre-model
  )

  object Baseline:
    val leversDouble = Map(
      NationalIncomeTaxRate -> 0.0d,
      ValueAddedTaxRate     -> 0.0d,
      WealthTaxEvasionRate  -> 0.15d,
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
    val leversDouble = Map(
      WealthTaxEvasionRate -> 0.15,
    )
    val leversSchedule = Map(
      WealthTaxSchedule -> warrenWealthTaxSchedule,
    )
    Counterfactual(
      leversDouble = leversDouble,
      leversSchedule = leversSchedule
    )

  object Rebaser extends AverageIncomeRebaser:
    override def apply(g : Group, model : Model) = g.averageIncome + (model.parametersDouble(RealizedGainsTaxableShare) * model.dataInt(RealizedCapitalGains)(g))
    override def requires : Set[InputLabel] = Set(RealizedCapitalGains,RealizedGainsTaxableShare)

  import SaezZucman2019Instruments.*

  val model = Model(
    groups           = SaezZucman2019DataBridge.groups,
    rebaser          = Rebaser,
    parametersDouble = parametersDouble,
    dataDouble       = dataDouble,
    dataInt          = dataInt,
    dataExcessCurve  = dataExcessCurve,
    leversDouble     = Baseline.leversDouble,
    leversSchedule   = Baseline.leversSchedule,
    instruments      = List(
                         NationalIncomeTax -> NationalIncomeTaxInstrument,
                         ValueAddedTax -> ValueAddedTaxInstrument,
                         WealthTax -> WealthTaxInstrument
                       )
  )
