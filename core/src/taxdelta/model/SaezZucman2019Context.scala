package taxdelta.model

object SaezZucman2019Context extends ModelContext:
  enum LPDouble extends Label:
    case RealizedGainsTaxableShare // income stats exclude realized gains, but the tax base does in part include them

  enum LDInt extends Label:
    case RealizedCapitalGains

  enum LDExcessCurve extends Label:
    case WealthExcessCurve

  enum LLDouble extends Label:
    case WealthTaxEvasionRate

  enum LLSchedule extends Label:
    case WealthTaxSchedule

  enum LInstrument extends Label:
    case WealthTax

  import LPDouble.*, LDInt.*, LDExcessCurve.*, LLDouble.*, LLSchedule.*, LInstrument.*

  override type LabelParameterDouble  = LPDouble
  override type LabelDataInt          = LDInt
  override type LabelDataExcessCurve  = LDExcessCurve
  override type LabelLeverDouble      = LLDouble
  override type LabelLeverSchedule    = LLSchedule
  override type LabelInstrument       = LInstrument

  val parametersDouble = Map(
    RealizedGainsTaxableShare -> 0.366
  )
  val dataInt = Map(
    RealizedCapitalGains -> SaezZucman2019DataBridge.realizedGains
  )
  val dataExcessCurve = Map(
    WealthExcessCurve -> SaezZucman2019DataBridge.wealthExcessCurves
  )

  object Baseline:
    val leversDouble = Map(
      WealthTaxEvasionRate -> 0.15,
    )
    val leversSchedule = Map(
      WealthTaxSchedule -> Schedule.empty
    )

  val WarrenCounterfactual =
    // the 0.005 & 0.0010 terms are a financial transactions tax awkwardly 
    // modeled as part of the wealth tax.
    val warrenWealthTaxSchedule = Schedule(
      IndexedSeq(0, 1_000_000, 50_000_000, 1_000_000_000),
      IndexedSeq(0.0005, 0.0010, 0.0010 + 0.02, 0.0010 + 0.06)
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
    override def requires : Set[Label] = Set(RealizedCapitalGains,RealizedGainsTaxableShare)

  object WealthTaxInstrument extends Instrument:
    def computeTaxRate( schedule : Schedule, excessCurve : ExcessCurve, evasionRate : Double, rebasedAverageIncome : Double ) : Double =
      val taxesDue = schedule.averageTax( excessCurve );
      val afterEvasion = (1 - evasionRate) * taxesDue;
      afterEvasion / rebasedAverageIncome
    def baselineTaxRate( model : Model )(g : Group) : Double =
      computeTaxRate( model.leversSchedule(WealthTaxSchedule), model.dataExcessCurve(WealthExcessCurve)(g), model.leversDouble(WealthTaxEvasionRate), model.rebasedAverageIncome(g) )
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )(g : Group) : Double =
      computeTaxRate( counterfactual.leversSchedule(WealthTaxSchedule), model.dataExcessCurve(WealthExcessCurve)(g), counterfactual.leversDouble(WealthTaxEvasionRate), model.rebasedAverageIncome(g) )
    override val requires = Set(WealthTaxSchedule,WealthExcessCurve,WealthTaxEvasionRate)

  val model = Model(
    groups           = SaezZucman2019DataBridge.groups,
    rebaser          = Rebaser,
    parametersDouble = parametersDouble,
    dataInt          = dataInt,
    dataExcessCurve  = dataExcessCurve,
    leversSchedule   = Baseline.leversSchedule,
    leversDouble     = Baseline.leversDouble,
    instruments      = Map( WealthTax -> WealthTaxInstrument )
  )


