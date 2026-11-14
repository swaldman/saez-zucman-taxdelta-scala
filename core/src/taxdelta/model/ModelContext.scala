package taxdelta.model

abstract class ModelContext:

  trait Label

  /* Constants assumed by the model */
  type LabelParameterDouble  <: Label
  type LabelParameterInt     <: Label
  type LabelParameterBoolean <: Label

  /* Per-group data */
  type LabelDataDouble      <: Label
  type LabelDataInt         <: Label
  type LabelDataBoolean     <: Label
  type LabelDataExcessCurve <: Label

  /* Lever values the model assumes but which change change in countefactuals */
  type LabelLeverDouble   <: Label
  type LabelLeverInt      <: Label
  type LabelLeverBoolean  <: Label
  type LabelLeverSchedule <: Label

  type LabelInstrument <: Label

  trait Requirer:
    def requires : Set[Label]

  /*
   * Sometimes the average of the income used to define groups does not
   * correspond to taxable income. For example, some income measures exclude
   * capital gains, even though these sometimes get taxed.
   *
   * This function gets applied to group average income to render it an
   * appropriate denominator for computing tax rates
   */
  object AverageIncomeRebaser:
    object DontRebase extends AverageIncomeRebaser:
      override def apply(g : Group, m : Model) : Double = g.averageIncome
      override def requires : Set[Label] = Set.empty
  abstract class AverageIncomeRebaser extends Function2[Group,Model,Double], Requirer;

  object Instrument:
    case class Result( baselineTaxRate : Double, reformedTaxRate : Double, deltaTaxRate : Double)
  trait Instrument extends Requirer:
    def baselineTaxRate( model : Model )(g : Group) : Double
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )(g : Group) : Double
    def run( model : Model, counterfactual : Counterfactual )(g : Group) : Instrument.Result =
      val b = baselineTaxRate(model)(g)
      val r = reformedTaxRate(model,counterfactual)(g)
      val d = r - b
      Instrument.Result(b,r,d)

  case class CompoundInstrument(instruments : Set[Instrument]) extends Instrument:
    override def baselineTaxRate( model : Model )(g : Group)                                  : Double = instruments.foldLeft(0.0d)((accum,next) => accum + next.baselineTaxRate(model)(g))
    override def reformedTaxRate( model : Model, counterfactual : Counterfactual )(g : Group) : Double = instruments.foldLeft(0.0d)((accum,next) => accum + next.reformedTaxRate(model,counterfactual)(g))
    override def requires : Set[Label] = instruments.foldLeft(Set.empty[Label])((accum,next) => accum ++ next.requires)

  final case class Model(
    groups            : Groups,
    parametersDouble  : Map[LabelParameterDouble,Double]                 = Map.empty,
    parametersInt     : Map[LabelParameterInt,Int]                       = Map.empty,
    parametersBoolean : Map[LabelParameterBoolean,Boolean]               = Map.empty,
    dataDouble        : Map[LabelDataDouble,Map[Group,Double]]           = Map.empty,
    dataInt           : Map[LabelDataInt,Map[Group,Int]]                 = Map.empty,
    dataBoolean       : Map[LabelDataBoolean,Map[Group,Boolean]]         = Map.empty,
    dataExcessCurve   : Map[LabelDataExcessCurve,Map[Group,ExcessCurve]] = Map.empty,
    leversDouble      : Map[LabelLeverDouble,Double]                     = Map.empty,
    leversInt         : Map[LabelLeverInt,Int]                           = Map.empty,
    leversBoolean     : Map[LabelLeverBoolean,Boolean]                   = Map.empty,
    leversSchedule    : Map[LabelLeverSchedule,Schedule]                 = Map.empty,
    rebaser           : AverageIncomeRebaser                             = AverageIncomeRebaser.DontRebase,
    instruments       : Map[LabelInstrument,Instrument]                  = Map.empty
  ):
    val rebasedAverageIncome = groups.toSeq.map(g => (g, rebaser(g, this))).toMap
    val rebasedTotalIncome   = rebasedAverageIncome.map( (k,v) => (k, v * k.size) ).toMap

    def run( counterfactual : Counterfactual ) : Map[LabelInstrument,Map[Group,Instrument.Result]] =
      val tups =
        instruments.map: (label,instrument) =>
          val results = groups.toSeq.map( g => (g, instrument.run(this,counterfactual)(g)) ).toMap
          (label,results)
      tups.toMap

    val requires : Set[Label] = rebaser.requires ++ instruments.flatMap( (k,v) => v.requires )
    val offers   : Set[Label] =
      parametersDouble.keySet  ++
      parametersInt.keySet     ++
      parametersBoolean.keySet ++
      dataDouble.keySet        ++
      dataInt.keySet           ++
      dataBoolean.keySet       ++
      dataExcessCurve.keySet   ++
      leversDouble.keySet      ++
      leversInt.keySet         ++
      leversBoolean.keySet     ++
      leversSchedule.keySet

    require(requires.intersect(offers) == requires, "Missing required information: " + (requires -- offers))

  final case class Counterfactual(
    leversDouble     : Map[LabelLeverDouble,Double]     = Map.empty,
    leversInt        : Map[LabelLeverInt,Int]           = Map.empty,
    leversBoolean    : Map[LabelLeverBoolean,Boolean]   = Map.empty,
    leversSchedule   : Map[LabelLeverSchedule,Schedule] = Map.empty,
  )
