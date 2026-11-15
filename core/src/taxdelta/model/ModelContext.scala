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
    trait Unified extends Instrument:
      protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double
      def baselineTaxRate( model : Model )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double = taxRate(model, None)(prev)(g)
      def reformedTaxRate( model : Model, counterfactual : Counterfactual )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double = taxRate(model, Some(counterfactual))(prev)(g)

  trait Instrument extends Requirer:
    def baselineTaxRate( model : Model )( prev : Map[LabelInstrument,Map[Group,Instrument.Result]] )(g : Group) : Double
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )( prev : Map[LabelInstrument,Map[Group,Instrument.Result]] )(g : Group) : Double
    def run( model : Model, counterfactual : Counterfactual )( prev : Map[LabelInstrument,Map[Group,Instrument.Result]] )(g : Group) : Instrument.Result =
      val b = baselineTaxRate(model)(prev)(g)
      val r = reformedTaxRate(model,counterfactual)(prev)(g)
      val d = r - b
      Instrument.Result(b,r,d)

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
    instruments       : List[Tuple2[LabelInstrument,Instrument]]         = List.empty
  ):
    val rebasedAverageIncome = groups.toSeq.map(g => (g, rebaser(g, this))).toMap
    val rebasedTotalIncome   = rebasedAverageIncome.map( (k,v) => (k, v * k.size) ).toMap

    def run( counterfactual : Counterfactual ) : Map[LabelInstrument,Map[Group,Instrument.Result]] =
      instruments.foldLeft(Map.empty[LabelInstrument,Map[Group,Instrument.Result]]): (accum, next) =>
        val (label,instrument) = next
        val results = groups.toSeq.map(g => (g,instrument.run(this,counterfactual)(accum)(g))).toMap
        accum + Tuple2(label,results)

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

    private lazy val duplicateInstrumentLabels : List[LabelInstrument] =
      instruments.map( _(0) )
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .collect { case (element, count) if count > 1 => element }
        .toList

    require(requires.intersect(offers) == requires, "Missing required information: " + (requires -- offers))
    require(duplicateInstrumentLabels.isEmpty, "Duplicate instrument labels declared: " + duplicateInstrumentLabels)

  final case class CounterfactualOrModel( counterfactual : Counterfactual, model : Model ):
    def double(l : LabelLeverDouble)     : Double   = counterfactual.leversDouble.getOrElse(l, model.leversDouble(l))
    def int(l : LabelLeverInt)           : Int      = counterfactual.leversInt.getOrElse(l, model.leversInt(l))
    def boolean(l : LabelLeverBoolean)   : Boolean  = counterfactual.leversBoolean.getOrElse(l, model.leversBoolean(l))
    def schedule(l : LabelLeverSchedule) : Schedule = counterfactual.leversSchedule.getOrElse(l, model.leversSchedule(l))

  final case class Counterfactual(
    leversDouble     : Map[LabelLeverDouble,Double]     = Map.empty,
    leversInt        : Map[LabelLeverInt,Int]           = Map.empty,
    leversBoolean    : Map[LabelLeverBoolean,Boolean]   = Map.empty,
    leversSchedule   : Map[LabelLeverSchedule,Schedule] = Map.empty,
  ):
    def orModel( model : Model ) = CounterfactualOrModel( this, model )
