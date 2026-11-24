package taxdelta.model

import scala.reflect.ClassTag

abstract class ModelContext:

  trait InputLabel

  /* Constants assumed by the model */
  type LabelParameterDouble  <: InputLabel
  type LabelParameterBoolean <: InputLabel
  type LabelParameterOther   <: InputLabel

  /* Per-group data */
  type LabelDataDouble  <: InputLabel
  type LabelDataBoolean <: InputLabel
  type LabelDataOther   <: InputLabel

  /* Precomputed aggregations of per-group data */
  type LabelAggregationDouble  <: InputLabel
  type LabelAggregationBoolean <: InputLabel
  type LabelAggregationOther   <: InputLabel

  /* Lever values the model assumes but which change in counterfactuals */
  type LabelLeverDouble  <: InputLabel
  type LabelLeverBoolean <: InputLabel
  type LabelLeverOther   <: InputLabel

  type LabelInstrument

  trait Requirer:
    def requires : Set[InputLabel]

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
      override def requires : Set[InputLabel] = Set.empty
  abstract class AverageIncomeRebaser extends Function2[Group,Model,Double], Requirer

  /* 
   * Subclass contexts may consume and so require inputs,
   * even though we don't.
   */
  def contextRequirements : Set[InputLabel] = Set.empty

  object Instrument:
    case class Result( baselineTaxRate : Double, reformedTaxRate : Double)(val runAgainst : Counterfactual):
      val deltaTaxRate : Double = reformedTaxRate - baselineTaxRate
      def taxRateUnder( counterfactual : Option[Counterfactual] ) : Double = // a convenience for use within API where we have either the relevant counterfactual or None
        require(
          counterfactual.fold(true)( _ == runAgainst ),
          "We can only compute the tax rate under no counterfactual (baseline) or the counterfactual we actually ran against. Requested ${counterfactual.get}, runAgainst ${runAgainst}."
        )
        counterfactual.fold(baselineTaxRate)(_ => reformedTaxRate)
    trait Unified extends Instrument:
      protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double
      def baselineTaxRate( model : Model )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double = taxRate(model, None)(prev)(g)
      def reformedTaxRate( model : Model, counterfactual : Counterfactual )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double = taxRate(model, Some(counterfactual))(prev)(g)

  trait Instrument extends Requirer:
    def baselineTaxRate( model : Model )( prev : Map[LabelInstrument,Map[Group,Instrument.Result]] )(g : Group) : Double
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )( prev : Map[LabelInstrument,Map[Group,Instrument.Result]] )(g : Group) : Double
    def depends : Set[LabelInstrument] = Set.empty
    def run( model : Model, counterfactual : Counterfactual )( prev : Map[LabelInstrument,Map[Group,Instrument.Result]] )(g : Group) : Instrument.Result =
      val b = baselineTaxRate(model)(prev)(g)
      val r = reformedTaxRate(model,counterfactual)(prev)(g)
      Instrument.Result(b,r)(counterfactual)

  sealed trait Scenario:
    def leversBoolean : Map[LabelLeverBoolean,Boolean]
    def leversDouble  : Map[LabelLeverDouble,Double]
    def leversOther   : Map[LabelLeverOther,Any]
    def leversOtherAs[T : ClassTag](llo : LabelLeverOther) : T = leversOther(llo).downcast[T]

  object Baseline:
    val empty = Baseline()
  final case class Baseline(
    leversBoolean    : Map[LabelLeverBoolean,Boolean] = Map.empty,
    leversDouble     : Map[LabelLeverDouble,Double]   = Map.empty,
    leversOther      : Map[LabelLeverOther,Any]       = Map.empty,
  ) extends Scenario

  object Counterfactual:
    val empty = Counterfactual()
  final case class Counterfactual(
    leversBoolean    : Map[LabelLeverBoolean,Boolean] = Map.empty,
    leversDouble     : Map[LabelLeverDouble,Double]   = Map.empty,
    leversOther      : Map[LabelLeverOther,Any]       = Map.empty,
  ) extends Scenario

  final case class Model(
    groups              : Groups,
    parametersBoolean   : Map[LabelParameterBoolean,Boolean]       = Map.empty,
    parametersDouble    : Map[LabelParameterDouble,Double]         = Map.empty,
    parametersOther     : Map[LabelParameterOther,Any]             = Map.empty,
    dataBoolean         : Map[LabelDataBoolean,Map[Group,Boolean]] = Map.empty,
    dataDouble          : Map[LabelDataDouble,Map[Group,Double]]   = Map.empty,
    dataOther           : Map[LabelDataOther,Map[Group,Any]]       = Map.empty,
    aggregationsBoolean : Map[LabelAggregationBoolean,Boolean]     = Map.empty,
    aggregationsDouble  : Map[LabelAggregationDouble,Double]       = Map.empty,
    aggregationsOther   : Map[LabelAggregationOther,Any]           = Map.empty,
    baseline            : Baseline                                 = Baseline.empty,
    rebaser             : AverageIncomeRebaser                     = AverageIncomeRebaser.DontRebase,
    instruments         : List[Tuple2[LabelInstrument,Instrument]] = List.empty
  ):
    val rebasedAverageIncome = groups.toIndexedSeq.map(g => (g, rebaser(g, this))).toMap
    val rebasedTotalIncome   = rebasedAverageIncome.map( (k,v) => (k, v * k.size) ).toMap

    def run( counterfactual : Counterfactual ) : Map[LabelInstrument,Map[Group,Instrument.Result]] =
      instruments.foldLeft(Map.empty[LabelInstrument,Map[Group,Instrument.Result]]): (accum, next) =>
        val (label,instrument) = next
        val results = groups.toIndexedSeq.map(g => (g,instrument.run(this,counterfactual)(accum)(g))).toMap
        accum + Tuple2(label,results)

    def runBaseline() : Map[LabelInstrument,Map[Group,Instrument.Result]] = run(Counterfactual.empty)

    val requires : Set[InputLabel] = rebaser.requires ++ instruments.flatMap( (k,v) => v.requires ) ++ contextRequirements
    val offers   : Set[InputLabel] =
      parametersDouble.keySet       ++
      parametersBoolean.keySet      ++
      parametersOther.keySet        ++
      dataDouble.keySet             ++
      dataBoolean.keySet            ++
      dataOther.keySet              ++
      aggregationsDouble.keySet     ++
      aggregationsBoolean.keySet    ++
      aggregationsOther.keySet      ++
      baseline.leversDouble.keySet  ++
      baseline.leversBoolean.keySet ++
      baseline.leversOther.keySet

    def orPrefer( counterfactual : Option[Counterfactual] )( lld : LabelLeverDouble )  : Double  = counterfactual.fold( baseline.leversDouble(lld) )( cf => cf.leversDouble.getOrElse(lld,baseline.leversDouble(lld)) )
    def orPrefer( counterfactual : Option[Counterfactual] )( llb : LabelLeverBoolean ) : Boolean = counterfactual.fold( baseline.leversBoolean(llb) )( cf => cf.leversBoolean.getOrElse(llb,baseline.leversBoolean(llb)) )
    def orPrefer( counterfactual : Option[Counterfactual] )( llo : LabelLeverOther )   : Any     = counterfactual.fold( baseline.leversOther(llo) )( cf => cf.leversOther.getOrElse(llo,baseline.leversOther(llo)) )

    def parametersOtherAs[T : ClassTag](lpo: LabelParameterOther) : T = parametersOther(lpo).downcast[T]
    def dataOtherAs[T : ClassTag](lpo: LabelDataOther)(g : Group) : T = dataOther(lpo)(g).downcast[T]
    def aggregationsOtherAs[T : ClassTag](lao: LabelAggregationOther) : T = aggregationsOther(lao).downcast[T]
    def orPreferAs[T : ClassTag]( counterfactual : Option[Counterfactual] )( llo : LabelLeverOther ) : T = orPrefer(counterfactual)(llo).downcast[T]

    private val duplicateInstrumentLabels : List[LabelInstrument] =
      instruments.map( _(0) )
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .collect { case (element, count) if count > 1 => element }
        .toList

    private val dependencyViolations : Map[LabelInstrument,Set[LabelInstrument]] =
      val raw =
        instruments.foldLeft( Tuple2(Set.empty[LabelInstrument], Map.empty[LabelInstrument,Set[LabelInstrument]]) ): (accum, next) =>
          val (label, instrument) = next
          val seen = accum(0)
          val bads = accum(1)
          val unsatisfied = instrument.depends -- seen
          val nextBads = if unsatisfied.nonEmpty then bads + Tuple2(label,unsatisfied) else bads
          (seen + label, nextBads)
      raw(1)

    require(requires.intersect(offers) == requires, "Missing required information: " + (requires -- offers))
    require(duplicateInstrumentLabels.isEmpty, "Duplicate instrument labels declared: " + duplicateInstrumentLabels)
    require(dependencyViolations.isEmpty, "Some instruments have unsatisfied dependencies to instruments which must be completed earlier: " + dependencyViolations)

