package taxdelta.model

abstract class ModelContext:

  trait InputLabel

  /* Constants assumed by the model */
  type LabelParameterDouble  <: InputLabel
  type LabelParameterBoolean <: InputLabel

  /* Per-group data */
  type LabelDataDouble      <: InputLabel
  type LabelDataBoolean     <: InputLabel
  type LabelDataExcessCurve <: InputLabel

  /* Lever values the model assumes but which change change in countefactuals */
  type LabelLeverDouble   <: InputLabel
  type LabelLeverBoolean  <: InputLabel
  type LabelLeverSchedule <: InputLabel

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
  abstract class AverageIncomeRebaser extends Function2[Group,Model,Double], Requirer;

  object Instrument:
    case class Result( baselineTaxRate : Double, reformedTaxRate : Double, deltaTaxRate : Double):
      def taxRateUnder( counterfactual : Option[Counterfactual] ) : Double = counterfactual.fold(baselineTaxRate)(_ => reformedTaxRate)
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
      val d = r - b
      Instrument.Result(b,r,d)

  final case class Counterfactual(
    leversDouble     : Map[LabelLeverDouble,Double]     = Map.empty,
    leversBoolean    : Map[LabelLeverBoolean,Boolean]   = Map.empty,
    leversSchedule   : Map[LabelLeverSchedule,Schedule] = Map.empty,
  )

  final case class Model(
    groups            : Groups,
    parametersDouble  : Map[LabelParameterDouble,Double]                 = Map.empty,
    parametersBoolean : Map[LabelParameterBoolean,Boolean]               = Map.empty,
    dataDouble        : Map[LabelDataDouble,Map[Group,Double]]           = Map.empty,
    dataBoolean       : Map[LabelDataBoolean,Map[Group,Boolean]]         = Map.empty,
    dataExcessCurve   : Map[LabelDataExcessCurve,Map[Group,ExcessCurve]] = Map.empty,
    leversDouble      : Map[LabelLeverDouble,Double]                     = Map.empty,
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

    val requires : Set[InputLabel] = rebaser.requires ++ instruments.flatMap( (k,v) => v.requires )
    val offers   : Set[InputLabel] =
      parametersDouble.keySet  ++
      parametersBoolean.keySet ++
      dataDouble.keySet        ++
      dataBoolean.keySet       ++
      dataExcessCurve.keySet   ++
      leversDouble.keySet      ++
      leversBoolean.keySet     ++
      leversSchedule.keySet

    def orPrefer( counterfactual : Option[Counterfactual] )( lld : LabelLeverDouble )   : Double   = counterfactual.fold( this.leversDouble(lld) )( cf => cf.leversDouble.getOrElse(lld,this.leversDouble(lld)) )
    def orPrefer( counterfactual : Option[Counterfactual] )( llb : LabelLeverBoolean )  : Boolean  = counterfactual.fold( this.leversBoolean(llb) )( cf => cf.leversBoolean.getOrElse(llb,this.leversBoolean(llb)) )
    def orPrefer( counterfactual : Option[Counterfactual] )( lls : LabelLeverSchedule ) : Schedule = counterfactual.fold( this.leversSchedule(lls) )( cf => cf.leversSchedule.getOrElse(lls,this.leversSchedule(lls)) )

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

