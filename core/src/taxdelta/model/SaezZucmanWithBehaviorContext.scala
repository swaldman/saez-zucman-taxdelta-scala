package taxdelta.model

import taxdelta.util.Numerics

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

class SaezZucman2019WithBehaviorContext( parameterOverrides : Map[String,Any] = Map.empty ) extends SaezZucman2019Context( parameterOverrides ):

  private val _defaultParametersDouble =
    super.defaultParametersDouble ++
    Map(
      LongRunAveragePreTaxIncomeGrowth          -> 4.807021763688295d, // computed as $G$147/$C$147 from SZ growth worksheet
      LongRunIncentiveElasticity                -> 0.25d,
      LongRunBargainingElasticity               -> 0.50d,
      LongRunImpliedMarginalTaxRateCap          -> 0.99d,
      LongRunMarginalTaxRateSmoothingHalfWindow -> 3d,
      LongRunMarginalTaxRateSmoothingPasses     -> 7d,
    )
  override def defaultParametersDouble = _defaultParametersDouble

  private val _defaultParametersOther =
    super.defaultParametersOther ++
    Map(
      LongRunBargainingWeightFunction -> ((g : Group) => longRunBargainingWeightForGroup(g)) // Group => Double, or Function1[Group,Double]
    )
  override def defaultParametersOther = _defaultParametersOther

  private val _dataDouble =
    super.dataDouble ++
    Map(
      LongRunPretaxIncome1980 -> SaezZucman2019DataBridge.GrowthWorksheet.pretaxIncomes1980,
      LongRunPretaxIncome2018 -> SaezZucman2019DataBridge.GrowthWorksheet.pretaxIncomes2018,
    )
  override def dataDouble = _dataDouble

  override def contextRequirements : Set[InputLabel] = 
    super.contextRequirements ++ Set(
      LongRunImpliedMarginalTaxRateCap,
      LongRunMarginalTaxRateSmoothingHalfWindow,
      LongRunMarginalTaxRateSmoothingPasses,
      LongRunIncentiveElasticity,
      LongRunBargainingWeightFunction,
      LongRunBargainingElasticity,
      NumberOfAdults,
      LongRunAveragePreTaxIncomeGrowth,
      LongRunPretaxIncome1980,
      LongRunPretaxIncome2018,
    )


  final case class PerScenarioGroupResult(afterTaxIncomePerAdult : Double, smoothedMarginalTaxRate : Double)
  final case class JointCounterfactualGroupResult(afterIncentiveIncomePerAdult : Double, bargainingWeight : Double, afterBargainingBeforeRedistributionIncomePerAdult : Double )

  final case class BehaviorResult(
    model                                   : Model,
    counterfactual                          : Counterfactual,
    baselineScenarioResult                  : Map[Group,PerScenarioGroupResult],
    counterfactualScenarioResult            : Map[Group,PerScenarioGroupResult],
    jointCounterfactualResult               : Map[Group,JointCounterfactualGroupResult],
    counterfactualIncomeAfterRedistribution : Map[Group,Double],
  )

  private def groupAfterTaxTotalIncomes( baseline : Boolean )(model : Model, runResult : Map[LabelInstrument,Map[Group,Instrument.Result]]) : Map[Group,Double] =
    def relevantTaxRate( result : Instrument.Result ) = if baseline then result.baselineTaxRate else result.reformedTaxRate
    val totalReformedTaxRates = model.groups.toIndexedSeq.map( g => (g, runResult.foldLeft(0d)( (accum,next) => accum + relevantTaxRate(next(1)(g)) )) ).toMap
    model.rebasedAverageIncome.map( (g,i) => (g, (1-totalReformedTaxRates(g)) * i) )

  private def gapSlopes( model : Model, groupAfterTaxTotalIncomes : Map[Group,Double] ) : IndexedSeq[Double] = // length: numGroups + 1
    val groupsSeq = model.groups.toIndexedSeq
    val size      = model.groups.size
    (-1 until size).map: index =>
      index match
        case -1                 => (groupAfterTaxTotalIncomes(groupsSeq(0))/model.rebasedAverageIncome(groupsSeq(0)))
        case n if n <  size - 1 => (groupAfterTaxTotalIncomes(groupsSeq(n+1))-groupAfterTaxTotalIncomes(groupsSeq(n)))/(model.rebasedAverageIncome(groupsSeq(n+1))-model.rebasedAverageIncome(groupsSeq(n)))
        case n if n == size - 1 => (groupAfterTaxTotalIncomes(groupsSeq(size-1))/model.rebasedAverageIncome(groupsSeq(size-1)))
        case n                  => throw new RuntimeException( s"Huh? n should always be between -1 and (size-1). size: $size, n: $n" )

  private def rawMarginalTaxRates( model : Model, gapSlopes : IndexedSeq[Double] ) : IndexedSeq[Double] = // length: numGroups
    val longRunImpliedMarginalTaxRateCap = model.parametersDouble(LongRunImpliedMarginalTaxRateCap)
    // this is the index from within gapSlope offset by plus one, we end up with one fewer value
    (1 to (gapSlopes.size-1)).map(i => math.min(1-(gapSlopes(i-1)+gapSlopes(i))/2,longRunImpliedMarginalTaxRateCap))

  private def smoothedMarginalTaxRates( model : Model, rawMarginalTaxRates : IndexedSeq[Double] ) : IndexedSeq[Double] =
    val longRunMarginalTaxRateSmoothingHalfWindow = model.parametersDouble(LongRunMarginalTaxRateSmoothingHalfWindow)
    val longRunMarginalTaxRateSmoothingPasses     = model.parametersDouble(LongRunMarginalTaxRateSmoothingPasses)
    Numerics.smooth(rawMarginalTaxRates, longRunMarginalTaxRateSmoothingHalfWindow.toInt, longRunMarginalTaxRateSmoothingPasses.toInt)

  private def perScenarioResults(model : Model, gatti : Map[Group,Double] ) : Map[Group,PerScenarioGroupResult] =
    val gss   = gapSlopes(model, gatti)
    val rmtrs = rawMarginalTaxRates(model, gss)
    val smtrs = model.groups.toIndexedSeq.zip(smoothedMarginalTaxRates(model,rmtrs)).toMap
    val tups  = model.groups.toIndexedSeq.map( g => Tuple2(g, PerScenarioGroupResult(gatti(g),smtrs(g))) )
    tups.toMap

  private def perScenarioResultsBaselineCounterfactual(model : Model, runResult : Map[LabelInstrument,Map[Group,Instrument.Result]]) : (Map[Group,PerScenarioGroupResult], Map[Group,PerScenarioGroupResult]) =
    val gattiBaseline       = groupAfterTaxTotalIncomes(baseline = true)(model,runResult)
    val gattiCounterfactual = groupAfterTaxTotalIncomes(baseline = false)(model,runResult)
    ( perScenarioResults(model, gattiBaseline), perScenarioResults(model, gattiCounterfactual) )

  private def jointCounterfactualGroupResult(model : Model, g : Group, baselineResult : PerScenarioGroupResult, counterfactualResult : PerScenarioGroupResult ) : JointCounterfactualGroupResult =
    val longRunIncentiveElasticity      = model.parametersDouble(LongRunIncentiveElasticity)
    val longRunBargainingWeightFunction = model.parametersOtherAs[Group => Double](LongRunBargainingWeightFunction)
    val longRunBargainingElasticity     = model.parametersDouble(LongRunBargainingElasticity)
    val weStillKeepRatio = (1-counterfactualResult.smoothedMarginalTaxRate)/(1-baselineResult.smoothedMarginalTaxRate)
    val afterIncentiveResponse = model.rebasedAverageIncome(g) * math.pow(weStillKeepRatio,longRunIncentiveElasticity)
    val bargainingWeight = longRunBargainingWeightFunction(g)
    val afterBargainingBeforeRedistributionIncome = afterIncentiveResponse * math.pow(weStillKeepRatio, longRunBargainingElasticity * bargainingWeight)
    JointCounterfactualGroupResult(afterIncentiveResponse, bargainingWeight, afterBargainingBeforeRedistributionIncome)

  def behaviorResult(model : Model, counterfactual : Counterfactual) : BehaviorResult =
    val (baselineResults, counterfactualResults) = perScenarioResultsBaselineCounterfactual(model, model.run(counterfactual))
    val jointCounterfactualResults = model.groups.toIndexedSeq.map(g => Tuple2(g, jointCounterfactualGroupResult(model, g, baselineResults(g), counterfactualResults(g)))).toMap

    val numberOfAdults = model.dataDouble(NumberOfAdults)

    val totalDollarsBargainedDown =
      jointCounterfactualResults.foldLeft(0d): (accum,next) =>
        val (g, jcgr) = next
        accum + ((jcgr.afterIncentiveIncomePerAdult - jcgr.afterBargainingBeforeRedistributionIncomePerAdult) * numberOfAdults(g))

    val pretaxIncomeFactorUnderEquitableGrowth = model.parametersDouble(LongRunAveragePreTaxIncomeGrowth)

    val pretaxIncomes1980  : Map[Group,Double] = model.dataDouble(LongRunPretaxIncome1980)
    val pretaxIncomes2018  : Map[Group,Double] = model.dataDouble(LongRunPretaxIncome2018)
    val shortfalls         : Map[Group,Double] = model.groups.toIndexedSeq.map(g => (g, math.max((pretaxIncomes1980(g) * pretaxIncomeFactorUnderEquitableGrowth) - pretaxIncomes2018(g), 0))).toMap

    val totalShortfalls = shortfalls.foldLeft(0d): (accum,next) => 
      val (g,shortfall) = next
      accum + (numberOfAdults(g) * shortfall)

    val dollarsRedistributablePerDollarOfShortfall = totalDollarsBargainedDown / totalShortfalls

    val counterfactualIncomeAfterRedistribution =
      val tups =
        model.groups.toIndexedSeq.map: g =>
          val preredistributionIncome = jointCounterfactualResults(g).afterBargainingBeforeRedistributionIncomePerAdult
          val shortfall               = shortfalls(g)
          val replacement             = shortfall * dollarsRedistributablePerDollarOfShortfall
          (g, preredistributionIncome + replacement)
      tups.toMap

    BehaviorResult( model, counterfactual, baselineResults, counterfactualResults, jointCounterfactualResults, counterfactualIncomeAfterRedistribution )


