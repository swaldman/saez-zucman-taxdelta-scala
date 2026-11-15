package taxdelta.model

import taxdelta.data.Sz2019Worksheet_databig

import scala.collection.immutable

object SaezZucman2019DataBridge:
  lazy val (groups : Groups, rowColValues : Map[Group,String] ) =
    val percentileGroups = immutable.SortedSet.newBuilder[PercentileGroup]
    val topGroups = Seq.newBuilder[TopGroup]
    val rowCols = Map.newBuilder[Group,String]
    (0 until Sz2019Worksheet_databig.numRows).foreach: i =>
      val rowColValue = Sz2019Worksheet_databig.value(i,0)
      val percentile = rowColValue.trim.toDouble
      val size       = Sz2019Worksheet_databig.value(i,1).toInt
      val threshold  = Sz2019Worksheet_databig.value(i,2).toInt
      val avg        = Sz2019Worksheet_databig.value(i,4).toInt
      if percentile < 100 then
        val g = PercentileGroup(size,threshold,avg,percentile)
        percentileGroups += g
        rowCols += Tuple2(g, rowColValue)
      else
        val g = TopGroup(size,threshold,avg)
        topGroups += g
        rowCols += Tuple2(g, rowColValue)
    val tgr = topGroups.result
    require(tgr.size == 1, s"We expect exactly one top group, encoded as a percentile greater than 100. Found ${tgr.size}: ${tgr}")
    (Groups(percentileGroups.result, tgr(0)), rowCols.result)

  lazy val wealthTaxSchedule = Schedule.empty

  private val DatabigWealthThresholds = IndexedSeq[Double](0, 1_000_000, 2_500_000, 5_000_000, 10_000_000, 25_000_000, 50_000_000, 100_000_000, 250_000_000, 500_000_000, 1_000_000_000)

  // this is fragile, we're relying on the ordering of the columns
  // and that they match DatabigWealthThresholds
  // but they are in fact known and fixed for now
  private def wealthExcessCurveForGroup(g : Group) : ExcessCurve.Default =
    val rowLabel = rowColValues(g)
    ExcessCurve.Default(DatabigWealthThresholds, Sz2019Worksheet_databig.header.filter(_.startsWith("wealth_")).map(c => Sz2019Worksheet_databig.doubleValue(rowLabel,c)))

  lazy val wealthExcessCurves : Map[Group,ExcessCurve.Default] = groups.toSeq.map( g => (g, wealthExcessCurveForGroup(g)) ).toMap

  private def intItemsForGroup(g : Group, colHeader : String) : Int =
    val rowLabel = rowColValues(g)
    Sz2019Worksheet_databig.value(rowLabel,colHeader).toInt

  private def realizedGainsForGroup(g : Group) : Int = intItemsForGroup(g, "fikgi")
  private def nationalIncomeTaxBaseForGroup(g : Group) : Int = intItemsForGroup(g, "nitaxbase")

  private def salesTaxShareOfIncomeForGroup(g : Group) : Double =
    val rowLabel = rowColValues(g)
    Sz2019Worksheet_databig.doubleValue(rowLabel,"salestax")

  lazy val realizedGains          : Map[Group,Int]    = groups.toSeq.map( g => (g, realizedGainsForGroup(g)) ).toMap
  lazy val nationalIncomeTaxBase  : Map[Group,Int]    = groups.toSeq.map( g => (g, nationalIncomeTaxBaseForGroup(g)) ).toMap
  lazy val salesTaxSharesOfIncome : Map[Group,Double] = groups.toSeq.map( g => (g, salesTaxShareOfIncomeForGroup(g)) ).toMap
