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

  private val DatabigIncomeThresholds = IndexedSeq[Double](0, 77_000, 165_000, 315_000, 400_000, 600_000, 1_000_000, 2_500_000, 5_000_000, 10_000_000, 25_000_000, 50_000_000)

  // this is fragile, we're relying on the ordering of the columns
  // and that they match DatabigWealthThresholds
  // but they are in fact known and fixed for now
  private def incomeExcessCurveForGroup(g : Group) : ExcessCurve.Default =
    val rowLabel = rowColValues(g)
    ExcessCurve.Default(DatabigIncomeThresholds, Sz2019Worksheet_databig.header.filter(_.startsWith("taxinc_")).map(c => Sz2019Worksheet_databig.doubleValue(rowLabel,c)))

  lazy val incomeExcessCurves : Map[Group,ExcessCurve.Default] = groups.toSeq.map( g => (g, incomeExcessCurveForGroup(g)) ).toMap

  private def intItemForGroup(g : Group, colHeader : String) : Int =
    val rowLabel = rowColValues(g)
    Sz2019Worksheet_databig.value(rowLabel,colHeader).toInt

  private def doubleItemForGroup(g : Group, colHeader : String) : Double =
    val rowLabel = rowColValues(g)
    Sz2019Worksheet_databig.doubleValue(rowLabel,colHeader)

  // should I rename the things that represent group averages as such
  private def fiscalCapitalGainsForGroup(g : Group)            : Int = intItemForGroup(g, "fikgi")
  private def fiscalDividendsForGroup(g : Group)               : Int = intItemForGroup(g, "fidiv")
  private def fiscalIncomeIncludingCapGainsForGroup(g : Group) : Int = intItemForGroup(g, "agi")
  private def nationalIncomeTaxBaseForGroup(g : Group)         : Int = intItemForGroup(g, "nitaxbase")
  private def numberOfAdultsForGroup(g : Group)                : Int = intItemForGroup(g, "nb")

  private def avgOrdinaryMarginalTaxRateForGroup(g : Group)                            : Double = doubleItemForGroup(g,"mtr")
  private def avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercentForGroup(g : Group) : Double = doubleItemForGroup(g,"mtrextra")
  private def corporateTaxShareOfIncomeForGroup(g : Group)                             : Double = doubleItemForGroup(g,"corptax")
  private def effectiveIncomeTaxShareOfIncomeForGroup(g : Group)                       : Double = doubleItemForGroup(g,"ditax")
  private def estateTaxShareOfIncomeForGroup(g : Group)                                : Double = doubleItemForGroup(g,"estatetax")
  private def healthTaxShareOfIncomeForGroup(g : Group)                                : Double = doubleItemForGroup(g,"healthtax")
  private def payrollTaxShareOfIncomeForGroup(g : Group)                               : Double = doubleItemForGroup(g,"govcontrib") // "Contributions for government social insurance" is NIPA term for payroll taxes: Social Security, Medicare, unemployment insurance and similar
  private def salesTaxShareOfIncomeForGroup(g : Group)                                 : Double = doubleItemForGroup(g,"salestax")
  private def propertyTaxShareOfIncomeForGroup(g : Group)                              : Double = doubleItemForGroup(g,"proptax")

  lazy val fiscalCapitalGains            : Map[Group,Int] = groups.toSeq.map( g => (g, fiscalCapitalGainsForGroup(g))            ).toMap
  lazy val fiscalDividends               : Map[Group,Int] = groups.toSeq.map( g => (g, fiscalDividendsForGroup(g))               ).toMap
  lazy val fiscalIncomeIncludingCapGains : Map[Group,Int] = groups.toSeq.map( g => (g, fiscalIncomeIncludingCapGainsForGroup(g)) ).toMap
  lazy val nationalIncomeTaxBase         : Map[Group,Int] = groups.toSeq.map( g => (g, nationalIncomeTaxBaseForGroup(g))         ).toMap
  lazy val numberOfAdults                : Map[Group,Int] = groups.toSeq.map( g => (g, numberOfAdultsForGroup(g))                ).toMap

  lazy val avgOrdinaryMarginalTaxRate                             : Map[Group,Double] = groups.toSeq.map( g => (g, avgOrdinaryMarginalTaxRateForGroup(g))                            ).toMap
  lazy val avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent  : Map[Group,Double] = groups.toSeq.map( g => (g, avgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercentForGroup(g)) ).toMap
  lazy val corporateTaxShareOfIncome                              : Map[Group,Double] = groups.toSeq.map( g => (g, corporateTaxShareOfIncomeForGroup(g))                             ).toMap
  lazy val estateTaxShareOfIncome                                 : Map[Group,Double] = groups.toSeq.map( g => (g, estateTaxShareOfIncomeForGroup(g))                                ).toMap
  lazy val healthTaxShareOfIncome                                 : Map[Group,Double] = groups.toSeq.map( g => (g, healthTaxShareOfIncomeForGroup(g))                                ).toMap
  lazy val payrollTaxShareOfIncome                                : Map[Group,Double] = groups.toSeq.map( g => (g, payrollTaxShareOfIncomeForGroup(g))                               ).toMap
  lazy val salesTaxShareOfIncome                                  : Map[Group,Double] = groups.toSeq.map( g => (g, salesTaxShareOfIncomeForGroup(g))                                 ).toMap
  lazy val effectiveIncomeTaxShareOfIncome                        : Map[Group,Double] = groups.toSeq.map( g => (g, effectiveIncomeTaxShareOfIncomeForGroup(g))                       ).toMap
  lazy val propertyTaxShareOfIncome                               : Map[Group,Double] = groups.toSeq.map( g => (g, propertyTaxShareOfIncomeForGroup(g))                              ).toMap

  lazy val totalNumberOfAdults = numberOfAdults.valuesIterator.foldLeft(0L)( _ + _ )

  lazy val totalCapitalGains =
    fiscalCapitalGains.iterator.foldLeft(0L): (accum, next) =>
        val (group, averageGain) = next
        accum + numberOfAdults(group).toLong * averageGain

  lazy val totalDividends =
      fiscalDividends.iterator.foldLeft(0L): (accum, next) =>
        val (group, averageDividends) = next
        accum + numberOfAdults(group).toLong * averageDividends

  lazy val totalGainsAndDividends = totalCapitalGains + totalDividends
