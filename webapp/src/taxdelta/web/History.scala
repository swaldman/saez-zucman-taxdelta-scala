package taxdelta.web

import taxdelta.data.Sz2019Worksheet_datahist as Datahist
import taxdelta.data.Sz2019Worksheet_history as Hist

/** Historical tax rates from the workbook, for display only. */
object History:

  /** Effective tax rates of the top 0.1% and bottom 90% by year, 1913–2018 (sheet `historical-data`,
   *  Piketty–Saez–Zucman DINA series; % of national income plus pure capital gains). */
  final case class Point(year: Int, top01: Double, bottom90: Double, bottom90WithHealth: Double)

  private def col(header: IndexedSeq[String], name: String): Int =
    val i = header.indexOf(name)
    require(i >= 0, s"no column '$name'")
    i

  lazy val points: IndexedSeq[Point] =
    val (y, t, b, bh) = (col(Hist.header, "year"), col(Hist.header, "top01"), col(Hist.header, "bottom90"), col(Hist.header, "bottom90health"))
    (0 until Hist.numRows).map(i => Point(Hist.doubleValue(i, y).toInt, Hist.doubleValue(i, t), Hist.doubleValue(i, b), Hist.doubleValue(i, bh)))

  /** Total effective tax rate by group (aligned with the model's groups) in 1950, 1960, …, 2010, 2018 (sheet
   *  `datahist`). `withHealth` counts private health insurance as a tax. */
  def ratesByGroup(year: Int, withHealth: Boolean): IndexedSeq[Double] =
    val c = col(Datahist.header, (if withHealth then "taxhealth" else "tax") + year)
    (0 until Datahist.numRows).map(Datahist.doubleValue(_, c))
