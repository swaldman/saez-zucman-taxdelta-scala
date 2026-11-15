package taxdelta.model

import taxdelta.data.Sz2019Worksheet_databig as Databig
import SaezZucman2019Context.*

/** Checks the Saez-Zucman 2019 instruments against the workbook's formulas (`calculatorbig`, columns Q, R, S),
 *  recomputed here independently, straight from the `databig` columns.
 *
 *  The reference uses the workbook's own rate denominator, the `denom` column. The model rebuilds it as
 *  `avg + 0.366 × fikgi`, which differs from `denom` by up to about 1e-4 relative (0.366 is rounded), so
 *  rates are compared to an absolute tolerance of 1e-5 rather than to machine precision.
 */
class SaezZucman2019Test extends munit.FunSuite:

  private val RateTolerance = 1e-5

  private val groups = model.groups.toSeq

  private def col(g: Group, name: String): Double = Databig.doubleValue(SaezZucman2019DataBridge.rowColValues(g), name)
  private def label(g: Group): String = SaezZucman2019DataBridge.rowColValues(g).trim
  private def denom(g: Group): Double = col(g, "denom")
  private def adults(g: Group): Double = col(g, "nb")

  // ---- Workbook reference ------------------------------------------------------------------------------------

  private val wealthGrid = IndexedSeq[Double](0, 1e6, 2.5e6, 5e6, 10e6, 25e6, 50e6, 100e6, 250e6, 500e6, 1e9, 2.5e9, 5e9, 10e9)
  private val wealthCols = IndexedSeq("wealth_0m", "wealth_1m", "wealth_2p5m", "wealth_5m", "wealth_10m", "wealth_25m",
    "wealth_50m", "wealth_100m", "wealth_250m", "wealth_500m", "wealth_1000m")

  /** Wealth excess at each grid threshold, as the workbook has it: above $1bn, imputed for the top 400 only
   *  (calculator!AQ22:AS22), zero for everyone else. */
  private def workbookWealthExcess(g: Group): Map[Double, Double] =
    val observed = wealthCols.map(col(g, _))
    val above1bn = observed.last
    val imputed = g match
      case _: TopGroup => IndexedSeq(0.9, 0.6, 0.45).map(_ * above1bn)
      case _           => IndexedSeq(0.0, 0.0, 0.0)
    wealthGrid.zip(observed ++ imputed).toMap

  /** Σ_k (r_k − r_{k−1}) · X(t_k), with the workbook's excesses at grid thresholds. */
  private def workbookWealthTaxRate(s: Schedule, evasion: Double)(g: Group): Double =
    val ex = workbookWealthExcess(g)
    val rates = 0.0 +: s.rates
    val tax = s.thresholds.indices.map(k => (rates(k + 1) - rates(k)) * ex(s.thresholds(k))).sum
    (1 - evasion) * tax / denom(g)

  private def workbookNationalIncomeTaxRate(rate: Double)(g: Group): Double =
    rate * col(g, "nitaxbase") / denom(g)

  /** calculatorbig!F137: the income-weighted mean of the current sales-tax rate. */
  private val workbookAverageSalesTaxRate =
    groups.map(g => adults(g) * denom(g) * col(g, "salestax")).sum / groups.map(g => adults(g) * denom(g)).sum

  private def workbookVatRate(v: Double)(g: Group): Double =
    (v / (1 + v)) * 0.4 * (0.75 * col(g, "salestax") / workbookAverageSalesTaxRate + 0.25)

  // ---- Counterfactuals ---------------------------------------------------------------------------------------

  /** The workbook's approximation of the Sanders wealth tax (its thresholds are snapped to the data grid:
   *  1% from $25m, not $32m), plus the financial transaction tax surcharge (0.05% below $1m, 0.1% above). */
  private val sandersSchedule = Schedule(
    IndexedSeq(0, 1e6, 25e6, 50e6, 250e6, 500e6, 1e9, 2.5e9, 5e9, 10e9),
    IndexedSeq(0.0005, 0.001, 0.011, 0.021, 0.031, 0.041, 0.051, 0.061, 0.071, 0.081)
  )
  private val sanders = Counterfactual(
    leversDouble = Map(WealthTaxEvasionRate -> 0.15),
    leversSchedule = Map(WealthTaxSchedule -> sandersSchedule)
  )
  private val nit7 = Counterfactual(leversDouble = Map(NationalIncomeTaxRate -> 0.07))
  private val vat10 = Counterfactual(leversDouble = Map(ValueAddedTaxRate -> 0.10))

  // ---- Helpers -----------------------------------------------------------------------------------------------

  private def reformed(cf: Counterfactual, instrument: LInstrument): Map[Group, Double] =
    model.run(cf)(instrument).map((g, r) => (g, r.reformedTaxRate))

  private def assertEveryGroup(actual: Map[Group, Double], expected: Group => Double)(using munit.Location): Unit =
    for g <- groups do
      assertEqualsDouble(actual(g), expected(g), RateTolerance, s"group ${label(g)}")

  /** Income-weighted aggregate rate (total tax / total income), with the workbook's weights n · denom. */
  private def aggregate(rate: Group => Double): Double =
    groups.map(g => rate(g) * adults(g) * denom(g)).sum / groups.map(g => adults(g) * denom(g)).sum

  // ---- Data and parameters -----------------------------------------------------------------------------------

  test("the model has 128 groups: 127 percentile groups and the top 400"):
    assertEquals(groups.length, 128)
    assertEquals(label(model.groups.topGroup), "400")

  test("rebased income approximates the workbook's denom to within 2e-4 relative"):
    for g <- groups do
      val rel = math.abs(model.rebasedAverageIncome(g) / denom(g) - 1)
      assert(rel < 2e-4, s"group ${label(g)}: rebased ${model.rebasedAverageIncome(g)} vs denom ${denom(g)}")

  test("the hard-coded average sales-tax rate (calculatorbig!F137) matches the data"):
    assertEqualsDouble(model.parametersDouble(AverageSalesTaxShareOfIncome), workbookAverageSalesTaxRate, 1e-12)

  test("above $1bn, wealth excesses are imputed for the top 400 only"):
    for g <- groups do
      val curve = model.dataExcessCurve(WealthExcessCurve)(g)
      val ex = workbookWealthExcess(g)
      for t <- wealthGrid do
        assertEqualsDouble(curve.excessFromThreshold(t), ex(t), 1e-6, s"group ${label(g)}, threshold $t")

  // ---- Baseline ----------------------------------------------------------------------------------------------

  test("instruments run in order and all report results"):
    val out = model.run(Counterfactual())
    assertEquals(out.keySet, Set[LInstrument](NationalIncomeTax, ValueAddedTax, WealthTax))
    assertEquals(model.instruments.map(_._1), List[LInstrument](NationalIncomeTax, ValueAddedTax, WealthTax))

  test("the baseline levies none of the three new taxes, and an empty counterfactual changes nothing"):
    for (lbl, byGroup) <- model.run(Counterfactual()); g <- groups do
      val r = byGroup(g)
      assertEqualsDouble(r.baselineTaxRate, 0.0, 0.0, s"$lbl baseline, group ${label(g)}")
      assertEqualsDouble(r.reformedTaxRate, 0.0, 0.0, s"$lbl reformed, group ${label(g)}")
      assertEqualsDouble(r.deltaTaxRate, 0.0, 0.0, s"$lbl delta, group ${label(g)}")

  // ---- Wealth tax (column Q) ---------------------------------------------------------------------------------

  test("Warren wealth tax: the worked example (methodology §10.1), P99.997–99.998"):
    val g = groups.find(label(_) == "99.997").get
    assertEqualsDouble(reformed(WarrenCounterfactual, WealthTax)(g), 0.13737, RateTolerance)

  test("Warren wealth tax: every group"):
    val schedule = WarrenCounterfactual.leversSchedule(WealthTaxSchedule)
    assertEveryGroup(reformed(WarrenCounterfactual, WealthTax), workbookWealthTaxRate(schedule, 0.15))

  test("Sanders wealth tax (brackets above $1bn): every group"):
    assertEveryGroup(reformed(sanders, WealthTax), workbookWealthTaxRate(sandersSchedule, 0.15))

  test("Sanders wealth tax: aggregate rate"):
    val r = reformed(sanders, WealthTax)
    assertEqualsDouble(aggregate(r), aggregate(workbookWealthTaxRate(sandersSchedule, 0.15)), 1e-6)

  // ---- National income tax (column R) ------------------------------------------------------------------------

  test("national income tax at 7% (the book plan): every group"):
    assertEveryGroup(reformed(nit7, NationalIncomeTax), workbookNationalIncomeTaxRate(0.07))

  test("national income tax at 7%: aggregate rate"):
    assertEqualsDouble(aggregate(reformed(nit7, NationalIncomeTax)), aggregate(workbookNationalIncomeTaxRate(0.07)), 1e-6)

  // ---- VAT (column S) ----------------------------------------------------------------------------------------

  test("VAT at 10%: every group"):
    assertEveryGroup(reformed(vat10, ValueAddedTax), workbookVatRate(0.10))

  test("VAT at 10%: the distribution profile averages to 1, so the aggregate rate is v/(1+v) × 0.4"):
    assertEqualsDouble(aggregate(reformed(vat10, ValueAddedTax)), (0.10 / 1.10) * 0.4, 1e-9)

  test("VAT at 10%: regressive, from about 8.8% of income at the bottom to about 2.2% for the top 400"):
    val r = reformed(vat10, ValueAddedTax)
    assertEqualsDouble(r(groups.head), 0.0883, 1e-4)
    assertEqualsDouble(r(model.groups.topGroup), 0.0222, 1e-4)
