package taxdelta.model

import taxdelta.data.Sz2019Worksheet_databig as Databig
import taxdelta.data.Sz2019Worksheet_calculatorbig as Calculatorbig

/** Checks the Saez-Zucman 2019 instruments against the workbook's formulas (`calculatorbig`, columns K/Y, L/T, Q, R, S, U, V),
 *  recomputed here independently, straight from the `databig` columns.
 *
 *  The reference uses the workbook's own rate denominator, the `denom` column. The model rebuilds it as
 *  `avg + 0.366 × fikgi`, which differs from `denom` by up to about 1e-4 relative (0.366 is rounded), so
 *  rates are compared to an absolute tolerance of 1e-5 rather than to machine precision.
 */
class SaezZucman2019Test extends munit.FunSuite:

  private val RateTolerance = 1e-5

  private val ctx = SaezZucman2019Context()
  import ctx.*

  /** The same model, with private health premiums counted as a tax (interface C16 = "yes"). */
  private val healthShown = SaezZucman2019Context(Map("CountPrivateHealthPremiumsAsTax" -> true))

  private val groups = mainModel.groups.toSeq

  private def col(g: Group, name: String): Double = Databig.doubleValue(SaezZucman2019DataBridge.rowColValues(g), name)
  private def label(g: Group): String = SaezZucman2019DataBridge.rowColValues(g).trim
  private def denom(g: Group): Double = col(g, "denom")
  private def adults(g: Group): Double = col(g, "nb")
  private def group(lbl: String): Group = groups.find(label(_) == lbl).get

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

  /** calculatorbig!L8 = databig!N · [interface C16 = "yes"]; T8 = L8 · (1 − reduction). */
  private def workbookHealthRate(shown: Boolean, reduction: Double)(g: Group): Double =
    (if shown then col(g, "healthtax") else 0.0) * (1 - reduction)

  /** calculatorbig!U8 = (topRate · J8 / 0.4) · (1 − avoidance) / (1 − 0.6), with J8 = databig!M (estatetax),
   *  topRate = interface D40 and avoidance = interface D39. */
  private def workbookEstateRate(topRate: Double, avoidance: Double)(g: Group): Double =
    (topRate * col(g, "estatetax") / 0.4) * (1 - avoidance) / (1 - 0.6)

  /** calculatorbig!V8 = (1 − R8 − S8) · H8 · R144 / Q144, with H8 = databig!L (corptax), R8 and S8 the group's
   *  proposed national income tax and VAT rates, Q144 = 210 + 8 and R144 = 1308 · rate + (96 if better taxation
   *  of multinationals, else 8), all in $bn. */
  private def workbookCorporateRate(rate: Double, multinationals: Boolean, nit: Double = 0.0, vat: Double = 0.0)(g: Group): Double =
    val kc = 1308 * (210.0 / 1308) + 1308 * (8.0 / 1308)
    val kp = 1308 * rate + 1308 * ((if multinationals then 96.0 else 8.0) / 1308)
    val crowdOut = 1 - workbookNationalIncomeTaxRate(nit)(g) - (if vat == 0.0 then 0.0 else workbookVatRate(vat)(g))
    crowdOut * col(g, "corptax") * kp / kc

  /** calculatorbig!K8 = databig!J (govcontrib); Y8 = K8 + C148 · 0.55 · databig!N. */
  private def workbookPayrollRate(headTax: Boolean)(g: Group): Double =
    col(g, "govcontrib") + (if headTax then 0.55 * col(g, "healthtax") else 0.0)

  // ---- Counterfactuals ---------------------------------------------------------------------------------------

  /** The workbook's approximation of the Sanders wealth tax (its thresholds are snapped to the data grid:
   *  1% from $25m, not $32m), plus the financial transaction tax surcharge (0.05% below $1m, 0.1% above). */
  private val sandersSchedule = Schedule(
    IndexedSeq(0, 1e6, 25e6, 50e6, 250e6, 500e6, 1e9, 2.5e9, 5e9, 10e9),
    IndexedSeq(0.0005, 0.001, 0.011, 0.021, 0.031, 0.041, 0.051, 0.061, 0.071, 0.081)
  )
  private val sanders = Counterfactual(
    leversDouble = Map(WealthTaxEvasionRate -> 0.15),
    leversOther  = Map(WealthTaxSchedule -> sandersSchedule)
  )
  private val nit7 = Counterfactual(leversDouble = Map(NationalIncomeTaxRate -> 0.07))
  private val vat10 = Counterfactual(leversDouble = Map(ValueAddedTaxRate -> 0.10))

  // ---- Helpers -----------------------------------------------------------------------------------------------

  private def reformed(cf: Counterfactual, instrument: LInstrument): Map[Group, Double] =
    mainModel.run(cf)(instrument).map((g, r) => (g, r.reformedTaxRate))

  private def baseline(instrument: LInstrument): Map[Group, Double] =
    mainModel.run(Counterfactual())(instrument).map((g, r) => (g, r.baselineTaxRate))

  private def delta(cf: Counterfactual, instrument: LInstrument): Map[Group, Double] =
    mainModel.run(cf)(instrument).map((g, r) => (g, r.deltaTaxRate))

  private def assertEveryGroup(actual: Map[Group, Double], expected: Group => Double)(using munit.Location): Unit =
    for g <- groups do
      assertEqualsDouble(actual(g), expected(g), RateTolerance, s"group ${label(g)}")

  /** Income-weighted aggregate rate (total tax / total income), with the workbook's weights n · denom. */
  private def aggregate(rate: Group => Double): Double =
    groups.map(g => rate(g) * adults(g) * denom(g)).sum / groups.map(g => adults(g) * denom(g)).sum

  // ---- Data and parameters -----------------------------------------------------------------------------------

  test("the model has 128 groups: 127 percentile groups and the top 400"):
    assertEquals(groups.length, 128)
    assertEquals(label(mainModel.groups.topGroup), "400")

  test("rebased income approximates the workbook's denom to within 2e-4 relative"):
    for g <- groups do
      val rel = math.abs(mainModel.rebasedAverageIncome(g) / denom(g) - 1)
      assert(rel < 2e-4, s"group ${label(g)}: rebased ${mainModel.rebasedAverageIncome(g)} vs denom ${denom(g)}")

  test("the hard-coded average sales-tax rate (calculatorbig!F137) matches the data"):
    assertEqualsDouble(mainModel.parametersDouble(AverageSalesTaxShareOfIncome), workbookAverageSalesTaxRate, 1e-12)

  test("above $1bn, wealth excesses are imputed for the top 400 only"):
    for g <- groups do
      val curve = mainModel.dataOtherAs[ExcessCurve](WealthExcessCurve)(g)
      val ex = workbookWealthExcess(g)
      for t <- wealthGrid do
        assertEqualsDouble(curve.excessFromThreshold(t), ex(t), 1e-6, s"group ${label(g)}, threshold $t")

  // ---- Baseline ----------------------------------------------------------------------------------------------

  test("instruments run in order and all report results"):
    val all = List[LInstrument](EstateTax, HealthInsuranceTax, NationalIncomeTax, PayrollTax, PropertyTax, SalesAndExciseTax, ValueAddedTax, WealthTax, CorporateTax, IndividualIncomeTax)
    assertEquals(mainModel.run(Counterfactual()).keySet, all.toSet)
    assertEquals(mainModel.instruments.map(_._1), all)

  test("the baseline levies none of the three new taxes"):
    for lbl <- List[LInstrument](NationalIncomeTax, ValueAddedTax, WealthTax); (g, r) <- mainModel.run(Counterfactual())(lbl) do
      assertEqualsDouble(r.baselineTaxRate, 0.0, 0.0, s"$lbl baseline, group ${label(g)}")

  test("an empty counterfactual changes nothing"):
    for (lbl, byGroup) <- mainModel.run(Counterfactual()); (g, r) <- byGroup do
      assertEqualsDouble(r.reformedTaxRate, r.baselineTaxRate, 0.0, s"$lbl, group ${label(g)}")
      assertEqualsDouble(r.deltaTaxRate, 0.0, 0.0, s"$lbl delta, group ${label(g)}")

  test("parameter overrides: unknown keys and wrongly typed values are rejected, known ones are applied"):
    intercept[IllegalArgumentException](SaezZucman2019Context(Map("NoSuchParameter" -> 1.0)))
    intercept[IllegalArgumentException](SaezZucman2019Context(Map("ConsumerIncidenceShare" -> "0.7")))
    intercept[IllegalArgumentException](SaezZucman2019Context(Map("CountPrivateHealthPremiumsAsTax" -> 1.0)))
    intercept[IllegalArgumentException](SaezZucman2019Context(Map("ConsumerIncidenceShare" -> true)))
    assertEquals(mainModel.parametersBoolean(CountPrivateHealthPremiumsAsTax), false)
    assertEquals(healthShown.mainModel.parametersBoolean(healthShown.CountPrivateHealthPremiumsAsTax), true)
    val c = SaezZucman2019Context(Map("ConsumerIncidenceShare" -> 0.7))
    assertEquals(c.mainModel.parametersDouble(c.ConsumerIncidenceShare), 0.7)

  test("parameter overrides: integer and other numeric values are accepted for double parameters on both platforms"):
    for v <- List[Any](1, 1L, 1.toShort, 1.toByte, 1.0f, BigInt(1), BigDecimal(1)) do
      val c = SaezZucman2019Context(Map("EmployerShareOfPrivateHealthPremiums" -> v))
      assertEquals(c.mainModel.parametersDouble(c.EmployerShareOfPrivateHealthPremiums), 1.0, s"value $v: ${v.getClass}")

  test("defaults match the saved workbook: premiums not shown as a tax, head-tax share 0.55"):
    assertEquals(mainModel.parametersBoolean(CountPrivateHealthPremiumsAsTax), false)
    assertEquals(mainModel.parametersDouble(EmployerShareOfPrivateHealthPremiums), 0.55)

  // ---- Wealth tax (column Q) ---------------------------------------------------------------------------------

  test("Warren wealth tax: the worked example (methodology §10.1), P99.997–99.998"):
    val g = groups.find(label(_) == "99.997").get
    assertEqualsDouble(reformed(counterfactuals.Warren, WealthTax)(g), 0.13737, RateTolerance)

  test("Warren wealth tax: every group"):
    val schedule = counterfactuals.Warren.leversOtherAs[Schedule](WealthTaxSchedule)
    assertEveryGroup(reformed(counterfactuals.Warren, WealthTax), workbookWealthTaxRate(schedule, 0.15))

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

  // ---- Sales and excise taxes (column X) ---------------------------------------------------------------------

  /** calculatorbig!X8 = F8 · (1 − C146), with F8 = databig!H (salestax) and C146 = interface D15. */
  private def workbookSalesRate(reduction: Double)(g: Group): Double = col(g, "salestax") * (1 - reduction)
  private def salesReduction(r: Double) = Counterfactual(leversDouble = Map(SalesAndExciseTaxReduction -> r))

  test("sales and excise: the baseline lever is no reduction (interface C15), and the baseline is salestax exactly"):
    assertEquals(mainModel.baseline.leversDouble(SalesAndExciseTaxReduction), 0.0)
    for (g, b) <- baseline(SalesAndExciseTax) do assertEqualsDouble(b, col(g, "salestax"), 0.0, s"group ${label(g)}")

  test("sales and excise: the book plan's 50% reduction (interface G15) halves every group"):
    assertEveryGroup(reformed(salesReduction(0.5), SalesAndExciseTax), workbookSalesRate(0.5))
    for (g, r) <- reformed(salesReduction(0.5), SalesAndExciseTax) do
      assertEqualsDouble(r, col(g, "salestax") / 2, 1e-17, s"group ${label(g)}")

  test("sales and excise: a negative reduction scales every group up proportionally, keeping the current profile"):
    val r = reformed(salesReduction(-0.2), SalesAndExciseTax)
    for g <- groups if col(g, "salestax") > 0 do
      assertEqualsDouble(r(g) / col(g, "salestax"), 1.2, 1e-12, s"group ${label(g)}")

  test("sales and excise: the aggregate is F137 · (1 − reduction)"):
    for red <- List(0.0, 0.5, 1.0, -0.2) do
      assertEqualsDouble(aggregate(reformed(salesReduction(red), SalesAndExciseTax)), mainModel.parametersDouble(AverageSalesTaxShareOfIncome) * (1 - red), 1e-12, s"reduction $red")

  test("sales and excise: no other instrument depends on the reduction (no crowd-out, unlike the VAT)"):
    val base = mainModel.run(Counterfactual())
    val cut  = mainModel.run(salesReduction(0.5))
    for lbl <- base.keySet - SalesAndExciseTax; g <- groups do
      assertEqualsDouble(cut(lbl)(g).reformedTaxRate, base(lbl)(g).reformedTaxRate, 0.0, s"$lbl, group ${label(g)}")

  test("sales and excise: declares its inputs, so a model without the salestax data or the reduction lever is rejected"):
    intercept[IllegalArgumentException](mainModel.copy(dataDouble = mainModel.dataDouble - SalesTaxShareOfIncome))
    intercept[IllegalArgumentException](mainModel.copy(baseline = mainModel.baseline.copy(leversDouble = mainModel.baseline.leversDouble - SalesAndExciseTaxReduction)))

  // ---- VAT (column S) ----------------------------------------------------------------------------------------

  test("VAT at 10%: every group"):
    assertEveryGroup(reformed(vat10, ValueAddedTax), workbookVatRate(0.10))

  test("VAT at 10%: the distribution profile averages to 1, so the aggregate rate is v/(1+v) × 0.4"):
    assertEqualsDouble(aggregate(reformed(vat10, ValueAddedTax)), (0.10 / 1.10) * 0.4, 1e-9)

  test("VAT at 10%: regressive, from about 8.8% of income at the bottom to about 2.2% for the top 400"):
    val r = reformed(vat10, ValueAddedTax)
    assertEqualsDouble(r(groups.head), 0.0883, 1e-4)
    assertEqualsDouble(r(mainModel.groups.topGroup), 0.0222, 1e-4)

  // ---- Private health insurance (columns L, T) ---------------------------------------------------------------

  private def shownRun(cf: healthShown.Counterfactual): Map[Group, healthShown.Instrument.Result] =
    healthShown.mainModel.run(cf)(healthShown.HealthInsuranceTax)

  test("health insurance, shown as a tax: the baseline is healthtax (column L) in every group"):
    assertEveryGroup(shownRun(healthShown.Counterfactual()).map((g, r) => (g, r.baselineTaxRate)), workbookHealthRate(shown = true, reduction = 0.0))

  test("health insurance, shown: Warren's single-payer removes all private premiums (column T = 0)"):
    val run = shownRun(healthShown.counterfactuals.Warren)
    assertEveryGroup(run.map((g, r) => (g, r.reformedTaxRate)), workbookHealthRate(shown = true, reduction = 1.0))
    assertEveryGroup(run.map((g, r) => (g, r.deltaTaxRate)), g => -col(g, "healthtax"))

  test("health insurance, shown: a partial reduction scales premiums by (1 − reduction)"):
    val cf = healthShown.Counterfactual(leversDouble = Map(healthShown.ReductionOfEmployerSponsoredPrivateHealthPremiums -> 0.4))
    assertEveryGroup(shownRun(cf).map((g, r) => (g, r.reformedTaxRate)), workbookHealthRate(shown = true, reduction = 0.4))

  test("health insurance, shown: premiums peak in the middle (about 12.9% at P50) and are negligible for the top 400"):
    val b = shownRun(healthShown.Counterfactual()).map((g, r) => (g, r.baselineTaxRate))
    assertEqualsDouble(b(group("50")), 0.1287, 1e-4)
    assert(b(mainModel.groups.topGroup) < 1e-4)

  test("health insurance, not shown (the default, interface C16 = \"no\"): zero before and after any reform"):
    for (g, r) <- mainModel.run(counterfactuals.Warren)(HealthInsuranceTax) do
      assertEqualsDouble(r.baselineTaxRate, 0.0, 0.0, s"group ${label(g)}")
      assertEqualsDouble(r.reformedTaxRate, 0.0, 0.0, s"group ${label(g)}")

  // ---- Payroll (columns K, Y) --------------------------------------------------------------------------------

  test("payroll: the baseline is govcontrib (column K) in every group, P0–1 matching the cached K8"):
    val b = baseline(PayrollTax)
    assertEveryGroup(b, workbookPayrollRate(headTax = false))
    assertEqualsDouble(b(groups.head), 0.12049213051795959, 1e-15)

  test("payroll: Warren's employer head tax adds 0.55 × healthtax in every group (column Y)"):
    assertEveryGroup(reformed(counterfactuals.Warren, PayrollTax), workbookPayrollRate(headTax = true))
    assertEveryGroup(delta(counterfactuals.Warren, PayrollTax), g => 0.55 * col(g, "healthtax"))

  test("payroll: the head tax is about 7 points at P50 and negligible for the top 400"):
    val d = delta(counterfactuals.Warren, PayrollTax)
    assertEqualsDouble(d(group("50")), 0.0708, 1e-4)
    assert(d(mainModel.groups.topGroup) < 1e-4)

  test("payroll: plans without the head tax leave payroll unchanged"):
    assertEveryGroup(delta(nit7, PayrollTax), _ => 0.0)
    assertEveryGroup(delta(sanders, PayrollTax), _ => 0.0)

  test("Warren, premiums shown: health plus payroll together cut the middle's rate by 0.45 × healthtax"):
    val h = shownRun(healthShown.counterfactuals.Warren).map((g, r) => (g, r.deltaTaxRate))
    val p = healthShown.mainModel.run(healthShown.counterfactuals.Warren)(healthShown.PayrollTax).map((g, r) => (g, r.deltaTaxRate))
    assertEveryGroup(groups.map(g => (g, h(g) + p(g))).toMap, g => -0.45 * col(g, "healthtax"))

  /* As in the workbook, Y8 adds the head tax whatever interface C16 says, so with premiums not shown as a tax,
   * Warren's head tax appears without the premiums it replaces (methodology §8.8, questions §7.3). */
  test("payroll: the head tax applies whether or not premiums are shown as a tax, as in the workbook"):
    assertEveryGroup(reformed(counterfactuals.Warren, PayrollTax), workbookPayrollRate(headTax = true))
    val shown = healthShown.mainModel.run(healthShown.counterfactuals.Warren)(healthShown.PayrollTax)
    assertEveryGroup(shown.map((g, r) => (g, r.reformedTaxRate)), workbookPayrollRate(headTax = true))

  test("payroll: the head tax scales with the EmployerShareOfPrivateHealthPremiums parameter"):
    val c = SaezZucman2019Context(Map("EmployerShareOfPrivateHealthPremiums" -> 1.0))
    for (g, r) <- c.mainModel.run(c.counterfactuals.Warren)(c.PayrollTax) do
      assertEqualsDouble(r.deltaTaxRate, col(g, "healthtax"), 1e-15, s"group ${label(g)}")

  // ---- Estate tax (column U) ---------------------------------------------------------------------------------

  /** Interface rows 40 (top rate) and 39 (avoidance) for the presets with estate-tax changes. */
  private def estate(topRate: Double, avoidance: Double) = Counterfactual(
    leversDouble = Map(EstateTaxProposedTopRate -> topRate, EstateTaxAnticipatedAvoidanceRate -> avoidance)
  )
  private val estateObama   = estate(0.40, 0.45)
  private val estateBook    = estate(0.40, 0.20)
  private val estateWarren  = estate(0.55, 0.40) // also Sanders
  private val estateButtigieg = estate(0.55, 0.30)

  test("estate tax: current law is a 40% top rate with 60% avoidance (interface C40, C39)"):
    assertEquals(mainModel.parametersDouble(EstateTaxCurrentStatutoryTopRate), 0.4)
    assertEquals(mainModel.parametersDouble(EstateTaxCurrentAvoidanceRate), 0.6)

  test("estate tax: the baseline is estatetax (column J) in every group"):
    assertEveryGroup(baseline(EstateTax), g => col(g, "estatetax"))
    assertEveryGroup(baseline(EstateTax), workbookEstateRate(0.4, 0.6))

  test("estate tax: the baseline aggregate matches the cached calculatorbig!J137"):
    assertEqualsDouble(aggregate(baseline(EstateTax)), 1.2568111096428183e-3, 1e-15)

  test("estate tax: every preset matches column U in every group"):
    for (name, cf, rate, avoid) <- List(
        ("Obama", estateObama, 0.40, 0.45), ("book", estateBook, 0.40, 0.20),
        ("Warren/Sanders", estateWarren, 0.55, 0.40), ("Buttigieg", estateButtigieg, 0.55, 0.30)) do
      val r = reformed(cf, EstateTax)
      for g <- groups do
        assertEqualsDouble(r(g), workbookEstateRate(rate, avoid)(g), 1e-15, s"$name, group ${label(g)}")

  test("estate tax: the reform scales every group by (rate / 0.4) × ((1 − avoidance) / 0.4); Warren's is 2.0625"):
    val b = baseline(EstateTax)
    val r = reformed(estateWarren, EstateTax)
    for g <- groups if b(g) > 0 do
      assertEqualsDouble(r(g) / b(g), 2.0625, 1e-12, s"group ${label(g)}")

  test("estate tax: the Obama preset's aggregate matches the cached calculatorbig!U137 (the saved active plan)"):
    assertEqualsDouble(aggregate(reformed(estateObama, EstateTax)), 1.7281152757588751e-3, 1e-15)

  test("estate tax: zero below P99.3; 0.67% of income at P99.9 and 1.11% for the top 400"):
    val b = baseline(EstateTax)
    for g <- groups.takeWhile(label(_) != "99.3") do
      assertEqualsDouble(b(g), 0.0, 0.0, s"group ${label(g)}")
    assert(b(group("99.3")) > 0)
    assertEqualsDouble(b(group("99.9")), 0.00673, 1e-5)
    assertEqualsDouble(b(mainModel.groups.topGroup), 0.01114, 1e-5)

  test("estate tax: overriding the current-law parameters moves the baseline levers with them"):
    val c = SaezZucman2019Context(Map("EstateTaxCurrentStatutoryTopRate" -> 0.5, "EstateTaxCurrentAvoidanceRate" -> 0.5))
    for (g, r) <- c.mainModel.run(c.Counterfactual())(c.EstateTax) do
      assertEqualsDouble(r.baselineTaxRate, col(g, "estatetax"), 1e-15, s"group ${label(g)}")
      assertEqualsDouble(r.deltaTaxRate, 0.0, 0.0, s"group ${label(g)}")

  test("estate tax: the Warren preset (counterfactuals.Warren) carries her estate levers, 55% top rate and 40% avoidance"):
    assertEveryGroup(reformed(counterfactuals.Warren, EstateTax), workbookEstateRate(0.55, 0.40))

  // ---- Corporate tax (column V) ------------------------------------------------------------------------------

  /** Interface rows 43 (effective rate on US profits) and 44 (better taxation of multinationals), plus the
   *  presets' national income tax (row 10); no preset has a VAT. */
  private def corporate(rate: Double, multinationals: Boolean, nit: Double = 0.0, vat: Double = 0.0) = Counterfactual(
    leversDouble  = Map(CorporateTaxProposedEffectiveRate -> rate, NationalIncomeTaxRate -> nit, ValueAddedTaxRate -> vat),
    leversBoolean = Map(CorporateTaxDeployStrongerEnforcement -> multinationals)
  )
  private val corporatePresets = List(
    ("Obama",     0.25, false, 0.00),
    ("book",      0.30, true,  0.07),
    ("Biden",     0.21, false, 0.00),
    ("Buttigieg", 0.25, true,  0.00),
    ("Sanders",   0.25, true,  0.02),
    ("Warren",    0.30, true,  0.00),
  )

  test("corporate tax: current law is $210bn on $1,308bn of US profits plus $8bn on foreign profits; $96bn if reformed"):
    assertEquals(mainModel.parametersDouble(CorporateTaxCurrentDomesticProfits), 1308.0)
    assertEquals(mainModel.parametersDouble(CorporateTaxCurrentDomesticReceipts), 210.0)
    assertEquals(mainModel.parametersDouble(CorporateTaxCurrentForeignReceipts), 8.0)
    assertEquals(mainModel.parametersDouble(CorporateTaxForeignReceiptsUnderStrongerEnforcement), 96.0)
    assertEquals(mainModel.baseline.leversDouble(CorporateTaxProposedEffectiveRate), 210.0 / 1308) // interface C43
    assertEquals(mainModel.baseline.leversBoolean(CorporateTaxDeployStrongerEnforcement), false)  // interface C44

  test("corporate tax: the baseline is corptax (column H) in every group, exactly"):
    assertEveryGroup(baseline(CorporateTax), g => col(g, "corptax"))
    for (g, b) <- baseline(CorporateTax) do assertEqualsDouble(b, col(g, "corptax"), 1e-17, s"group ${label(g)}")

  test("corporate tax: the baseline aggregate matches the cached calculatorbig!H137"):
    assertEqualsDouble(aggregate(baseline(CorporateTax)), 1.2546739521496047e-2, 1e-15)

  test("corporate tax: Warren's preset (counterfactuals.Warren) has a 30% rate and better taxation of multinationals"):
    assertEveryGroup(reformed(counterfactuals.Warren, CorporateTax), workbookCorporateRate(0.30, true))

  test("corporate tax: declares all its inputs, so a model without the corptax data is rejected"):
    intercept[IllegalArgumentException](mainModel.copy(dataDouble = mainModel.dataDouble - CorporateTaxShareOfIncome))

  test("corporate tax: runs after the national income tax and VAT, and a model ordering it earlier is rejected"):
    val order = mainModel.instruments.map(_._1)
    assert(order.indexOf(CorporateTax) > order.indexOf(NationalIncomeTax))
    assert(order.indexOf(CorporateTax) > order.indexOf(ValueAddedTax))
    val corp = mainModel.instruments.find(_._1 == CorporateTax).get
    intercept[IllegalArgumentException](mainModel.copy(instruments = corp :: mainModel.instruments.filterNot(_ == corp)))

  test("corporate tax: every preset matches column V in every group (with the book's and Sanders' crowd-out)"):
    for (name, rate, mn, nit) <- corporatePresets do
      val r = reformed(corporate(rate, mn, nit), CorporateTax)
      for g <- groups do
        assertEqualsDouble(r(g), workbookCorporateRate(rate, mn, nit)(g), RateTolerance, s"$name, group ${label(g)}")

  test("corporate tax: without crowd-out every group is scaled by K_p / K_c (Obama 335/218, Warren 488.4/218)"):
    for (cf, ratio) <- List((corporate(0.25, false), 335.0 / 218), (corporate(0.30, true), 488.4 / 218), (corporate(0.21, false), 282.68 / 218)) do
      val r = reformed(cf, CorporateTax)
      for g <- groups if col(g, "corptax") > 0 do
        assertEqualsDouble(r(g) / col(g, "corptax"), ratio, 1e-12, s"group ${label(g)}")

  test("corporate tax: better taxation of multinationals adds (96 − 8)/218 of the current tax"):
    val off = reformed(corporate(210.0 / 1308, false), CorporateTax)
    val on  = reformed(corporate(210.0 / 1308, true), CorporateTax)
    for g <- groups do
      assertEqualsDouble(off(g), col(g, "corptax"), 1e-17, s"off, group ${label(g)}")
      assertEqualsDouble(on(g), col(g, "corptax") * (210 + 96.0) / 218, 1e-15, s"on, group ${label(g)}")

  test("corporate tax: the Obama preset matches cached V8 (P0–1), V135 (top 400) and V137 (aggregate)"):
    val r = reformed(corporate(0.25, false), CorporateTax)
    assertEqualsDouble(r(groups.head), 1.5595917795024334e-3, 1e-15)
    assertEqualsDouble(r(mainModel.groups.topGroup), 7.803074152576267e-2, 1e-15)
    assertEqualsDouble(aggregate(r), 1.9280540090372361e-2, 1e-15)

  test("corporate tax: crowd-out uses the reformed NIT and VAT rates, (1 − R − S), for a 7% NIT and a 10% VAT"):
    val cf = corporate(210.0 / 1308, false, nit = 0.07, vat = 0.10)
    val out = mainModel.run(cf)
    for g <- groups do
      val expected = (1 - out(NationalIncomeTax)(g).reformedTaxRate - out(ValueAddedTax)(g).reformedTaxRate) * col(g, "corptax")
      assertEqualsDouble(out(CorporateTax)(g).reformedTaxRate, expected, 1e-15, s"group ${label(g)}")
      assertEqualsDouble(out(CorporateTax)(g).reformedTaxRate, workbookCorporateRate(210.0 / 1308, false, 0.07, 0.10)(g), RateTolerance, s"group ${label(g)}")

  // ---- Individual income tax (column W) ----------------------------------------------------------------------

  private val incomeThresholds = IndexedSeq[Double](0, 77e3, 165e3, 315e3, 400e3, 600e3, 1e6, 2.5e6, 5e6, 10e6, 25e6, 50e6)
  private val taxincCols = IndexedSeq("taxinc_0k", "taxinc_77k", "taxinc_165k", "taxinc_315k", "taxinc_400k", "taxinc_600k",
    "taxinc_1000k", "taxinc_2500k", "taxinc_5000k", "taxinc_10000k", "taxinc_25000k", "taxinc_50000k")

  /** Bracket rates, interface rows 51–62: current law (column C) and the presets (D, G, H, I, J, K). */
  private val currentLawRates = IndexedSeq(0.12, 0.22, 0.24, 0.32, 0.35, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37)
  private def incomeSchedule(rates: IndexedSeq[Double]) = Schedule(incomeThresholds, rates)

  /** An income tax preset: brackets, interface rows 48 (gains fully taxed), 49 (dividends fully taxed) and
   *  50 (integration), plus the corporate levers and national income tax that integration and crowd-out read. */
  private case class IncomeTaxPreset(name: String, rates: IndexedSeq[Double], gains: Boolean, dividends: Boolean,
                                     integration: Boolean, corpRate: Double, multinationals: Boolean, nit: Double)

  private val incomeTaxPresets = List(
    IncomeTaxPreset("Obama",     IndexedSeq(0.13, 0.25, 0.28, 0.33, 0.36, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40), false, false, false, 0.25, false, 0.00),
    IncomeTaxPreset("book",      IndexedSeq(0.12, 0.22, 0.24, 0.32, 0.40, 0.55, 0.55, 0.55, 0.55, 0.60, 0.60, 0.60), true,  true,  true,  0.30, true,  0.07),
    IncomeTaxPreset("Biden",     IndexedSeq(0.12, 0.22, 0.24, 0.33, 0.36, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40), true,  true,  false, 0.21, false, 0.00),
    IncomeTaxPreset("Buttigieg", IndexedSeq(0.12, 0.22, 0.24, 0.36, 0.52, 0.52, 0.52, 0.52, 0.52, 0.52, 0.52, 0.52), true,  false, false, 0.25, true,  0.00),
    IncomeTaxPreset("Sanders",   IndexedSeq(0.16, 0.26, 0.28, 0.36, 0.45, 0.56, 0.61, 0.61, 0.66, 0.68, 0.68, 0.68), true,  true,  false, 0.25, true,  0.02),
    IncomeTaxPreset("Warren",    IndexedSeq(0.12, 0.22, 0.24, 0.37, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55), true,  true,  false, 0.30, true,  0.00),
  )

  /** The workbook couples retained-earnings attribution to integration (both live in BW and CA), so a preset's
   *  integration switch sets both of the model's levers. */
  private def incomeTaxCounterfactual(p: IncomeTaxPreset, vat: Double = 0.0) = Counterfactual(
    leversOther    = Map(IndividualIncomeTaxSchedule -> incomeSchedule(p.rates)),
    leversBoolean  = Map(
      IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate                -> p.gains,
      IndividualIncomeTaxDividendsTaxedAtOrdinaryRate                   -> p.dividends,
      IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate -> p.integration,
      CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates                 -> p.integration,
      CorporateTaxDeployStrongerEnforcement                             -> p.multinationals),
    leversDouble   = Map(CorporateTaxProposedEffectiveRate -> p.corpRate, NationalIncomeTaxRate -> p.nit, ValueAddedTaxRate -> vat)
  )
  private def preset(name: String) = incomeTaxPresets.find(_.name == name).get

  /** calculatorbig!BB137 · Σ adults: Σ_g nb · (fikgi + fidiv), the base over which BB allocates $166.6bn. */
  private val workbookTotalGainsAndDividends = groups.map(g => adults(g) * (col(g, "fikgi") + col(g, "fidiv"))).sum

  /** calculatorbig!CE8: Σ_k (Δp_k − Δc_k) · taxinc_k, with Δ the step in the marginal rate at threshold k. */
  private def workbookBracketChange(rates: IndexedSeq[Double])(g: Group): Double =
    def steps(r: IndexedSeq[Double]) = r.indices.map(k => r(k) - (if k == 0 then 0.0 else r(k - 1)))
    val (dp, dc) = (steps(rates), steps(currentLawRates))
    taxincCols.indices.map(k => (dp(k) - dc(k)) * col(g, taxincCols(k))).sum

  /** calculatorbig!W8 = (1 − R8 − S8) · max(G8 + (CH8 + CI8)/C8, 0), recomputed from databig:
   *   - CE: the bracket change; BX = fikgi/agi, BY = fidiv/agi;
   *   - CI: CB = (1 − BX − BY)·CE, CC = (1 − BX)·CE, CD = (1 − BY)·CE or CE, by switches (d, k);
   *         under integration CF = CE·(1 + CA/agi) or CG = CE·(1 + CA/agi − BX);
   *   - CA = fidiv·Q142/(1 − Q142) + BB/(1 − Q142), BB = (fikgi + fidiv)·166.6e9 / Σ nb·(fikgi + fidiv);
   *   - CH = [i]·BW + [d ∧ ¬i]·fidiv·mtrextra + [k]·fikgi·mtrextra,
   *         BW = fidiv·((mtr − R142)/(1 − Q142) − min(0.2, mtr)) + BB·(mtr − R142)/(1 − Q142);
   *   - Q142 = (150 + 8)/1308, R142 = R141 − (Q141 − Q142) + R143 (or, without the double count, R141 − 60/1308 + R143). */
  private def workbookIncomeTaxRate(p: IncomeTaxPreset, vat: Double = 0.0, doubleCount: Boolean = true)(g: Group): Double =
    val (kg, div, agi, mtr, mtrx) = (col(g, "fikgi"), col(g, "fidiv"), col(g, "agi"), col(g, "mtr"), col(g, "mtrextra"))
    val ce = workbookBracketChange(p.rates)(g)
    val (bx, by) = (kg / agi, div / agi)
    val q142 = (150.0 + 8) / 1308
    val r143 = (if p.multinationals then 96.0 else 8.0) / 1308
    val r142 = if doubleCount then p.corpRate - (210.0 / 1308 - q142) + r143 else p.corpRate - 60.0 / 1308 + r143
    val bb = (kg + div) * 166.6e9 / workbookTotalGainsAndDividends
    val ca = div * q142 / (1 - q142) + bb / (1 - q142)
    val ci =
      if p.integration then (if p.gains then ce * (1 + ca / agi) else ce * (1 + ca / agi - bx))
      else (p.dividends, p.gains) match
        case (false, false) => (1 - bx - by) * ce
        case (true,  false) => (1 - bx) * ce
        case (false, true)  => (1 - by) * ce
        case (true,  true)  => ce
    val bw = div * ((mtr - r142) / (1 - q142) - math.min(0.2, mtr)) + bb * (mtr - r142) / (1 - q142)
    val ch = (if p.integration then bw else 0.0) + (if p.dividends && !p.integration then div * mtrx else 0.0) + (if p.gains then kg * mtrx else 0.0)
    val crowdOut = 1 - workbookNationalIncomeTaxRate(p.nit)(g) - (if vat == 0.0 then 0.0 else workbookVatRate(vat)(g))
    crowdOut * math.max(col(g, "ditax") + (ch + ci) / denom(g), 0)

  test("income tax: the baseline schedule is current law (interface C51:C62) and every treatment switch is off"):
    assertEquals(mainModel.baseline.leversOtherAs[Schedule](IndividualIncomeTaxSchedule), incomeSchedule(currentLawRates))
    for l <- List(IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate, IndividualIncomeTaxDividendsTaxedAtOrdinaryRate,
                  IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate, CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates) do
      assertEquals(mainModel.baseline.leversBoolean(l), false, l.toString)
    assertEquals(mainModel.parametersDouble(CorporateTaxCurrentStateReceipts), 60.0)
    assertEquals(mainModel.parametersDouble(CloselyHeldCorpTotalRetainedEarnings), 166.6)
    assertEquals(mainModel.parametersBoolean(ReplicateWorkbookForeignDoubleCount), true)

  test("income tax: the taxable-income excess curves return the taxinc_ columns at their thresholds"):
    for g <- groups; k <- incomeThresholds.indices do
      assertEqualsDouble(mainModel.dataOtherAs[ExcessCurve](IncomeExcessCurve)(g).excessFromThreshold(incomeThresholds(k)), col(g, taxincCols(k)), 0.0,
        s"group ${label(g)}, threshold ${incomeThresholds(k)}")

  test("income tax: national gains plus dividends (Σ nb · (fikgi + fidiv)) are totalled without overflow, $1,145bn"):
    assertEqualsDouble(SaezZucman2019DataBridge.totalGainsAndDividends.toDouble, workbookTotalGainsAndDividends, 0.0)
    assertEqualsDouble(workbookTotalGainsAndDividends, 1.145140211439e12, 1.0)

  test("income tax: the baseline is ditax (column G) in every group, exactly"):
    for (g, b) <- baseline(IndividualIncomeTax) do assertEqualsDouble(b, col(g, "ditax"), 0.0, s"group ${label(g)}")

  test("income tax: the Obama preset matches cached W8 (P0–1), W135 (top 400) and W137 (aggregate)"):
    val r = reformed(incomeTaxCounterfactual(preset("Obama")), IndividualIncomeTax)
    assertEqualsDouble(r(groups.head), 4.1187257339462043e-3, RateTolerance)
    assertEqualsDouble(r(mainModel.groups.topGroup), 9.4781867571797962e-2, RateTolerance)
    assertEqualsDouble(aggregate(r), 0.12189620155133546, 1e-6)

  test("income tax: the Obama preset's bracket change at P0–1 is CE8 = $12.90 and, with the preferences kept, CI8 = $12.79"):
    val g = groups.head
    assertEqualsDouble(workbookBracketChange(preset("Obama").rates)(g), 12.9, 1e-9)
    val r = reformed(incomeTaxCounterfactual(preset("Obama")), IndividualIncomeTax)(g)
    assertEqualsDouble((r - col(g, "ditax")) * mainModel.rebasedAverageIncome(g), 12.790717375630869, 1e-6)

  test("income tax: the Warren worked example (methodology §10.2), P99.997–99.998, W = 0.36360"):
    assertEqualsDouble(reformed(counterfactuals.Warren, IndividualIncomeTax)(group("99.997")), 0.36360, RateTolerance)

  test("income tax: the Warren preset (counterfactuals.Warren) carries her brackets and taxes gains and dividends as ordinary income"):
    assertEquals(counterfactuals.Warren.leversOtherAs[Schedule](IndividualIncomeTaxSchedule), incomeSchedule(preset("Warren").rates))
    assertEveryGroup(reformed(counterfactuals.Warren, IndividualIncomeTax), workbookIncomeTaxRate(preset("Warren")))

  test("income tax: every preset matches column W in every group (with the book's integration and crowd-out)"):
    for p <- incomeTaxPresets do
      val r = reformed(incomeTaxCounterfactual(p), IndividualIncomeTax)
      for g <- groups do
        assertEqualsDouble(r(g), workbookIncomeTaxRate(p)(g), RateTolerance, s"${p.name}, group ${label(g)}")

  test("income tax: every preset's aggregate matches the workbook's"):
    for p <- incomeTaxPresets do
      val r = reformed(incomeTaxCounterfactual(p), IndividualIncomeTax)
      assertEqualsDouble(aggregate(r), aggregate(workbookIncomeTaxRate(p)), 1e-6, p.name)

  test("income tax: every combination of the three switches, under the book's brackets, matches column W"):
    for gains <- List(false, true); dividends <- List(false, true); integration <- List(false, true) do
      val p = preset("book").copy(name = s"k=$gains d=$dividends i=$integration", gains = gains, dividends = dividends, integration = integration, nit = 0.0)
      val r = reformed(incomeTaxCounterfactual(p), IndividualIncomeTax)
      for g <- groups do
        assertEqualsDouble(r(g), workbookIncomeTaxRate(p)(g), RateTolerance, s"${p.name}, group ${label(g)}")

  test("income tax: with current brackets, taxing gains as ordinary income adds mtrextra · fikgi and nothing else"):
    val p = IncomeTaxPreset("gains only", currentLawRates, true, false, false, 210.0 / 1308, false, 0.0)
    val r = reformed(incomeTaxCounterfactual(p), IndividualIncomeTax)
    for g <- groups do
      val expected = math.max(col(g, "ditax") + col(g, "mtrextra") * col(g, "fikgi") / mainModel.rebasedAverageIncome(g), 0)
      assertEqualsDouble(r(g), expected, 1e-15, s"group ${label(g)}")

  test("income tax: with current brackets, kept preferences leave every group's rate unchanged, whatever the corporate levers"):
    val p = IncomeTaxPreset("nothing", currentLawRates, false, false, false, 0.30, true, 0.0)
    for (g, r) <- mainModel.run(incomeTaxCounterfactual(p))(IndividualIncomeTax) do
      assertEqualsDouble(r.deltaTaxRate, 0.0, 0.0, s"group ${label(g)}")

  test("income tax: without the double count and with no corporate reform, the credit rate equals the gross-up rate (τ_p = τ_c)"):
    val c = SaezZucman2019Context(Map("ReplicateWorkbookForeignDoubleCount" -> false))
    val p = IncomeTaxPreset("integration only", currentLawRates, false, false, true, 210.0 / 1308, false, 0.0)
    val cf = c.Counterfactual(
      leversBoolean = Map(c.IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate -> true, c.CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates -> true))
    val r = c.mainModel.run(cf)(c.IndividualIncomeTax)
    val tau = 158.0 / 1308
    for g <- groups do
      val (div, mtr) = (col(g, "fidiv"), col(g, "mtr"))
      val re = (col(g, "fikgi") + div) * 166.6e9 / workbookTotalGainsAndDividends
      val change = (mtr - tau) * (div + re) / (1 - tau) - math.min(0.2, mtr) * div
      val expected = math.max(col(g, "ditax") + change / c.mainModel.rebasedAverageIncome(g), 0)
      assertEqualsDouble(r(g).reformedTaxRate, expected, 1e-15, s"group ${label(g)}")
      assertEqualsDouble(r(g).reformedTaxRate, workbookIncomeTaxRate(p, doubleCount = false)(g), RateTolerance, s"group ${label(g)}")

  test("income tax: the workbook's double count lowers the book plan's rate wherever there are dividends or retained earnings"):
    val c = SaezZucman2019Context(Map("ReplicateWorkbookForeignDoubleCount" -> false))
    val p = preset("book")
    val cf = c.Counterfactual(
      leversOther    = Map(c.IndividualIncomeTaxSchedule -> incomeSchedule(p.rates)),
      leversBoolean  = Map(c.IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate -> true, c.IndividualIncomeTaxDividendsTaxedAtOrdinaryRate -> true,
        c.IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate -> true, c.CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates -> true,
        c.CorporateTaxDeployStrongerEnforcement -> true),
      leversDouble   = Map(c.CorporateTaxProposedEffectiveRate -> p.corpRate, c.NationalIncomeTaxRate -> p.nit))
    val without = c.mainModel.run(cf)(c.IndividualIncomeTax)
    val withDc = reformed(incomeTaxCounterfactual(p), IndividualIncomeTax)
    for g <- groups do
      assertEqualsDouble(without(g).reformedTaxRate, workbookIncomeTaxRate(p, doubleCount = false)(g), RateTolerance, s"group ${label(g)}")
      if col(g, "fidiv") + col(g, "fikgi") > 0 && withDc(g) > 0 then
        assert(withDc(g) < without(g).reformedTaxRate, s"group ${label(g)}")

  test("income tax: integration refunds overpaid corporate tax at the bottom, and pays more at the top"):
    val p = IncomeTaxPreset("integration only", currentLawRates, false, false, true, 210.0 / 1308, false, 0.0)
    val d = delta(incomeTaxCounterfactual(p), IndividualIncomeTax)
    assert(d(groups.head) < 0, s"P0–1: ${d(groups.head)}")
    assert(d(mainModel.groups.topGroup) > 0, s"top 400: ${d(mainModel.groups.topGroup)}")

  test("income tax: crowd-out uses the reformed NIT and VAT rates, (1 − R − S), for a 7% NIT and a 10% VAT"):
    val p = preset("Warren").copy(nit = 0.07)
    val out = mainModel.run(incomeTaxCounterfactual(p, vat = 0.10))
    val alone = reformed(incomeTaxCounterfactual(p.copy(nit = 0.0)), IndividualIncomeTax)
    for g <- groups do
      val expected = (1 - out(NationalIncomeTax)(g).reformedTaxRate - out(ValueAddedTax)(g).reformedTaxRate) * alone(g)
      assertEqualsDouble(out(IndividualIncomeTax)(g).reformedTaxRate, expected, 1e-15, s"group ${label(g)}")
      assertEqualsDouble(out(IndividualIncomeTax)(g).reformedTaxRate, workbookIncomeTaxRate(p, vat = 0.10)(g), RateTolerance, s"group ${label(g)}")

  test("income tax: the floor keeps rates at zero, not negative, when brackets are abolished"):
    val p = IncomeTaxPreset("no brackets", IndexedSeq.fill(12)(0.0), false, false, false, 210.0 / 1308, false, 0.0)
    val r = reformed(incomeTaxCounterfactual(p), IndividualIncomeTax)
    assert(groups.exists(r(_) == 0.0), "some group is floored")
    for g <- groups do
      assert(r(g) >= 0.0, s"group ${label(g)}: ${r(g)}")
      assertEqualsDouble(r(g), workbookIncomeTaxRate(p)(g), RateTolerance, s"group ${label(g)}")

  test("income tax: declares its inputs, so a model without the bracket schedule or the mtr data is rejected"):
    intercept[IllegalArgumentException](mainModel.copy(baseline = mainModel.baseline.copy(leversOther = mainModel.baseline.leversOther - IndividualIncomeTaxSchedule)))
    intercept[IllegalArgumentException](mainModel.copy(dataDouble = mainModel.dataDouble - AvgOrdinaryMarginalTaxRate))

  test("income tax: declares the precomputed gains-and-dividends total, so a model without that aggregation is rejected"):
    intercept[IllegalArgumentException](mainModel.copy(aggregationsDouble = mainModel.aggregationsDouble - TotalGainsAndDividends))

  test("income tax: the gains-and-dividends aggregation is Σ nb · (fikgi + fidiv) over every group, straight from databig"):
    val expected = groups.map(g => adults(g) * (col(g, "fikgi") + col(g, "fidiv"))).sum
    assertEquals(mainModel.aggregationsDouble(TotalGainsAndDividends), expected)

  test("income tax: runs after the national income tax and VAT, and a model ordering it earlier is rejected"):
    val order = mainModel.instruments.map(_._1)
    assert(order.indexOf(IndividualIncomeTax) > order.indexOf(NationalIncomeTax))
    assert(order.indexOf(IndividualIncomeTax) > order.indexOf(ValueAddedTax))
    val iit = mainModel.instruments.find(_._1 == IndividualIncomeTax).get
    intercept[IllegalArgumentException](mainModel.copy(instruments = iit :: mainModel.instruments.filterNot(_ == iit)))

  // ---- Property tax (column I) -------------------------------------------------------------------------------

  test("property tax: the baseline is proptax (column I) in every group, exactly"):
    for (g, b) <- baseline(PropertyTax) do assertEqualsDouble(b, col(g, "proptax"), 0.0, s"group ${label(g)}")

  test("property tax: the baseline aggregate matches the cached calculatorbig!I137"):
    assertEqualsDouble(aggregate(baseline(PropertyTax)), 2.7725578159961208e-2, 1e-15)

  test("property tax: no lever moves it, under any preset (the workbook never reforms it)"):
    val cfs = counterfactuals.Warren :: sanders :: nit7 :: vat10 :: salesReduction(0.5) :: incomeTaxPresets.map(incomeTaxCounterfactual(_))
    for cf <- cfs; (g, r) <- mainModel.run(cf)(PropertyTax) do
      assertEqualsDouble(r.reformedTaxRate, col(g, "proptax"), 0.0, s"group ${label(g)}")
      assertEqualsDouble(r.deltaTaxRate, 0.0, 0.0, s"group ${label(g)}")

  test("property tax: declares its input, so a model without the proptax data is rejected"):
    intercept[IllegalArgumentException](mainModel.copy(dataDouble = mainModel.dataDouble - PropertyTaxShareOfIncome))

  // ---- Totals (columns E and Z) ------------------------------------------------------------------------------

  /** The sum of every instrument's rate, for each group. */
  private def total(rates: Map[LInstrument, Map[Group, Instrument.Result]], pick: Instrument.Result => Double): Map[Group, Double] =
    groups.map(g => (g, rates.values.map(byGroup => pick(byGroup(g))).sum)).toMap

  /** The saved workbook's active plan (interface column D, Obama): estate, corporate and income tax changes;
   *  no wealth tax, NIT, VAT, health or sales changes, and no payroll head tax. */
  private val obamaAll =
    val iit = incomeTaxCounterfactual(preset("Obama"))
    Counterfactual(
      leversDouble   = iit.leversDouble ++ estateObama.leversDouble,
      leversBoolean  = iit.leversBoolean,
      leversOther    = iit.leversOther)

  test("totals: the model now covers every tax in the workbook; the current total is effrate, the sum of databig H–M"):
    val t = total(mainModel.run(Counterfactual()), _.baselineTaxRate)
    for g <- groups do
      assertEqualsDouble(t(g), col(g, "effrate"), 1e-7, s"group ${label(g)}")

  test("totals: the current total matches the cached calculatorbig!E8 (P0–1), E135 (top 400) and E137 (aggregate)"):
    val t = total(mainModel.run(Counterfactual()), _.baselineTaxRate)
    assertEqualsDouble(t(groups.head), 0.27588452569489164, 1e-7)
    assertEqualsDouble(t(mainModel.groups.topGroup), 0.23041529022157192, 1e-7)
    assertEqualsDouble(aggregate(t), 0.2771540411051811, 1e-7)

  test("totals: with private premiums shown as a tax, the current total is effrate + healthtax (E = G + L)"):
    val t = healthShown.mainModel.run(healthShown.Counterfactual()).values.map(_.map((g, r) => (g, r.baselineTaxRate)))
    for g <- groups do
      val sum = t.map(byGroup => byGroup.collectFirst { case (h, v) if label(h) == label(g) => v }.get).sum
      assertEqualsDouble(sum, col(g, "effrate") + col(g, "healthtax"), 1e-7, s"group ${label(g)}")

  test("totals: the saved active plan (Obama) matches the cached total rate Z8 (P0–1), Z135 (top 400) and Z137 (aggregate)"):
    val t = total(mainModel.run(obamaAll), _.reformedTaxRate)
    assertEqualsDouble(t(groups.head), 0.27803246956704319, RateTolerance)
    assertEqualsDouble(t(mainModel.groups.topGroup), 0.26433688109052716, RateTolerance)
    assertEqualsDouble(aggregate(t), 0.29276439871392085, 1e-6)

  test("totals: Warren's full plan (counterfactuals.Warren) matches Z = I + Y + Q…X in every group"):
    val t = total(mainModel.run(counterfactuals.Warren), _.reformedTaxRate)
    val warrenWealth = counterfactuals.Warren.leversOtherAs[Schedule](WealthTaxSchedule)
    def workbookTotal(g: Group) =
      col(g, "proptax")                                  // I
        + workbookPayrollRate(headTax = true)(g)         // Y
        + workbookWealthTaxRate(warrenWealth, 0.15)(g)   // Q
        + workbookHealthRate(shown = false, 1.0)(g)      // T (R and S are zero)
        + workbookEstateRate(0.55, 0.40)(g)              // U
        + workbookCorporateRate(0.30, true)(g)           // V
        + workbookIncomeTaxRate(preset("Warren"))(g)     // W
        + workbookSalesRate(0.0)(g)                      // X
    assertEveryGroup(t, workbookTotal)

  test("totals: the deltas sum to the change in the total, and only the reformed taxes move under Warren"):
    val out = mainModel.run(counterfactuals.Warren)
    val d = total(out, _.deltaTaxRate)
    val (b, r) = (total(out, _.baselineTaxRate), total(out, _.reformedTaxRate))
    for g <- groups do assertEqualsDouble(d(g), r(g) - b(g), 1e-15, s"group ${label(g)}")
    for lbl <- List[LInstrument](NationalIncomeTax, ValueAddedTax, PropertyTax, SalesAndExciseTax); (g, x) <- out(lbl) do
      assertEqualsDouble(x.deltaTaxRate, 0.0, 0.0, s"$lbl, group ${label(g)}")

  // ---- Presets (calculatorbig columns EA–EN) -----------------------------------------------------------------

  /* The workbook keeps each preset's total-rate curve, pasted from earlier runs of the plan, in calculatorbig
   * EA–EG (private health premiums shown as a tax) and EH–EN (not shown). Its rows are databig's, in order. */
  private val pastedPresetColumns = Seq(
    ("EA", "biden", true), ("EB", "buttigieg", true), ("EC", "warren", true), ("ED", "sanders", true),
    ("EE", "book", true), ("EF", "current", true), ("EG", "obama", true),
    ("EH", "biden", false), ("EI", "buttigieg", false), ("EJ", "warren", false), ("EK", "sanders", false),
    ("EL", "book", false), ("EM", "current", false), ("EN", "obama", false))

  /* EE and EF were pasted from an earlier version of the data: they differ from the current data's run in the
   * top 0.1% only (up to ~2e-3). The evidence: under the book plan private premiums are abolished, so its curve
   * cannot depend on whether they are shown as a tax, yet EE and EL differ. (The main repo's parity engine,
   * which reproduces the other twelve columns to 1e-12, finds the same.) */
  private val stalePasteTolerance = Map("EE" -> 2e-3, "EF" -> 3e-4)

  private def pastedColumn(column: String): IndexedSeq[Double] =
    val c = Calculatorbig.header.indexOf(column)
    (0 until Calculatorbig.numRows).map(Calculatorbig.doubleValue(_, c))

  test("presets: the cached calculatorbig rows line up with databig's groups (same adults, nb = column B)"):
    assertEquals(Calculatorbig.numRows, groups.length)
    val b = pastedColumn("B")
    for (g, i) <- groups.zipWithIndex do assertEquals(b(i), adults(g), s"group ${label(g)}")

  test("presets: the counterfactuals cover every plan the workbook presets"):
    assertEquals(counterfactuals.all.keySet, pastedPresetColumns.map(_._2).toSet - "current")

  for (column, plan, shown) <- pastedPresetColumns do
    test(s"presets: '$plan' (premiums shown as a tax = $shown) reproduces the workbook's pasted total, column $column, in every group"):
      val ctx = if shown then healthShown else this.ctx
      val cf  = if plan == "current" then ctx.Counterfactual() else ctx.counterfactuals.all(plan)
      val out = ctx.mainModel.run(cf).values
      val gs  = ctx.mainModel.groups.toSeq
      val expected = pastedColumn(column)
      val tolerance = stalePasteTolerance.getOrElse(column, RateTolerance)
      for (g, i) <- gs.zipWithIndex do
        val t = out.map(_(g).reformedTaxRate).sum
        assertEqualsDouble(t, expected(i), tolerance, s"$column $plan, group ${label(g)}")

  test("presets: counterfactuals.Obama is the saved active plan, matching the cached Z column in every group"):
    val t = total(mainModel.run(counterfactuals.Obama), _.reformedTaxRate)
    val z = pastedColumn("Z")
    for (g, i) <- groups.zipWithIndex do assertEqualsDouble(t(g), z(i), RateTolerance, s"group ${label(g)}")

  test("presets: counterfactuals.Sanders carries the workbook's grid-snapped wealth tax with the FTT surcharge"):
    val s = counterfactuals.Sanders.leversOtherAs[Schedule](WealthTaxSchedule)
    assertEquals(s.thresholds, sandersSchedule.thresholds)
    for (a, e) <- s.rates.zip(sandersSchedule.rates) do assertEqualsDouble(a, e, 1e-15)
    assertEquals(s.rates.length, sandersSchedule.rates.length)
