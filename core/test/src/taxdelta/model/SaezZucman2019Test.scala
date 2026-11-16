package taxdelta.model

import taxdelta.data.Sz2019Worksheet_databig as Databig

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

  private val groups = model.groups.toSeq

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
    leversSchedule = Map(WealthTaxSchedule -> sandersSchedule)
  )
  private val nit7 = Counterfactual(leversDouble = Map(NationalIncomeTaxRate -> 0.07))
  private val vat10 = Counterfactual(leversDouble = Map(ValueAddedTaxRate -> 0.10))

  // ---- Helpers -----------------------------------------------------------------------------------------------

  private def reformed(cf: Counterfactual, instrument: LInstrument): Map[Group, Double] =
    model.run(cf)(instrument).map((g, r) => (g, r.reformedTaxRate))

  private def baseline(instrument: LInstrument): Map[Group, Double] =
    model.run(Counterfactual())(instrument).map((g, r) => (g, r.baselineTaxRate))

  private def delta(cf: Counterfactual, instrument: LInstrument): Map[Group, Double] =
    model.run(cf)(instrument).map((g, r) => (g, r.deltaTaxRate))

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
    val all = List[LInstrument](EstateTax, HealthInsuranceTax, NationalIncomeTax, PayrollTax, ValueAddedTax, WealthTax, CorporateTax)
    assertEquals(model.run(Counterfactual()).keySet, all.toSet)
    assertEquals(model.instruments.map(_._1), all)

  test("the baseline levies none of the three new taxes"):
    for lbl <- List[LInstrument](NationalIncomeTax, ValueAddedTax, WealthTax); (g, r) <- model.run(Counterfactual())(lbl) do
      assertEqualsDouble(r.baselineTaxRate, 0.0, 0.0, s"$lbl baseline, group ${label(g)}")

  test("an empty counterfactual changes nothing"):
    for (lbl, byGroup) <- model.run(Counterfactual()); (g, r) <- byGroup do
      assertEqualsDouble(r.reformedTaxRate, r.baselineTaxRate, 0.0, s"$lbl, group ${label(g)}")
      assertEqualsDouble(r.deltaTaxRate, 0.0, 0.0, s"$lbl delta, group ${label(g)}")

  test("parameter overrides: unknown keys and wrongly typed values are rejected, known ones are applied"):
    intercept[IllegalArgumentException](SaezZucman2019Context(Map("NoSuchParameter" -> 1.0)))
    intercept[IllegalArgumentException](SaezZucman2019Context(Map("ConsumerIncidenceShare" -> "0.7")))
    intercept[IllegalArgumentException](SaezZucman2019Context(Map("CountPrivateHealthPremiumsAsTax" -> 1.0)))
    intercept[IllegalArgumentException](SaezZucman2019Context(Map("ConsumerIncidenceShare" -> true)))
    assertEquals(model.parametersBoolean(CountPrivateHealthPremiumsAsTax), false)
    assertEquals(healthShown.model.parametersBoolean(healthShown.CountPrivateHealthPremiumsAsTax), true)
    val c = SaezZucman2019Context(Map("ConsumerIncidenceShare" -> 0.7))
    assertEquals(c.model.parametersDouble(c.ConsumerIncidenceShare), 0.7)

  test("parameter overrides: integer and other numeric values are accepted for double parameters on both platforms"):
    for v <- List[Any](1, 1L, 1.toShort, 1.toByte, 1.0f, BigInt(1), BigDecimal(1)) do
      val c = SaezZucman2019Context(Map("EmployerShareOfPrivateHealthPremiums" -> v))
      assertEquals(c.model.parametersDouble(c.EmployerShareOfPrivateHealthPremiums), 1.0, s"value $v: ${v.getClass}")

  test("defaults match the saved workbook: premiums not shown as a tax, head-tax share 0.55"):
    assertEquals(model.parametersBoolean(CountPrivateHealthPremiumsAsTax), false)
    assertEquals(model.parametersDouble(EmployerShareOfPrivateHealthPremiums), 0.55)

  // ---- Wealth tax (column Q) ---------------------------------------------------------------------------------

  test("Warren wealth tax: the worked example (methodology §10.1), P99.997–99.998"):
    val g = groups.find(label(_) == "99.997").get
    assertEqualsDouble(reformed(counterfactuals.Warren, WealthTax)(g), 0.13737, RateTolerance)

  test("Warren wealth tax: every group"):
    val schedule = counterfactuals.Warren.leversSchedule(WealthTaxSchedule)
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

  // ---- VAT (column S) ----------------------------------------------------------------------------------------

  test("VAT at 10%: every group"):
    assertEveryGroup(reformed(vat10, ValueAddedTax), workbookVatRate(0.10))

  test("VAT at 10%: the distribution profile averages to 1, so the aggregate rate is v/(1+v) × 0.4"):
    assertEqualsDouble(aggregate(reformed(vat10, ValueAddedTax)), (0.10 / 1.10) * 0.4, 1e-9)

  test("VAT at 10%: regressive, from about 8.8% of income at the bottom to about 2.2% for the top 400"):
    val r = reformed(vat10, ValueAddedTax)
    assertEqualsDouble(r(groups.head), 0.0883, 1e-4)
    assertEqualsDouble(r(model.groups.topGroup), 0.0222, 1e-4)

  // ---- Private health insurance (columns L, T) ---------------------------------------------------------------

  private def shownRun(cf: healthShown.Counterfactual): Map[Group, healthShown.Instrument.Result] =
    healthShown.model.run(cf)(healthShown.HealthInsuranceTax)

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
    assert(b(model.groups.topGroup) < 1e-4)

  test("health insurance, not shown (the default, interface C16 = \"no\"): zero before and after any reform"):
    for (g, r) <- model.run(counterfactuals.Warren)(HealthInsuranceTax) do
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
    assert(d(model.groups.topGroup) < 1e-4)

  test("payroll: plans without the head tax leave payroll unchanged"):
    assertEveryGroup(delta(nit7, PayrollTax), _ => 0.0)
    assertEveryGroup(delta(sanders, PayrollTax), _ => 0.0)

  test("Warren, premiums shown: health plus payroll together cut the middle's rate by 0.45 × healthtax"):
    val h = shownRun(healthShown.counterfactuals.Warren).map((g, r) => (g, r.deltaTaxRate))
    val p = healthShown.model.run(healthShown.counterfactuals.Warren)(healthShown.PayrollTax).map((g, r) => (g, r.deltaTaxRate))
    assertEveryGroup(groups.map(g => (g, h(g) + p(g))).toMap, g => -0.45 * col(g, "healthtax"))

  /* As in the workbook, Y8 adds the head tax whatever interface C16 says, so with premiums not shown as a tax,
   * Warren's head tax appears without the premiums it replaces (methodology §8.8, questions §7.3). */
  test("payroll: the head tax applies whether or not premiums are shown as a tax, as in the workbook"):
    assertEveryGroup(reformed(counterfactuals.Warren, PayrollTax), workbookPayrollRate(headTax = true))
    val shown = healthShown.model.run(healthShown.counterfactuals.Warren)(healthShown.PayrollTax)
    assertEveryGroup(shown.map((g, r) => (g, r.reformedTaxRate)), workbookPayrollRate(headTax = true))

  test("payroll: the head tax scales with the EmployerShareOfPrivateHealthPremiums parameter"):
    val c = SaezZucman2019Context(Map("EmployerShareOfPrivateHealthPremiums" -> 1.0))
    for (g, r) <- c.model.run(c.counterfactuals.Warren)(c.PayrollTax) do
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
    assertEquals(model.parametersDouble(EstateTaxCurrentStatutoryTopRate), 0.4)
    assertEquals(model.parametersDouble(EstateTaxCurrentAvoidanceRate), 0.6)

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
    assertEqualsDouble(b(model.groups.topGroup), 0.01114, 1e-5)

  test("estate tax: overriding the current-law parameters moves the baseline levers with them"):
    val c = SaezZucman2019Context(Map("EstateTaxCurrentStatutoryTopRate" -> 0.5, "EstateTaxCurrentAvoidanceRate" -> 0.5))
    for (g, r) <- c.model.run(c.Counterfactual())(c.EstateTax) do
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
    assertEquals(model.parametersDouble(CorporateTaxCurrentDomesticProfits), 1308.0)
    assertEquals(model.parametersDouble(CorporateTaxCurrentDomesticReceipts), 210.0)
    assertEquals(model.parametersDouble(CorporateTaxCurrentForeignReceipts), 8.0)
    assertEquals(model.parametersDouble(CorporateTaxForeignReceiptsUnderStrongerEnforcement), 96.0)
    assertEquals(model.leversDouble(CorporateTaxProposedEffectiveRate), 210.0 / 1308) // interface C43
    assertEquals(model.leversBoolean(CorporateTaxDeployStrongerEnforcement), false)  // interface C44

  test("corporate tax: the baseline is corptax (column H) in every group, exactly"):
    assertEveryGroup(baseline(CorporateTax), g => col(g, "corptax"))
    for (g, b) <- baseline(CorporateTax) do assertEqualsDouble(b, col(g, "corptax"), 1e-17, s"group ${label(g)}")

  test("corporate tax: the baseline aggregate matches the cached calculatorbig!H137"):
    assertEqualsDouble(aggregate(baseline(CorporateTax)), 1.2546739521496047e-2, 1e-15)

  test("corporate tax: Warren's preset (counterfactuals.Warren) has a 30% rate and better taxation of multinationals"):
    assertEveryGroup(reformed(counterfactuals.Warren, CorporateTax), workbookCorporateRate(0.30, true))

  test("corporate tax: declares all its inputs, so a model without the corptax data is rejected"):
    intercept[IllegalArgumentException](model.copy(dataDouble = model.dataDouble - CorporateTaxShareOfIncome))

  test("corporate tax: runs after the national income tax and VAT, and a model ordering it earlier is rejected"):
    val order = model.instruments.map(_._1)
    assert(order.indexOf(CorporateTax) > order.indexOf(NationalIncomeTax))
    assert(order.indexOf(CorporateTax) > order.indexOf(ValueAddedTax))
    val corp = model.instruments.find(_._1 == CorporateTax).get
    intercept[IllegalArgumentException](model.copy(instruments = corp :: model.instruments.filterNot(_ == corp)))

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
    assertEqualsDouble(r(model.groups.topGroup), 7.803074152576267e-2, 1e-15)
    assertEqualsDouble(aggregate(r), 1.9280540090372361e-2, 1e-15)

  test("corporate tax: crowd-out uses the reformed NIT and VAT rates, (1 − R − S), for a 7% NIT and a 10% VAT"):
    val cf = corporate(210.0 / 1308, false, nit = 0.07, vat = 0.10)
    val out = model.run(cf)
    for g <- groups do
      val expected = (1 - out(NationalIncomeTax)(g).reformedTaxRate - out(ValueAddedTax)(g).reformedTaxRate) * col(g, "corptax")
      assertEqualsDouble(out(CorporateTax)(g).reformedTaxRate, expected, 1e-15, s"group ${label(g)}")
      assertEqualsDouble(out(CorporateTax)(g).reformedTaxRate, workbookCorporateRate(210.0 / 1308, false, 0.07, 0.10)(g), RateTolerance, s"group ${label(g)}")
