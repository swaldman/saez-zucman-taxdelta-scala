package taxdelta.model

import taxdelta.data.Sz2019Worksheet_calculatorbig as Calculatorbig
import taxdelta.data.Sz2019Worksheet_growth as Growth

/** Checks the experimental long-run behavioral block (calculatorbig CU–DO) against the workbook's cached run of its
 *  saved active plan (Obama, private premiums not shown as a tax), stage by stage:
 *
 *  - DB, DC: smoothed implied marginal tax rates, current and proposed
 *  - DD:     income after the real (incentive) response,     y · ratio^0.25
 *  - DE:     income after the bargaining response,           DD · ratio^(0.5 φ)
 *  - DG:     each group's shortfall versus equitable growth since 1980
 *  - DH:     the bargaining change, redistributed in proportion to the shortfall
 *  - DI:     final long-run income, DE + DH
 *
 *  The model's pre-tax income y is its rebased average income, which differs from the workbook's `denom` by up to
 *  about 7e-5 relative. Income levels therefore match to about 1e-4 relative; the response factors, which cancel y,
 *  match far more tightly.
 */
class SaezZucman2019BehaviorTest extends munit.FunSuite:

  private val ctx = SaezZucman2019WithBehaviorContext()
  import ctx.*

  private val groups = mainModel.groups.toIndexedSeq
  private val adults = mainModel.dataDouble(NumberOfAdults)
  private def y(g: Group): Double = mainModel.rebasedAverageIncome(g)
  private def label(g: Group): String = SaezZucman2019DataBridge.rowColValues(g).trim

  private def cached(column: String): IndexedSeq[Double] =
    val c = Calculatorbig.header.indexOf(column)
    (0 until Calculatorbig.numRows).map(Calculatorbig.doubleValue(_, c))

  private val obama = behaviorResult(mainModel, counterfactuals.Obama)

  private def incentive(r: BehaviorResult)(g: Group)  = r.jointCounterfactualResult(g).afterIncentiveIncomePerAdult
  private def bargained(r: BehaviorResult)(g: Group)  = r.jointCounterfactualResult(g).afterBargainingBeforeRedistributionIncomePerAdult
  private def final_(r: BehaviorResult)(g: Group)     = r.counterfactualIncomeAfterRedistribution(g)
  private def redistributed(r: BehaviorResult)(g: Group) = final_(r)(g) - bargained(r)(g)

  private def bottom90 = groups.take(90) // P0 … P89
  private def top10    = groups.drop(90) // P90 … top 400

  // ---- Setup and data ----------------------------------------------------------------------------------------

  test("long-run: the behavior context builds, keeps every static instrument, and its model offers all it requires"):
    assertEquals(mainModel.instruments.map(_._1.toString), SaezZucman2019Context().mainModel.instruments.map(_._1.toString))
    assert(mainModel.requires.subsetOf(mainModel.offers), (mainModel.requires -- mainModel.offers).toString)

  test("long-run: the static results are unchanged by the behavior context"):
    val base = SaezZucman2019Context()
    val a = mainModel.run(counterfactuals.Obama)
    val b = base.mainModel.run(base.counterfactuals.Obama)
    for (lbl, byGroup) <- a; (g, i) <- groups.zipWithIndex do
      assertEquals(byGroup(g).reformedTaxRate, b(base.LInstrument.valueOf(lbl.toString))(base.mainModel.groups.toIndexedSeq(i)).reformedTaxRate, s"$lbl, group ${label(g)}")

  test("long-run: its parameters can be overridden like any other"):
    val c = SaezZucman2019WithBehaviorContext(Map("LongRunIncentiveElasticity" -> 0.3))
    assertEquals(c.mainModel.parametersDouble(c.LongRunIncentiveElasticity), 0.3)

  test("long-run: the growth factor is growth!G147/C147, the adult-weighted means excluding the 'Of which: Top 400' subset row"):
    def col(n: String) = { val c = Growth.header.indexOf(n); (0 until Growth.numRows - 1).map(Growth.doubleValue(_, c)) }
    def mean(n: String, v: String) = col(n).zip(col(v)).map(_ * _).sum / col(n).sum
    assertEquals(Growth.value(Growth.numRows - 1, 0).trim, "Of which: Top 400")
    val derived = mean("nb2018", "peinc2018") / mean("nb1980", "peinc1980")
    assertEqualsDouble(mainModel.parametersDouble(LongRunAveragePreTaxIncomeGrowth), derived, 1e-14)

  test("long-run: the growth incomes line up with the model's groups by position, the top 400 taking the subset row"):
    val (c1980, c2018) = (Growth.header.indexOf("peinc1980"), Growth.header.indexOf("peinc2018"))
    for (g, i) <- groups.zipWithIndex do
      assertEquals(mainModel.dataDouble(LongRunPretaxIncome1980)(g), Growth.doubleValue(i, c1980), s"group ${label(g)}")
      assertEquals(mainModel.dataDouble(LongRunPretaxIncome2018)(g), Growth.doubleValue(i, c2018), s"group ${label(g)}")

  test("long-run: the shortfall versus equitable growth matches DG in every group"):
    val f  = mainModel.parametersDouble(LongRunAveragePreTaxIncomeGrowth)
    val dg = cached("DG")
    for (g, i) <- groups.zipWithIndex do
      val shortfall = math.max(f * mainModel.dataDouble(LongRunPretaxIncome1980)(g) - mainModel.dataDouble(LongRunPretaxIncome2018)(g), 0)
      assertEqualsDouble(shortfall, dg(i), 1e-8, s"group ${label(g)}")

  test("long-run: only the bottom 90% (P0–P89) fall short of equitable growth"):
    val dg = cached("DG")
    for i <- 0 until 90 do assert(dg(i) > 0, s"row $i")
    for i <- 90 until groups.length do assertEquals(dg(i), 0.0, s"row $i")

  // ---- Implied marginal tax rates ----------------------------------------------------------------------------

  test("long-run: the smoothed implied marginal tax rates match DB (current) and DC (Obama) in every group"):
    val (db, dc) = (cached("DB"), cached("DC"))
    for (g, i) <- groups.zipWithIndex do
      assertEqualsDouble(obama.baselineScenarioResult(g).smoothedMarginalTaxRate, db(i), 2e-5, s"DB, group ${label(g)}")
      assertEqualsDouble(obama.counterfactualScenarioResult(g).smoothedMarginalTaxRate, dc(i), 2e-5, s"DC, group ${label(g)}")

  test("long-run: the after-tax incomes are the workbook's CX (current) and CY (Obama), to the income denominator's precision"):
    val (cx, cy) = (cached("CX"), cached("CY"))
    for (g, i) <- groups.zipWithIndex do
      assertEqualsDouble(obama.baselineScenarioResult(g).afterTaxIncomePerAdult / cx(i), 1.0, 1e-4, s"CX, group ${label(g)}")
      assertEqualsDouble(obama.counterfactualScenarioResult(g).afterTaxIncomePerAdult / cy(i), 1.0, 1e-4, s"CY, group ${label(g)}")

  // ---- Responses ---------------------------------------------------------------------------------------------

  test("long-run: the bargaining weight is 0 through P89, rises by 0.1 a percentile from P90, and is 1 from P99 up"):
    for g <- groups do
      val w = obama.jointCounterfactualResult(g).bargainingWeight
      g match
        case pg: PercentileGroup =>
          val expected = math.min(math.max(pg.percentileLowerBound - 89, 0) * 0.1, 1.0)
          assertEqualsDouble(w, expected, 1e-12, s"group ${label(g)}")
        case _: TopGroup => assertEquals(w, 1.0)

  test("long-run: the incentive response factor matches the workbook's DD / C in every group"):
    val (dd, c) = (cached("DD"), cached("C"))
    for (g, i) <- groups.zipWithIndex do
      assertEqualsDouble(incentive(obama)(g) / y(g), dd(i) / c(i), 5e-6, s"group ${label(g)}")

  test("long-run: the bargaining response factor matches the workbook's DE / DD in every group"):
    val (dd, de) = (cached("DD"), cached("DE"))
    for (g, i) <- groups.zipWithIndex do
      assertEqualsDouble(bargained(obama)(g) / incentive(obama)(g), de(i) / dd(i), 1e-6, s"group ${label(g)}")

  test("long-run: incomes after the incentive (DD) and bargaining (DE) responses match the workbook, to 1e-4 relative"):
    val (dd, de) = (cached("DD"), cached("DE"))
    for (g, i) <- groups.zipWithIndex do
      assertEqualsDouble(incentive(obama)(g) / dd(i), 1.0, 1e-4, s"DD, group ${label(g)}")
      assertEqualsDouble(bargained(obama)(g) / de(i), 1.0, 1e-4, s"DE, group ${label(g)}")

  // ---- Redistribution ----------------------------------------------------------------------------------------

  test("long-run: the redistributed bargaining income matches DH, to 1e-5 relative below P90 and exactly zero above"):
    val dh = cached("DH")
    for (g, i) <- bottom90.zipWithIndex do assertEqualsDouble(redistributed(obama)(g) / dh(i), 1.0, 1e-5, s"group ${label(g)}")
    for g <- top10 do assertEquals(redistributed(obama)(g), 0.0, s"group ${label(g)}")

  test("long-run: final income matches DI in every group, to 1e-4 relative"):
    val di = cached("DI")
    for (g, i) <- groups.zipWithIndex do
      assertEqualsDouble(final_(obama)(g) / di(i), 1.0, 1e-4, s"group ${label(g)}")

  test("long-run: redistribution returns the bargaining change in full, so total income changes only by the incentive response"):
    val bargainingChange = groups.map(g => adults(g) * (bargained(obama)(g) - incentive(obama)(g))).sum
    val redistributedSum = groups.map(g => adults(g) * redistributed(obama)(g)).sum
    assert(bargainingChange < 0, "under Obama the top loses through bargaining")
    assertEqualsDouble(redistributedSum / -bargainingChange, 1.0, 1e-12)
    val finalTotal     = groups.map(g => adults(g) * final_(obama)(g)).sum
    val incentiveTotal = groups.map(g => adults(g) * incentive(obama)(g)).sum
    assertEqualsDouble(finalTotal / incentiveTotal, 1.0, 1e-12)

  test("long-run: under Obama the top 10% lose through bargaining and the bottom 90% gain from the redistribution"):
    for g <- top10 do assert(bargained(obama)(g) <= incentive(obama)(g), s"group ${label(g)}")
    for g <- bottom90 do assert(redistributed(obama)(g) > 0, s"group ${label(g)}")

  // ---- Invariants --------------------------------------------------------------------------------------------

  test("long-run: with no reform there is no response: every income stays at y exactly"):
    val r = behaviorResult(mainModel, Counterfactual())
    for g <- groups do
      assertEquals(incentive(r)(g), y(g), s"DD, group ${label(g)}")
      assertEquals(bargained(r)(g), y(g), s"DE, group ${label(g)}")
      assertEquals(final_(r)(g), y(g), s"DI, group ${label(g)}")

  test("long-run: the current and proposed marginal rates are computed identically, so the baseline result is the same under any plan"):
    val r = behaviorResult(mainModel, counterfactuals.Warren)
    for g <- groups do
      assertEquals(r.baselineScenarioResult(g), obama.baselineScenarioResult(g), s"group ${label(g)}")

  test("long-run: with no elasticities there is no response, whatever the plan"):
    val c = SaezZucman2019WithBehaviorContext(Map("LongRunIncentiveElasticity" -> 0.0, "LongRunBargainingElasticity" -> 0.0))
    val r = c.behaviorResult(c.mainModel, c.counterfactuals.Warren)
    for g <- c.mainModel.groups.toIndexedSeq do
      assertEquals(r.counterfactualIncomeAfterRedistribution(g), c.mainModel.rebasedAverageIncome(g))
