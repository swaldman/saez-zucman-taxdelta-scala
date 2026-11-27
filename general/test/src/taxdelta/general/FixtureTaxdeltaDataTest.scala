package taxdelta.general

import taxdelta.model.TopGroup

class FixtureTaxdeltaDataTest extends munit.FunSuite:

  val Year = 2023
  lazy val all : Map[String, FixtureTaxdeltaData] = FixtureTaxdeltaData.byYear(Year)
  lazy val yf : YearFixtures = fixtures.AllFixtures.years.find(_.year == Year).get

  test("the countries generated are exactly those the catalog marks as supporting the income tax"):
    val c = yf.catalog
    val supported = (0 until c.numRows).filter(c.string(_, "individualIncomeTax") == "yes").map(c.string(_, "iso")).toSet
    assertEquals(all.keySet, supported)
    assert(supported.size >= 30, s"expected most OECD countries, found ${supported.size}")
    assert(supported.contains("US"))

  test("metadata"):
    val us = all("US")
    assertEquals(us.countryName, "United States")
    assertEquals(us.yearUsdPPP, 2024)
    assertEqualsDouble(us.xppp_us, 1.0, 1e-12)
    for d <- all.values do
      assert(d.defl > 0 && d.xppp_us > 0, d.iso)
      assert(d.notes.nonEmpty, d.iso)
      for gen <- Seq("gf-groups", "oecd-pit", "iit-base", "catalog") do
        assert(d.notes.exists(_.generator == gen), s"${d.iso} has no notes from $gen")

  test("groups: 127, ordered, consistent thresholds, sizes add up to the population"):
    for d <- all.values do
      val gs = d.groupSeq
      assertEquals(gs.length, 127, d.iso)
      assertEquals(d.groups.size, 127, d.iso)
      assert(gs.last.isInstanceOf[TopGroup], d.iso)
      assertEquals(d.groups.toIndexedSeq, gs, d.iso)
      for i <- gs.indices do
        assert(gs(i).minIncome <= gs(i).averageIncome + 1e-6, s"${d.iso} group $i: min above average")
        if i > 0 then assert(gs(i).minIncome >= gs(i - 1).minIncome, s"${d.iso} group $i: thresholds decrease")
      val total = gs.map(_.size.toLong).sum
      assertEqualsDouble(total.toDouble / d.population, 1.0, 0.001, d.iso)

  test("group excess curves: exact below the group, zero above it, never increasing"):
    for d <- all.values; (g, curve) <- d.dataIncomeExcessCurve.get do
      assertEqualsDouble(curve.excessFromThreshold(g.minIncome), g.averageIncome - g.minIncome, 1e-6 * (1 + g.averageIncome))
      assertEqualsDouble(curve.excessFromThreshold(g.minIncome - 1000), g.averageIncome - g.minIncome + 1000, 1e-6 * (1 + g.averageIncome))
      val hi = if g.isInstanceOf[TopGroup] then g.averageIncome * 100 else g.averageIncome * 2 + 1
      val ts = (0 to 50).map(k => g.minIncome + (hi - g.minIncome) * k / 50)
      val xs = ts.map(curve.excessFromThreshold)
      assert(xs.forall(_ >= -1e-9), s"${d.iso} $g: negative excess")
      assert(xs.zip(xs.tail).forall((a, b) => b <= a + 1e-6), s"${d.iso} $g: excess increases")

  /** Income tax at the calibrated base, as a share of pretax income, summed over groups. */
  def revenueShare( d : FixtureTaxdeltaData ) : Double =
    val lambda = d.paramIndividualIncomeTaxCalibratedBaseShareOfIncome.get
    val sched  = d.baselineIndividualIncomeTaxSchedule.get
    val curves = d.dataIncomeExcessCurve.get
    val gs = d.groupSeq
    gs.map(g => g.size * sched.averageTax(ScaledExcessCurve(curves(g), lambda))).sum / gs.map(g => g.size * g.averageIncome).sum

  test("calibration: the current schedule on the calibrated base reproduces Gethin–Fisher's income tax total"):
    for d <- all.values do
      val target = d.individualIncomeTaxTargetShareOfIncome.get
      assertEqualsDouble(revenueShare(d), target, 1e-6 * math.max(target, 1e-3), d.iso)
      val observed = d.groupSeq.map(g => g.totalIncome * d.dataEffectiveIncomeTaxShareOfIncome.get(g)).sum /
                     d.groupSeq.map(_.totalIncome).sum
      // the target is the size-weighted total of the per-group rates (up to rounding sizes to whole people)
      assertEqualsDouble(observed, target, 1e-4 * math.max(target, 1e-3), d.iso)

  test("calibrated base shares are plausible"):
    for d <- all.values do
      val l = d.paramIndividualIncomeTaxCalibratedBaseShareOfIncome.get
      assert(l > 0.1 && l < 1.0, s"${d.iso}: λ = $l")

  test("schedules start at zero with increasing thresholds and sensible rates"):
    for d <- all.values do
      val s = d.baselineIndividualIncomeTaxSchedule.get
      assertEquals(s.thresholds.head, 0.0, d.iso)
      assert(s.rates.forall(r => r >= 0 && r < 0.8), s"${d.iso}: ${s.rates}")

  test("the milestone instrument formula: unchanged below a new top bracket, higher above it"):
    // max(current(g) + (new(g) − cur(g)) / y(g), 0) with new = cur gives current(g)
    val d = all("FR")
    val lambda = d.paramIndividualIncomeTaxCalibratedBaseShareOfIncome.get
    val cur = d.baselineIndividualIncomeTaxSchedule.get
    val proposed = cur.withMinimumRateAbove(500_000, 0.6)
    // France, like most countries in WID and Gethin–Fisher, has zero income in P0–5: no tax changes there, and
    // a rate of tax on no income is undefined, so the instrument must leave those groups at their current rate.
    assert(d.groupSeq.take(5).forall(_.averageIncome == 0.0))
    for g <- d.groupSeq do
      val curve = ScaledExcessCurve(d.dataIncomeExcessCurve.get(g), lambda)
      val change = proposed.averageTax(curve) - cur.averageTax(curve)
      if g.averageIncome <= 0 then assertEqualsDouble(change, 0.0, 1e-12)
      val delta = if g.averageIncome > 0 then change / g.averageIncome else 0.0
      val rate = math.max(d.dataEffectiveIncomeTaxShareOfIncome.get(g) + delta, 0)
      if g.averageIncome < 400_000 then assertEqualsDouble(delta, 0.0, 1e-12)
      else assert(delta >= 0 && rate >= d.dataEffectiveIncomeTaxShareOfIncome.get(g))
