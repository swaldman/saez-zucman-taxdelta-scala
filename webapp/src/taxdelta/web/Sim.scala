package taxdelta.web

import taxdelta.model.*

/** Display information about one income group, in model order (127 percentile groups, then the top 400).
 *  @param percentile    lower bound of the g-percentile; 400 for the top group (shown as "Top 400")
 *  @param pretaxIncome  average pre-tax national income, the ranking variable (`avg`)
 *  @param income        the rate denominator: pre-tax income plus pure capital gains (the model's rebased income) */
final case class GroupInfo(percentile: Double, label: String, adults: Double, pretaxIncome: Double, income: Double)

/** Effective tax rates by instrument and group, current law vs a plan, with the workbook's revenue conversions. */
final case class Outcome(ids: IndexedSeq[String], baseline: Map[String, IndexedSeq[Double]], proposed: Map[String, IndexedSeq[Double]]):
  private def n = Sim.groups.map(_.adults)
  private def y = Sim.groups.map(_.income)
  private def agg(rates: IndexedSeq[Double]): Double =
    rates.indices.map(i => rates(i) * n(i) * y(i)).sum / rates.indices.map(i => n(i) * y(i)).sum

  lazy val baselineTotal: IndexedSeq[Double] = Sim.groups.indices.map(i => ids.map(baseline(_)(i)).sum)
  lazy val proposedTotal: IndexedSeq[Double] = Sim.groups.indices.map(i => ids.map(proposed(_)(i)).sum)

  def baselineAverageRate: Double = agg(baselineTotal)
  def proposedAverageRate: Double = agg(proposedTotal)
  def baselineAverageRate(id: String): Double = agg(baseline(id))
  def proposedAverageRate(id: String): Double = agg(proposed(id))

  /** Net revenue change as a share of national income. */
  def revenueShareOfIncome: Double = proposedAverageRate - baselineAverageRate
  /** Net revenue change in one year, in billions of 2019 dollars. */
  def annualRevenue: Double = revenueShareOfIncome * Sim.NationalIncome
  /** The workbook's 2021–2030 budget window: 4% nominal growth, starting two years out. */
  def budgetWindowRevenue: Double =
    val g = Sim.BudgetGrowth
    annualRevenue * math.pow(1 + g, Sim.BudgetWindowOffsetYears) * (math.pow(1 + g, Sim.BudgetWindowYears) - 1) / g

/** The long-run behavioral block's results for one plan, by group (calculatorbig DB, DC, DI). */
final case class Behavior(currentMtr: IndexedSeq[Double], proposedMtr: IndexedSeq[Double], dynamicIncome: IndexedSeq[Double])

/** Runs the reworked model for the UI. The "private health insurance as a tax" option is a parameter of
 *  the model's context, not a lever, so there is one context for each setting. */
final class Sim private (val c: SaezZucman2019WithBehaviorContext):
  import c.*
  private val L = Levers
  private val model = c.mainModel
  private val gs: IndexedSeq[Group] = model.groups.toIndexedSeq

  val healthShown: Boolean = model.parametersBoolean(CountPrivateHealthPremiumsAsTax)

  /** Instruments in display order, with the UI's ids and labels (health only when shown as a tax). */
  val instruments: IndexedSeq[(LInstrument, String, String)] =
    IndexedSeq(
      (PropertyTax, "property", "Property taxes"),
      (PayrollTax, "payroll", "Payroll taxes"),
      (WealthTax, "wealth", "Progressive wealth tax (incl. any FTT)"),
      (NationalIncomeTax, "nit", "National income tax"),
      (ValueAddedTax, "vat", "Value added tax"),
      (HealthInsuranceTax, "health", "Private health insurance"),
      (EstateTax, "estate", "Estate tax"),
      (CorporateTax, "corporate", "Corporate tax"),
      (IndividualIncomeTax, "incomeTax", "Individual income tax"),
      (SalesAndExciseTax, "sales", "Sales and excise taxes")
    ).filter((l, _, _) => healthShown || l != HealthInsuranceTax)

  def counterfactual(p: Policy): Counterfactual =
    val wealth = if p.flag(L.ftt) then Sim.withFtt(p.schedule(L.wealthSchedule)) else p.schedule(L.wealthSchedule)
    Counterfactual(
      leversBoolean = Map(
        CorporateTaxDeployStrongerEnforcement                             -> p.flag(L.corpMultinationals),
        EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax          -> p.flag(L.healthHeadTax),
        IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate                -> p.flag(L.pitCapitalGains),
        IndividualIncomeTaxDividendsTaxedAtOrdinaryRate                   -> p.flag(L.pitDividends),
        // the workbook's single integration switch sets both of the model's levers
        IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate -> p.flag(L.pitIntegration),
        CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates                 -> p.flag(L.pitIntegration),
      ),
      leversDouble = Map(
        CorporateTaxProposedEffectiveRate                 -> p.num(L.corpRate),
        EstateTaxAnticipatedAvoidanceRate                 -> p.num(L.estateAvoidance),
        EstateTaxProposedTopRate                          -> p.num(L.estateTopRate),
        NationalIncomeTaxRate                             -> p.num(L.nitRate),
        ValueAddedTaxRate                                 -> p.num(L.vatRate),
        ReductionOfEmployerSponsoredPrivateHealthPremiums -> p.num(L.healthReduction),
        SalesAndExciseTaxReduction                        -> p.num(L.salesReduction),
        WealthTaxEvasionRate                              -> p.num(L.wealthEvasion),
      ),
      leversOther = Map(
        IndividualIncomeTaxSchedule -> p.schedule(L.pitSchedule),
        WealthTaxSchedule           -> wealth,
      )
    )

  /** A counterfactual as UI lever values; levers it doesn't set take their baseline values. */
  def policyOf(cf: Counterfactual): Policy =
    val o = Some(cf)
    val (wealth0, ftt) = Sim.splitFtt(model.orPreferAs[Schedule](o)(WealthTaxSchedule))
    // No wealth tax: offer the workbook's wealth thresholds at 0% as a starting point for the bracket editor.
    val wealth = if wealth0.rates.forall(_ == 0.0) then Schedule(Sim.WealthGrid, Sim.WealthGrid.map(_ => 0.0)) else wealth0
    Policy(Map(
      L.nitRate            -> LeverValue.Num(model.orPrefer(o)(NationalIncomeTaxRate)),
      L.vatRate            -> LeverValue.Num(model.orPrefer(o)(ValueAddedTaxRate)),
      L.salesReduction     -> LeverValue.Num(model.orPrefer(o)(SalesAndExciseTaxReduction)),
      L.healthReduction    -> LeverValue.Num(model.orPrefer(o)(ReductionOfEmployerSponsoredPrivateHealthPremiums)),
      L.healthHeadTax      -> LeverValue.Flag(model.orPrefer(o)(EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax)),
      L.wealthEvasion      -> LeverValue.Num(model.orPrefer(o)(WealthTaxEvasionRate)),
      L.wealthSchedule     -> LeverValue.Sched(wealth),
      L.ftt                -> LeverValue.Flag(ftt),
      L.estateAvoidance    -> LeverValue.Num(model.orPrefer(o)(EstateTaxAnticipatedAvoidanceRate)),
      L.estateTopRate      -> LeverValue.Num(model.orPrefer(o)(EstateTaxProposedTopRate)),
      L.corpRate           -> LeverValue.Num(model.orPrefer(o)(CorporateTaxProposedEffectiveRate)),
      L.corpMultinationals -> LeverValue.Flag(model.orPrefer(o)(CorporateTaxDeployStrongerEnforcement)),
      L.pitCapitalGains    -> LeverValue.Flag(model.orPrefer(o)(IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate)),
      L.pitDividends       -> LeverValue.Flag(model.orPrefer(o)(IndividualIncomeTaxDividendsTaxedAtOrdinaryRate)),
      L.pitIntegration     -> LeverValue.Flag(model.orPrefer(o)(IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate)),
      L.pitSchedule        -> LeverValue.Sched(model.orPreferAs[Schedule](o)(IndividualIncomeTaxSchedule)),
    ))

  def run(p: Policy): Outcome =
    val res = model.run(counterfactual(p))
    Outcome(
      instruments.map(_._2),
      instruments.map((l, id, _) => id -> gs.map(g => res(l)(g).baselineTaxRate)).toMap,
      instruments.map((l, id, _) => id -> gs.map(g => res(l)(g).reformedTaxRate)).toMap)

  def behavioral(p: Policy): Behavior =
    val r = behaviorResult(model, counterfactual(p))
    Behavior(
      gs.map(g => r.baselineScenarioResult(g).smoothedMarginalTaxRate),
      gs.map(g => r.counterfactualScenarioResult(g).smoothedMarginalTaxRate),
      gs.map(g => r.counterfactualIncomeAfterRedistribution(g)))

  private[web] def groupInfos: IndexedSeq[GroupInfo] = gs.map { g =>
    val raw = SaezZucman2019DataBridge.rowColValues(g).trim
    g match
      case pg: PercentileGroup => GroupInfo(pg.percentileLowerBound, raw, g.size, g.averageIncome, model.rebasedAverageIncome(g))
      case _: TopGroup         => GroupInfo(raw.toDouble, raw, g.size, g.averageIncome, model.rebasedAverageIncome(g))
  }

object Sim:
  /** The wealth thresholds of the workbook's interface (`calculatorbig` wealth grid). Declared first: `current`
   *  and `presets` below use it while this object initializes. */
  val WealthGrid: IndexedSeq[Double] = IndexedSeq(0, 1e6, 2.5e6, 5e6, 10e6, 25e6, 50e6, 100e6, 250e6, 500e6, 1e9, 2.5e9, 5e9, 10e9)

  /** One context per setting of the health-insurance option. */
  val plain: Sim = new Sim(SaezZucman2019WithBehaviorContext())
  val shown: Sim = new Sim(SaezZucman2019WithBehaviorContext(Map("CountPrivateHealthPremiumsAsTax" -> true)))
  def apply(healthAsTax: Boolean): Sim = if healthAsTax then shown else plain

  val groups: IndexedSeq[GroupInfo] = plain.groupInfos

  /** Current law, and the workbook's preset plans ("current", "book", "obama", "biden", "buttigieg", "sanders", "warren"),
   *  as lever values. They don't depend on the health option. */
  val current: Policy = plain.policyOf(plain.c.Counterfactual())
  val presets: Map[String, Policy] = plain.c.counterfactuals.all.map((k, cf) => k -> plain.policyOf(cf)) + ("current" -> current)

  /** The workbook's macro conversions (interface Q8, R8): national income of 17,546 × 1.038 $bn, and a ten-year
   *  window (2021–2030) growing at 4% from two years out. */
  val NationalIncome: Double = 17546.0 * 1.038
  val BudgetGrowth: Double = 0.04
  val BudgetWindowOffsetYears: Int = 2
  val BudgetWindowYears: Int = 10

  def withFtt(s: Schedule): Schedule = Ftt.add(s)
  def splitFtt(s: Schedule): (Schedule, Boolean) = Ftt.split(s)

  /** The workbook's growth sheet: the raise each group needs to be back on the equitable-growth path (columns L, M). */
  private def growthColumn(name: String): IndexedSeq[Double] =
    import taxdelta.data.Sz2019Worksheet_growth as G
    val c = G.header.indexOf(name)
    (0 until G.numRows).map(G.doubleValue(_, c))
  lazy val missingPretaxRaise: IndexedSeq[Double] = growthColumn("missingPretaxRaise")
  lazy val missingPosttaxRaise: IndexedSeq[Double] = growthColumn("missingPosttaxRaise")

/** The workbook models a 0.1% financial transaction tax as a wealth tax surcharge: 0.05% below $1m, 0.1% above.
 *  (A separate object, so its constants are initialized before `Sim` uses them to build its presets.) */
private object Ftt:
  private val Low = 0.0005
  private val High = 0.001
  private val Threshold = 1e6
  private def surcharge(t: Double) = if t < Threshold then Low else High
  private def clean(r: Double) = math.round(r * 1e10) / 1e10

  def add(s: Schedule): Schedule =
    val t = s.insertThreshold(Threshold)
    Schedule(t.thresholds, t.thresholds.indices.map(k => t.rates(k) + surcharge(t.thresholds(k))))

  /** Separates an FTT surcharge from a wealth schedule, if it carries one; merges brackets left with equal rates. */
  def split(s: Schedule): (Schedule, Boolean) =
    val hasFtt = s.thresholds.headOption.contains(0.0) && s.thresholds.contains(Threshold) &&
      s.thresholds.indices.forall(k => s.rates(k) >= surcharge(s.thresholds(k)) - 1e-12) &&
      math.abs(s.rates(0) - Low) < 1e-12
    if !hasFtt then (s, false)
    else
      val rs = s.thresholds.indices.map(k => clean(s.rates(k) - surcharge(s.thresholds(k))))
      val kept = s.thresholds.indices.filter(k => k == 0 || rs(k) != rs(k - 1))
      (Schedule(kept.map(s.thresholds), kept.map(rs)), true)
