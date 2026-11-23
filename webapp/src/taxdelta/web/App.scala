package taxdelta.web

/** Display-oriented helpers shared by both pages. */
object App:
  val groups: IndexedSeq[GroupInfo] = Sim.groups
  val xs: IndexedSeq[Double] = groups.indices.map(_.toDouble)
  val ticks: Seq[(Double, String)] = Format.groupTicks(groups)
  val labels: IndexedSeq[String] = groups.indices.map(Format.groupLabel(groups, _))
  /** Tooltip subtitle. Groups are ranked by pre-tax income; rates are shares of pre-tax income plus
   *  pure capital gains (`income`), shown too where the two differ by 1% or more. */
  def groupSub(i: Int): String =
    val g = groups(i)
    if math.abs(g.income / g.pretaxIncome - 1) < 0.01 then s"Average pre-tax income ${Format.amount(g.pretaxIncome)}"
    else s"Average pre-tax income ${Format.amount(g.pretaxIncome)} (${Format.amount(g.income)} with pure capital gains)"

  final case class Plan(key: String, label: String, cls: String)
  /** The plans of the workbook's figure, in a fixed order; each keeps its color everywhere. */
  val figurePlans: Seq[Plan] = Seq(
    Plan("current", "Current law (2018)", "series-1"),
    Plan("obama", "Obama (repeal 2017 tax cut)", "series-2"),
    Plan("biden", "Biden", "series-3"),
    Plan("buttigieg", "Buttigieg", "series-4"),
    Plan("warren", "Warren", "series-5"),
    Plan("sanders", "Sanders", "series-6")
  )
  val bookPlan: Plan = Plan("book", "Saez–Zucman book plan", "series-7")
  val allPlans: Seq[Plan] = figurePlans :+ bookPlan
  def planLabel(key: String): String = allPlans.find(_.key == key).map(_.label).getOrElse(key)

  def run(policy: Policy, health: Boolean): Outcome = Sim(health).run(policy)
  def behavioral(policy: Policy, health: Boolean): Behavior = Sim(health).behavioral(policy)

  /** Income-weighted average rate over groups selected by percentile. */
  def groupRate(out: Outcome, proposed: Boolean, which: GroupInfo => Boolean): Double =
    val is = groups.indices.filter(i => which(groups(i)))
    val rates = if proposed then out.proposedTotal else out.baselineTotal
    val w = is.map(i => groups(i).adults * groups(i).income)
    is.lazyZip(w).map((i, wi) => rates(i) * wi).sum / w.sum

  val bottom50: GroupInfo => Boolean = _.percentile < 50
  val bottom90: GroupInfo => Boolean = _.percentile < 90
  val top01: GroupInfo => Boolean = _.percentile >= 99.9 - 1e-9
  val top400: GroupInfo => Boolean = _.percentile > 100

  /** Tax categories for breakdown charts: at most 8, in a fixed order with fixed colors. */
  final case class Category(label: String, cls: String, ids: Seq[String])
  val categories: Seq[Category] = Seq(
    Category("Individual income tax", "series-1", Seq("incomeTax")),
    Category("Payroll taxes", "series-2", Seq("payroll")),
    Category("Sales & excise taxes", "series-3", Seq("sales")),
    Category("Corporate tax", "series-4", Seq("corporate")),
    Category("Property taxes", "series-5", Seq("property")),
    Category("Wealth & estate taxes", "series-6", Seq("wealth", "estate")),
    Category("New broad taxes (NIT, VAT)", "series-7", Seq("nit", "vat")),
    Category("Private health insurance", "series-8", Seq("health"))
  )

  def breakdown(out: Outcome, proposed: Boolean): Seq[Series] =
    val rates = if proposed then out.proposed else out.baseline
    categories.flatMap { c =>
      val vs = xs.indices.map(i => c.ids.flatMap(rates.get).map(_(i)).sum)
      if vs.forall(v => math.abs(v) < 1e-12) then None else Some(Series(c.label, c.cls, vs))
    }

  /** A breakdown with a leading "Total tax" column (the height of the stack), for data tables. */
  def withTotal(parts: Seq[Series]): Seq[Series] =
    Series("Total tax", "", xs.indices.map(i => parts.map(_.values(i)).sum)) +: parts
