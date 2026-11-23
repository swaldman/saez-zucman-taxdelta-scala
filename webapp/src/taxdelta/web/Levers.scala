package taxdelta.web

import taxdelta.model.Schedule

import scala.scalajs.js

/** The UI's own description of a plan: one value per lever, keyed by a stable id.
 *
 *  The ids (and the JSON in shared links) are those of the original saez-zucman-taxsim-scala site, so its
 *  links open here too. [[Sim]] translates a [[Policy]] into a `Counterfactual` of the model's context. */
enum LeverValue:
  case Num(x: Double)
  case Flag(b: Boolean)
  case Sched(s: Schedule)

final case class Policy(values: Map[String, LeverValue]):
  def ++(o: Policy): Policy = Policy(values ++ o.values)
  def set(id: String, v: Double): Policy = Policy(values.updated(id, LeverValue.Num(v)))
  def set(id: String, v: Boolean): Policy = Policy(values.updated(id, LeverValue.Flag(v)))
  def set(id: String, v: Schedule): Policy = Policy(values.updated(id, LeverValue.Sched(v)))
  def get(id: String): Option[LeverValue] = values.get(id)
  def num(id: String): Double = values(id) match
    case LeverValue.Num(x) => x
    case v                 => throw new IllegalArgumentException(s"lever '$id' is not a number: $v")
  def flag(id: String): Boolean = values(id) match
    case LeverValue.Flag(b) => b
    case v                  => throw new IllegalArgumentException(s"lever '$id' is not a flag: $v")
  def schedule(id: String): Schedule = values(id) match
    case LeverValue.Sched(s) => s
    case v                   => throw new IllegalArgumentException(s"lever '$id' is not a schedule: $v")

object Policy:
  val empty: Policy = Policy(Map.empty)

enum LeverKind:
  case Slider(min: Double, max: Double, step: Double, percent: Boolean = true)
  case Toggle
  case Brackets(maxRate: Double)

final case class LeverSpec(id: String, label: String, section: String, kind: LeverKind, help: String = "")

/** Lever ids and the controls generated for them. */
object Levers:
  val nitRate            = "nit.rate"
  val vatRate            = "vat.rate"
  val salesReduction     = "sales.reduction"
  val healthReduction    = "health.reduction"
  val healthHeadTax      = "health.headTax"
  val wealthEvasion      = "wealth.evasion"
  val wealthSchedule     = "wealth.schedule"
  val ftt                = "ftt.enabled"
  val estateAvoidance    = "estate.avoidance"
  val estateTopRate      = "estate.topRate"
  val corpRate           = "corp.rate"
  val corpMultinationals = "corp.multinationals"
  val pitCapitalGains    = "pit.capitalGainsFull"
  val pitDividends       = "pit.dividendsFull"
  val pitIntegration     = "pit.integration"
  val pitSchedule        = "pit.schedule"
  val healthAsTax        = "options.healthAsTax"

  val OptionsSection = "Options"

  import LeverKind.*
  val schema: IndexedSeq[LeverSpec] = IndexedSeq(
    LeverSpec(nitRate, "National income tax rate", "Broad taxes", Slider(0, 0.2, 0.005)),
    LeverSpec(vatRate, "Value added tax rate", "Broad taxes", Slider(0, 0.2, 0.005)),
    LeverSpec(salesReduction, "Sales and excise tax reduction", "Broad taxes", Slider(0, 1, 0.05),
      "0% = current; 50% ≈ eliminating sales taxes only"),
    LeverSpec(healthReduction, "Private health insurance replaced by public funding", "Broad taxes", Slider(0, 1, 0.05)),
    LeverSpec(healthHeadTax, "Employer head tax equal to employers' health contributions", "Broad taxes", Toggle),
    LeverSpec(wealthEvasion, "Wealth tax evasion rate", "Wealth tax", Slider(0, 1, 0.05)),
    LeverSpec(wealthSchedule, "Wealth tax brackets", "Wealth tax", Brackets(0.1)),
    LeverSpec(ftt, "0.1% financial transaction tax", "Wealth tax", Toggle,
      "Modeled as a wealth tax of 0.05% below $1m and 0.1% above"),
    LeverSpec(estateAvoidance, "Estate tax avoidance rate", "Estate tax", Slider(0, 1, 0.05)),
    LeverSpec(estateTopRate, "Estate tax top marginal rate", "Estate tax", Slider(0, 1, 0.01)),
    LeverSpec(corpRate, "Effective corporate tax rate on US profits", "Corporate tax", Slider(0, 1, 0.01)),
    LeverSpec(corpMultinationals, "Better taxation of multinationals", "Corporate tax", Toggle),
    LeverSpec(pitCapitalGains, "Capital gains taxed as ordinary income", "Individual income tax", Toggle),
    LeverSpec(pitDividends, "Dividends taxed as ordinary income", "Individual income tax", Toggle),
    LeverSpec(pitIntegration, "Integrate dividend and corporate taxes", "Individual income tax", Toggle),
    LeverSpec(pitSchedule, "Income tax brackets", "Individual income tax", Brackets(1.0)),
    LeverSpec(healthAsTax, "Show private health insurance as a tax", OptionsSection, Toggle)
  )
  val byId: Map[String, LeverSpec] = schema.map(s => s.id -> s).toMap

/** Compact JSON for policies, as in the original site's links:
 *  `{"vat.rate":0.05,"ftt.enabled":true,"pit.schedule":{"thresholds":[0,77000],"rates":[0.12,0.22]}}`. */
object PolicyJson:

  def write(p: Policy): String =
    val o = js.Dictionary.empty[js.Any]
    for (k, v) <- p.values.toSeq.sortBy(_._1) do
      o(k) = v match
        case LeverValue.Num(x)   => x
        case LeverValue.Flag(b)  => b
        case LeverValue.Sched(s) => js.Dictionary[js.Any]("thresholds" -> js.Array(s.thresholds*), "rates" -> js.Array(s.rates*))
    js.JSON.stringify(o)

  /** Parses a policy, skipping (rather than failing on) entries that aren't well formed. */
  def read(s: String): Policy =
    val o = js.JSON.parse(s).asInstanceOf[js.Dictionary[js.Any]]
    def nums(a: js.Any): Option[IndexedSeq[Double]] =
      if js.Array.isArray(a) then
        val arr = a.asInstanceOf[js.Array[js.Any]]
        Option.when(arr.forall(x => js.typeOf(x) == "number"))(arr.map(_.asInstanceOf[Double]).toIndexedSeq)
      else None
    val entries = o.toSeq.flatMap { (k, v) =>
      js.typeOf(v) match
        case "number"  => Some(k -> LeverValue.Num(v.asInstanceOf[Double]))
        case "boolean" => Some(k -> LeverValue.Flag(v.asInstanceOf[Boolean]))
        case "object" if v != null =>
          val d = v.asInstanceOf[js.Dictionary[js.Any]]
          for
            ts <- d.get("thresholds").flatMap(nums)
            rs <- d.get("rates").flatMap(nums)
            if ts.nonEmpty && ts.length == rs.length
          yield k -> LeverValue.Sched(Controls.normalize(ts, rs))
        case _ => None
    }
    Policy(entries.toMap)
