package taxdelta.web

import com.raquo.laminar.api.L.*
import org.scalajs.dom

import scala.scalajs.js.URIUtils

/** Interactive simulator: levers generated from the UI's lever schema, results recomputed on every change. */
object SimulatorPage:

  private val OptionsSection = Levers.OptionsSection

  /** The saved workbook's option: private health insurance not counted as a tax. */
  private val defaultOptions = Policy(Map(Levers.healthAsTax -> LeverValue.Flag(false)))

  /** Initial state from the URL query (`plan` = JSON of levers that differ from current law). */
  final case class State(policy: Policy, options: Policy)

  /** A link's value is used only if it is of the kind its lever expects. */
  private def fits(id: String, v: LeverValue): Boolean =
    (Levers.byId.get(id).map(_.kind), v) match
      case (Some(_: LeverKind.Slider), _: LeverValue.Num)     => true
      case (Some(LeverKind.Toggle), _: LeverValue.Flag)       => true
      case (Some(_: LeverKind.Brackets), _: LeverValue.Sched) => true
      case _                                                  => false

  def parseQuery(query: String): State =
    val params = query.split("&").filter(_.contains("=")).map { kv =>
      val Array(k, v) = kv.split("=", 2)
      k -> URIUtils.decodeURIComponent(v)
    }.toMap
    val changes = params.get("plan").flatMap(s => scala.util.Try(PolicyJson.read(s)).toOption).getOrElse(Policy.empty)
    val valid = changes.values.filter(fits)
    val policy = Sim.current ++ Policy(valid.filter((k, _) => Levers.byId(k).section != OptionsSection))
    val opts = defaultOptions ++ Policy(valid.filter((k, _) => Levers.byId(k).section == OptionsSection))
    State(policy, opts)

  def query(s: State): String =
    val cur = Sim.current ++ defaultOptions
    val all = s.policy ++ s.options
    val diff = Policy(all.values.filter((k, v) => !cur.get(k).contains(v)))
    if diff.values.isEmpty then "" else "?plan=" + URIUtils.encodeURIComponent(PolicyJson.write(diff))

  /** Instrument labels, from the context that includes every instrument. */
  private val instrumentLabel: Map[String, String] = Sim.shown.instruments.map((_, id, l) => id -> l).toMap

  def apply(initialQuery: String): HtmlElement =
    val init = parseQuery(initialQuery)
    val policy = Var(init.policy)
    val options = Var(init.options)
    val copied = Var(false)
    val current = Sim.current

    val outcome: Signal[Outcome] = policy.signal.combineWith(options.signal).map((p, o) => App.run(p, o.flag(Levers.healthAsTax)))
    val state: Signal[State] = policy.signal.combineWith(options.signal).map(State.apply)

    def numSig(pol: Var[Policy], id: String) = pol.signal.map(_.num(id))
    def leverControl(spec: LeverSpec): HtmlElement =
      val target = if spec.section == OptionsSection then options else policy
      val base = if spec.section == OptionsSection then defaultOptions else current
      spec.kind match
        case k: LeverKind.Slider =>
          Controls.slider(spec, k, numSig(target, spec.id), base.num(spec.id), v => target.update(_.set(spec.id, v)))
        case LeverKind.Toggle =>
          Controls.toggle(spec, target.signal.map(_.flag(spec.id)), base.flag(spec.id), v => target.update(_.set(spec.id, v)))
        case k: LeverKind.Brackets =>
          Controls.brackets(spec, k, target.signal.map(_.schedule(spec.id)), base.schedule(spec.id), v => target.update(_.set(spec.id, v)))

    val sections = Levers.schema.map(_.section).distinct
    val panel = div(cls := "card panel",
      div(cls := "panel-actions",
        select(aria.label := "Start from a plan",
          option(value := "", "Load a plan…"),
          App.allPlans.map(p => option(value := p.key, p.label)),
          onChange.mapToValue --> { k =>
            if k.nonEmpty then policy.set(current ++ Sim.presets(k))
          },
          value <-- policy.signal.map(_ => "")
        ),
        button(cls := "btn", "Reset", onClick --> { _ => policy.set(current) }),
        button(cls := "btn primary", "Copy link", onClick --> { _ =>
          dom.window.navigator.clipboard.writeText(dom.window.location.href)
          copied.set(true)
          dom.window.setTimeout(() => copied.set(false), 1800)
        }),
        child <-- copied.signal.map(c => if c then span(cls := "toast", "Link copied") else emptyNode)
      ),
      sections.map { sec =>
        fieldSet(cls := "section", legend(sec), Levers.schema.filter(_.section == sec).map(leverControl))
      }
    )

    val totals = outcome.map(o => Seq(
      Series("Current law", "series-1", o.baselineTotal),
      Series("Your plan", "series-2", o.proposedTotal, emph = true)))
    val breakdown = outcome.map(o => App.breakdown(o, proposed = true))

    def stat(label: String, value: Signal[String], delta: Signal[String] = Val(""), hero: Boolean = false) =
      div(cls := (if hero then "stat hero" else "stat"),
        div(cls := "label", label), div(cls := "value", child.text <-- value), div(cls := "delta", child.text <-- delta))

    def rateStat(label: String, which: GroupInfo => Boolean) =
      stat(label, outcome.map(o => Format.pct(App.groupRate(o, true, which))),
        outcome.map(o => s"Current law: ${Format.pct(App.groupRate(o, false, which))}"))

    val results = div(
      div(cls := "stats",
        stat("Net new revenue, % of national income", outcome.map(o => Format.signedPct(o.revenueShareOfIncome)), Val("relative to current law"), hero = true),
        stat("Per year (2019 dollars)", outcome.map(o => Format.bn(o.annualRevenue, signed = true)), outcome.map(o => s"${Format.bn(o.budgetWindowRevenue, signed = true)} over 2021–2030")),
        rateStat("Bottom 50% tax rate", App.bottom50),
        rateStat("Top 0.1% tax rate", App.top01),
        rateStat("Top 400 tax rate", App.top400)
      ),
      div(cls := "card",
        div(cls := "card-head", div(cls := "titles",
          h2("Total tax rate by income group"),
          p("All taxes as a share of pre-tax income: current law vs your plan. Hover or use ←/→ for details."))),
        Charts.legend(totals),
        Charts.lineChart(App.xs, totals, App.ticks, App.labels, App.groupSub, ariaLabel = "Total tax rate by income group, current law vs your plan"),
        Charts.dataTable(App.labels, totals)
      ),
      div(cls := "card",
        div(cls := "card-head", div(cls := "titles", h2("Your plan, by tax"), p("Effective rate of each tax by income group."))),
        Charts.legend(breakdown, rects = true),
        Charts.lineChart(App.xs, breakdown, App.ticks, App.labels, App.groupSub, stacked = true, height = 300, ariaLabel = "Your plan by tax"),
        Charts.dataTable(App.labels, breakdown.map(App.withTotal))
      ),
      div(cls := "card",
        div(cls := "card-head", div(cls := "titles", h2("Revenue by tax"), p("Share of national income; change in dollars per year."))),
        div(cls := "table-wrap", styleAttr := "max-height:none",
          child <-- outcome.map { o =>
            table(
              thead(tr(th("Tax"), th("Current law"), th("Your plan"), th("Change per year"))),
              tbody(o.ids.map { id =>
                val (b, p) = (o.baselineAverageRate(id), o.proposedAverageRate(id))
                tr(td(instrumentLabel(id)), td(Format.pct(b, 2)), td(Format.pct(p, 2)),
                  td(cls := (if p - b > 1e-9 then "pos" else if p - b < -1e-9 then "neg" else ""),
                    if math.abs(p - b) < 1e-9 then "–" else Format.bn((p - b) * Sim.NationalIncome, signed = true)))
              },
              tr(td(b("Total")), td(b(Format.pct(o.baselineAverageRate, 2))), td(b(Format.pct(o.proposedAverageRate, 2))),
                td(b(Format.bn(o.annualRevenue, signed = true)))))
            )
          }
        )
      )
    )

    div(cls := "page",
      state --> { s =>
        val q = query(s)
        dom.window.history.replaceState(null, "", "#/simulator" + q)
      },
      div(cls := "lede",
        h1("Design your own tax system"),
        p(cls := "secondary",
          "Move the levers and watch the tax rate of every income group, and the revenue raised, update instantly. ",
          "Brackets can be set at any threshold: type amounts like 400k, $2.5m or 1bn. ",
          "Use “Copy link” to share your plan.")
      ),
      div(cls := "sim", panel, results)
    )
