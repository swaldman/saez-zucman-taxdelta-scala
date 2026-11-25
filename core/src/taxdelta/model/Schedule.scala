package taxdelta.model

object Schedule:
  val empty: Schedule = Schedule(IndexedSeq(0.0), IndexedSeq(0.0))
  def flat(rate: Double): Schedule = Schedule(IndexedSeq(0.0), IndexedSeq(rate))
  def fromUpperBoundedBrackets(untilThresholds : IndexedSeq[Double], rates : IndexedSeq[Double]) =
    require(
      rates.size == untilThresholds.size + 1,
      s"We expect upper bounds paired with rates, plus a final rate with no upper bound, so rates.size (${rates.size}) should be untilThresholds.size + 1 (${untilThresholds.size + 1})."
    )
    Schedule( 0d +: untilThresholds, rates )
  def fromLowerBoundedBrackets(fromThresholds : IndexedSeq[Double], rates : IndexedSeq[Double]) =
    require(
      rates.size == fromThresholds.size,
      s"We expect lower bounds paired with rates, plus a final rate with no upper bound, so rates.size (${rates.size}) should be fromThresholds.size (${fromThresholds.size}), and values before the earliest rate are presumed untaxed."
    )
    Schedule( fromThresholds, rates )
/** A piecewise-linear marginal-rate schedule: `rates(k)` applies to the part of the base above
 *  `thresholds(k)` (and below `thresholds(k+1)`). Thresholds are strictly increasing. */
final case class Schedule(thresholds: IndexedSeq[Double], rates: IndexedSeq[Double]):
  require(thresholds.length == rates.length, s"${thresholds.length} thresholds but ${rates.length} rates")
  require(thresholds.indices.drop(1).forall(k => thresholds(k) > thresholds(k - 1)), s"thresholds must increase: $thresholds")

  /** Average tax for a group whose average excess over threshold t is `excess(t)`:
   *  Σ_k (r_k − r_{k−1}) · excess(t_k). Exact for any distribution within the group. */
  def averageTax(excess: Double => Double): Double =
    var s = 0.0
    var prev = 0.0
    var k = 0
    while k < rates.length do
      val dr = rates(k) - prev
      if dr != 0.0 then s += dr * excess(thresholds(k))
      prev = rates(k)
      k += 1
    s

  def averageTax(excessCurve : ExcessCurve) : Double = averageTax( excessCurve.excessFromThreshold )

  /** Marginal rate applying at base level x. */
  def rateAt(x: Double): Double =
    val k = thresholds.lastIndexWhere(_ <= x)
    if k < 0 then 0.0 else rates(k)

  /** Splits the bracket containing `t` at `t` (no change in tax). */
  def insertThreshold(t: Double): Schedule =
    if thresholds.contains(t) then this
    else
      val k = thresholds.indexWhere(_ > t)
      val at = if k < 0 then thresholds.length else k
      Schedule(thresholds.patch(at, Seq(t), 0), rates.patch(at, Seq(rateAt(t)), 0))

  /** Raises every marginal rate above `t` to at least `r` (e.g. "a 50% top bracket above $3m"). */
  def withMinimumRateAbove(t: Double, r: Double): Schedule =
    val s = insertThreshold(t)
    s.copy(rates = s.rates.zip(s.thresholds).map((rate, th) => if th >= t then math.max(rate, r) else rate))

  def withRates(rs: IndexedSeq[Double]): Schedule = copy(rates = rs)
  def scaled(f: Double): Schedule = copy(rates = rates.map(_ * f))

  /** Adds a fixed surcharge to every bracket (`first` for the first bracket, `others` above). */
  def plusSurcharge(first: Double, others: Double): Schedule =
    copy(rates = rates.zipWithIndex.map((r, k) => r + (if k == 0 then first else others)))
