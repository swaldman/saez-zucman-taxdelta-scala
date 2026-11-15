package taxdelta.model

object ExcessCurve:
  /** For one income group, the average excess of a base (wealth, taxable income, …) over a
   *  threshold, X(t) = E[max(x − t, 0)], tabulated at grid thresholds and interpolated in between.
   *
   *  X is non-increasing and convex in t, with slope −P(x > t) ∈ [−1, 0]. Between grid points:
   *   - when both endpoints are positive and t_k > 0, we interpolate log-linearly in (log t, log X).
   *     This is exact when the group's distribution has a Pareto tail on the interval:
   *     with survival ∝ t^(−a), X(t) ∝ t^(1−a). The result is clamped to the lower bound below.
   *   - otherwise (the first segment from t = 0, or the last segment before X reaches 0) the true
   *     value lies between the chord (an upper bound, by convexity) and max(X_k − (t − t_k), X_{k+1})
   *     (a lower bound, since the slope is ≥ −1). We take the midpoint of the two bounds.
   *  Above the last grid point we extend the last Pareto segment, or return 0 if X is already 0.
   *  At grid thresholds the curve returns the tabulated values exactly.
   */
  final case class Default(thresholds: IndexedSeq[Double], values: IndexedSeq[Double]) extends ExcessCurve:
    require(thresholds.nonEmpty && thresholds.length == values.length, "thresholds/values mismatch")
    require(thresholds.indices.drop(1).forall(k => thresholds(k) > thresholds(k - 1)), s"thresholds must increase: $thresholds")

    def imputeHigherBrackets( higherThresholds : IndexedSeq[Double], decliningWealthFactors : IndexedSeq[Double] ) : Default =
      require(higherThresholds.nonEmpty && higherThresholds.length == decliningWealthFactors.length, "higher thresholds/declining wealth factors mismatch")
      require(higherThresholds(0) > thresholds.last, s"imputed higher thresholds must be higher than the highest empirical threshold: thresholds ${thresholds}, higher thresholds ${higherThresholds}")
      require(higherThresholds.indices.drop(1).forall(k => higherThresholds(k) > higherThresholds(k - 1)), s"imputed higher thresholds must increase: $higherThresholds")
      require(decliningWealthFactors.indices.drop(1).forall(k => decliningWealthFactors(k) <= decliningWealthFactors(k - 1)), s"declining wealth factors must not increase: $decliningWealthFactors")
      require(decliningWealthFactors.forall(f => f >= 0 && f <= 1), s"declining wealth factors must be between 0 and 1: $decliningWealthFactors")
      val base = values.last
      val extraValues = decliningWealthFactors.map(_ * base)
      Default(thresholds ++ higherThresholds, values ++ extraValues)

    override def excessFromThreshold(t: Double): Double =
      val n = thresholds.length
      if t <= thresholds(0) then values(0) // below the grid: no information, hold the first value
      else
        // locate segment k with t_k < t ≤ t_{k+1}
        var k = 0
        while k + 1 < n && thresholds(k + 1) < t do k += 1
        if k + 1 < n then
          if thresholds(k + 1) == t then values(k + 1) else segment(k, t)
        else extrapolate(t)

    private def segment(k: Int, t: Double): Double =
      val (t0, t1, x0, x1) = (thresholds(k), thresholds(k + 1), values(k), values(k + 1))
      if x0 <= 0.0 then 0.0
      else if x1 > 0.0 && t0 > 0.0 then
        val b = math.log(x1 / x0) / math.log(t1 / t0)
        // Convex, so below the chord; clamp to the slope ≥ −1 lower bound where the tail is not Pareto.
        math.max(x0 * math.pow(t / t0, b), math.max(x0 - (t - t0), x1))
      else
        val chord = x0 + (x1 - x0) * (t - t0) / (t1 - t0)
        val lower = math.max(x0 - (t - t0), x1)
        (chord + math.min(lower, chord)) / 2

    private def extrapolate(t: Double): Double =
      val n = thresholds.length
      val (tl, xl) = (thresholds(n - 1), values(n - 1))
      if xl <= 0.0 then 0.0
      else if n >= 2 && values(n - 2) > 0.0 && thresholds(n - 2) > 0.0 && xl < values(n - 2) then
        val b = math.log(xl / values(n - 2)) / math.log(tl / thresholds(n - 2))
        xl * math.pow(t / tl, b)
      else math.max(xl - (t - tl), 0.0) // no tail information: lower bound
  object Default:
    val zero: ExcessCurve = ExcessCurve.Default(IndexedSeq(0.0), IndexedSeq(0.0))
trait ExcessCurve:
  def excessFromThreshold(t: Double): Double

