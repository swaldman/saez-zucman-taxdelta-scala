package taxdelta.general

import taxdelta.model.ExcessCurve

/** For one income group, X(t) = E[max(x − t, 0)], the average excess of income over a threshold t, built from
 *  only what the groups fixture gives us: the group's lower threshold m, its average a, and (for all but the top
 *  group) its upper bound u, the next group's lower threshold.
 *
 *   - t ≤ m: everyone in the group is above t, so X(t) = a − t exactly.
 *   - bounded group, t ≥ u: X(t) = 0.
 *   - bounded group, m < t < u: any distribution on [m, u] with mean a lies between two bounds:
 *     max(a − t, 0) (everyone at a) and the chord from (m, a − m) to (u, 0) (everyone at m or u).
 *     We take their midpoint, as `ExcessCurve.Default` does on such segments. Groups are narrow (1% or less of
 *     the population), so the error is small.
 *   - top group, t > m: a Pareto tail with minimum m and mean a, so α = a / (a − m) and
 *     X(t) = (a − m) (m / t)^(α − 1).
 *
 *  The calibration of the income tax base (gen_iit_base.py, in the private saez-zucman-taxdelta-data-sources
 *  repository) uses exactly this construction; if it changes here, it must change there, or the calibrated base will no longer reproduce
 *  observed revenue. */
final case class GroupExcessCurve( minIncome : Double, upperBound : Option[Double], averageIncome : Double ) extends ExcessCurve:
  override def excessFromThreshold( t : Double ) : Double =
    val m = minIncome
    val a = averageIncome
    if t <= m then a - t
    else upperBound match
      case None =>
        if a <= m || m <= 0 then 0.0
        else
          val alpha = a / (a - m)
          (a - m) * math.pow(m / t, alpha - 1)
      case Some(u) =>
        if t >= u || u <= m then 0.0
        else
          val chord = (a - m) * (u - t) / (u - m)
          val lower = math.max(a - t, 0.0)
          (chord + math.min(lower, chord)) / 2

/** The excess curve of a base scaled by `factor`: the excess of factor · x over t is factor · X(t / factor).
 *  Used to apply a schedule to the calibrated share of income. */
final case class ScaledExcessCurve( underlying : ExcessCurve, factor : Double ) extends ExcessCurve:
  require(factor > 0, s"A base scale factor must be positive, found $factor")
  override def excessFromThreshold( t : Double ) : Double = factor * underlying.excessFromThreshold(t / factor)
