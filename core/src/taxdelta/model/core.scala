package taxdelta.model

import scala.collection.immutable

trait Parameter[L]:
  def label : L
  def value : Any

sealed trait Group:
  def size          : Int
  def minIncome     : Double
  def averageIncome : Double
  def totalIncome   : Double = size * averageIncome

object PercentileGroup:
  given Ordering[PercentileGroup] = Ordering.by[PercentileGroup,Tuple3[Double,Double,Int]]( pg => (pg.percentileLowerBound, pg.averageIncome, pg.size) )
final case class PercentileGroup( size : Int, minIncome : Double, averageIncome : Double, percentileLowerBound : Double ) extends Group
final case class TopGroup( size : Int, minIncome : Double, averageIncome : Double ) extends Group;

final case class Groups( percentileGroups : immutable.SortedSet[PercentileGroup], topGroup : TopGroup ):
  def contains( g : Group ) : Boolean =
    g match
      case pg : PercentileGroup => percentileGroups.contains(pg)
      case tg : TopGroup        => tg == topGroup
  def toSeq = percentileGroups.toSeq :+ topGroup

/*
 *  we only have data up to wealth > $1B, so we make some assumptions
 *  we assume 90% of wealth over $1B also over $2.5B,
 *  but only  60% of wealth over $1B is over $5B,
 *  and only  45% of wealth over $1B is also over $10B.
 */
val ImputedHigherBrackets = IndexedSeq[Double](2_500_000_000, 5_000_000_000, 10_000_000_000)
val ImputationFactors     = IndexedSeq[Double](0.9d, 0.6d, 0.45d)
val ImputationZeroes      = ImputationFactors.map( _ => 0.0d )

def imputeHigherBrackets( topGroupOnly : Boolean )( tup : Tuple2[Group,ExcessCurve.Default] ) : Tuple2[Group,ExcessCurve.Default] =
  val (k,v) = tup
  if topGroupOnly then
    k match
      case tg : TopGroup => (k, v.imputeHigherBrackets(ImputedHigherBrackets,ImputationFactors))
      case _             => (k, v.imputeHigherBrackets(ImputedHigherBrackets,ImputationZeroes))
  else
    (k, v.imputeHigherBrackets(ImputedHigherBrackets,ImputationFactors) )

def imputeHigherBracketsTopGroupOnly( tup : Tuple2[Group,ExcessCurve.Default] ) : Tuple2[Group,ExcessCurve.Default] = imputeHigherBrackets( true )( tup )

