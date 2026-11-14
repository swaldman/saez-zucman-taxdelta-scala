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

