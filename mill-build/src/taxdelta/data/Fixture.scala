package taxdelta.data

import java.nio.charset.StandardCharsets
import scala.util.Using

object FixtureSource:
  def fromCsvResource(packageName : String, objectName : String, rp : String) : FixtureSource =
    val csvData = Using.resource(this.getClass().getClassLoader().getResourceAsStream(rp)): is =>
      new String(is.readAllBytes(), StandardCharsets.UTF_8)
    val csv = Csv.parse(csvData)
    FixtureSource(packageName,objectName,csv)
final class FixtureSource(packageName : String, objectName : String, csv : Csv):
  private final val TQ = "\"\"\""
  private def row(r : IndexedSeq[String]) : String = s"""IndexedSeq(${r.mkString(TQ,TQ+","+TQ,TQ)})"""
  private def rows : String = csv.rows.map(row).mkString("    ",",\n    ","")
  override def toString() : String=
    s"""|package ${packageName}
        |
        |object ${objectName}:
        |  val header = IndexedSeq(${csv.header.mkString(TQ,TQ+","+TQ,TQ)})
        |  val rows   = IndexedSeq(
        |${rows}
        |  )
        |
        |  val numColumns = header.length
        |  val numRows    = rows.length
        |
        |  require(numColumns > 0, "We insist on there being at least one column.")
        |  require(rows.forall(_.length == numColumns), "All rows must be the same size, as defined by the header.")
        |
        |  private val rowIndex: Map[String, Int] = rows.map(_(0)).zipWithIndex.toMap
        |  private val colIndex: Map[String, Int] = header.zipWithIndex.toMap
        |
        |  def hasRow(row: String): Boolean       = rowIndex.contains(row)
        |  def hasColumn(column: String): Boolean = colIndex.contains(column)
        |
        |  def rowColumn = header(0)
        |
        |  def value(row : Int, col : Int) : String = rows(row)(col)
        |
        |  def value(row : String, col : String) : String = value(rowIndex(row), colIndex(col))
        |
        |  private def double( s : String, blank : Double) : Double = if s.isEmpty then blank else s.toDouble
        |
        |  def doubleValue(row : Int, col : Int, blank : Double = 0.0) = double(value(row,col).trim,blank)
        |
        |  // can't have two overloads with default values grrrr
        |  def doubleValue(row : String, col : String, blank : Double) = double(value(row,col).trim,blank)
        |  def doubleValue(row : String, col : String) = double(value(row,col).trim,0.0d)
        |""".stripMargin
