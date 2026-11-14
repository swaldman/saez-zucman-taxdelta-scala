package taxdelta.data

/** A minimal RFC-4180-ish CSV reader (quoted fields, doubled quotes), sufficient for our datasets. */
final case class Csv(header: IndexedSeq[String], rows: IndexedSeq[IndexedSeq[String]]):
  val numColumns = header.length
  val numRows    = rows.length

  require(numColumns > 0, "We insist on there being at least one column.")
  require(rows.forall(_.length == numColumns), "All rows must be the same size, as defined by the header.")

  private val rowIndex: Map[String, Int] = rows.map(_(0)).zipWithIndex.toMap
  private val colIndex: Map[String, Int] = header.zipWithIndex.toMap

  def hasRow(row: String): Boolean       = rowIndex.contains(row)
  def hasColumn(column: String): Boolean = colIndex.contains(column)

  def rowColumn = header(0)

  def value(row : Int, col : Int) : String = rows(row)(col)

  def value(row : String, col : String) : String = value(rowIndex(row), colIndex(col))

  private def double( s : String, blank : Double) : Double = if s.isEmpty then blank else s.toDouble

  def doubleValue(row : Int, col : Int, blank : Double = 0.0) = double(value(row,col).trim,blank)

  // can't have two overloads with default values grrrr
  def doubleValue(row : String, col : String, blank : Double) = double(value(row,col).trim,blank)
  def doubleValue(row : String, col : String) = double(value(row,col).trim,0.0d)

object Csv:
  def parse(text: String): Csv =
    val records = parseRecords(text).filter(r => !(r.length == 1 && r.head.isEmpty))
    require(records.nonEmpty, "empty CSV")
    Csv(records.head, records.tail)

  private def parseRecords(text: String): IndexedSeq[IndexedSeq[String]] =
    val out = IndexedSeq.newBuilder[IndexedSeq[String]]
    var row = IndexedSeq.newBuilder[String]
    val field = new StringBuilder
    var inQuotes = false
    var i = 0
    def endField(): Unit = { row += field.toString; field.clear() }
    def endRow(): Unit = { endField(); out += row.result(); row = IndexedSeq.newBuilder[String] }
    while i < text.length do
      val c = text.charAt(i)
      if inQuotes then
        if c == '"' then
          if i + 1 < text.length && text.charAt(i + 1) == '"' then { field += '"'; i += 1 }
          else inQuotes = false
        else field += c
      else
        c match
          case '"'  => inQuotes = true
          case ','  => endField()
          case '\n' => endRow()
          case '\r' => ()
          case _    => field += c
      i += 1
    if field.nonEmpty || text.nonEmpty && text.last != '\n' then endRow()
    out.result()
