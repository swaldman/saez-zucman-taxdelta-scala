package taxdelta.general

import scala.collection.immutable

import taxdelta.model.{ExcessCurve,Group,Groups,PercentileGroup,Schedule,TopGroup}

object FixtureTaxdeltaData:
  /** Every country-year the build generated fixtures for, by year then ISO2. */
  lazy val byYear : Map[Int, Map[String, FixtureTaxdeltaData]] =
    fixtures.AllFixtures.years.map( yf => yf.year -> yf.byIso.map( (iso, cf) => iso -> new FixtureTaxdeltaData(yf, cf) ) ).toMap

  def apply( year : Int, iso : String ) : FixtureTaxdeltaData =
    byYear.get(year).flatMap(_.get(iso)).getOrElse:
      throw new NoSuchElementException(s"No general-model data for $iso in $year")

/** A country-year of the general model, read from the generated fixtures. */
final class FixtureTaxdeltaData( yf : YearFixtures, cf : CountryFixtures ) extends TaxdeltaData:
  import TaxdeltaData.*

  private val c = yf.countries.row(cf.iso)

  val iso         : String  = cf.iso
  val year        : Int     = yf.year
  val countryName : String  = yf.countries.string(c, "countryName")
  val region      : String  = yf.countries.string(c, "region")
  val yearUsdPPP  : Int     = yf.countries.double(c, "yearUsdPPP").toInt
  val defl        : Double  = yf.countries.double(c, "defl")
  val xppp_us     : Double  = yf.countries.double(c, "xppp_us")
  val population  : Long    = yf.countries.double(c, "population").toLong
  val quality     : Quality = Quality.valueOf(yf.countries.string(c, "quality"))

  /** Gethin–Fisher's total pretax income over WID's national income: a check on the currency conversion. */
  val nationalIncomeCheck : Option[Double] = yf.countries.doubleOption(c, "niCheck")

  // ---- groups --------------------------------------------------------------------------------------------
  private val g = cf.groups
  private val n = g.numRows

  /** The groups in fixture order, bottom to top; the last is the top group. */
  val groupSeq : IndexedSeq[Group] =
    (0 until n).map: i =>
      val size = math.round(g.double(i, "size")).toInt
      val min  = g.double(i, "minIncome")
      val avg  = g.double(i, "averageIncome")
      if i < n - 1 then PercentileGroup(size, min, avg, g.double(i, "percentileLowerBound"))
      else TopGroup(size, min, avg)

  val groups : Groups =
    Groups(immutable.SortedSet.from(groupSeq.init.map(_.asInstanceOf[PercentileGroup])), groupSeq.last.asInstanceOf[TopGroup])

  // ---- individual income tax -----------------------------------------------------------------------------
  private val base = yf.individualIncomeTaxBase

  val paramIndividualIncomeTaxCalibratedBaseShareOfIncome : Option[Double] =
    if base.hasRow(iso) then base.doubleOption(iso, "calibratedBaseShareOfIncome") else None

  /** The share of pretax income the calibrated base reproduces: Gethin–Fisher's personal income tax total. */
  val individualIncomeTaxTargetShareOfIncome : Option[Double] =
    if base.hasRow(iso) then base.doubleOption(iso, "targetShareOfIncome") else None

  val baselineIndividualIncomeTaxSchedule : Option[Schedule] =
    val s = cf.incomeTaxSchedule
    Some(Schedule.fromLowerBoundedBrackets(s.doubles("lowerThreshold"), s.doubles("rate")))

  val dataEffectiveIncomeTaxShareOfIncome : Option[Map[Group, Double]] =
    Some(groupSeq.indices.map(i => groupSeq(i) -> g.double(i, "effectiveIncomeTaxShareOfIncome")).toMap)

  /** Pretax income, unscaled; apply the calibrated base share with `ScaledExcessCurve`. */
  val dataIncomeExcessCurve : Option[Map[Group, ExcessCurve]] =
    Some(groupSeq.indices.map: i =>
      val gr = groupSeq(i)
      val upper = if i + 1 < n then Some(groupSeq(i + 1).minIncome) else None
      gr -> (GroupExcessCurve(gr.minIncome, upper, gr.averageIncome) : ExcessCurve)
    .toMap)

  // ---- notes ---------------------------------------------------------------------------------------------
  override val notes : IndexedSeq[Note] =
    val t = cf.notes
    (0 until t.numRows).map(i => Note(t.string(i, "id"), t.string(i, "generator"), t.string(i, "topic"), t.string(i, "note")))

  override def toString : String = s"FixtureTaxdeltaData($iso, $year)"
