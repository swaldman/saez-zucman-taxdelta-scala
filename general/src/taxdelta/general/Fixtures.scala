package taxdelta.general

/** What the build generates from mill-build/resources/fixtures/general/<year>/, one object per year
 *  (`taxdelta.general.fixtures.Fixtures<year>`) and one per included country
 *  (`taxdelta.general.fixtures.Fixtures<year>_<ISO2>`). See `gen_*.py` in fixture-generators/ of the private
 *  saez-zucman-taxdelta-data-sources repository for the meaning of every column. */
trait YearFixtures:
  def year : Int

  /** countries.csv: iso, countryName, region, yearUsdPPP, defl, xppp_us, population, quality, widQualityScore, niCheck */
  def countries : Table

  /** catalog.csv: for each country, which inputs exist and which instruments are achievable. */
  def catalog : Table

  /** individual-income-tax-base.csv: iso, calibratedBaseShareOfIncome, targetShareOfIncome, scheduleShareAtFullBase */
  def individualIncomeTaxBase : Table

  /** Countries for which the build generated full fixtures: those with every instrument the general model
   *  currently supports (for now, the individual income tax). */
  def byIso : Map[String, CountryFixtures]

trait CountryFixtures:
  def iso : String

  /** groups/<ISO2>.csv: gperc, percentileLowerBound, percentileUpperBound, size, minIncome, averageIncome,
   *  effectiveIncomeTaxShareOfIncome. 127 g-percentiles; the last is the top group. National currency, current prices. */
  def groups : Table

  /** oecd/pit/<ISO2>.csv: bracket, lowerThreshold, rate. Central, sub-central and surtaxes combined. */
  def incomeTaxSchedule : Table

  /** notes/<ISO2>.csv: id, generator, topic, note */
  def notes : Table
