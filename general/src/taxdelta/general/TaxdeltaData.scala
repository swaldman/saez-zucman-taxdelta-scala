package taxdelta.general

import taxdelta.model.{ExcessCurve,Group,Groups,Schedule}

object TaxdeltaData:
  val SomeZero  = Some(0d)
  val SomeFalse = Some(false)
  enum Quality:
    case Deep, Credible, Modeled

  /** How the data pipeline handled an ambiguity or a gap, for one country-year. `generator` is the fixture
   *  generator that wrote it; `topic` groups notes for display (e.g. "income-tax-schedule"). */
  final case class Note( id : String, generator : String, topic : String, text : String )

import TaxdeltaData.*

trait TaxdeltaData:
  def countryName : String
  def iso         : String
  def year        : Int
  def groups      : Groups
  def yearUsdPPP  : Int
  def defl        : Double
  def xppp_us     : Double

  def quality : TaxdeltaData.Quality

  // notes on sources, ambiguities and missing data, for users to examine
  def notes : IndexedSeq[TaxdeltaData.Note] = IndexedSeq.empty

  // for the general models, there's usually no need to rebase
  def averageIncomeRebaseFactor : Option[Group => Double] = Some(_ => 1.0d)
 
  // soon, but not yet
  //
  // def paramAverageSalesTaxShareOfIncome                        : Option[Double]
  // def paramAverageValueAddedTaxableConsumptionShareOfIncome    : Option[Double]
  // def paramConsumerIncidenceShare                              : Option[Double] // to which VAT applies
  // def paramCorporateTaxCurrentDomesticProfits                  : Option[Double]
  // def paramCorporateTaxCurrentDomesticReceipts                 : Option[Double]
  // def paramCorporateTaxCurrentForeignReceipts                  : Option[Double]
  // def paramCorporateTaxForeignReceiptsUnderStrongerEnforcement : Option[Double]
  // def paramEstateTaxCurrentAvoidanceRate                       : Option[Double]
  // def paramEstateTaxCurrentStatutoryTopRate                    : Option[Double]
  // final def baselineCorporateTaxDeployStrongerEnforcement            : Option[Boolean] = SomeFalse
  // def baselineCorporateTaxProposedEffectiveRate                : Option[Double]
  // def baselineEstateTaxCurrentStatutoryTopRate                 : Option[Double]
  // def baselineEstateTaxProposedTopRate                         : Option[Double]
  // def baselineEstateTaxAnticipatedAvoidanceRate                : Option[Double]

  // we're going to leave out a separate "national income (flat) tax" instrument,
  // since we are merging sometimes-flat subnational taxes into the straight
  // individual income tax. if a country adds a flat income tax at a national
  // level, we'll just merge that in as well
  //
  // def baselineNationalIncomeTaxRate                            : Option[Double]
  // def dataNationalIncomeTaxBase                                : Option[Map[Group,Double]]

  // soon, but not yet
  //
  // final def baselineSalesAndExciseTaxReduction                 : Option[Double] = SomeZero
  // def baselineValueAddedTaxRate                                : Option[Double]
  // def baselineWealthTaxEvasionRate                             : Option[Double]
  // def baselineWealthTaxSchedule                                : Option[Schedule]
  // def dataCorporateShareOfIncome                               : Option[Map[Group,Double]]
  // def dataEstateTaxShareOfIncome                               : Option[Map[Group,Double]]
  // def dataPayrollTaxShareOfIncome                              : Option[Map[Group,Double]]
  // def dataPropertyTaxShareOfIncome                             : Option[Map[Group,Double]]
  // def dataSalesTaxShareOfIncome                                : Option[Map[Group,Double]]
  // def dataWealthExcessCurve                                    : Option[Map[Group,Double]]

  // I suspect we will have to smplify Individual Income Tax, reduce levers to
  // just the schedule alone. But for now...
  //
  // def paramCorporateTaxCurrentStateReceipts                             : Option[Double]
  // def baselineCloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates         : Option[Boolean]

  // Fraction of each individual's pretax national income treated as income-tax base: incomes are scaled by
  // this factor before applying the schedule. Calibrated so the current schedule reproduces the observed
  // individual income tax total (Gethin–Fisher tax_dir_pit); it absorbs allowances, deductions, credits,
  // preferential rates and filing-unit effects, so it is not a measured taxable-income share.
  def paramIndividualIncomeTaxCalibratedBaseShareOfIncome               : Option[Double]

  def baselineIndividualIncomeTaxSchedule                               : Option[Schedule]
  // def individualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate : Option[Boolean] = Some(false)
  // def dataCloselyHeldCorpTotalRetainedEarnings                          : Option[Double]
  // def dataAverageOrdinaryMarginalTaxRate                                : Option[Map[Group,Double]]
  // def dataAvgPortionOfOrdinaryMarginalTaxRateAbovePreferenceRate        : Option[Map[Group,Double]]
  def dataEffectiveIncomeTaxShareOfIncome                               : Option[Map[Group,Double]]
  // def dataFiscalCapitalGains                                            : Option[Map[Group,Double]]
  // def dataFiscalDividends                                               : Option[Map[Group,Double]]
  // def dataIncomeIncludingCapitalGains                                   : Option[Map[Group,Double]]
  def dataIncomeExcessCurve                                             : Option[Map[Group,ExcessCurve]]
  // def aggregateFiscalCapGainsAndDividendsTotal                          : Option[Double]


  // we skipped "replicateWorkbookForeignDoubleCount"
  // HealthTaxInstrument

