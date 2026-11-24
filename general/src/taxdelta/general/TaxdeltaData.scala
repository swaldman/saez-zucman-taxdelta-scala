package taxdelta.general

import taxdelta.model.{ExcessCurve,Group,Groups,Schedule}

object TaxdeltaData:
  enum Quality:
    case Deep, Credible, Modeled

trait TaxdeltaData:
  def countryName : String
  def iso         : String
  def year        : Int
  def groups      : Groups
  def yearUsdPPP  : Int
  def defl        : Double
  def xppp_us     : Double

  def quality : TaxdeltaData.Quality

  def averageIncomeRebaseFactor : Option[Group => Double]
  def paramAverageSalesTaxShareOfIncome                        : Option[Double]
  def paramAverageValueAddedTaxableConsumptionShareOfIncome    : Option[Double]
  def paramConsumerIncidenceShare                              : Option[Double]
  def paramCorporateTaxCurrentDomesticProfits                  : Option[Double]
  def paramCorporateTaxCurrentDomesticReceipts                 : Option[Double]
  def paramCorporateTaxCurrentForeignReceipts                  : Option[Double]
  def paramCorporateTaxForeignReceiptsUnderStrongerEnforcement : Option[Double]
  def paramEstateTaxCurrentAvoidanceRate                       : Option[Double]
  def paramEstateTaxCurrentStatutoryTopRate                    : Option[Double]
  def baselineCorporateTaxDeployStrongerEnforcement            : Option[Double]
  def baselineCorporateTaxProposedEffectiveRate                : Option[Double]
  def baselineEstateTaxCurrentStatutoryTopRate                 : Option[Double]
  def baselineEstateTaxProposedTopRate                         : Option[Double]
  def baselineEstateTaxAnticipatedAvoidanceRate                : Option[Double]
  def baselineNationalIncomeTaxRate                            : Option[Double]
  def baselineSalesAndExciseTaxReduction                       : Option[Double]
  def baselineValueAddedTaxRate                                : Option[Double]
  def baselineWealthTaxEvasionRate                             : Option[Double]
  def baselineWealthTaxSchedule                                : Option[Schedule]
  def dataCorporateShareOfIncome                               : Option[Map[Group,Double]]
  def dataEstateTaxShareOfIncome                               : Option[Map[Group,Double]]
  def dataNationalIncomeTaxBase                                : Option[Map[Group,Double]]
  def dataPayrollTaxShareOfIncome                              : Option[Map[Group,Double]]
  def dataPropertyTaxShareOfIncome                             : Option[Map[Group,Double]]
  def dataSalesTaxShareOfIncome                                : Option[Map[Group,Double]]
  def dataWealthExcessCurve                                    : Option[Map[Group,Double]]

  // I suspect we will have to Individual Income Tax, reduce levers to
  // just the schedule alone. But for now...
  def paramCorporateTaxCurrentStateReceipts                             : Option[Double]
  def baselineCloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates         : Option[Boolean]
  def baselineIndividualIncomeTaxSchedule                               : Option[Schedule]
  def individualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate : Option[Boolean]
  def dataCloselyHeldCorpTotalRetainedEarnings                          : Option[Double]
  def dataAverageOrdinaryMarginalTaxRate                                : Option[Map[Group,Double]]
  def dataAvgPortionOfOrdinaryMarginalTaxRateAbovePreferenceRate        : Option[Map[Group,Double]]
  def dataEffectiveIncomeTaxShareOfIncome                               : Option[Map[Group,Double]]
  def dataFiscalCapitalGains                                            : Option[Map[Group,Double]]
  def dataFiscalDividends                                               : Option[Map[Group,Double]]
  def dataIncomeIncludingCapitalGains                                   : Option[Map[Group,Double]]
  def dataIncomeExcessCurve                                             : Option[Map[Group,ExcessCurve]]
  def aggregateFiscalCapGainsAndDividendsTotal                          : Option[Double]


  // we skipped "replicateWorkbookForeignDoubleCount"
  // HealthTaxInstrument

