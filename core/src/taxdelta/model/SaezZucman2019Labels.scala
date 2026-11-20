package taxdelta.model

import scala.util.control.NonFatal

abstract class SaezZucman2019Labels extends ModelContext:
  // LP means "label of parameter"
  enum LPBoolean extends InputLabel:
    case CountPrivateHealthPremiumsAsTax
    case ReplicateWorkbookForeignDoubleCount

  // Note: LongRun- prefixed params are to the experimental behavioral model
  enum LPDouble extends InputLabel:
    case AverageSalesTaxShareOfIncome
    case AverageValueAddedTaxableConsumptionShareOfIncome
    case CloselyHeldCorpTotalRetainedEarnings
    case ConsumerIncidenceShare
    case CorporateTaxCurrentDomesticProfits
    case CorporateTaxCurrentDomesticReceipts
    case CorporateTaxCurrentForeignReceipts
    case CorporateTaxCurrentStateReceipts
    case CorporateTaxForeignReceiptsUnderStrongerEnforcement
    case EmployerShareOfPrivateHealthPremiums
    case EstateTaxCurrentAvoidanceRate
    case EstateTaxCurrentStatutoryTopRate
    case FiscalCapitalGainsTaxableShare // income stats exclude realized gains, but the tax base does in part include them
    case LongRunAveragePreTaxIncomeGrowth
    case LongRunBargainingElasticity
    case LongRunIncentiveElasticity
    case LongRunImpliedMarginalTaxRateCap
    case LongRunMarginalTaxRateSmoothingHalfWindow
    case LongRunMarginalTaxRateSmoothingPasses

  enum LPOther extends InputLabel:
    case LongRunBargainingWeightFunction // a function Group => Double

  // LD means "label of a (per-group) data item"
  enum LDDouble extends InputLabel:
    case AvgOrdinaryMarginalTaxRate
    case AvgPortionOfOrdinaryMarginalTaxRateAboveTwentyPercent
    case CorporateTaxShareOfIncome
    case EffectiveIncomeTaxShareOfIncome
    case EmployersHealthInsuranceContributionShareOfIncome
    case EstateTaxShareOfIncome
    case PayrollTaxShareOfIncome
    case PropertyTaxShareOfIncome
    case SalesTaxShareOfIncome
    case NationalIncomeTaxBase
    case NumberOfAdults
    case FiscalCapitalGains
    case FiscalDividends
    case FiscalIncomeIncludingCapitalGains
    case LongRunPretaxIncome1980
    case LongRunPretaxIncome2018 // basically the same as denom, or our rebased agi, but there are some small differences, so we use this to reproduce the worksheet

  enum LDOther extends InputLabel:
    case IncomeExcessCurve
    case WealthExcessCurve

  // LA means "label of an aggregation precomputed from the data"
  enum LADouble extends InputLabel:
    case TotalGainsAndDividends

  // LL means "label of a potential policy lever"
  enum LLBoolean extends InputLabel:
    case CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates
    case CorporateTaxDeployStrongerEnforcement
    case EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax
    case IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate
    case IndividualIncomeTaxDividendsTaxedAtOrdinaryRate
    case IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate

  enum LLDouble extends InputLabel:
    case CorporateTaxProposedEffectiveRate
    case EstateTaxAnticipatedAvoidanceRate
    case EstateTaxProposedTopRate
    case NationalIncomeTaxRate // a flat tax
    case ValueAddedTaxRate
    case ReductionOfEmployerSponsoredPrivateHealthPremiums
    case SalesAndExciseTaxReduction // existing tax rescaled by (1 - SalesAndExciseTaxReduction)
    case WealthTaxEvasionRate

  enum LLOther extends InputLabel:
    case IndividualIncomeTaxSchedule
    case WealthTaxSchedule

  enum LInstrument:
    case CorporateTax
    case EstateTax
    case HealthInsuranceTax
    case IndividualIncomeTax
    case NationalIncomeTax
    case PayrollTax
    case PropertyTax
    case SalesAndExciseTax
    case ValueAddedTax
    case WealthTax

  export LPBoolean.*, LPDouble.*, LPOther.*, LDDouble.*, LDOther.*, LADouble.*, LLBoolean.*, LLDouble.*, LLOther.*, LInstrument.*

  override type LabelParameterBoolean  = LPBoolean
  override type LabelParameterDouble   = LPDouble
  override type LabelParameterOther    = LPOther
  override type LabelDataDouble        = LDDouble
  override type LabelDataOther         = LDOther
  override type LabelAggregationDouble = LADouble
  override type LabelLeverBoolean      = LLBoolean
  override type LabelLeverDouble       = LLDouble
  override type LabelLeverOther        = LLOther
  override type LabelInstrument        = LInstrument

  private def toParameterBoolean( key : String, value : Any ) : Option[Tuple2[LPBoolean,Boolean]] = 
    value match
      case vb : Boolean => // we can't rely on exception from cast when running under JS
        try
          Some( Tuple2( LPBoolean.valueOf(key), vb ) )
        catch
          case NonFatal(t) => None
      case _ => None

  private def toParameterDouble( key : String, value : Any ) : Option[Tuple2[LPDouble,Double]] =
    value match
      case vd : Double => // we can't rely on exception from cast when running under JS
        try
          Some( Tuple2( LPDouble.valueOf(key), vd ) )
        catch
          case NonFatal(t) => None
      case vx : (Byte | Short | Int | Long | Float | BigInt | BigDecimal) => // the JS version will let other numerics pass through the first case, so for consistency...
        val d : Double =
          vx match
            case b  : Byte       => b.toDouble
            case s  : Short      => s.toDouble
            case i  : Int        => i.toDouble
            case l  : Long       => l.toDouble
            case f  : Float      => f.toDouble
            case bi : BigInt     => bi.toDouble
            case bd : BigDecimal => bd.toDouble
        try
          Some( Tuple2( LPDouble.valueOf(key), d ) )
        catch
          case NonFatal(t) => None
      case _ => None

  private def toParameterOther( key : String, value : Any ) : Option[Tuple2[LPOther,Any]] =
    try
      Some( Tuple2( LPOther.valueOf(key), value ) )
    catch
      case NonFatal(t) => None

  protected def findOverrideParametersBoolean( parameterOverrides : Map[String,Any] ) : Map[LPBoolean,Boolean] = parameterOverrides.map( toParameterBoolean ).flatten.toMap
  protected def findOverrideParametersDouble ( parameterOverrides : Map[String,Any] ) : Map[LPDouble,Double]   = parameterOverrides.map( toParameterDouble ).flatten.toMap
  protected def findOverrideParametersOther  ( parameterOverrides : Map[String,Any] ) : Map[LPOther,Any]       = parameterOverrides.map( toParameterOther ).flatten.toMap

