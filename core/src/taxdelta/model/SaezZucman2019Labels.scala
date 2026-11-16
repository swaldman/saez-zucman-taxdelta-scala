package taxdelta.model

import scala.util.control.NonFatal

abstract class SaezZucman2019Labels extends ModelContext:
  enum LPBoolean extends InputLabel:
    case CountPrivateHealthPremiumsAsTax

  enum LPDouble extends InputLabel:
    case AverageSalesTaxShareOfIncome
    case AverageValueAddedTaxableConsumptionShareOfIncome
    case ConsumerIncidenceShare
    case EmployerShareOfPrivateHealthPremiums
    case EstateTaxCurrentAvoidanceRate
    case EstateTaxCurrentStatutoryTopRate
    case RealizedGainsTaxableShare // income stats exclude realized gains, but the tax base does in part include them

  enum LDDouble extends InputLabel:
    case EmployersHealthInsuranceContributionShareOfIncome
    case EstateTaxShareOfIncome
    case PayrollTaxShareOfIncome
    case SalesTaxShareOfIncome

  enum LDInt extends InputLabel:
    case NationalIncomeTaxBase
    case RealizedCapitalGains

  enum LDExcessCurve extends InputLabel:
    case WealthExcessCurve

  enum LLBoolean extends InputLabel:
    case EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax

  enum LLDouble extends InputLabel:
    case EstateTaxAnticipatedAvoidanceRate
    case EstateTaxProposedTopRate
    case NationalIncomeTaxRate // presumably a flat tax?
    case ValueAddedTaxRate
    case ReductionOfEmployerSponsoredPrivateHealthPremiums
    case WealthTaxEvasionRate

  enum LLSchedule extends InputLabel:
    case WealthTaxSchedule

  enum LInstrument:
    case EstateTax
    case HealthInsuranceTax
    case NationalIncomeTax
    case PayrollTax
    case ValueAddedTax
    case WealthTax

  export LPBoolean.*, LPDouble.*, LDDouble.*, LDInt.*, LDExcessCurve.*, LLBoolean.*, LLDouble.*, LLSchedule.*, LInstrument.*

  override type LabelParameterBoolean = LPBoolean
  override type LabelParameterDouble  = LPDouble
  override type LabelDataDouble       = LDDouble
  override type LabelDataInt          = LDInt
  override type LabelDataExcessCurve  = LDExcessCurve
  override type LabelLeverBoolean     = LLBoolean
  override type LabelLeverDouble      = LLDouble
  override type LabelLeverSchedule    = LLSchedule
  override type LabelInstrument       = LInstrument

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

  private def toParameterBoolean( key : String, value : Any ) : Option[Tuple2[LPBoolean,Boolean]] = 
    value match
      case vb : Boolean => // we can't rely on exception from cast when running under JS
        try
          Some( Tuple2( LPBoolean.valueOf(key), vb ) )
        catch
          case NonFatal(t) => None
      case _ => None

  protected def findOverrideParametersDouble ( parameterOverrides : Map[String,Any] ) : Map[LPDouble,Double]   = parameterOverrides.map( toParameterDouble ).flatten.toMap
  protected def findOverrideParametersBoolean( parameterOverrides : Map[String,Any] ) : Map[LPBoolean,Boolean] = parameterOverrides.map( toParameterBoolean ).flatten.toMap
