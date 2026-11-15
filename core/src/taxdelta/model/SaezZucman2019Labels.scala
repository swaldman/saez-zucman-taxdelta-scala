package taxdelta.model

abstract class SaezZucman2019Labels extends ModelContext:
  //enum LPBoolean extends Label:
  //  case ImputeHigherWealthBracketsTopGroupOnly

  enum LPDouble extends InputLabel:
    case AverageSalesTaxShareOfIncome
    case AverageValueAddedTaxableConsumptionShareOfIncome
    case ConsumerIncidenceShare
    case RealizedGainsTaxableShare // income stats exclude realized gains, but the tax base does in part include them

  enum LDDouble extends InputLabel:
    case SalesTaxShareOfIncome

  enum LDInt extends InputLabel:
    case NationalIncomeTaxBase
    case RealizedCapitalGains

  enum LDExcessCurve extends InputLabel:
    case WealthExcessCurve

  enum LLDouble extends InputLabel:
    case NationalIncomeTaxRate // presumably a flat tax?
    case ValueAddedTaxRate
    case WealthTaxEvasionRate

  enum LLSchedule extends InputLabel:
    case WealthTaxSchedule

  enum LInstrument extends ResultLabel:
    case NationalIncomeTax
    case ValueAddedTax
    case WealthTax

  export LPDouble.*, LDDouble.*, LDInt.*, LDExcessCurve.*, LLDouble.*, LLSchedule.*, LInstrument.*

  //override type LabelParameterBoolean = LPBoolean
  override type LabelParameterDouble  = LPDouble
  override type LabelDataDouble       = LDDouble
  override type LabelDataInt          = LDInt
  override type LabelDataExcessCurve  = LDExcessCurve
  override type LabelLeverDouble      = LLDouble
  override type LabelLeverSchedule    = LLSchedule
  override type LabelInstrument       = LInstrument
