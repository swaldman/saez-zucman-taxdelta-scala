package taxdelta.model

abstract class SaezZucman2019Labels extends ModelContext:
  enum LPDouble extends Label:
    case AverageSalesTaxShareOfIncome
    case AverageValueAddedTaxableConsumptionShareOfIncome
    case BehaviorallySensitiveShareOfTaxableConsumption
    case RealizedGainsTaxableShare // income stats exclude realized gains, but the tax base does in part include them

  enum LDDouble extends Label:
    case SalesTaxShareOfIncome

  enum LDInt extends Label:
    case NationalIncomeTaxBase
    case RealizedCapitalGains

  enum LDExcessCurve extends Label:
    case WealthExcessCurve

  enum LLDouble extends Label:
    case NationalIncomeTaxRate // presumably a flat tax?
    case ValueAddedTaxRate
    case WealthTaxEvasionRate

  enum LLSchedule extends Label:
    case WealthTaxSchedule

  enum LInstrument extends Label:
    case NationalIncomeTax
    case WealthTax

  export LPDouble.*, LDDouble.*, LDInt.*, LDExcessCurve.*, LLDouble.*, LLSchedule.*, LInstrument.*

  override type LabelParameterDouble  = LPDouble
  override type LabelDataDouble       = LDDouble
  override type LabelDataInt          = LDInt
  override type LabelDataExcessCurve  = LDExcessCurve
  override type LabelLeverDouble      = LLDouble
  override type LabelLeverSchedule    = LLSchedule
  override type LabelInstrument       = LInstrument
