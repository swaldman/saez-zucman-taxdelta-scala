package taxdelta.model

abstract class SaezZucman2019Labels extends ModelContext:
  enum LPDouble extends Label:
    case RealizedGainsTaxableShare // income stats exclude realized gains, but the tax base does in part include them

  enum LDInt extends Label:
    case RealizedCapitalGains

  enum LDExcessCurve extends Label:
    case WealthExcessCurve

  enum LLDouble extends Label:
    case WealthTaxEvasionRate

  enum LLSchedule extends Label:
    case WealthTaxSchedule

  enum LInstrument extends Label:
    case WealthTax

  export LPDouble.*, LDInt.*, LDExcessCurve.*, LLDouble.*, LLSchedule.*, LInstrument.*

  override type LabelParameterDouble  = LPDouble
  override type LabelDataInt          = LDInt
  override type LabelDataExcessCurve  = LDExcessCurve
  override type LabelLeverDouble      = LLDouble
  override type LabelLeverSchedule    = LLSchedule
  override type LabelInstrument       = LInstrument
