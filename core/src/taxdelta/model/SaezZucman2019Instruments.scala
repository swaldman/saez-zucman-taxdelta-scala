package taxdelta.model

object SaezZucman2019Instruments:

  import SaezZucman2019Context.*

  object WealthTaxInstrument extends Instrument:
    private def computeTaxRate( schedule : Schedule, excessCurve : ExcessCurve, evasionRate : Double, rebasedAverageIncome : Double ) : Double =
      val taxesDue = schedule.averageTax( excessCurve );
      val afterEvasion = (1 - evasionRate) * taxesDue;
      afterEvasion / rebasedAverageIncome
    def baselineTaxRate( model : Model )(g : Group) : Double =
      computeTaxRate( model.leversSchedule(WealthTaxSchedule), model.dataExcessCurve(WealthExcessCurve)(g), model.leversDouble(WealthTaxEvasionRate), model.rebasedAverageIncome(g) )
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )(g : Group) : Double =
      computeTaxRate( counterfactual.orModel(model).schedule(WealthTaxSchedule), model.dataExcessCurve(WealthExcessCurve)(g), counterfactual.orModel(model).double(WealthTaxEvasionRate), model.rebasedAverageIncome(g) )
    override val requires = Set(WealthTaxSchedule,WealthExcessCurve,WealthTaxEvasionRate)

  object NationalIncomeTaxInstrument extends Instrument:
    private def computeTaxRate( proposedNationalIncomeTaxRate : Double, nationalIncomeTaxBase : Double, rebasedAverageIncome : Double ) : Double =
      (proposedNationalIncomeTaxRate * nationalIncomeTaxBase) / rebasedAverageIncome
    def baselineTaxRate( model : Model )(g : Group) : Double =
      computeTaxRate( model.leversDouble(NationalIncomeTaxRate), model.dataInt(NationalIncomeTaxBase)(g), model.rebasedAverageIncome(g) )
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )(g : Group) : Double =
      computeTaxRate( counterfactual.orModel(model).double(NationalIncomeTaxRate), model.dataInt(NationalIncomeTaxBase)(g), model.rebasedAverageIncome(g) )
    override val requires = Set(NationalIncomeTaxBase,NationalIncomeTaxRate)

  object ValueAddedTaxInstrument extends Instrument:
    private def groupSpecificWeightingOfTaxableSales( groupSpecificSalesTaxShareOfIncome : Double, averageSalesTaxShareOfIncome : Double, behaviorallySensitiveShareOfConsumption : Double) : Double =
      (behaviorallySensitiveShareOfConsumption * (groupSpecificSalesTaxShareOfIncome / averageSalesTaxShareOfIncome)) + ((1-behaviorallySensitiveShareOfConsumption) * 1)
    private def computeTaxRate( valueAddedTaxRate : Double, averageValueAddedTaxableConsumptionShareOfIncome : Double, groupSpecificSalesTaxShareOfIncome : Double, averageSalesTaxShareOfIncome : Double, behaviorallySensitiveShareOfConsumption : Double ) : Double =
      val taxRateOnVatPurchases = valueAddedTaxRate / (1+valueAddedTaxRate)
      val groupWeight = groupSpecificWeightingOfTaxableSales( groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, behaviorallySensitiveShareOfConsumption )
      taxRateOnVatPurchases * averageValueAddedTaxableConsumptionShareOfIncome * groupWeight
    def baselineTaxRate( model : Model )(g : Group) : Double =
      val valueAddedTaxRate = model.leversDouble(ValueAddedTaxRate)
      val averageValueAddedTaxableConsumptionShareOfIncome = model.parametersDouble(AverageValueAddedTaxableConsumptionShareOfIncome)
      val groupSpecificSalesTaxShareOfIncome = model.dataDouble(SalesTaxShareOfIncome)(g)
      val averageSalesTaxShareOfIncome = model.parametersDouble(AverageSalesTaxShareOfIncome)
      val behaviorallySensitiveShareOfConsumption = model.parametersDouble(BehaviorallySensitiveShareOfTaxableConsumption)
      computeTaxRate( valueAddedTaxRate, averageValueAddedTaxableConsumptionShareOfIncome, groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, behaviorallySensitiveShareOfConsumption )
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )(g : Group) : Double =
      val valueAddedTaxRate = counterfactual.orModel(model).double(ValueAddedTaxRate)
      val averageValueAddedTaxableConsumptionShareOfIncome = model.parametersDouble(AverageValueAddedTaxableConsumptionShareOfIncome)
      val groupSpecificSalesTaxShareOfIncome = model.dataDouble(SalesTaxShareOfIncome)(g)
      val averageSalesTaxShareOfIncome = model.parametersDouble(AverageSalesTaxShareOfIncome)
      val behaviorallySensitiveShareOfConsumption = model.parametersDouble(BehaviorallySensitiveShareOfTaxableConsumption)
      computeTaxRate( valueAddedTaxRate, averageValueAddedTaxableConsumptionShareOfIncome, groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, behaviorallySensitiveShareOfConsumption )
    override val requires = Set(ValueAddedTaxRate,AverageValueAddedTaxableConsumptionShareOfIncome,SalesTaxShareOfIncome,AverageSalesTaxShareOfIncome,BehaviorallySensitiveShareOfTaxableConsumption)
