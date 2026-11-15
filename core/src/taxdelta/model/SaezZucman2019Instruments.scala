package taxdelta.model

object SaezZucman2019Instruments:

  import SaezZucman2019Context.*

  object WealthTaxInstrument extends Instrument:
    private def computeTaxRate( schedule : Schedule, excessCurve : ExcessCurve, evasionRate : Double, rebasedAverageIncome : Double ) : Double =
      val taxesDue = schedule.averageTax( excessCurve );
      val afterEvasion = (1 - evasionRate) * taxesDue;
      afterEvasion / rebasedAverageIncome
    def baselineTaxRate( model : Model )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      computeTaxRate( model.leversSchedule(WealthTaxSchedule), model.dataExcessCurve(WealthExcessCurve)(g), model.leversDouble(WealthTaxEvasionRate), model.rebasedAverageIncome(g) )
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      computeTaxRate( counterfactual.orModel(model).schedule(WealthTaxSchedule), model.dataExcessCurve(WealthExcessCurve)(g), counterfactual.orModel(model).double(WealthTaxEvasionRate), model.rebasedAverageIncome(g) )
    override val requires = Set(WealthTaxSchedule,WealthExcessCurve,WealthTaxEvasionRate)

  object NationalIncomeTaxInstrument extends Instrument:
    private def computeTaxRate( proposedNationalIncomeTaxRate : Double, nationalIncomeTaxBase : Double, rebasedAverageIncome : Double ) : Double =
      (proposedNationalIncomeTaxRate * nationalIncomeTaxBase) / rebasedAverageIncome
    def baselineTaxRate( model : Model )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      computeTaxRate( model.leversDouble(NationalIncomeTaxRate), model.dataInt(NationalIncomeTaxBase)(g), model.rebasedAverageIncome(g) )
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      computeTaxRate( counterfactual.orModel(model).double(NationalIncomeTaxRate), model.dataInt(NationalIncomeTaxBase)(g), model.rebasedAverageIncome(g) )
    override val requires = Set(NationalIncomeTaxBase,NationalIncomeTaxRate)

  object ValueAddedTaxInstrument extends Instrument:
    private def groupSpecificWeightingOfTaxableSales( groupSpecificSalesTaxShareOfIncome : Double, averageSalesTaxShareOfIncome : Double, consumerIncidenceShare : Double) : Double =
      (consumerIncidenceShare * (groupSpecificSalesTaxShareOfIncome / averageSalesTaxShareOfIncome)) + ((1-consumerIncidenceShare) * 1)
    private def computeTaxRate( valueAddedTaxRate : Double, averageValueAddedTaxableConsumptionShareOfIncome : Double, groupSpecificSalesTaxShareOfIncome : Double, averageSalesTaxShareOfIncome : Double, consumerIncidenceShare : Double ) : Double =
      val taxRateOnVatPurchases = valueAddedTaxRate / (1+valueAddedTaxRate)
      val groupWeight = groupSpecificWeightingOfTaxableSales( groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, consumerIncidenceShare )
      taxRateOnVatPurchases * averageValueAddedTaxableConsumptionShareOfIncome * groupWeight
    def baselineTaxRate( model : Model )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val valueAddedTaxRate = model.leversDouble(ValueAddedTaxRate)
      val averageValueAddedTaxableConsumptionShareOfIncome = model.parametersDouble(AverageValueAddedTaxableConsumptionShareOfIncome)
      val groupSpecificSalesTaxShareOfIncome = model.dataDouble(SalesTaxShareOfIncome)(g)
      val averageSalesTaxShareOfIncome = model.parametersDouble(AverageSalesTaxShareOfIncome)
      val consumerIncidenceShare = model.parametersDouble(ConsumerIncidenceShare)
      computeTaxRate( valueAddedTaxRate, averageValueAddedTaxableConsumptionShareOfIncome, groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, consumerIncidenceShare )
    def reformedTaxRate( model : Model, counterfactual : Counterfactual )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val valueAddedTaxRate = counterfactual.orModel(model).double(ValueAddedTaxRate)
      val averageValueAddedTaxableConsumptionShareOfIncome = model.parametersDouble(AverageValueAddedTaxableConsumptionShareOfIncome)
      val groupSpecificSalesTaxShareOfIncome = model.dataDouble(SalesTaxShareOfIncome)(g)
      val averageSalesTaxShareOfIncome = model.parametersDouble(AverageSalesTaxShareOfIncome)
      val consumerIncidenceShare = model.parametersDouble(ConsumerIncidenceShare)
      computeTaxRate( valueAddedTaxRate, averageValueAddedTaxableConsumptionShareOfIncome, groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, consumerIncidenceShare )
    override val requires = Set(ValueAddedTaxRate,AverageValueAddedTaxableConsumptionShareOfIncome,SalesTaxShareOfIncome,AverageSalesTaxShareOfIncome,ConsumerIncidenceShare)
