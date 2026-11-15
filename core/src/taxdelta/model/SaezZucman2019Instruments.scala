package taxdelta.model

object SaezZucman2019Instruments:

  import SaezZucman2019Context.*

  object WealthTaxInstrument extends Instrument.Unified:
    private def computeTaxRate( schedule : Schedule, excessCurve : ExcessCurve, evasionRate : Double, rebasedAverageIncome : Double ) : Double =
      val taxesDue = schedule.averageTax( excessCurve );
      val afterEvasion = (1 - evasionRate) * taxesDue;
      afterEvasion / rebasedAverageIncome
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val schedule = model.orPrefer(counterfactual)(WealthTaxSchedule)
      val evasionRate = model.orPrefer(counterfactual)(WealthTaxEvasionRate)
      computeTaxRate( schedule, model.dataExcessCurve(WealthExcessCurve)(g), evasionRate, model.rebasedAverageIncome(g) )
    override val requires = Set(WealthTaxSchedule,WealthExcessCurve,WealthTaxEvasionRate)

  object NationalIncomeTaxInstrument extends Instrument.Unified:
    private def computeTaxRate( proposedNationalIncomeTaxRate : Double, nationalIncomeTaxBase : Double, rebasedAverageIncome : Double ) : Double =
      (proposedNationalIncomeTaxRate * nationalIncomeTaxBase) / rebasedAverageIncome
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val nitRate = model.orPrefer(counterfactual)(NationalIncomeTaxRate)
      computeTaxRate( nitRate, model.dataInt(NationalIncomeTaxBase)(g), model.rebasedAverageIncome(g) )
    override val requires = Set(NationalIncomeTaxBase,NationalIncomeTaxRate)

  object ValueAddedTaxInstrument extends Instrument.Unified:
    private def groupSpecificWeightingOfTaxableSales( groupSpecificSalesTaxShareOfIncome : Double, averageSalesTaxShareOfIncome : Double, consumerIncidenceShare : Double) : Double =
      (consumerIncidenceShare * (groupSpecificSalesTaxShareOfIncome / averageSalesTaxShareOfIncome)) + ((1-consumerIncidenceShare) * 1)
    private def computeTaxRate( valueAddedTaxRate : Double, averageValueAddedTaxableConsumptionShareOfIncome : Double, groupSpecificSalesTaxShareOfIncome : Double, averageSalesTaxShareOfIncome : Double, consumerIncidenceShare : Double ) : Double =
      val taxRateOnVatPurchases = valueAddedTaxRate / (1+valueAddedTaxRate)
      val groupWeight = groupSpecificWeightingOfTaxableSales( groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, consumerIncidenceShare )
      taxRateOnVatPurchases * averageValueAddedTaxableConsumptionShareOfIncome * groupWeight
    protected def taxRate( model : Model, counterfactual : Option[Counterfactual] )(prev : Map[LabelInstrument,Map[Group,Instrument.Result]])(g : Group) : Double =
      val valueAddedTaxRate = model.orPrefer(counterfactual)(ValueAddedTaxRate)
      val averageValueAddedTaxableConsumptionShareOfIncome = model.parametersDouble(AverageValueAddedTaxableConsumptionShareOfIncome)
      val groupSpecificSalesTaxShareOfIncome = model.dataDouble(SalesTaxShareOfIncome)(g)
      val averageSalesTaxShareOfIncome = model.parametersDouble(AverageSalesTaxShareOfIncome)
      val consumerIncidenceShare = model.parametersDouble(ConsumerIncidenceShare)
      computeTaxRate( valueAddedTaxRate, averageValueAddedTaxableConsumptionShareOfIncome, groupSpecificSalesTaxShareOfIncome, averageSalesTaxShareOfIncome, consumerIncidenceShare )
    override val requires = Set(ValueAddedTaxRate,AverageValueAddedTaxableConsumptionShareOfIncome,SalesTaxShareOfIncome,AverageSalesTaxShareOfIncome,ConsumerIncidenceShare)
