package taxdelta.model

// The plans preset in taxsimulator2.xlsx (interface sheet), as Saez and Zucman approximated them in
// January 2020. See taxsimsummary_v2.pdf for how each candidate's proposals were mapped to the levers.
// Levers a plan does not set keep their baseline (current-law) values.
class SaezZucman2019Counterfactuals[C <: SaezZucman2019Context & Singleton](val context : C):

  import context.*

  // the workbook's bracket thresholds, shared by every plan's income tax schedule
  private val incomeTaxThresholds =
    IndexedSeq(0d, 77_000, 165_000, 315_000, 400_000, 600_000, 1_000_000, 2_500_000, 5_000_000, 10_000_000, 25_000_000, 50_000_000)

  val Obama =
    val obamaIncomeTaxSchedule = Schedule(
      incomeTaxThresholds,
      IndexedSeq(0.13, 0.25, 0.28, 0.33, 0.36, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40)
    )
    val leversDouble = Map(
      CorporateTaxProposedEffectiveRate -> 0.25d,
      EstateTaxProposedTopRate          -> 0.40d,
      EstateTaxAnticipatedAvoidanceRate -> 0.45d,
    )
    val leversOther = Map(
      IndividualIncomeTaxSchedule -> obamaIncomeTaxSchedule,
    )
    Counterfactual(
      leversDouble = leversDouble,
      leversOther = leversOther
    )

  // Saez and Zucman's own proposal, from The Triumph of Injustice (2019)
  val Book =
    val bookIncomeTaxSchedule = Schedule(
      incomeTaxThresholds,
      IndexedSeq(0.12, 0.22, 0.24, 0.32, 0.40, 0.55, 0.55, 0.55, 0.55, 0.60, 0.60, 0.60)
    )
    val bookWealthTaxSchedule = Schedule(
      IndexedSeq(50_000_000, 1_000_000_000),
      IndexedSeq(      0.02,         0.035)
    )
    val leversBoolean = Map(
      CloselyHeldCorpTaxRetainedEarningsAtOrdinaryRates                 -> true,
      CorporateTaxDeployStrongerEnforcement                             -> true,
      IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate                -> true,
      IndividualIncomeTaxDividendsTaxedAtOrdinaryRate                   -> true,
      IndividualIncomeTaxIntegratedTaxOnDividendedProfitsAtOrdinaryRate -> true,
    )
    val leversDouble = Map(
      CorporateTaxProposedEffectiveRate                 -> 0.30d,
      EstateTaxProposedTopRate                          -> 0.40d,
      EstateTaxAnticipatedAvoidanceRate                 -> 0.20d,
      NationalIncomeTaxRate                             -> 0.07d, // finances single-payer health care
      ReductionOfEmployerSponsoredPrivateHealthPremiums -> 1.00d, // single-payer!
      SalesAndExciseTaxReduction                        -> 0.50d,
      WealthTaxEvasionRate                              -> 0.15d,
    )
    val leversOther = Map(
      IndividualIncomeTaxSchedule -> bookIncomeTaxSchedule,
      WealthTaxSchedule           -> bookWealthTaxSchedule,
    )
    Counterfactual(
      leversBoolean = leversBoolean,
      leversDouble = leversDouble,
      leversOther = leversOther
    )

  val Biden =
    val bidenIncomeTaxSchedule = Schedule(
      incomeTaxThresholds,
      IndexedSeq(0.12, 0.22, 0.24, 0.33, 0.36, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40, 0.40)
    )
    val leversBoolean = Map(
      IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate -> true,
      IndividualIncomeTaxDividendsTaxedAtOrdinaryRate    -> true,
    )
    val leversDouble = Map(
      CorporateTaxProposedEffectiveRate -> 0.21d,
    )
    val leversOther = Map(
      IndividualIncomeTaxSchedule -> bidenIncomeTaxSchedule,
    )
    Counterfactual(
      leversBoolean = leversBoolean,
      leversDouble = leversDouble,
      leversOther = leversOther
    )

  val Buttigieg =
    val buttigiegIncomeTaxSchedule = Schedule(
      incomeTaxThresholds,
      IndexedSeq(0.12, 0.22, 0.24, 0.36, 0.52, 0.52, 0.52, 0.52, 0.52, 0.52, 0.52, 0.52)
    )
    // no wealth tax; this is only the financial transactions tax, awkwardly
    // modeled as part of the wealth tax.
    val buttigiegWealthTaxSchedule = Schedule(
      IndexedSeq(     0,   1_000_000),
      IndexedSeq(0.0005,      0.0010)
    )
    val leversBoolean = Map(
      CorporateTaxDeployStrongerEnforcement              -> true,
      IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate -> true,
    )
    val leversDouble = Map(
      CorporateTaxProposedEffectiveRate -> 0.25d,
      EstateTaxProposedTopRate          -> 0.55d,
      EstateTaxAnticipatedAvoidanceRate -> 0.30d,
      WealthTaxEvasionRate              -> 0.15d,
    )
    val leversOther = Map(
      IndividualIncomeTaxSchedule -> buttigiegIncomeTaxSchedule,
      WealthTaxSchedule           -> buttigiegWealthTaxSchedule,
    )
    Counterfactual(
      leversBoolean = leversBoolean,
      leversDouble = leversDouble,
      leversOther = leversOther
    )

  val Sanders =
    val sandersIncomeTaxSchedule = Schedule(
      incomeTaxThresholds,
      IndexedSeq(0.16, 0.26, 0.28, 0.36, 0.45, 0.56, 0.61, 0.61, 0.66, 0.68, 0.68, 0.68)
    )
    // the 0.0005 & 0.0010 terms are a financial transactions tax awkwardly
    // modeled as part of the wealth tax. the workbook snaps Sanders' wealth tax
    // brackets to its data grid (e.g. 1% from $25m rather than $32m).
    val sandersWealthTaxSchedule = Schedule(
      IndexedSeq(    0d,   1_000_000,      25_000_000,      50_000_000,     250_000_000,     500_000_000,   1_000_000_000,    2_500_000_000d,  5_000_000_000d, 10_000_000_000d),
      IndexedSeq(0.0005,      0.0010,   0.0010 + 0.01,   0.0010 + 0.02,   0.0010 + 0.03,   0.0010 + 0.04,   0.0010 + 0.05,   0.0010 + 0.06,   0.0010 + 0.07,   0.0010 + 0.08)
    )
    val leversBoolean = Map(
      CorporateTaxDeployStrongerEnforcement              -> true,
      IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate -> true,
      IndividualIncomeTaxDividendsTaxedAtOrdinaryRate    -> true,
    )
    val leversDouble = Map(
      CorporateTaxProposedEffectiveRate                 -> 0.25d,
      EstateTaxProposedTopRate                          -> 0.55d,
      EstateTaxAnticipatedAvoidanceRate                 -> 0.40d,
      NationalIncomeTaxRate                             -> 0.02d,
      ReductionOfEmployerSponsoredPrivateHealthPremiums -> 1.00d, // single-payer!
      WealthTaxEvasionRate                              -> 0.15d,
    )
    val leversOther = Map(
      IndividualIncomeTaxSchedule -> sandersIncomeTaxSchedule,
      WealthTaxSchedule           -> sandersWealthTaxSchedule,
    )
    Counterfactual(
      leversBoolean = leversBoolean,
      leversDouble = leversDouble,
      leversOther = leversOther
    )

  val Warren =
    val warrenIncomeTaxSchedule = Schedule(
      incomeTaxThresholds,
      IndexedSeq(0.12, 0.22, 0.24, 0.37, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55)
    )

    // the 0.0005 & 0.0010 terms are a financial transactions tax awkwardly
    // modeled as part of the wealth tax.
    val warrenWealthTaxSchedule = Schedule(
      IndexedSeq(     0,   1_000_000,      50_000_000,   1_000_000_000),
      IndexedSeq(0.0005,      0.0010,   0.0010 + 0.02,   0.0010 + 0.06)
    )
    val leversBoolean =  Map(
      CorporateTaxDeployStrongerEnforcement                    -> true,
      EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax -> true,
      IndividualIncomeTaxCapitalGainsTaxedAtOrdinaryRate       -> true,
      IndividualIncomeTaxDividendsTaxedAtOrdinaryRate          -> true,
    )
    val leversDouble = Map(
      CorporateTaxProposedEffectiveRate                 -> 0.30d,
      EstateTaxProposedTopRate                          -> 0.55d,
      EstateTaxAnticipatedAvoidanceRate                 -> 0.40d,
      ReductionOfEmployerSponsoredPrivateHealthPremiums -> 1.00d, // single-payer!
      WealthTaxEvasionRate                              -> 0.15d,
    )
    val leversOther = Map(
      IndividualIncomeTaxSchedule -> warrenIncomeTaxSchedule,
      WealthTaxSchedule           -> warrenWealthTaxSchedule,
    )
    Counterfactual(
      leversBoolean = leversBoolean,
      leversDouble = leversDouble,
      leversOther = leversOther
    )

  // every preset, by the name the workbook's interface sheet gives it
  val all : Map[String,Counterfactual] = Map(
    "obama"     -> Obama,
    "book"      -> Book,
    "biden"     -> Biden,
    "buttigieg" -> Buttigieg,
    "sanders"   -> Sanders,
    "warren"    -> Warren,
  )
