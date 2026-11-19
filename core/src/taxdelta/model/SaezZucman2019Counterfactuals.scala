package taxdelta.model

class SaezZucman2019Counterfactuals[C <: SaezZucman2019Context & Singleton](val context : C):

  import context.*

  val Warren =
    val warrenIncomeTaxSchedule = Schedule(
      IndexedSeq(0d, 77_000, 165_000, 315_000, 400_000, 600_000, 1_000_000, 2_500_000, 5_000_000, 10_000_000, 25_000_000, 50_000_000),
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

