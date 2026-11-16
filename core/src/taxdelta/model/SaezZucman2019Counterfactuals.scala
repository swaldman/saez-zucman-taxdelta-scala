package taxdelta.model

class SaezZucman2019Counterfactuals[C <: SaezZucman2019Context & Singleton](val context : C):

  import context.*

  val Warren =
    // the 0.0005 & 0.0010 terms are a financial transactions tax awkwardly
    // modeled as part of the wealth tax.
    val warrenWealthTaxSchedule = Schedule(
      IndexedSeq(     0,   1_000_000,      50_000_000,   1_000_000_000),
      IndexedSeq(0.0005,      0.0010,   0.0010 + 0.02,   0.0010 + 0.06)
    )
    val leversBoolean =  Map(
      EmployerSponsoredHealthPremiumsBecomeHeadTaxAsPayrollTax -> true
    )
    val leversDouble = Map(
      EstateTaxProposedTopRate                          -> 0.55d,
      EstateTaxAnticipatedAvoidanceRate                 -> 0.40d,
      ReductionOfEmployerSponsoredPrivateHealthPremiums -> 1.00d, // single-payer!
      WealthTaxEvasionRate                              -> 0.15d,
    )
    val leversSchedule = Map(
      WealthTaxSchedule -> warrenWealthTaxSchedule,
    )
    Counterfactual(
      leversBoolean = leversBoolean,
      leversDouble = leversDouble,
      leversSchedule = leversSchedule
    )

