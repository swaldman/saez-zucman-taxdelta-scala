# General model: to do

The model in `core` (`SaezZucman2019Context` and its calculators) is bespoke for the US. We leave it
largely as-is and use it as the reference. The general model (`taxdelta.general`, built from a
`TaxdeltaData`) needs its own versions of the instruments, written to tolerate the general case.
Data come from Gethin and Fisher-Post (GF) plus WID, all individuals with income split equally
(`999j`), and a new US model is built from the same sources for comparability.

## Instruments to generalize

### VAT: a level lever, calibrated to observed revenue

- Many countries already have a VAT, so the lever is the **level** of the standard rate, and its
  baseline is the country's current rate (`0` where there is no VAT, as in the US).
- Take the existing VAT out of the sales and excise column. VAT revenue comes from OECD Revenue
  Statistics, code 5111. Spread it with the same consumption profile, so `sales(g) = tax_ind(g) − VAT(g)`
  and baseline totals by group don't change. (GF's `tax_ind` also includes "other taxes", OECD 6000.)
- Calibrate the taxable share of consumption C, which replaces the US model's fixed 0.4, so that the
  current rate reproduces observed revenue: `C · v₀/(1+v₀) = VAT revenue / national income`. (The
  profile weights average to 1.) C picks up reduced rates, exemptions and non-compliance.
- Where there's no VAT, nothing can be calibrated: use 0.4, or the average of the calibrated countries.
- Limitation: one standard-rate lever with C held fixed assumes reduced rates move in proportion.
- Crowd-out must use the change in VAT from baseline, not its level. (`core` already does this, and
  the general instruments must keep it.)

### Wealth tax: scale by a calibrated factor, not an additive offset

- Countries with a wealth tax (Spain, Norway, Switzerland, Colombia, …) have a nonzero baseline schedule.
- Apply the statutory schedule, scaled by a factor k calibrated so that the modeled total equals
  observed revenue: `k · (1 − evasion) · Σ_g n_g · S₀(curve_g) = observed revenue`. Observed revenue
  is OECD Revenue Statistics code 4210 (recurrent taxes on individuals' net wealth).
- Why a factor, not an offset: an offset would fix the baseline level but not the size of reforms,
  which would stay inflated by narrow statutory bases (exemptions, valuation discounts). An offset would
  also leave a tax in place after abolition, and could push rates negative. k stands for the part of the
  base that's legally excluded. Keep it a parameter separate from the evasion lever, so a plan can also
  broaden the base.
- Calibrate k on the total, not group by group: GF doesn't observe the wealth tax by group. Its
  property and wealth column (`tax_dir_wea`) is spread by an assumed profile.
- Property column = `tax_dir_wea − k·W(S₀)(g) − estate(g)`, so baseline totals are preserved.
  Check that it stays non-negative.
- Switzerland's wealth tax is cantonal: use a representative or revenue-weighted schedule.
- Where there's no wealth tax: S₀ is empty and nothing can be calibrated. Use k = 1 (Saez–Zucman: only
  the 15% evasion) or the average of the calibrated countries.
- Wealth excess curves by *income* group: WID `hweal` ranks wealth on its own, and there's no joint
  distribution with income. Either spread the total by the corporate/capital profile (GF's convention)
  or assume a rank correlation between income and wealth. Thresholds must be in local currency at the
  schedule's prices.

### Estate tax: tolerate a current top rate of zero

- The US instrument scales the current estate tax by `proposedTopRate / currentTopRate`, which divides
  by zero where there's no estate tax (Australia, Canada, Sweden, Norway, India, …).
- Either offer no lever in those countries, or reformulate the estate tax as a rate applied to an
  assumed taxable estate base.
- Current estate tax by group: separate it from GF's `tax_dir_wea` using OECD code 4300's share of
  4000, and spread it by the corporate profile (GF's convention).
- Statutory top rates: EY's estate and inheritance tax guide. Inheritance taxes have rates that depend
  on the heir's relationship, so "top rate" needs a convention.
- Current avoidance rate: 0.6, the US model's value, as a uniform assumption.

### Individual income tax: schedule only, calibrated to GF

- The only lever is the schedule:
  `(1 − ΔNIT − ΔVAT) · max(pit(g) + [T_new(curve_g) − T_base(curve_g)] / y(g), 0)`, with `pit = tax_dir_pit / a_pre`.
- Preferences (capital gains, dividends), corporate integration and retained earnings of closely held
  firms stay in the US/deep-tier model. Their inputs are effectively unavailable across countries.
- Taxable-income excess curves: build them from the pre-tax distribution (WID thresholds, interpolated
  within groups). Scale each group by a "taxable share" calibrated so the baseline schedule reproduces
  GF's `tax_dir_pit` by group. GF microsimulated income tax from statutory schedules, so this inversion
  is consistent with how their data were made.
- The average ordinary marginal rate then follows from the schedule.
- Schedules: OECD Tax Database for its members; EY/PwC guides elsewhere. GF compiled schedules but
  didn't publish them, so ask the authors.
- Filing unit: single-filer schedule where individuals are taxed separately; for joint filing, the
  couple schedule with halved thresholds (approximately income splitting).
- Caveat: bracket changes apply to all taxable income, including income now taxed at preferential rates.

## Other decisions

- **Health:** no `HealthTaxInstrument` in the general model. Public health financing is already in GF's
  social contributions or other taxes, and GF's US figures don't count private premiums either. A
  possible later optional instrument: OECD's "non-tax compulsory payments" (mandatory private health
  insurance in Switzerland and the Netherlands).
- **Payroll:** `tax_soc` unchanged, with no health term (drop the US head-tax lever). A payroll-rate
  lever could be added later.
- **Corporate:**
  - Receipts: GF macro `taxa_pinc_corp`.
  - Profits: WID `mprico999i` (94 countries), else a uniform share of national income.
  - The instrument uses only ratios of these totals, so express them as shares of national income.
  - Current foreign receipts = 0: the US concept is residence-based.
  - Stronger enforcement: country estimates from Tørsløv–Wier–Zucman (*Missing Profits of Nations*)
    or the EU Tax Observatory's minimum-tax top-up estimates. Elsewhere a uniform uplift (the US
    workbook implies about +40% of corporate receipts).
- **National income tax:** baseline 0 means "no such tax exists". France's CSG, the nearest case, is
  already in `tax_dir_pit`. Base: `a_pre` × a uniform factor (about 0.907 in the US workbook).
- **Uniform assumptions:** consumer incidence share 0.75; wealth-tax evasion 0.15; estate avoidance 0.6.
- **Lever baselines in `TaxdeltaData`:** "reform" levers whose baseline is no change by definition (sales
  reduction, stronger enforcement, national income tax) can be `final def` in the trait. Use `def`, not
  `val`, to avoid initialization-order nulls. "Level" levers (VAT rate, estate top rate, corporate
  effective rate, wealth and income-tax schedules) are country data.
- **`dataWealthExcessCurve`** should be `Map[Group,ExcessCurve]`.

## First milestone: the individual income tax alone (data done, 2023)

- **Data:** `FixtureTaxdeltaData(2023, iso)` for the 36 countries the catalog marks `individualIncomeTax = yes`:
  every OECD member except Germany (formula tariff) and Switzerland (cantonal). Generated by the build from
  `mill-build/resources/fixtures/general/2023/`, which the scripts in `fixture-generators/` of the private
  sibling repository `saez-zucman-taxdelta-data-sources` write, in this order: `gen_groups.py`, `gen_oecd_pit.py`, `gen_iit_base.py`, `gen_catalog.py`.
- **Instrument:** rate(g) = max(current(g) + [S_new(g) − S_cur(g)] / y(g), 0), where S(g) is
  `schedule.averageTax(ScaledExcessCurve(dataIncomeExcessCurve(g), λ))` and λ is
  `paramIndividualIncomeTaxCalibratedBaseShareOfIncome`. No crowd-out yet.
- **Zero-income groups:** 150 of Gethin–Fisher's 176 countries (31 of the 36) have zero average income in
  P0–5, inherited from WID. A schedule change takes nothing from them, but a rate on no income is 0/0. The
  instrument must leave them at their current rate (0), and the UI should show them as having no income
  rather than a 0% rate.
- **Keep in step:** `GroupExcessCurve` and `gen_iit_base.py` must build group excess curves identically, or λ
  stops reproducing observed revenue. The test "calibration: …" checks this.
- **Revenue:** report the change in revenue as a share of national income: Σ size·ΔS / Σ size·y.
