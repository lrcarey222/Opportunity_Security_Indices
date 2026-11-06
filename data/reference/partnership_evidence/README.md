# Partnership evidence layer

Curated, date-stamped external evidence for the Partnership Memos (docs/partnership_memos.md). The memo renders
from these files; nothing is fetched at render time.

- `policy_evidence.csv`: one row per agreement, institution, project, financing, dispute or market fact. `countries`
  lists ISO3 codes joined by `|`; a pair's memo uses rows naming both countries. `verification`: verified (the
  primary source was read), partial (secondary source or search summary), unverified (not found or single weak
  source). `access_date` is the vintage printed in the memo.
- `bilateral_mechanisms.csv`: the inputs to the Master Energy Agreement assessment (scope, pipeline, finance, KPIs,
  review, reciprocity, sunset), with the evidence ids behind each.
- `institutions.csv`: public institutions by country and instrument family.

Vintage 2026-11-06 (US-Japan): 85 items from two research passes (agreements and finance; sector projects). Partial
and unverified items must be checked before citation. Known open points at this vintage: the operative US tariff on
Japan after the February 2026 IEEPA ruling; whether China extends the suspension of its October 2025 rare-earth
controls (due to lapse around 2026-11-10); AESC ownership; the Westinghouse figure ($80bn vs $100bn).
