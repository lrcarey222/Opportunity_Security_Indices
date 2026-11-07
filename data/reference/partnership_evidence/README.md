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

Vintage 2026-11-06, six further pairs (one research pass each, by evidence-id prefix): US-Korea `USKR-` 33 items
(8 verified, 25 partial), US-Germany `USDE-` 27 (3 verified, 24 partial; EU-level trade instruments carry
`USA|DEU` and say "EU-level" in the name), Japan-Korea `JPKR-` 33 (31 partial, 2 unverified; METI pages refused
automated access, so nothing is primary), US-Australia `USAU-` 24 (4 verified, 20 partial), Korea-Canada `KRCA-` 27
(8 verified, 17 partial, 2 unverified) and Korea-Australia `KRAU-` 27 (2 verified, 24 partial, 1 unverified).
Mechanisms are bilateral (or plurilateral) government channels only: corporate chains, unilateral policies and
defence programmes were left out of `bilateral_mechanisms.csv` (they stay in `policy_evidence.csv`). Rows that were
unverified and had no source URL were dropped. Institutions were added for KOR, DEU, AUS and CAN (institutional
reference, names as of 2025-26: Korea's trade ministry MOTIR and the Ministry of Climate, Energy and Environment;
Germany's BMWE). Known open points at this vintage: the US Section 301 rate per partner after July 2026; the
POSCO Future M-GM and EcoPro-Ford cathode plants in Becancour; the reported Cameco-KHNP uranium deal; GM's October
2026 termination of the VAC magnet offtake.

Vintage 2026-11-06 (US-Japan): 85 items from two research passes (agreements and finance; sector projects). Partial
and unverified items must be checked before citation. Known open points at this vintage: the operative US tariff on
Japan after the February 2026 IEEPA ruling; whether China extends the suspension of its October 2025 rare-earth
controls (due to lapse around 2026-11-10); AESC ownership; the Westinghouse figure ($80bn vs $100bn).
