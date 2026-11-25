# Partnership evidence layer

Curated, date-stamped external evidence for the Partnership Memos (docs/partnership_memos.md). The memo renders
from these files; nothing is fetched at render time.

- `policy_evidence.csv`: one row per agreement, institution, project, financing, dispute or market fact. `countries`
  lists ISO3 codes joined by `|`; a pair's memo uses rows naming both countries. `verification`: verified (the
  primary source was read), partial (secondary source or search summary), unverified (not found or single weak
  source). `access_date` is the vintage printed in the memo.
- `bilateral_mechanisms.csv`: the inputs to the Master Energy Agreement assessment (scope, pipeline, finance, KPIs,
  review, reciprocity, sunset), with the evidence ids behind each.
- `institutions.csv`: public institutions by country and instrument family; `scope_compacts` limits an institution
  to the compacts it serves (blank = all; `fuel` = units outside the compacts).
- `plurilateral_mechanisms.csv`: plurilateral bodies (and bundles of bilateral MOUs, type `bilateral_bundle`) that
  cover a compact; same columns as `bilateral_mechanisms.csv`; read by the Compact and Group memos only.

Vintage 2026-11-17 (the Quad group memo; one research pass each): the Quad `QUAD-` 25 (20 verified, 5 partial) with
six Quad mechanisms (`QUAD-M1`..`M6`: the ministerial and summit cycle, energy security, export-credit agencies,
ports, cables, semiconductors), Japan-India `JPIN-` 26 (18 verified), Japan-Australia `JPAU-` 23 (10 verified) and
India-Australia `INAU-` 26 (13 verified), each pair with five mechanisms. Corrections: the Quad critical-minerals
framework is one plurilateral row (`QUAD-M7`, formerly `USJ-QUAD`, rebuilt from the framework text; the old row had a column shift, and
`USAU-M4` duplicated it); E33 (an unverified report of the 4 May 2026 Japan-Australia statement, Australian dollars
read as US dollars) is replaced by the verified `JPAU-05`; UJ-046 (Rio Tinto's Rincon lithium project) is in
Argentina, not Australia, and now carries the JBIC loan terms. Open points at this vintage: the Manila Quad leaders'
meeting of 17 Nov 2026 rests on wire reports (`QUAD-10`..`12`, partial) until the official statement is published;
the handover of the Quad chair from India to Australia is reported but not official.

Vintage 2026-11-13 (three pairs, two refreshes, three compacts; one research pass each, by evidence-id prefix):
US-Canada `USCA-` 28 (25 verified, 3 partial), US-India `USIN-` 38 (15 verified, 13 partial, 10 unverified),
US-EU `USEU-` 30 (all verified; EU-level items carry `USA|EUU`, member-state items the member's code), Solar compact
`CSOL-` 40 (22 verified, 18 partial), Batteries compact `CBAT-` 30 (14 verified, 16 partial; CBAT-13 dropped as a
duplicate of USEU-20), Critical Minerals compact `CCRM-` 38 (17 verified, 21 partial). The US-Korea and US-Australia
refreshes re-checked the partial items those memos cite against primary sources (25 rows updated, mostly to verified,
including two UJ- items) and added 4 `USKR-` and 5 `USAU-` items for developments to 2026-11-13. The shared
web-search quota ran out early in the pass, so most items come from directly fetched official pages (hence the high
verified share) and some topics are thinner than intended. New files and rows: `plurilateral_mechanisms.csv` (20
compact-level mechanisms, plus the Quad and US-Japan-Korea rows copied from the bilateral file), 20 bilateral
mechanisms (two unilateral tariff "mechanisms" dropped), institutions for India and the EU (`EUU`) from the research,
and 49 institutional-reference rows for frequent compact members (POL, IDN, MYS, VNM, THA, TWN, FRA, GBR, CHL, COD,
SWE, FIN, ESP, ITA, NLD, MEX, HUN, BRA, SGP, NOR, PER) added from general knowledge and marked so; check their names
before citation. Known open points at this vintage: China's extension (9 Nov 2026, to 10 Jan 2027) covers only the
October 2025 export-control package; the separate suspension of the gallium/germanium/antimony ban on the US runs to
27 Nov 2026 with no extension reported; the outcome of the Section 232 processed-critical-minerals deadline (13 Jul
2026) and the refined-copper duty; company rates in the US solar AD/CVD cases on India, Indonesia and Laos beyond those
listed; the products covered by the US Section 338 duties and import bans on Canada (annexes are images); 2026
progress against the EU's US$750bn energy and US$600bn investment intents; the EU Battery Booster.

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
