"""Check the Trade War Tracker's Section 338 Canada autos basket against the official HTSUS text.

Reads HTSUS chapter 99 (PDF from hts.usitc.gov, `reststop/file?release=currentRelease&filename=Chapter 99`), finds
U.S. note 51 subdivision (b)(3) (heading 9903.03.14) and (c) (the Section 232 exemption), extracts every HTS number
listed in (b)(3), and compares it with the tracker's list (tariff-lists/canada-338-autos-2026-10-30.csv). Writes one
row per tracker code (in_hts TRUE/FALSE) and one per HTS code missing from the tracker, plus a summary line.

Usage: python -I check_s338_hts.py <chapter99.pdf> <tracker list csv> <basket hs6 txt> <out.csv>
"""
import csv
import re
import sys

from pypdf import PdfReader

pdf, tracker_csv, basket_txt, out = sys.argv[1:5]
text = "\n".join((p.extract_text() or "") for p in PdfReader(pdf).pages)
rev = re.search(r"Revision \d+ \(\d{4}\)", text)
start = text.find("(3) Heading 9903.03.14 applies to articles classifiable")
end = text.find("(c) As provided in heading 9903.03.15", start)
if start < 0 or end < 0:
    sys.exit("U.S. note 51(b)(3) not found in this chapter 99 text")
sub = text[start:end]
note_c = text[end:end + 1200]
# HTS numbers are printed run together ("7308.90.607308.90.30"): every number starts with dddd.dd, so split each
# whitespace-separated chunk at those starts and keep the digits of each piece (8 or 10 digits).
found = set()
for chunk in sub.split():
    starts = [m.start() for m in re.finditer(r"\d{4}\.\d{2}", chunk)]
    for i, s0 in enumerate(starts):
        piece = re.sub(r"\D", "", chunk[s0:starts[i + 1] if i + 1 < len(starts) else len(chunk)])
        if len(piece) in (6, 8, 10):
            found.add(piece)
basket = {ln.strip() for ln in open(basket_txt, encoding="utf-8") if ln.strip()}
rows = []
with open(tracker_csv, encoding="utf-8") as f:
    trk = [r for r in csv.DictReader(f)]
col = "HTSUS" if "HTSUS" in trk[0] else list(trk[0].keys())[0]
tcodes = sorted({re.sub(r"\D", "", r[col]) for r in trk if r[col]})
for c in tcodes:
    hit = c in found or any(f.startswith(c) or c.startswith(f) for f in found if len(f) >= 8)
    rows.append({"code": c, "source": "tracker", "in_hts_note51b3": hit, "in_basket_hs6": c[:6] in basket})
for c in sorted(found):
    if not any(c.startswith(t) or t.startswith(c) for t in tcodes):
        rows.append({"code": c, "source": "hts_only", "in_hts_note51b3": True, "in_basket_hs6": c[:6] in basket})
stacks = "9903.03.13" in note_c and "9903.03.14" not in note_c[:400]
with open(out, "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=["code", "source", "in_hts_note51b3", "in_basket_hs6"])
    w.writeheader()
    w.writerows(rows)
t_basket = [r for r in rows if r["source"] == "tracker" and r["in_basket_hs6"]]
print(f"HTSUS {rev.group(0) if rev else '?'}: note 51(b)(3) lists {len(found)} numbers; tracker basket {len(tcodes)} codes, "
      f"{sum(r['in_hts_note51b3'] for r in rows if r['source'] == 'tracker')} found in the HTS text; "
      f"energy-basket codes {len(t_basket)}, {sum(r['in_hts_note51b3'] for r in t_basket)} found; "
      f"HTS-only codes {sum(r['source'] == 'hts_only' for r in rows)}; 232 exemption limited to 9903.03.13: {stacks}")
