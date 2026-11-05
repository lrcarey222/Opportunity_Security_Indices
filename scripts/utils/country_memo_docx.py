"""Word (.docx) files of the Country Insights policy memos (spec_country.md B7.4).

Run with `python -I` by scripts/88_export_country_data.R --render-memos (scripts/utils/country_memos.R):

    python -I country_memo_docx.py --in memo.ast.json --out memo.docx [--figures DIR]
                                                                           build the file from the memo AST
    python -I country_memo_docx.py --check memo.docx --ast memo.ast.json   read it back; exit 1 unless the
                                                                           paragraph texts equal the AST's
    python -I country_memo_docx.py --pdf-text memo.pdf [--out text.json]   print (or write) {"pages": n, "text": ...}

The AST is the memo document of R/dashboard/country/country_memo_lint.R (country_memo_document()): the
same structure the screen and the PDF are rendered from, so the three carry the same words. US Letter,
0.75 in margins, Arial 10.5 pt (Word may not have Roboto); the summary as a one-cell table shaded F3F4F6
with an orange (F58229) left border; numbered Heading 1 sections; List Bullet items; superscript source
numbers; the source list as List Number paragraphs; the label in the footer. Needs python-docx >= 1.1
and pypdf.

A country strategy brief (the AST's "format" is "brief", scripts/92_export_country_briefs.R) adds
sub-headings (Heading 2), the box of strategic choices (a two-column table, each cell topped by its choice
colour), pipe tables, figures (the PNG files f<n>.png in --figures, 6.9 in wide, with a bold title and an
italic caption) and a technical appendix after the sources; the read-back rule is expected_texts_brief().
"""

import argparse
import json
import sys

from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_BREAK
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches, Pt, RGBColor

INK = RGBColor(0x11, 0x18, 0x27)
MUTED = RGBColor(0x4B, 0x55, 0x63)
# the choice colours of the brief's box (config/country_brief.yml charts.colors)
BOX_COLOR = {"secure": "F58229", "preserve": "1889B0", "partner": "55C4C5", "accelerate": "A3B63B", "watch": "9CA3AF"}


def item_text(item):
    """The paragraph text Word reads back: the inlines, then the source numbers joined by commas."""
    text = "".join(n["v"] for n in item["inlines"])
    refs = item.get("refs") or []
    return text + (",".join(str(r) for r in refs) if refs else "")


def expected_texts(doc):
    """Body paragraph texts in reading order (the same rule as country_memo_texts() in R)."""
    out = [doc["title"], doc["subtitle"], doc["label"], "Summary"]
    out += [item_text(it) for it in doc["summary"]]
    for s in doc["sections"]:
        out.append(f"{s['n']}. {s['title']}")
        for b in s["blocks"]:
            if b["type"] == "ul":
                out += [item_text(it) for it in b["items"]]
            else:
                out.append(item_text(b))
    out.append("Sources")
    out += [s["text"] for s in doc["sources"]]
    out.append(doc["label"])
    return out


def add_runs(par, item, size=None):
    for n in item["inlines"]:
        run = par.add_run(n["v"])
        if n["t"] == "strong":
            run.bold = True
        elif n["t"] == "em":
            run.italic = True
        if size:
            run.font.size = size
    refs = item.get("refs") or []
    if refs:
        sup = par.add_run(",".join(str(r) for r in refs))
        sup.font.superscript = True
        sup.font.color.rgb = MUTED


def shade_cell(cell, fill="F3F4F6", border="F58229"):
    tc_pr = cell._tc.get_or_add_tcPr()
    shd = OxmlElement("w:shd")
    shd.set(qn("w:val"), "clear")
    shd.set(qn("w:color"), "auto")
    shd.set(qn("w:fill"), fill)
    tc_pr.append(shd)
    borders = OxmlElement("w:tcBorders")
    for side in ("top", "left", "bottom", "right"):
        el = OxmlElement(f"w:{side}")
        if side == "left":
            el.set(qn("w:val"), "single")
            el.set(qn("w:sz"), "24")
            el.set(qn("w:space"), "0")
            el.set(qn("w:color"), border)
        else:
            el.set(qn("w:val"), "nil")
        borders.append(el)
    tc_pr.append(borders)
    mar = OxmlElement("w:tcMar")
    for side, w in (("top", 120), ("left", 200), ("bottom", 80), ("right", 160)):
        el = OxmlElement(f"w:{side}")
        el.set(qn("w:w"), str(w))
        el.set(qn("w:type"), "dxa")
        mar.append(el)
    tc_pr.append(mar)


def hairline_above(par, color="D1D5DB"):
    p_pr = par._p.get_or_add_pPr()
    bdr = OxmlElement("w:pBdr")
    top = OxmlElement("w:top")
    top.set(qn("w:val"), "single")
    top.set(qn("w:sz"), "4")
    top.set(qn("w:space"), "4")
    top.set(qn("w:color"), color)
    bdr.append(top)
    p_pr.append(bdr)


def build(doc, out):
    d = Document()
    sec = d.sections[0]
    sec.page_width, sec.page_height = Inches(8.5), Inches(11)
    for side in ("left_margin", "right_margin", "top_margin", "bottom_margin"):
        setattr(sec, side, Inches(0.75))
    normal = d.styles["Normal"]
    normal.font.name = "Arial"
    normal.font.size = Pt(10.5)
    normal.font.color.rgb = INK
    normal.element.rPr.rFonts.set(qn("w:eastAsia"), "Arial")
    for name in ("Title", "Heading 1", "Heading 2", "List Bullet", "List Number"):
        st = d.styles[name]
        st.font.name = "Arial"
        st.font.color.rgb = INK
    d.styles["Title"].font.size = Pt(20)
    d.styles["Heading 1"].font.size = Pt(13)
    d.styles["Heading 2"].font.size = Pt(10)

    cp = d.core_properties
    cp.title = doc["title"]
    cp.subject = "Country Insights policy memo"
    cp.author = "NEIS Center, Allied Industrial Base Explorer (AI-generated, checked against the data)"
    cp.keywords = "energy security; economic opportunity; Strategic Index; Partnership Strength Index; " + doc["iso3"]
    cp.comments = f"paper baseline as of {doc.get('as_of_label') or doc.get('as_of')}; fact sheet sha256 {doc.get('facts_sha256', '')}"

    d.add_paragraph(doc["title"], style="Title")
    sub = d.add_paragraph()
    r = sub.add_run(doc["subtitle"])
    r.font.size = Pt(9)
    r.font.color.rgb = MUTED
    lab = d.add_paragraph()
    r = lab.add_run(doc["label"])
    r.italic = True
    r.font.size = Pt(9.5)
    r.font.color.rgb = MUTED
    hairline_above(lab)

    table = d.add_table(rows=1, cols=1)
    cell = table.rows[0].cells[0]
    shade_cell(cell)
    head = cell.paragraphs[0]
    hr = head.add_run("Summary")
    hr.bold = True
    hr.font.size = Pt(12)
    for it in doc["summary"]:
        add_runs(cell.add_paragraph(style="List Bullet"), it)
    d.add_paragraph()  # space after the box (empty: skipped by the read-back)

    for s in doc["sections"]:
        d.add_paragraph(f"{s['n']}. {s['title']}", style="Heading 1")
        for b in s["blocks"]:
            if b["type"] == "ul":
                for it in b["items"]:
                    add_runs(d.add_paragraph(style="List Bullet"), it)
            else:
                add_runs(d.add_paragraph(), b)

    src_head = d.add_paragraph("Sources", style="Heading 2")
    hairline_above(src_head)
    for s in doc["sources"]:
        p = d.add_paragraph(style="List Number")
        r = p.add_run(s["text"])
        r.font.size = Pt(8.5)
    end = d.add_paragraph()
    r = end.add_run(doc["label"])
    r.italic = True
    r.font.size = Pt(9)
    r.font.color.rgb = MUTED
    hairline_above(end)

    foot = sec.footer.paragraphs[0]
    foot.alignment = WD_ALIGN_PARAGRAPH.LEFT
    fr = foot.add_run(doc["label"] + (f"  {doc['footer']}" if doc.get("footer") else ""))
    fr.italic = True
    fr.font.size = Pt(8)
    fr.font.color.rgb = MUTED
    d.save(out)


# ---- Country strategy briefs ------------------------------------------------------------------------


def blocks_texts(blocks):
    """Paragraph texts of brief blocks in reading order (table cells row by row; picture paragraphs carry no text)."""
    out = []
    for b in blocks:
        t = b["type"]
        if t == "p":
            out.append(item_text(b))
        elif t == "ul":
            out += [item_text(it) for it in b["items"]]
        elif t == "h3":
            out.append(b["text"])
        elif t == "choices":
            for it in b["items"]:
                out += [it["label"], item_text(it)]
        elif t == "table":
            out += [h for h in b["head"] if h.strip()]
            for row in b["rows"]:
                out += [item_text(c) for c in row if item_text(c).strip()]
            if b.get("refs"):
                out.append("Sources: " + ", ".join(str(r) for r in b["refs"]) + ".")
        elif t == "figure":
            out.append(f"Figure {b['n']}. {b['title']}")
            out.append(item_text(b["caption"]))
    return out


def expected_texts_brief(doc):
    """Body paragraph texts of a brief in reading order (table cells included)."""
    out = [x for x in (doc.get("eyebrow") or "", doc["title"], doc["subtitle"], doc["label"]) if x]
    for s in doc["sections"]:
        out.append(f"{s['n']}. {s['title']}")
        out += blocks_texts(s["blocks"])
    out.append("Sources")
    out += [s["text"] for s in doc["sources"]]
    if doc.get("appendix"):
        out.append(doc["appendix"]["title"])
        out += blocks_texts(doc["appendix"]["blocks"])
    out.append(doc.get("disclosure") or doc["label"])
    return out


def top_border_cell(cell, color, fill="F9FAFB"):
    tc_pr = cell._tc.get_or_add_tcPr()
    shd = OxmlElement("w:shd")
    shd.set(qn("w:val"), "clear")
    shd.set(qn("w:color"), "auto")
    shd.set(qn("w:fill"), fill)
    tc_pr.append(shd)
    borders = OxmlElement("w:tcBorders")
    for side in ("top", "left", "bottom", "right"):
        el = OxmlElement(f"w:{side}")
        el.set(qn("w:val"), "single")
        if side == "top":
            el.set(qn("w:sz"), "24")
            el.set(qn("w:space"), "0")
            el.set(qn("w:color"), color)
        else:
            el.set(qn("w:sz"), "4")
            el.set(qn("w:color"), "FFFFFF")
        borders.append(el)
    tc_pr.append(borders)


def rule_cell(cell, bottom="E5E7EB"):
    tc_pr = cell._tc.get_or_add_tcPr()
    borders = OxmlElement("w:tcBorders")
    for side in ("top", "left", "bottom", "right"):
        el = OxmlElement(f"w:{side}")
        if side == "bottom":
            el.set(qn("w:val"), "single")
            el.set(qn("w:sz"), "4")
            el.set(qn("w:color"), bottom)
        else:
            el.set(qn("w:val"), "nil")
        borders.append(el)
    tc_pr.append(borders)


def add_blocks(d, blocks, figdir):
    import os

    for b in blocks:
        t = b["type"]
        if t == "p":
            add_runs(d.add_paragraph(), b)
        elif t == "ul":
            for it in b["items"]:
                add_runs(d.add_paragraph(style="List Bullet"), it)
        elif t == "h3":
            d.add_paragraph(b["text"], style="Heading 2")
        elif t == "choices":
            items = b["items"]
            tab = d.add_table(rows=(len(items) + 1) // 2, cols=2)
            for i, it in enumerate(items):
                cell = tab.rows[i // 2].cells[i % 2]
                top_border_cell(cell, BOX_COLOR.get(it.get("kind"), "9CA3AF"))
                r = cell.paragraphs[0].add_run(it["label"])
                r.bold = True
                r.font.size = Pt(8.5)
                add_runs(cell.add_paragraph(), it, size=Pt(9))
            d.add_paragraph()  # space after the box (empty: skipped by the read-back)
        elif t == "table":
            head = b["head"]
            tab = d.add_table(rows=1 + len(b["rows"]), cols=len(head))
            for j, h in enumerate(head):
                cell = tab.rows[0].cells[j]
                rule_cell(cell, bottom="111827")
                r = cell.paragraphs[0].add_run(h)
                r.bold = True
                r.font.size = Pt(8)
            for i, row in enumerate(b["rows"]):
                for j, c in enumerate(row):
                    cell = tab.rows[i + 1].cells[j]
                    rule_cell(cell)
                    add_runs(cell.paragraphs[0], c, size=Pt(8.5))
            if b.get("refs"):
                r = d.add_paragraph().add_run("Sources: " + ", ".join(str(x) for x in b["refs"]) + ".")
                r.font.size = Pt(7.5)
                r.font.color.rgb = MUTED
            else:
                d.add_paragraph()
        elif t == "figure":
            r = d.add_paragraph().add_run(f"Figure {b['n']}. {b['title']}")
            r.bold = True
            png = os.path.join(figdir, f"f{b['n']}.png") if figdir else None
            if png and os.path.exists(png):
                d.add_picture(png, width=Inches(6.9))
            cp = d.add_paragraph()
            for n in b["caption"]["inlines"]:
                run = cp.add_run(n["v"])
                run.italic = True
                run.font.size = Pt(8.5)
                run.font.color.rgb = MUTED
            refs = b["caption"].get("refs") or []
            if refs:
                sup = cp.add_run(",".join(str(x) for x in refs))
                sup.font.superscript = True
                sup.font.color.rgb = MUTED


def build_brief(doc, out, figdir=None):
    d = Document()
    sec = d.sections[0]
    sec.page_width, sec.page_height = Inches(8.5), Inches(11)
    for side in ("left_margin", "right_margin"):
        setattr(sec, side, Inches(0.75))
    for side in ("top_margin", "bottom_margin"):
        setattr(sec, side, Inches(0.7))
    normal = d.styles["Normal"]
    normal.font.name = "Arial"
    normal.font.size = Pt(10.2)
    normal.font.color.rgb = INK
    normal.element.rPr.rFonts.set(qn("w:eastAsia"), "Arial")
    for name in ("Title", "Heading 1", "Heading 2", "List Bullet", "List Number"):
        st = d.styles[name]
        st.font.name = "Arial"
        st.font.color.rgb = INK
    d.styles["Title"].font.size = Pt(21)
    d.styles["Heading 1"].font.size = Pt(13.5)
    d.styles["Heading 2"].font.size = Pt(11)

    cp = d.core_properties
    cp.title = doc["title"]
    cp.subject = "Country strategy brief: " + doc["subtitle"]
    cp.author = "NEIS Center, Allied Industrial Base Explorer (AI-generated, checked against the data)"
    cp.keywords = "Allied Industrial Base; Yards and Fences; Secure; Preserve; Catch Up; Partner; Accelerate; " + doc["iso3"]
    cp.comments = f"NEIS baseline as of {doc.get('as_of_label') or doc.get('as_of')}; facts sha256 {doc.get('facts_sha256', '')}"

    if doc.get("eyebrow"):
        r = d.add_paragraph().add_run(doc["eyebrow"])
        r.font.size = Pt(8.5)
        r.font.color.rgb = MUTED
    d.add_paragraph(doc["title"], style="Title")
    r = d.add_paragraph().add_run(doc["subtitle"])
    r.font.size = Pt(12)
    r.font.color.rgb = MUTED
    lab = d.add_paragraph()
    r = lab.add_run(doc["label"])
    r.italic = True
    r.font.size = Pt(8.5)
    r.font.color.rgb = MUTED
    hairline_above(lab)

    for s in doc["sections"]:
        d.add_paragraph(f"{s['n']}. {s['title']}", style="Heading 1")
        add_blocks(d, s["blocks"], figdir)

    src_head = d.add_paragraph("Sources", style="Heading 2")
    hairline_above(src_head)
    for s in doc["sources"]:
        r = d.add_paragraph(style="List Number").add_run(s["text"])
        r.font.size = Pt(8)
    if doc.get("appendix"):
        d.add_paragraph().add_run().add_break(WD_BREAK.PAGE)
        d.add_paragraph(doc["appendix"]["title"], style="Heading 1")
        add_blocks(d, doc["appendix"]["blocks"], figdir)
    # the disclosure closes the brief (2026-11-03, 17D: the title area carries the quiet provenance line)
    end = d.add_paragraph()
    r = end.add_run(doc.get("disclosure") or doc["label"])
    r.italic = True
    r.font.size = Pt(8.5)
    r.font.color.rgb = MUTED
    hairline_above(end)

    foot = sec.footer.paragraphs[0]
    foot.alignment = WD_ALIGN_PARAGRAPH.LEFT
    fr = foot.add_run(doc.get("footer") or doc["label"])
    fr.italic = True
    fr.font.size = Pt(7.5)
    fr.font.color.rgb = MUTED
    d.save(out)


def body_texts(path):
    """Paragraph texts of the body in document order, table cells included; empty paragraphs skipped."""
    d = Document(path)
    out = []
    for el in d.element.body.iterchildren():
        tag = el.tag.split("}")[-1]
        if tag == "p":
            t = "".join(x.text or "" for x in el.iter(qn("w:t")))
            if t.strip():
                out.append(t)
        elif tag == "tbl":
            for p in el.iter(qn("w:p")):
                t = "".join(x.text or "" for x in p.iter(qn("w:t")))
                if t.strip():
                    out.append(t)
    return out


def check(path, ast_path):
    with open(ast_path, encoding="utf-8") as f:
        doc = json.load(f)
    want = expected_texts_brief(doc) if doc.get("format") == "brief" else expected_texts(doc)
    got = body_texts(path)
    if got == want:
        print(f"ok: {len(got)} paragraphs")
        return 0
    n = min(len(got), len(want))
    for i in range(n):
        if got[i] != want[i]:
            print(f"paragraph {i + 1} differs:\n  docx: {got[i][:200]}\n  ast:  {want[i][:200]}")
            break
    else:
        print(f"{len(got)} paragraphs in the file, {len(want)} expected")
    return 1


def pdf_text(path, out=None):
    from pypdf import PdfReader

    reader = PdfReader(path)
    text = "\n".join((p.extract_text() or "") for p in reader.pages)
    res = {"pages": len(reader.pages), "text": text}
    # to a file when asked: R reads a long single-line stdout in pieces of about 8 kB (system2)
    if out:
        with open(out, "w", encoding="utf-8") as f:
            json.dump(res, f, ensure_ascii=True)
    else:
        json.dump(res, sys.stdout, ensure_ascii=True)
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--in", dest="inp")
    ap.add_argument("--out")
    ap.add_argument("--check")
    ap.add_argument("--ast")
    ap.add_argument("--pdf-text")
    ap.add_argument("--figures")
    a = ap.parse_args()
    if a.pdf_text:
        return pdf_text(a.pdf_text, a.out)
    if a.check:
        return check(a.check, a.ast)
    if a.inp and a.out:
        with open(a.inp, encoding="utf-8") as f:
            doc = json.load(f)
        if doc.get("format") == "brief":
            build_brief(doc, a.out, a.figures)
        else:
            build(doc, a.out)
        return 0
    ap.print_help()
    return 2


if __name__ == "__main__":
    sys.exit(main())
