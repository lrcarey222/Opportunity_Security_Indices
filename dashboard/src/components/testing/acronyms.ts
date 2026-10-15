// The bare-acronym test helper (spec_country.md C3.1, C3.4; the user's 2026-10-12 edit "less assumption
// that users are familiar with our terms"). Every module's view tests call it on each route:
//
//   expect(bareAcronyms(container)).toEqual([]);
//
// It scans the places where an acronym may never stand alone (headings h1-h4, table headers, legends,
// labels, buttons, nav links, tabs, eyebrows, .label, figure captions and tags; their text and their
// aria-label) and returns each match of the paper's acronyms that is neither inside an <abbr> nor in
// parentheses directly after its full name ("Concentration (HHI)", "Partnership Strength Index (PSI)").
// Allowed everywhere and therefore not listed: EU, UN, GDP, USD, NATO, G7 and units.
// Test-only (no IO); lives under components/ so every module's tests can import it.

/** The scanned selectors (C3.4). */
export const ACRONYM_SCOPES = ['h1', 'h2', 'h3', 'h4', 'th', 'legend', 'label', 'button', 'nav a', '[role="tab"]', '.eyebrow', '.label', 'figcaption', '.tag'] as const;

/** Each acronym and the full names it may follow in parentheses. */
export const ACRONYM_NAMES: Readonly<Record<string, readonly string[]>> = {
  'CM-FDI': ['Factory projects', 'Cleantech Manufacturing FDI', 'Cleantech Manufacturing FDI dataset', 'clean-tech manufacturing projects', 'manufacturing projects'],
  ES: ['Energy Security'],
  EO: ['Economic Opportunity'],
  SI: ['Strategic Index'],
  PSI: ['Partnership Strength Index', 'Partnership Strength'],
  FS: ['Friendshore'],
  PO: ['Partnership Opportunity'],
  DP: ['Development Potential'],
  HHI: ['Concentration', 'Herfindahl-Hirschman Index', 'concentration index'],
  RCA: ['Revealed comparative advantage'],
  PAMS: ['IEA Policies and Measures', 'Policies and Measures'],
  CAT: ['Climate Action Tracker', 'Climate Action Tracker rating'],
  TRL: ['Technology readiness', 'Technology Readiness Level'],
  IPEF: ['Indo-Pacific Economic Framework'],
  MSP: ['Minerals Security Partnership'],
  POL: ['Policy activity', 'Policy'],
};

// CM-FDI first: the alternation must try the longer token before FDI-free ones
const ACRONYM_RE = /(?<![\w-])(CM-FDI|PSI|PAMS|IPEF|HHI|RCA|CAT|TRL|MSP|POL|ES|EO|SI|FS|PO|DP)(?![\w-])/g;

/** The visible text of an element, leaving out <abbr> elements (an <abbr title> is allowed). */
function textWithoutAbbr(el: Element): string {
  let out = '';
  const walk = (n: Node) => {
    if (n.nodeType === 3) {
      out += n.nodeValue ?? '';
      return;
    }
    if (n.nodeType !== 1) return;
    const e = n as Element;
    if (e.tagName === 'ABBR') {
      // keep the place of the abbreviation (so "Index (<abbr>PSI</abbr>)" does not read as a bare "()")
      out += ' ';
      return;
    }
    // hidden helpers (screen-reader-only text still counts; aria-hidden decoration does not)
    if (e.getAttribute('aria-hidden') === 'true') return;
    for (const c of Array.from(e.childNodes)) walk(c);
  };
  walk(el);
  return out.replace(/\s+/g, ' ').trim();
}

/** True when the acronym at `index` is inside "(...)" that directly follows one of its full names. */
function afterFullName(text: string, acronym: string, index: number): boolean {
  const open = text.lastIndexOf('(', index);
  if (open < 0) return false;
  // the parenthesis holds only the acronym (and maybe whitespace)
  const close = text.indexOf(')', index);
  if (close < 0 || text.slice(open + 1, close).trim() !== acronym) return false;
  const before = text.slice(0, open).trimEnd().toLowerCase();
  return (ACRONYM_NAMES[acronym] ?? []).some((name) => before.endsWith(name.toLowerCase()));
}

/** The bare acronyms in one string ("PSI in \"PSI rank\""); none when the text spells them out. */
export function bareAcronymsInText(text: string): string[] {
  const out: string[] = [];
  for (const m of text.matchAll(ACRONYM_RE)) {
    const a = m[1];
    if (afterFullName(text, a, m.index ?? 0)) continue;
    out.push(`${a} in "${text.length > 80 ? `${text.slice(0, 77)}...` : text}"`);
  }
  return out;
}

/**
 * Every bare acronym under `root`, one entry per occurrence, deduplicated by element and text:
 * `['PSI in "Partner-level PSI"', ...]`. An empty list means the page spells its terms out.
 */
export function bareAcronyms(root: ParentNode): string[] {
  const seen = new Set<Element>();
  const found: string[] = [];
  for (const sel of ACRONYM_SCOPES) {
    for (const el of Array.from(root.querySelectorAll(sel))) {
      if (seen.has(el)) continue;
      seen.add(el);
      if (el.closest('abbr')) continue;
      const texts = [textWithoutAbbr(el)];
      const aria = el.getAttribute('aria-label');
      if (aria) texts.push(aria);
      for (const t of texts) found.push(...bareAcronymsInText(t));
    }
  }
  return [...new Set(found)];
}
