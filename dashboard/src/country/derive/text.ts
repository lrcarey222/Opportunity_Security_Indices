// Generated sentences of a Country profile (spec_country.md A4.2), built from fact-sheet fields only:
// every number prints its display string `d` (computed once in R), every enum prints through the sheet's
// `words`. Pure: no React. The memo is written separately (part B); these are the page's own templates.

import { stageShort } from '../../lib/stages.ts';
import {
  cellOf, cellOrder, COUNTRY_STAGES, COUNTRY_TECHS, dOf, parseCellKey, QUADRANTS, type CellFacts, type Driver, type FactSheet, type PartnerRow, type Quadrant, type Tier,
} from '../contract.ts';

/** "a, b and c" */
export function joinAnd(xs: readonly string[]): string {
  if (xs.length <= 1) return xs.join('');
  return `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`;
}

/**
 * Sectors in words, grouped by technology in the cell order, the stages lower-case in chain order:
 * ["geothermal-refining", "coal-recovery", "geothermal-extraction"] -> "Coal (recovery) and Geothermal
 * (extraction and refining)". Keys that are not sectors are skipped.
 */
export function sectorsByTechText(keys: readonly string[]): string {
  const by = new Map<string, string[]>();
  for (const k of [...new Set(keys)].filter((x) => cellOrder(x) >= 0).sort((a, b) => cellOrder(a) - cellOrder(b))) {
    const p = parseCellKey(k)!;
    const tech = COUNTRY_TECHS.find((t) => t.slug === p.tech)!.label;
    const stage = COUNTRY_STAGES.find((s) => s.slug === p.stage)!.label.toLowerCase();
    by.set(tech, [...(by.get(tech) ?? []), stage]);
  }
  return joinAnd([...by].map(([tech, stages]) => `${tech} (${joinAnd(stages)})`));
}

/** Capitalise the first letter. */
export const cap = (s: string): string => (s ? s[0].toUpperCase() + s.slice(1) : s);

/** An economy's name in running text: "the United States", "the Netherlands", "Japan". */
export function proseName(name: string): string {
  if (/^the /i.test(name)) return name;
  const the =
    /^(United |Russian Federation$|European Union$|Holy See$|Gambia$|Bahamas$|Netherlands$|Philippines$|Maldives$|Comoros$|Seychelles$|Czech Republic$|Dominican Republic$|Central African Republic$|Kyrgyz Republic$|Slovak Republic$|Democratic Republic|Republic of )/.test(
      name,
    ) || / Islands$/.test(name);
  return the ? `the ${name}` : name;
}

/** The possessive of a name in running text: "Japan's", "the United States'", "the Netherlands'". */
export function poss(name: string): string {
  return name.endsWith('s') ? `${name}'` : `${name}'s`;
}

export function tierPhrase(tier: Tier): string {
  return tier === 'core' ? 'a core member of the allied coalition' : tier === 'partner' ? 'a coalition partner' : 'outside the allied coalition';
}

export function tierTag(tier: Tier): string {
  return tier === 'core' ? 'CORE MEMBER' : tier === 'partner' ? 'PARTNER' : 'NOT IN THE COALITION';
}

/** The corner's display word: the contract's QUADRANTS label (Grow, Upgrade, Exposed, Stable). Since
 *  2026-10-28 the app owns these names and ignores the sheet's words.quadrant, which keeps the older
 *  Secure and Watch until the next Country export. */
export function quadrantWord(q: Quadrant | null | undefined): string {
  if (!q) return '';
  return QUADRANTS.find((x) => x.id === q)?.label ?? q;
}

/** "Batteries · Ref" (short scatter label: the stage's short form, lib/stages.ts STAGE_SHORT). */
export function shortCellLabel(c: CellFacts): string {
  return `${c.tech} · ${stageShort(c.stage)}`;
}

/** The page lede (S0): scored, non-rankable and no-data variants. */
export function ledeOf(sheet: FactSheet): string {
  const m = sheet.meta;
  const s = sheet.summary;
  const name = proseName(m.country);
  const first = `${cap(proseName(name))} is ${tierPhrase(m.tier)}.`;
  if (m.status === 'no_data') {
    const psi = s.psi_1 ? ` The partner the Partnership Strength Index rates highest for it is ${proseName(s.psi_1.name)}.` : '';
    return `${first} It has no Energy Security or Economic Opportunity scores${m.status_reason ? `: ${m.status_reason}` : ''}.${psi}`;
  }
  if (m.status === 'non_rankable') {
    return `${first} ${cap(poss(proseName(name)))} index scores are shown but not ranked against other economies: ${m.status_reason ?? 'most of its data are filled in'}.`;
  }
  const parts: string[] = [first];
  const upgrade = s.quadrants.upgrade;
  const nUp = upgrade.v ?? 0;
  const howMany = nUp === 0 ? 'none' : dOf(upgrade);
  let mid = `On the NEIS baseline, ${howMany} of its ${dOf(s.n_placed)} placed sectors ${nUp === 0 || nUp === 1 ? 'combines' : 'combine'} high economic opportunity with low energy security for it (the ${quadrantWord('upgrade')} corner, split at its own median sector)`;
  if (m.has_si && s.top_si) {
    const top = cellOf(sheet, s.top_si.cell);
    const nxt = cellOf(sheet, s.top_si.next);
    const topL = top?.label_prose ?? s.top_si.label;
    if (s.top_si.near_tie && nxt) mid += `, and its top two Strategic Index sectors, ${topL} and ${nxt.label_prose}, are almost tied.`;
    else mid += `, and its top Strategic Index sector is ${topL}.`;
  } else {
    mid += '.';
  }
  parts.push(mid);
  if (!m.has_si) parts.push('It has no Strategic Index because it has no International Energy Agency policy records.');
  if (s.psi_1) parts.push(`The partner the Partnership Strength Index rates highest for it is ${proseName(s.psi_1.name)}.`);
  return parts.join(' ');
}

/** "low" / "middle" / "high" from a percentile (thirds of the economies scored). */
export function levelWord(pct: number | null | undefined): 'low' | 'middle' | 'high' | null {
  if (pct === null || pct === undefined || !Number.isFinite(pct)) return null;
  return pct < 100 / 3 ? 'low' : pct > 200 / 3 ? 'high' : 'middle';
}

const catWord = (d: Driver): string => d.category.toLowerCase();

/** The top drivers of a pillar in their order of importance (most negative first for ES below the median). */
export function topDrivers(drivers: Driver[], pillar: 'es' | 'eo'): Driver[] {
  const top = drivers.filter((d) => d.top && !d.constant);
  const imp = (d: Driver) => d.impact.v ?? 0;
  // ES: the categories pulling down first (most negative); EO: pushing up first (most positive)
  return [...top].sort((a, b) => (pillar === 'es' ? imp(a) - imp(b) : imp(b) - imp(a)));
}

/**
 * The header sentence of a "why" panel (S4): "Energy Security is low here (higher than 12% of the other
 * economies) mainly because of energy imports and reserves; Economic Opportunity is high (higher than
 * 81% of them) mainly because of trade and investment." Clauses without top drivers drop "mainly because".
 */
export function whySentence(c: CellFacts, drivers: { es: Driver[]; eo: Driver[] } = c.drivers): string {
  const clause = (pl: 'es' | 'eo', name: string, them: boolean): string | null => {
    const p = c[pl];
    if (!p || p.v === null) return null;
    const lvl = levelWord(p.pct.v);
    const pctTxt = p.pct.v === null ? '' : them ? ` (higher than ${p.pct.d} of them)` : ` (higher than ${p.pct.d} of the other ${p.n_others.d} economies)`;
    const top = topDrivers(drivers[pl] ?? [], pl).slice(0, 2);
    // a driver explains the level only when it pushes the same way (above the median: positive impacts);
    // a top driver pulling the other way is named with "despite"
    const up = p.side === 'above' || (p.side == null && (p.pct.v ?? 0) >= 50);
    const byAbs = (a: Driver, b: Driver) => Math.abs(b.impact.v ?? 0) - Math.abs(a.impact.v ?? 0);
    const pro = top.filter((d) => (up ? (d.impact.v ?? 0) > 0 : (d.impact.v ?? 0) < 0)).sort(byAbs);
    const con = top.filter((d) => !pro.includes(d)).sort(byAbs);
    const because = `${pro.length ? ` mainly because of ${joinAnd(pro.map(catWord))}` : ''}${con.length ? `${pro.length ? ',' : ''} despite ${joinAnd(con.map(catWord))}` : ''}`;
    const lvlTxt = lvl ? `${lvl} here` : 'shown here';
    return `${name} is ${lvlTxt}${pctTxt}${because}`;
  };
  const a = clause('es', 'Energy Security', false);
  const b = clause('eo', 'Economic Opportunity', a !== null);
  const xs = [a, b].filter((x): x is string => !!x);
  return xs.length ? `${xs.join('; ')}.` : '';
}

/**
 * The plain-words line of a raw figure's note (the sheet's `note_d`, 2026-10-14), or null. An EU member's
 * regional figure (note eu_region_share) says it is not the country's own.
 */
/**
 * The edition after a source label without nested brackets (as the memo source list,
 * country_memo_source_edition): " (2025)"; " (data to 2025)" when the edition's lead ("2026") is already in
 * the label; else ", October 2026 extract (as of 5 October 2026)". Empty when there is no edition.
 */
export function editionSuffix(label: string, edition: string | null | undefined): string {
  const ed = (edition ?? '').trim();
  if (!ed) return '';
  if (!ed.includes('(')) return ` (${ed})`;
  const lead = ed.replace(/\(.*$/, '').trim();
  const rest = ed.replace(/^[^(]*/, '').trim();
  if (lead && label.includes(lead) && /^\([^()]*\)$/.test(rest)) return ` ${rest}`;
  return `, ${ed}`;
}

export function rawNoteLine(raw: { note: string | null; note_d?: string | null } | null | undefined, name: string): string | null {
  if (!raw || !raw.note) return null;
  if (raw.note === 'eu_region_share') return `${cap(raw.note_d ?? "the EU's figure, carried by each EU member")}, not ${poss(name)} own figure.`;
  if (raw.note_d) return `${cap(raw.note_d)}.`;
  if (raw.note === 'production_assumed_zero') return 'No source reports this production separately; it is taken as zero and said so.';
  return null;
}

/**
 * A sentence for the top drivers of a "why" panel that rest on the EU's regional figure (a variable with raw
 * note eu_region_share): "The share of world manufacturing capacity, 2035 (announced) behind foreign
 * dependency is the EU's figure, which every EU member carries, not Finland's own." Null when none does.
 */
export function euRegionSentence(drivers: { es: Driver[]; eo: Driver[] }, name: string): string | null {
  const hits: string[] = [];
  for (const pl of ['es', 'eo'] as const) {
    for (const d of drivers[pl] ?? []) {
      if (!d.top || d.constant) continue;
      for (const v of d.vars) {
        if (v.raw?.note !== 'eu_region_share') continue;
        const h = `the ${v.label.charAt(0).toLowerCase()}${v.label.slice(1)} behind ${catWord(d)}`;
        if (!hits.includes(h)) hits.push(h);
      }
    }
  }
  if (!hits.length) return null;
  return `${cap(joinAnd(hits))} ${hits.length === 1 ? "is the EU's figure" : "are the EU's figures"}, which every EU member carries, not ${poss(name)} own.`;
}

/** The intro of S4 by the sheet's priority rule. */
export function whyIntro(sheet: FactSheet): string {
  const name = proseName(sheet.meta.country);
  const cells = sheet.priority_sectors.map((k) => cellOf(sheet, k)).filter((c): c is CellFacts => !!c);
  const list = joinAnd(cells.map((c) => c.label_prose));
  switch (sheet.priority_rule) {
    case 'upgrade':
      return `${cells.length === 1 ? 'One' : cap(numberWord(cells.length))} of ${poss(name)} sectors ${cells.length === 1 ? 'combines' : 'combine'} high economic opportunity with low energy security (against ${poss(name)} own median sector) and ${cells.length === 1 ? 'ranks' : 'rank'} high on the Strategic Index: ${list}. For each, the bars show which kinds of data set each score apart from the middle economy in the same sector, with the figures behind them.`;
    case 'top_si_fallback':
      return `None of ${poss(name)} placed sectors is in the ${quadrantWord('upgrade')} corner, so this section explains its top Strategic Index sectors: ${list}. The bars show which kinds of data set each score apart from the middle economy in the same sector, with the figures behind them.`;
    case 'top_eo':
      return `${cap(name)} has no Strategic Index, so this section explains the placed sectors where its Economic Opportunity is highest: ${list}.`;
    default:
      return `No sector can be explained here: ${sheet.meta.status === 'no_data' ? (sheet.meta.status_reason ?? 'there are no scores') : 'no sector can be compared with other economies'}.`;
  }
}

/** Small counts in words for running text (UI only; the memo never does this). */
export function numberWord(n: number): string {
  const w = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten'];
  return n >= 0 && n <= 10 ? w[n] : String(n);
}

/** Why a cell is not placed on the scatter. */
export function notPlacedReason(c: CellFacts): string {
  if (c.quadrant_reason) return c.quadrant_reason;
  if (!c.es) return 'no Energy Security score for any economy in this sector';
  if (!c.eo) return 'no Economic Opportunity score';
  if (!c.es.rankable) return `Energy Security not comparable${c.es.reason ? `: ${c.es.reason}` : ''}`;
  if (!c.eo.rankable) return `Economic Opportunity not comparable${c.eo.reason ? `: ${c.eo.reason}` : ''}`;
  return 'not placed';
}

/** The "Main reason" cell of a partner row: "Reliable supplier, strongest in oil extraction and gas extraction". */
export function partnerReason(sheet: FactSheet, r: PartnerRow): string {
  if (!r.reason) return '–';
  const word = cap(sheet.words?.reason?.[r.reason] ?? r.reason_d ?? r.reason);
  const cells = r.reason === 'fs' ? r.fs_best5 : r.reason === 'po' ? r.po_best5 : [];
  const names = cells.slice(0, 2).map((k) => cellOf(sheet, k)?.label_prose ?? k);
  return names.length ? `${word}, strongest in ${joinAnd(names)}` : word;
}

/** Agreement names in words ("the G7, NATO"), or "none". */
export function agreementsText(sheet: FactSheet, xs: readonly string[]): string {
  if (!xs.length) return 'none';
  return xs.map((a) => sheet.words?.agreement?.[a] ?? a).join(', ');
}

/** The UN-vote cell of a partner row. */
export function alignmentText(r: PartnerRow): string {
  const t = r.ties;
  const base = t.alignment_d || '–';
  if (t.alignment_basis === 'inherited') return `${base} (inherits its sovereign's position)`;
  if (t.alignment_basis === 'neutral') return `${base} (no votes of its own)`;
  return base;
}

/** The match column words (sentence case), or a dash. */
export function matchText(sheet: FactSheet, r: PartnerRow): string {
  return r.match ? cap(sheet.words?.match?.[r.match] ?? r.match) : '–';
}

/** "Not placed (n): Geothermal — Extraction (no Energy Security score ...), ..." */
export function notPlacedLine(sheet: FactSheet): string | null {
  const xs = sheet.cells.filter((c) => !c.placed);
  if (!xs.length) return null;
  return `Not placed (${xs.length}): ${xs.map((c) => `${c.label} (${notPlacedReason(c)})`).join('; ')}.`;
}
