// A country's strategic summary (2026-10-20; task spec s.17-18): what the AIB Country roles page and the
// Country profile's "Role in the Allied Industrial Base" say about one economy from the strategic layer
// (doc.strategic, read through derive/strategic.ts). Its Strategic AIB Roles (primary, secondary, beyond
// the cap, each with both gates), what it contributes, what it needs and who could supply it, its most
// important strategic compacts, its highest-value partners with the pair's relationship thesis, and where
// new capacity matters.
//
// Every reading is the export's: the narrative slots (countries[].slots, the R templates of
// aib_country_role_narratives()) give the words and their order, and their items as codes
// (countries[].items, since 2026-10-21: node ids with their shares, gate metrics, node functions, ISO3
// codes). This module joins each item to what the bundle already holds: a node to its label and strategic
// compact, a partner to its name and the pair's thesis, a needed node to the allied economies with the
// largest supply position in it (the division of labour's own shares). A bundle without `items` falls back
// on splitting a slot's prose back into its items (splitSlotList). No score, no role and no ranking is
// computed here. Pure (no React, no IO).

import type { AibCompactRole, AibNode, AibStrategic, AibStrategicCompact } from '../../contract.ts';
import { isNum } from '../../../lib/format.ts';
import { cap, poss, proseName } from '../../../country/derive/text.ts';
import {
  compactRolesOfCountry, countryStrategicOf, divisionCells, nodeHomeOf, roleDefOf, strategicScenarioDef, thesisOf, type BilateralThesis, type CountryStrategic, type RoleGate,
  type StrategicRole,
} from '../../derive/strategic.ts';

// ---- Plain words ---------------------------------------------------------------------------------------

/** What each Strategic AIB Role means (task spec s.9), keyed by its code; the export names the roles. */
export const ROLE_MEANING: Readonly<Record<string, string>> = {
  RPA: 'holds important upstream resources or processing capability',
  STM: 'has specialized capabilities that are difficult or inefficient to replicate widely',
  SMI: 'manufactures or integrates large parts of a value chain at scale',
  DMA: 'has a large demand and procurement base that can support allied-scale specialization',
  CIA: 'plays an important outward-investment, financing or capacity-building role',
  DSP: 'has strong potential to host strategically valuable new capacity',
  CBH: 'repeatedly connects complementary countries, technologies or workstreams',
};

/**
 * The summary's heading: "Japan's role in the Allied Industrial Base", "The United States' role ..."; a
 * name written with a comma ("Korea, Rep.") takes no possessive: "Role in the Allied Industrial Base: Korea, Rep.".
 */
export function summaryHeading(name: string): string {
  if (name.includes(',')) return `Role in the Allied Industrial Base: ${name}`;
  return `${cap(poss(proseName(name)))} role in the Allied Industrial Base`;
}

/** The narratives' stage words (R aib_strategic_function_labels()) in plain plural words. */
export const FOCUS_STAGES: Readonly<Record<string, string>> = {
  'upstream resource': 'upstream resources',
  processing: 'processing',
  'component, material and equipment': 'components, materials and equipment',
  'assembled system and end-product': 'assembled systems and end products',
};

/** The same plain words keyed by node function (the focus slot's items). */
export const FUNCTION_STAGES: Readonly<Record<string, string>> = {
  resource: 'upstream resources',
  processing: 'processing',
  components: 'components, materials and equipment',
  systems: 'assembled systems and end products',
};

/**
 * A node label for prose: an aggregated node's generated label ("Uranium/fuel Upstream: aggregated
 * low-coverage sub-sectors (Spent fuel & nuclear scrap, Uranium ores)") as its members ("Spent fuel &
 * nuclear scrap and uranium ores (aggregated)"), as the R narratives write it; any other label unchanged.
 */
export function proseNodeLabel(label: string): string {
  const m = /^.*: aggregated low-coverage sub-sectors \((.*)\)$/.exec(label);
  if (!m) return label;
  const parts = m[1].split(', ').map((p, i) => (i > 0 && /^[A-Z][a-z]/.test(p) ? `${p[0].toLowerCase()}${p.slice(1)}` : p));
  const list = parts.length <= 1 ? parts.join('') : `${parts.slice(0, -1).join(', ')} and ${parts[parts.length - 1]}`;
  return `${list} (aggregated)`;
}

/**
 * The focus slot ("assembled system and end-product and component, material and equipment": one or two
 * stage labels joined by " and ", the stronger first) as its stages in plain words; the slot as written
 * when it is not one or two known labels.
 */
export function focusStages(focus: string | null | undefined): string[] {
  if (!focus) return [];
  const labels = Object.keys(FOCUS_STAGES);
  if (FOCUS_STAGES[focus]) return [FOCUS_STAGES[focus]];
  for (const a of labels) for (const b of labels) if (a !== b && focus === `${a} and ${b}`) return [FOCUS_STAGES[a], FOCUS_STAGES[b]];
  return [focus];
}

/** "Strongest stage of the chain: upstream resources." / "Strongest stages of the chain: X; then Y." */
export function focusWords(stages: readonly string[]): string | null {
  if (!stages.length) return null;
  if (stages.length === 1) return `Strongest stage of the chain: ${stages[0]}.`;
  return `Strongest stages of the chain: ${stages[0]}; then ${stages.slice(1).join('; then ')}.`;
}

/**
 * The firstOn slot (the gate labels where it is first among allied economies, comma-joined) as its
 * measures; the slot as written when a part is not a known gate label.
 */
export function firstOnMeasures(s: AibStrategic, firstOn: string | null | undefined): string[] {
  if (!firstOn) return [];
  const known = new Map(s.roleDefs.flatMap((d) => d.gateLabels).map((l) => [l, l]));
  const parts = splitSlotList(firstOn, known);
  return parts.length > 0 && parts.every((p) => p.key && !p.detail) ? parts.map((p) => p.label) : [firstOn];
}

/** From this many relationship logics on, a relationship reads as broad (as the Partnerships page's thesis does). */
export const BROAD_LOGIC = 4;

/**
 * A pair's relationship logic in words: "A trade and technology relationship", "An investment
 * relationship", from four logics on "A broad relationship: trade, investment, ... and market access".
 */
export function logicWords(logic: readonly string[]): string {
  if (!logic.length) return 'No relationship logic recorded';
  const list = logic.length === 1 ? logic[0] : `${logic.slice(0, -1).join(', ')} and ${logic[logic.length - 1]}`;
  if (logic.length >= BROAD_LOGIC) return `A broad relationship: ${list}`;
  return `${/^[aeiou]/i.test(logic[0]) ? 'An' : 'A'} ${list} relationship`;
}

/** A role status in words ("Primary role", "Not held"). */
export function roleStatusWords(status: string | null): string {
  if (status === 'primary') return 'Primary role';
  if (status === 'secondary') return 'Secondary role';
  if (status === 'qualifies beyond the cap') return 'Qualifies, beyond the cap';
  return 'Not held';
}

/** "1st", "2nd", "3rd", "11th", "97th". */
export function ordinal(n: number): string {
  const k = Math.round(n);
  const t = k % 100;
  if (t >= 11 && t <= 13) return `${k}th`;
  return `${k}${k % 10 === 1 ? 'st' : k % 10 === 2 ? 'nd' : k % 10 === 3 ? 'rd' : 'th'}`;
}

/** A gate percentile (0-1) in words: "97th percentile" ("–" when absent). */
export const percentileWords = (p: number | null): string => (isNum(p) ? `${ordinal(p * 100)} percentile` : '–');

/**
 * A role metric's value as printed, by the metric's kind: US$ billions (keys ending _usd_bn), a share
 * (keys with share, the net-import shares, the scale-up rate) or a count of positions, compacts or
 * workstreams.
 */
export function metricText(key: string, value: number | null): string {
  if (!isNum(value)) return '–';
  if (key.endsWith('_usd_bn')) return `US$${value.toLocaleString('en-US', { maximumFractionDigits: value >= 100 ? 0 : 1 })} bn`;
  if (key.includes('share') || key.startsWith('net_import') || key.endsWith('_rate')) {
    if (value > 0 && value < 0.001) return '<0.1%';
    return `${(value * 100).toFixed(value < 0.1 ? 1 : 0)}%`;
  }
  return value.toLocaleString('en-US', { maximumFractionDigits: 1 });
}

/** One gate in words: "outward direct investment: US$1,547 bn (97th percentile)"; "...: no data" when the export has no value. */
export function gateWords(g: RoleGate): string {
  if (!isNum(g.value)) return `${g.label}: no data`;
  return `${g.label}: ${metricText(g.metric, g.value)}${isNum(g.pct) ? ` (${percentileWords(g.pct)})` : ''}`;
}

/** The role rule in one paragraph, with the section's own thresholds. */
export function roleRuleWords(s: AibStrategic): string {
  const t = s.thresholds;
  const p = (x: number) => ordinal(x * 100);
  return (
    `Each role has two evidence measures, one of size and one of breadth or depth, taken from the model's results and the partner data. ` +
    `An economy holds a role as primary when both measures are above zero and at or above the ${p(t.pPrimary)} percentile of the economies assessed for it, ` +
    `and as secondary when both are at or above the ${p(t.pSecondary)}. It keeps at most ${t.maxPrimary} primary and ${t.maxSecondary} secondary roles; ` +
    `a further role that meets the bar is listed as qualifying beyond the cap. The two measures are never averaged and there is no overall score: ` +
    `the lower of the two percentiles only orders an economy's own roles.`
  );
}

// ---- Splitting a slot's list back into items ----------------------------------------------------------

export interface SlotPart {
  /** the item as written (a node label, an economy's name) */
  label: string;
  /** the known key it names (a node id, an ISO3 code), null when the label is not known */
  key: string | null;
  /** the parenthesis after it ("59%", "48% of allied demand"), null when none */
  detail: string | null;
}

/** The index of the parenthesis closing the one that opens at `open` (-1 when unbalanced). */
function closing(text: string, open: number): number {
  let depth = 0;
  for (let i = open; i < text.length; i += 1) {
    if (text[i] === '(') depth += 1;
    else if (text[i] === ')') {
      depth -= 1;
      if (depth === 0) return i;
    }
  }
  return -1;
}

/**
 * Split a slot's comma-separated list ("Silver powder (59%), Wafers (28%)"; "Japan, Korea, Rep., United
 * States") into its items. Labels themselves may hold commas and parentheses ("Magnets, magnet parts &
 * assemblies", "PGMs (iridium, ruthenium)"), so the longest `known` label at each position wins; an
 * unknown item runs to the next ", " or " (". A parenthesis right after an item is its detail.
 */
export function splitSlotList(text: string | null | undefined, known: ReadonlyMap<string, string>): SlotPart[] {
  if (!text) return [];
  const labels = [...known.keys()].filter(Boolean).sort((a, b) => b.length - a.length);
  const ends = (i: number) => i >= text.length || text.startsWith(', ', i) || text.startsWith(' (', i);
  const out: SlotPart[] = [];
  let i = 0;
  while (i < text.length) {
    let label = labels.find((l) => text.startsWith(l, i) && ends(i + l.length)) ?? null;
    const key = label !== null ? (known.get(label) ?? null) : null;
    if (label === null) {
      const rest = text.slice(i);
      const m = /^(.*?)(?=, | \(|$)/.exec(rest);
      label = m && m[1] ? m[1] : rest;
    }
    i += label.length;
    let detail: string | null = null;
    // one or more parentheses: an unknown label's own parentheses join it, the last one is the detail
    while (text.startsWith(' (', i)) {
      const close = closing(text, i + 1);
      if (close < 0) break;
      if (detail !== null) label = `${label} (${detail})`;
      detail = text.slice(i + 2, close);
      i = close + 1;
    }
    out.push({ label: label.trim(), key, detail });
    if (text.startsWith(', ', i)) i += 2;
    else if (i < text.length) {
      // an unexpected character: keep the rest as one unknown item rather than lose it
      out.push({ label: text.slice(i).replace(/^[,\s]+/, ''), key: null, detail: null });
      break;
    }
  }
  return out.filter((p) => p.label.length > 0);
}

// ---- The summary ------------------------------------------------------------------------------------------

/** One item of a slot list joined to the node and the strategic compact it names. */
export interface SlotEntry {
  label: string;
  nodeId: string | null;
  compact: AibStrategicCompact | null;
  /** the export's figure for the item ("59% of world supply", "48% of allied demand"), null when none */
  detail: string | null;
}

/** An allied economy with a supply position in a node (the division of labour's share of world supply). */
export interface SupplierShare {
  iso3: string;
  share: number;
}

export interface NeedEntry extends SlotEntry {
  /** the allied economies (other than the one read) with the largest supply position in the node, at most two */
  suppliers: SupplierShare[];
}

export interface PartnerEntry {
  /** null when the name is not one of the bundle's economies */
  iso3: string | null;
  name: string;
  thesis: BilateralThesis | null;
}

export interface StrategicSummary {
  iso3: string;
  allied: boolean;
  majorSupplier: boolean;
  /** the decoded roles (primary, secondary, beyond the cap, every gate) */
  cs: CountryStrategic;
  /** "Combines capital & investment, specialized technology and scale manufacturing roles" */
  headline: string;
  /** "secondary coordination / bridge role" */
  secondaryLine: string | null;
  /** "compacts with a specialist position" (the gates where it is first among allied economies) */
  firstOn: string | null;
  /** firstOn split into its measures (firstOnMeasures()) */
  firstOnList: string[];
  contributes: SlotEntry[];
  /** "component, material and equipment" */
  focus: string | null;
  /** focus as its stages of the chain in plain words, the stronger first (focusStages()) */
  focusList: string[];
  needs: NeedEntry[];
  /** "a significant net buyer of upstream materials (top quartile of allied net imports)" */
  dependency: string | null;
  /** every compact role of the economy, the most relevant first */
  compacts: AibCompactRole[];
  partners: PartnerEntry[];
  /** the scenario the partners are read on (its label: "Managed Interdependence") */
  partnersScenario: string;
  capacity: SlotEntry[];
  /** "US$4.1bn of unmet allied demand" */
  capacityTotal: string | null;
  /** the strategic compacts where it is a capacity-building host */
  hostIn: AibStrategicCompact[];
  narrative: string | null;
}

/** Below this share of world supply an economy is not listed as a possible supplier of a need (the division's one-dot bar). */
export const SUPPLIER_MIN_SHARE = 0.02;

const nodeLabelsOf = (nodes: readonly Pick<AibNode, 'id' | 'label'>[]): Map<string, string> => new Map(nodes.map((n) => [n.label, n.id]));

/** The node and compact of a slot item (the detail rewritten by `detail`). */
function entryOf(s: AibStrategic, p: SlotPart, detail: (d: string | null) => string | null): SlotEntry {
  const home = p.key ? nodeHomeOf(s, p.key) : null;
  return { label: p.label, nodeId: p.key, compact: home?.compact ?? null, detail: detail(p.detail) };
}

/** The allied economies with the largest supply position in a node (other than `self`), at most `n`. */
export function alliedSuppliersOf(s: AibStrategic, nodeId: string, self: string, n = 2): SupplierShare[] {
  const home = nodeHomeOf(s, nodeId);
  if (!home) return [];
  const allied = new Set(s.countries.filter((c) => c.allied).map((c) => c.iso3));
  return divisionCells(home.compact)
    .filter((c) => c.node === nodeId && c.iso3 !== self && allied.has(c.iso3) && isNum(c.supply) && c.supply >= SUPPLIER_MIN_SHARE)
    .sort((a, b) => (b.supply ?? 0) - (a.supply ?? 0) || a.iso3.localeCompare(b.iso3))
    .slice(0, n)
    .map((c) => ({ iso3: c.iso3, share: c.supply as number }));
}

/** The headline when the export wrote no narrative for the economy: its primary roles by name, or that it has none. */
function headlineOf(s: AibStrategic, cs: CountryStrategic): string {
  if (!cs.primary.length && !cs.secondary.length) return 'No strategic AIB role at the current thresholds; it contributes through its compact-specific roles';
  if (!cs.primary.length) return 'No primary strategic role at the current thresholds';
  return cs.primary.map((r) => roleDefOf(s, r.code)?.name ?? r.name).join(', ');
}

/**
 * One economy's strategic summary (null when the layer does not assess it: every economy with a compact
 * role is assessed).
 * `nodes`: the document's nodes (labels); `names`: the document's economy names (the narrative's partners
 * are names, joined back to codes here).
 */
export function strategicSummaryOf(
  s: AibStrategic,
  nodes: readonly Pick<AibNode, 'id' | 'label'>[],
  names: Readonly<Record<string, string>>,
  iso3: string,
): StrategicSummary | null {
  const iso = iso3.toUpperCase();
  const cs = countryStrategicOf(s, iso);
  const compacts = compactRolesOfCountry(s, iso);
  if (!cs) return null;
  const slots = cs.slots;
  const items = cs.items;
  const labels = nodeLabelsOf(nodes);
  const labelOf = new Map(nodes.map((n) => [n.id, n.label]));
  const share = (v: number | null, of: string) => (isNum(v) ? `${Math.round(v * 100)}% of ${of}` : null);
  const nodeEntry = (id: string, detail: string | null): SlotEntry => ({ label: proseNodeLabel(labelOf.get(id) ?? id), nodeId: id, compact: nodeHomeOf(s, id)?.compact ?? null, detail });
  const pct = (d: string | null) => (d && /^\d+(\.\d+)?%$/.test(d) ? `${d} of world supply` : d);
  const contributes = items
    ? items.contributes.map(([id, v]) => nodeEntry(id, share(v, 'world supply')))
    : splitSlotList(slots?.contributes, labels).map((p) => entryOf(s, p, pct));
  const needs = (items ? items.needs.map(([id, v]) => nodeEntry(id, share(v, 'allied demand'))) : splitSlotList(slots?.needs, labels).map((p) => entryOf(s, p, (d) => d))).map(
    (e): NeedEntry => ({ ...e, suppliers: e.nodeId ? alliedSuppliersOf(s, e.nodeId, iso) : [] }),
  );
  // "Lithium chemicals, Solar cells (US$4.1bn of unmet allied demand)": the total closes the list
  const capText = slots?.capacity ?? null;
  const capMatch = capText ? /^(.*) \((US\$[^()]*)\)$/.exec(capText) : null;
  const capacity = items ? items.capacity.map(([id]) => nodeEntry(id, null)) : splitSlotList(capMatch ? capMatch[1] : capText, labels).map((p) => entryOf(s, p, (d) => d));
  let capacityTotal: string | null = capMatch ? capMatch[2] : null;
  if (items) capacityTotal = items.capacity.length && isNum(items.capacityUsdBn) ? `US$${items.capacityUsdBn.toFixed(1)}bn of unmet allied demand` : null;
  const byName = new Map(Object.entries(names).map(([k, v]) => [v, k]));
  const partners = items
    ? items.partners.map((k): PartnerEntry => ({ iso3: k, name: names[k] ?? k, thesis: thesisOf(s, iso, k) }))
    : splitSlotList(slots?.partners, byName).map((p): PartnerEntry => ({ iso3: p.key, name: p.label, thesis: p.key ? thesisOf(s, iso, p.key) : null }));
  const gateLabel = new Map(s.roleDefs.flatMap((d) => d.gates.map((g, i) => [g, d.gateLabels[i] ?? g] as const)));
  const firstOnList = items ? items.firstOn.map((g) => gateLabel.get(g) ?? g) : firstOnMeasures(s, slots?.firstOn);
  const focusList = items ? items.focus.map((f) => FUNCTION_STAGES[f] ?? f) : focusStages(slots?.focus);
  const hostIn = compacts.filter((r) => r.functions.includes('Capacity-building host')).flatMap((r) => {
    const c = s.compacts.find((x) => x.id === r.compact);
    return c ? [c] : [];
  });
  return {
    iso3: iso,
    allied: cs.allied,
    majorSupplier: cs.majorSupplier,
    cs,
    headline: slots?.headline || headlineOf(s, cs),
    secondaryLine: slots?.secondary || (cs.secondary.length && !slots ? `secondary: ${cs.secondary.map((r) => r.name).join(', ')}` : null),
    firstOn: slots?.firstOn || null,
    firstOnList,
    contributes,
    focus: slots?.focus || null,
    focusList,
    needs,
    dependency: slots?.dependency || null,
    compacts,
    partners,
    partnersScenario: strategicScenarioDef(s, 'managed')?.label ?? 'Managed Interdependence',
    capacity,
    capacityTotal,
    hostIn,
    narrative: cs.narrative,
  };
}

/** The roles of a summary in display order: primary, secondary, then beyond the cap. */
export const heldRoles = (cs: CountryStrategic): StrategicRole[] => [...cs.primary, ...cs.secondary, ...cs.beyondCap];

/**
 * The economy a strategic summary opens on when the reader has chosen none: the first allied economy, by
 * name, that holds a primary role (else the first assessed one). An alphabetical default, not a ranking.
 */
export function defaultEconomy(s: AibStrategic, name: (iso: string) => string): string | null {
  const by = (a: { iso3: string }, b: { iso3: string }) => name(a.iso3).localeCompare(name(b.iso3), 'en');
  const pick = [...s.countries].filter((c) => c.allied && c.primary.length > 0).sort(by)[0] ?? [...s.countries].sort(by)[0];
  return pick?.iso3 ?? null;
}

/** The choices of an economy selector: allied economies by name, then the major non-allied suppliers. */
export function economyOptions(s: AibStrategic, name: (iso: string) => string): { value: string; label: string; group: string }[] {
  const by = (a: string, b: string) => name(a).localeCompare(name(b), 'en');
  const allied = s.countries.filter((c) => c.allied).map((c) => c.iso3).sort(by);
  const other = s.countries.filter((c) => !c.allied).map((c) => c.iso3).sort(by);
  return [
    ...allied.map((iso) => ({ value: iso, label: name(iso), group: 'Allied economies' })),
    ...other.map((iso) => ({ value: iso, label: name(iso), group: 'Major non-allied suppliers (supply roles only)' })),
  ];
}

const NON_ALLIED = / \(non-allied supplier\)$/;

/**
 * A compact role in words: "Processing anchor in metals & alloys (25% of world exports or production)"; a
 * non-allied supplier's role says so at the end ("...; a non-allied supplier").
 */
export function compactRoleWords(r: AibCompactRole): string {
  const share = isNum(r.leadShare) ? ` (${Math.round(r.leadShare * 100)}% of ${r.leadBasis ? r.leadBasis.replace(/^share of /, '') : 'the step'})` : '';
  const nonAllied = NON_ALLIED.test(r.role);
  return `${r.role.replace(NON_ALLIED, '')}${r.leadStep ? ` in ${r.leadStep.toLowerCase()}` : ''}${share}${nonAllied ? '; a non-allied supplier' : ''}`;
}

/** A compact role's other functions, after its lead role ("also scale manufacturer, demand anchor"). */
export function otherFunctions(r: AibCompactRole): string[] {
  const lead = r.role.replace(NON_ALLIED, '');
  return r.functions.filter((f) => f !== lead);
}
