// The three partnership lenses (2026-10-19; aib_scenarios_spec.md s.5 C and s.6), shared by the Allied
// Industrial Base Partnerships page (/aib/partnerships) and the Country profile's Partnerships section:
// four kinds of partnership (Trade, Investment, Development, Geopolitical), each read through three lenses,
//
//   Today            observed data as they are (2024 trade, investment positions and projects, aid, votes
//                    and agreements): no model and no preferences
//   OSI ranking   the Opportunity Security Indices at the NEIS defaults (preference-weighted scores)
//   AIB model        the Allied Industrial Base model under the toggled scenario (a counterfactual market
//                    structure, not a forecast)
//
// and a cross-lens summary per kind: the partners each lens names, grouped by which lenses name them, in
// plain words ("a tie today that both OSI and the AIB keep", "ranked high by OSI, not chosen by the AIB",
// "chosen by the AIB, not a tie today"). A lens with no data at the focus is left out of the comparison and
// says so in one line. Pure (no React, no IO).

export const PARTNERSHIP_KINDS = ['trade', 'investment', 'development', 'geopolitical'] as const;
export type PartnershipKind = (typeof PARTNERSHIP_KINDS)[number];

export const KIND_LABEL: Readonly<Record<PartnershipKind, string>> = {
  trade: 'Trade',
  investment: 'Investment',
  development: 'Development',
  geopolitical: 'Geopolitical',
};

/** A kind from a URL value (Trade by default). */
export const kindOf = (v: string | null | undefined): PartnershipKind => ((PARTNERSHIP_KINDS as readonly string[]).includes(v ?? '') ? (v as PartnershipKind) : 'trade');

/** The lenses, in the order the panels show them. */
export const LENS_IDS = ['today', 'osi', 'aib'] as const;
export type LensId = (typeof LENS_IDS)[number];

/** Each lens's panel heading and what it is, in plain words. */
export const LENS_INFO: Readonly<Record<LensId, { title: string; short: string; words: string }>> = {
  today: { title: 'Today', short: 'today', words: 'Observed data as they are: no analysis and no preferences.' },
  osi: {
    title: 'OSI ranking',
    short: 'OSI',
    words: 'The Opportunity Security Indices at the NEIS defaults: rankings built on preference-weighted scores (a Country profile can change the Perspective).',
  },
  aib: {
    title: 'AIB analysis',
    short: 'the AIB',
    words: 'The Allied Industrial Base analysis under the chosen scenario: a stress test of how the market could be organized, not a forecast. Dollar values are 2024 trade values.',
  },
};

/** What each lens shows for each kind (the line under the panel heading). */
export const KIND_LENS_WORDS: Readonly<Record<PartnershipKind, Record<LensId, string>>> = {
  trade: {
    today: 'Who supplies whom today: observed 2024 trade.',
    osi: 'Partnership Opportunity: the partners the index rates the most attractive export markets.',
    aib: 'The new and expanded trade relationships that recover supply under the scenario when allies coordinate.',
  },
  investment: {
    today: 'Direct-investment positions (IMF) and clean-tech factory projects as they stand.',
    osi: 'Friendshore: the partners the index rates the most attractive for friendshoring production and investment.',
    aib: 'Where new capacity would help most under the scenario: the countries best placed to add it, their alternatives and the investors they serve.',
  },
  development: {
    today: 'Official development aid by donor and recipient (OECD). Export credits are not in this build yet.',
    osi: 'Development Potential: middle- and lower-income partners where building supply chains together pays off most.',
    aib: 'Where new capacity should go under the scenario: the countries that could add capacity at the supply-chain steps that need it.',
  },
  geopolitical: {
    today: 'UN General Assembly vote alignment, shared economic-security agreements and coalition membership today.',
    osi: "The OSI's allied coalition (core members and partners) and the alignment part of Friendshore.",
    aib: 'The compact coalitions under the scenario: the smallest group of partners that meets the target, critical supply partners and critical dependencies outside.',
  },
};

/** One lens's partners at the focus, for the cross-lens summary. */
export interface LensNames {
  /** ready: `keys` is what the lens names (possibly none); loading; none: the lens has no data at this focus */
  status: 'ready' | 'loading' | 'none';
  /** ISO3 codes, the lens's own order (most important first) */
  keys: readonly string[];
  /** why the lens has no data here (one line; status none) */
  reason?: string | null;
}

export const LENS_LOADING: LensNames = { status: 'loading', keys: [] };
export const lensNone = (reason: string): LensNames => ({ status: 'none', keys: [], reason });
/** A ready lens naming at most `n` partners (duplicates and the focus itself left out). */
export function lensNames(keys: readonly string[], n = 10, focus?: string | null): LensNames {
  const out: string[] = [];
  for (const k of keys) {
    if (!k || k === focus || out.includes(k)) continue;
    out.push(k);
    if (out.length >= n) break;
  }
  return { status: 'ready', keys: out };
}

/** Which lenses name a partner: true / false where the lens is compared, null where it is not. */
export type Membership = Record<LensId, boolean | null>;

export interface ReadingGroup {
  /** "today+osi+aib", "osi", ... */
  id: string;
  members: Membership;
  /** the partners, in the lenses' order */
  keys: string[];
  /** the reading in plain words ("A tie today that both OSI and the AIB keep") */
  words: string;
}

export interface CrossLens {
  /** the lenses with data at the focus (ready) */
  compared: LensId[];
  /** the lenses still loading */
  pending: LensId[];
  /** the lenses with no data at the focus, with their reason */
  absent: { id: LensId; reason: string | null }[];
  /** every partner named by a compared lens, grouped by which lenses name it (fixed order; empty groups left out) */
  groups: ReadingGroup[];
  /** how many partners the compared lenses name */
  total: number;
}

const GROUP_ORDER: readonly (readonly LensId[])[] = [['today', 'osi', 'aib'], ['today', 'osi'], ['today', 'aib'], ['osi', 'aib'], ['today'], ['osi'], ['aib']];

const POSITIVE: Readonly<Record<LensId, string>> = { today: 'a tie today', osi: 'ranked high by OSI', aib: 'chosen by the AIB' };
const NEGATIVE: Readonly<Record<LensId, string>> = { today: 'not a tie today', osi: 'not ranked high by OSI', aib: 'not chosen by the AIB' };

const capFirst = (s: string): string => (s ? s[0].toUpperCase() + s.slice(1) : s);

/**
 * A membership pattern in plain words. Only the compared lenses speak: a lens that is not compared (null) is
 * never said to agree or differ.
 *   today, OSI and AIB      "A tie today that both OSI and the AIB keep"
 *   today and OSI           "A tie today that OSI ranks high, not chosen by the AIB"
 *   today and AIB           "A tie today that the AIB keeps, not ranked high by OSI"
 *   OSI and AIB             "Ranked high by OSI and chosen by the AIB, not a tie today"
 *   one lens                "Ranked high by OSI, not chosen by the AIB and not a tie today"
 */
export function readingWords(m: Membership): string {
  const ids = LENS_IDS.filter((k) => m[k] !== null);
  const pos = ids.filter((k) => m[k] === true);
  const neg = ids.filter((k) => m[k] === false);
  const negWords = neg.length ? `, ${neg.map((k) => NEGATIVE[k]).join(' and ')}` : '';
  if (m.today === true) {
    const others = pos.filter((k) => k !== 'today');
    if (others.length === 2) return 'A tie today that both OSI and the AIB keep';
    if (others.length === 1) return `A tie today that ${others[0] === 'osi' ? 'OSI ranks high' : 'the AIB keeps'}${negWords}`;
    return `A tie today${negWords}`;
  }
  return `${capFirst(pos.map((k) => POSITIVE[k]).join(' and '))}${negWords}`;
}

/**
 * The cross-lens summary: every partner named by a ready lens, grouped by which ready lenses name it (the
 * groups in a fixed order: all three, the pairs, then one lens each). Within a group, partners keep the best
 * position any lens gives them. Loading lenses and lenses with no data at the focus are listed apart.
 */
export function crossLens(lenses: Readonly<Record<LensId, LensNames>>): CrossLens {
  const compared = LENS_IDS.filter((k) => lenses[k].status === 'ready');
  const pending = LENS_IDS.filter((k) => lenses[k].status === 'loading');
  const absent = LENS_IDS.filter((k) => lenses[k].status === 'none').map((id) => ({ id, reason: lenses[id].reason ?? null }));
  const pos = new Map<string, number>();
  for (const k of compared) lenses[k].keys.forEach((x, i) => pos.set(x, Math.min(pos.get(x) ?? Infinity, i)));
  const sets = Object.fromEntries(LENS_IDS.map((k) => [k, new Set(lenses[k].keys)])) as Record<LensId, Set<string>>;
  const groups: ReadingGroup[] = [];
  for (const pattern of GROUP_ORDER) {
    if (!pattern.every((k) => compared.includes(k))) continue;
    const keys = [...pos.keys()]
      .filter((x) => compared.every((k) => sets[k].has(x) === pattern.includes(k)))
      .sort((a, b) => (pos.get(a) ?? 0) - (pos.get(b) ?? 0) || a.localeCompare(b));
    if (!keys.length) continue;
    const members = Object.fromEntries(LENS_IDS.map((k) => [k, compared.includes(k) ? pattern.includes(k) : null])) as Membership;
    groups.push({ id: pattern.join('+'), members, keys, words: readingWords(members) });
  }
  return { compared, pending, absent, groups, total: pos.size };
}

/**
 * Where one partner (the pair's other economy) sits in the cross-lens summary: the reading of the group
 * that names it ("A tie today that both OSI and the AIB keep"), or null when no compared lens names it
 * (each lens names only its leading partners). Undefined when no lens is compared yet.
 */
export function pairReading(cl: CrossLens, key: string): string | null | undefined {
  if (!cl.compared.length) return undefined;
  return cl.groups.find((g) => g.keys.includes(key))?.words ?? null;
}

/** "Today and OSI ranking", "the three lenses": the compared lenses in words. */
export function comparedWords(ids: readonly LensId[]): string {
  if (ids.length === 3) return 'the three lenses';
  return ids.map((k) => LENS_INFO[k].title).join(' and ');
}

/**
 * The summary's first sentence: how many partners the compared lenses name and how many of them more than
 * one lens names ("The three lenses name 14 partners: 3 by all three, 4 by two of them and 7 by one.").
 */
export function summaryLine(cl: CrossLens, noun: { one: string; many: string }): string {
  if (cl.compared.length < 2) return '';
  const by = (n: number) => cl.groups.filter((g) => LENS_IDS.filter((k) => g.members[k] === true).length === n).reduce((s, g) => s + g.keys.length, 0);
  const parts = cl.compared.length === 3 ? [[by(3), 'by all three'], [by(2), 'by two of them'], [by(1), 'by one']] : [[by(2), 'by both'], [by(1), 'by one']];
  const words = (parts as [number, string][]).filter(([n]) => n > 0).map(([n, w]) => `${n} ${w}`);
  const list = words.length > 1 ? `${words.slice(0, -1).join(', ')} and ${words[words.length - 1]}` : (words[0] ?? '');
  return `${capFirst(comparedWords(cl.compared))} name ${cl.total} ${cl.total === 1 ? noun.one : noun.many}${list ? `: ${list}` : ''}.`;
}
