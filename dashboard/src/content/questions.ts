// Start-with-a-question entries for Home and the Sector landing (spec_sector.md 2.1, 2.8, 4.1, 8.8).
// Editorial content: change the wording here, not in the views. Written by the integrator in phase 0;
// owned by builder B from then on.
//
// Each question may carry a live headline fact. scripts/87 (`sector_headlines()`) computes the numbers
// by the fixed rule named in src/sector/contract.ts HEADLINE_RULES and writes them to
// data/sector/manifest.json `headlines`; the wording of the fact is the `fact` template below.
//
// Template syntax: `{key}` or `{key|format}`, where `key` is one of the rule's `values` (numbers) or
// `labels` (strings) in HEADLINE_RULES. Formats:
//   pct    share 0-1 as a whole percent      0.737 -> "74%"
//   pct1   share 0-1, one decimal            0.737 -> "73.7%"
//   int    grouped integer                   1234 -> "1,234"
//   usd    US dollars, scaled                1.006e12 -> "USD 1.01 tn"; 2.027e11 -> "USD 203 bn"
//   usd_bn a value already in USD billions   1875.2 -> "USD 1,875 bn"
//   ej     exajoules, 3 significant figures  6.812 -> "6.81 EJ"
//   cap    a label with its first letter capitalised ("cobalt" -> "Cobalt")
// A fact is shown only when the manifest has a headline with the question's `headline_id`, its
// `template` equals the rule version in HEADLINE_RULES, and every placeholder resolves; otherwise the
// question shows without a fact (no error).

import { HEADLINE_RULES, type HeadlineId, type SectorHeadline } from '../sector/contract.ts';

export interface Question {
  /** stable id (2.8) */
  id: string;
  /** the question, also the card's accessible name */
  question: string;
  /** in-app link; a headline's own href (the item its rule found) takes precedence */
  href: string;
  /** the manifest headline that supplies the fact, or null for none */
  headline_id: HeadlineId | null;
  /** shown on Home (six); every entry is shown on the Sector landing */
  home: boolean;
  /** the fact template (syntax above), or null when the question has no headline */
  fact: string | null;
}

/** Phase-1 set, in display order. ("development", 2.8, joins with the phase-2 sector-dev chunk.) */
export const QUESTIONS: readonly Question[] = [
  {
    id: 'supply',
    question: 'Who controls supply?',
    href: '/sector/minerals/lithium',
    headline_id: 'supply',
    home: true,
    fact: "{geo_name} refines {share|pct} of the world's {item_text}",
  },
  {
    id: 'concentration',
    question: 'Which supply chains are most concentrated?',
    href: '/sector/concentration',
    headline_id: 'concentration',
    home: true,
    fact: 'In {n|int} of {n_total|int} sector measures, a single economy holds half or more',
  },
  {
    id: 'dependence',
    question: 'Who buys from China?',
    // the fallback before the manifest arrives; the headline's own href names the cell its rule found
    // (dependence@1: the largest Components or Assembly trade cell, Electric Vehicles — Assembly in the 2026-10-28 build)
    href: '/sector/electric-vehicles/assembly#trade',
    headline_id: 'dependence',
    home: true,
    fact: 'China supplies {share|pct} of the {total_usd|usd} of {cell_label} exports',
  },
  {
    id: 'investment',
    question: 'Who is investing, and how fast?',
    href: '/sector/explore/investment',
    headline_id: 'investment',
    home: true,
    fact: '{total_bn|usd_bn} of clean-energy investment: China {china|pct}, coalition members {coalition|pct}',
  },
  {
    id: 'reserves',
    question: 'Where are the reserves?',
    href: '/sector/explore/resources?m=resources.mineral_reserves&item=lithium',
    headline_id: 'reserves',
    home: true,
    fact: "{geo_name} holds {share|pct} of the world's known {item_text} reserves",
  },
  {
    id: 'energy',
    question: 'Who produces and consumes energy?',
    href: '/sector/explore/consumption?m=consumption.balance&item=gas',
    headline_id: 'energy',
    home: true,
    fact: 'Largest net gas exporter: {exporter_name}, {exporter_ej|ej}; largest net importer: {importer_name}, {importer_ej|ej}',
  },
  {
    id: 'policy',
    question: 'Where is policy moving?',
    href: '/sector/explore/policy',
    headline_id: 'policy',
    home: false,
    fact: '{n|int} technology-specific policy records are dated 2026-2030',
  },
  {
    id: 'prices',
    question: 'What do the inputs cost?',
    href: '/sector/explore/prices',
    headline_id: 'prices',
    home: false,
    fact: '{item_text|cap} has been the most volatile input over five years ({vol|pct} a year)',
  },
];

export const HOME_QUESTIONS: readonly Question[] = QUESTIONS.filter((q) => q.home);

/** A question's fact, filled from the manifest headlines. */
export interface QuestionFact {
  text: string;
  /** "IEA Critical Minerals Dataset 2026" (muted line) */
  source: string;
  /** "2025", "end-2025" (muted line) */
  year: string;
  /** where the card links: the headline's href */
  href: string;
}

const PLACEHOLDER_RE = /\{([a-z0-9_]+)(?:\|([a-z0-9_]+))?\}/g;
const FORMATS = ['pct', 'pct1', 'int', 'usd', 'usd_bn', 'ej', 'cap'] as const;
type Format = (typeof FORMATS)[number];

const grouped = (x: number, digits: number) => x.toLocaleString('en-US', { minimumFractionDigits: digits, maximumFractionDigits: digits });
const sig3 = (x: number) => Number(x.toPrecision(3)).toLocaleString('en-US', { maximumFractionDigits: 20 });

function fmtUsd(x: number): string {
  const a = Math.abs(x);
  if (a >= 1e12) return `USD ${sig3(x / 1e12)} tn`;
  if (a >= 1e9) return `USD ${sig3(x / 1e9)} bn`;
  if (a >= 1e6) return `USD ${sig3(x / 1e6)} m`;
  return `USD ${grouped(x, 0)}`;
}

function formatNumber(x: number, f: Format | undefined): string | null {
  switch (f) {
    case 'pct':
      return `${grouped(x * 100, 0)}%`;
    case 'pct1':
      return `${grouped(x * 100, 1)}%`;
    case 'int':
      return grouped(x, 0);
    case 'usd':
      return fmtUsd(x);
    case 'usd_bn':
      return `USD ${grouped(x, 0)} bn`;
    case 'ej':
      return `${sig3(x)} EJ`;
    case undefined:
      return sig3(x);
    default:
      return null;
  }
}

/** The text of a template filled from one headline, or null when a placeholder does not resolve. */
export function fillTemplate(template: string, values: Record<string, number>, labels: Record<string, string>): string | null {
  let ok = true;
  const text = template.replace(PLACEHOLDER_RE, (_m, key: string, fmt: string | undefined) => {
    const f = fmt as Format | undefined;
    if (f !== undefined && !(FORMATS as readonly string[]).includes(f)) ok = false;
    else if (Object.prototype.hasOwnProperty.call(values, key) && Number.isFinite(values[key])) {
      const s = f === 'cap' ? null : formatNumber(values[key], f);
      if (s !== null) return s;
      ok = false;
    } else if (Object.prototype.hasOwnProperty.call(labels, key) && typeof labels[key] === 'string' && labels[key] !== '') {
      const s = labels[key];
      if (f === undefined) return s;
      if (f === 'cap') return s.charAt(0).toUpperCase() + s.slice(1);
      ok = false;
    } else ok = false;
    return '';
  });
  return ok ? text : null;
}

/** The placeholder keys of a template, for the contract test against HEADLINE_RULES. */
export function templateKeys(template: string): { key: string; format: string | null }[] {
  return [...template.matchAll(PLACEHOLDER_RE)].map((m) => ({ key: m[1], format: m[2] ?? null }));
}

/** The fact of a question from manifest.headlines, or null (no headline, another rule version, unresolved). */
export function questionFact(q: Question, headlines: readonly SectorHeadline[] | null | undefined): QuestionFact | null {
  if (!q.headline_id || !q.fact || !headlines) return null;
  const h = headlines.find((x) => x.id === q.headline_id);
  if (!h || h.template !== HEADLINE_RULES[q.headline_id].rule) return null;
  const text = fillTemplate(q.fact, h.values ?? {}, h.labels ?? {});
  if (text === null) return null;
  return { text, source: h.source, year: h.year, href: h.href.startsWith('/sector') ? h.href : q.href };
}
