// The shipped bundles as the QC layer reads them (docs/QC_SYSTEM_DESIGN.md 3, 6.1). RawBundles is what a loader
// read from public/data (parsed JSON, never decoded); decodeBundles() runs the app's own decoders over it
// (posture decodeCore / checkPostureDetail, AIB checkCompactsDoc) and keeps every decode problem instead of
// throwing, so a broken bundle is reported by the QC rows rather than hiding the rest. The release stamps
// (review/release.ts) are read from the manifests here, once. Pure: no IO (the Node loaders are in qc/node/).

import { checkPostureDetail, decodeCore, type PostureCore, type PostureDetail } from '../../posture/contract.ts';
import { checkCompactsDoc, type AibBaselineScenarioDoc, type AibDoc, type AibManagedScenarioDoc } from '../../aib/contract.ts';
import type { MemoDoc } from '../../country/contract.ts';
import type { BundleFileStamp } from '../model.ts';
import type { ReleaseStamps } from '../release.ts';

/** The partnership memo manifest (data/partnerships/manifest.json; main has no typed reader of it, so it is declared here). */
export interface MemoEntryLite {
  pair?: string;
  compact?: string;
  country_a?: string;
  country_b?: string;
  members?: string[];
  title?: string;
  json: string;
  generated_at?: string;
  as_of?: string;
  evidence_access?: string;
  verified?: boolean;
  format?: string;
}
export interface MemoManifestLite {
  schema: string;
  schema_version: number;
  pairs: Record<string, MemoEntryLite>;
  compacts: Record<string, MemoEntryLite>;
}

/** Parsed JSON documents as read from public/data (each optional: a missing bundle is a QC finding, not a crash). */
export interface RawBundles {
  posture?: { manifest: unknown; core: unknown; details: Record<string, unknown> } | null;
  aib?: { manifest: unknown; doc: unknown; managed?: unknown; baseline?: unknown } | null;
  /** memos keyed by pair (USA_JPN) or compact id (SC-SOLAR); manifestSha256 = sha256 of the manifest text, LF-normalised (release.ts memosManifestSha256; the release key memos part) */
  partnerships?: { manifest: unknown; manifestSha256?: string | null; memos: Record<string, unknown> } | null;
  country?: { manifest: unknown } | null;
  sector?: { manifest: unknown } | null;
  /** data/manifest.json: the Strategic Insights engine bundle */
  strategic?: { manifest: unknown } | null;
  /** every bundle file the loader read, with its hash and stamps */
  files?: BundleFileStamp[];
}

export interface DecodedPosture {
  manifest: Record<string, unknown> | null;
  core: PostureCore;
  /** ISO3 -> the decoded detail document */
  details: Record<string, PostureDetail>;
}
export interface DecodedAib {
  manifest: Record<string, unknown> | null;
  doc: AibDoc;
  managed: AibManagedScenarioDoc | null;
  baseline: AibBaselineScenarioDoc | null;
  /** checkCompactsDoc problems (the document is still read: QC reports them) */
  problems: string[];
}
export interface DecodedPartnerships {
  manifest: MemoManifestLite;
  manifestSha256: string | null;
  /** pair or compact id -> memo document */
  memos: Record<string, MemoDoc>;
}

export interface DecodeProblem {
  bundle: 'posture' | 'aib' | 'partnerships' | 'country' | 'sector' | 'strategic';
  problem: string;
}

export interface DecodedBundles {
  posture: DecodedPosture | null;
  aib: DecodedAib | null;
  partnerships: DecodedPartnerships | null;
  /** the manifests of the bundles the release key reads but QC does not decode */
  manifests: { country: Record<string, unknown> | null; sector: Record<string, unknown> | null; strategic: Record<string, unknown> | null };
  stamps: ReleaseStamps;
  files: BundleFileStamp[];
  problems: DecodeProblem[];
}

export const isObj = (v: unknown): v is Record<string, unknown> => typeof v === 'object' && v !== null && !Array.isArray(v);
export const strOrNull = (v: unknown): string | null => (typeof v === 'string' && v !== '' ? v : null);
export const numOrNull = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);
const errText = (e: unknown): string => (e instanceof Error ? ((e as { problems?: string[] }).problems?.join('; ') ?? e.message) : String(e));

/** The release stamps (review/release.ts) from the raw manifests. */
export function releaseStampsOf(raw: RawBundles): ReleaseStamps {
  const pm = isObj(raw.posture?.manifest) ? raw.posture.manifest : null;
  const cm = isObj(raw.country?.manifest) ? raw.country.manifest : null;
  const sm = isObj(raw.sector?.manifest) ? raw.sector.manifest : null;
  const am = isObj(raw.aib?.manifest) ? raw.aib.manifest : null;
  const em = isObj(raw.strategic?.manifest) ? raw.strategic.manifest : null;
  return {
    posture: pm ? { created: strOrNull(pm.created), git_commit: strOrNull(pm.git_commit) } : null,
    country: cm ? { created: strOrNull(cm.created), git_commit: strOrNull(cm.git_commit) } : null,
    sector: sm ? { bundle_id: strOrNull(sm.bundle_id) } : null,
    aib: am ? { created: strOrNull(am.created) } : null,
    strategic: em ? { bundle_id: strOrNull(em.bundle_id) } : null,
    memos: raw.partnerships ? { manifest_sha256: raw.partnerships.manifestSha256 ?? null } : null,
  };
}

function decodePosture(raw: NonNullable<RawBundles['posture']>, problems: DecodeProblem[]): DecodedPosture | null {
  let core: PostureCore;
  try {
    core = decodeCore(raw.core);
  } catch (e) {
    problems.push({ bundle: 'posture', problem: `core does not decode: ${errText(e)}` });
    return null;
  }
  const details: Record<string, PostureDetail> = {};
  for (const iso of Object.keys(raw.details).sort()) {
    try {
      details[iso] = checkPostureDetail(raw.details[iso]);
    } catch (e) {
      problems.push({ bundle: 'posture', problem: `detail ${iso} does not decode: ${errText(e)}` });
    }
  }
  return { manifest: isObj(raw.manifest) ? raw.manifest : null, core, details };
}

function decodeAib(raw: NonNullable<RawBundles['aib']>, problems: DecodeProblem[]): DecodedAib | null {
  if (!isObj(raw.doc)) {
    problems.push({ bundle: 'aib', problem: 'the compacts document is missing or not an object' });
    return null;
  }
  const docProblems = checkCompactsDoc(raw.doc);
  for (const p of docProblems.slice(0, 20)) problems.push({ bundle: 'aib', problem: p });
  const doc = raw.doc as unknown as AibDoc;
  if (!Array.isArray(doc.nodes)) {
    problems.push({ bundle: 'aib', problem: 'the compacts document has no nodes' });
    return null;
  }
  return {
    manifest: isObj(raw.manifest) ? raw.manifest : null,
    doc,
    managed: isObj(raw.managed) ? (raw.managed as unknown as AibManagedScenarioDoc) : null,
    baseline: isObj(raw.baseline) ? (raw.baseline as unknown as AibBaselineScenarioDoc) : null,
    problems: docProblems,
  };
}

function decodePartnerships(raw: NonNullable<RawBundles['partnerships']>, problems: DecodeProblem[]): DecodedPartnerships | null {
  const m = raw.manifest;
  if (!isObj(m) || m.schema !== 'osi-partnership-memos' || !isObj(m.pairs)) {
    problems.push({ bundle: 'partnerships', problem: 'manifest.json is not an osi-partnership-memos manifest' });
    return null;
  }
  const manifest: MemoManifestLite = {
    schema: String(m.schema),
    schema_version: numOrNull(m.schema_version) ?? 1,
    pairs: m.pairs as Record<string, MemoEntryLite>,
    compacts: isObj(m.compacts) ? (m.compacts as Record<string, MemoEntryLite>) : {},
  };
  const memos: Record<string, MemoDoc> = {};
  for (const [id, d] of Object.entries(raw.memos)) {
    if (!isObj(d) || !Array.isArray(d.sections)) {
      problems.push({ bundle: 'partnerships', problem: `memo ${id} is not a memo document` });
      continue;
    }
    memos[id] = d as unknown as MemoDoc;
  }
  return { manifest, manifestSha256: raw.manifestSha256 ?? null, memos };
}

/** Decode every bundle present, recording (never throwing) the problems. */
export function decodeBundles(raw: RawBundles): DecodedBundles {
  const problems: DecodeProblem[] = [];
  const posture = raw.posture ? decodePosture(raw.posture, problems) : null;
  const aib = raw.aib ? decodeAib(raw.aib, problems) : null;
  const partnerships = raw.partnerships ? decodePartnerships(raw.partnerships, problems) : null;
  return {
    posture,
    aib,
    partnerships,
    manifests: {
      country: isObj(raw.country?.manifest) ? raw.country.manifest : null,
      sector: isObj(raw.sector?.manifest) ? raw.sector.manifest : null,
      strategic: isObj(raw.strategic?.manifest) ? raw.strategic.manifest : null,
    },
    stamps: releaseStampsOf(raw),
    files: raw.files ?? [],
    problems,
  };
}

/** "ISO|Tech|Stage", the QC layer's cell key. */
export const cellKey = (iso3: string, tech: string, stage: string): string => `${iso3}|${tech}|${stage}`;
