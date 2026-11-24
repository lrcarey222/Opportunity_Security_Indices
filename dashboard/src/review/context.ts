// Page context and submission envelopes (docs/QC_SYSTEM_DESIGN.md 4.4). capturePageContext reads the
// router location (not window.location, so it is right in the artifact build's memory router), the route
// params and the build stamps, and names what the reviewer was looking at: country, technology, stage,
// node, layer, pair, compact, scenario. newSubmission wraps a payload in the envelope every submission
// shares, with a unique submission_id and the reviewer stamp (a digest of the profile without the name,
// and the expertise tags the admin weights by). Pure when the caller passes the clock (captured_at / now)
// and the random source (uuid); otherwise the current time and crypto.randomUUID are used.

import {
  SUBMISSION_SCHEMA,
  SUBMISSION_SCHEMA_VERSION,
  type PageContext,
  type PayloadByKind,
  type ReviewerProfile,
  type ReviewerStamp,
  type Submission,
  type SubmissionKind,
} from './model.ts';
import type { BuildInfo } from '../shell/buildInfo.ts';
import { parseCellSlug, supplyStageOfSlug } from '../lib/stages.ts';
import { canonicalJson, sha256HexSync } from './privacy.ts';
import { normaliseProfile, normaliseSectorTag, normaliseStageTag } from './tags.ts';

export interface LocationLike {
  pathname: string;
  search?: string;
  hash?: string;
}

export interface ContextExtra {
  /** ISO time of the capture (the caller's clock; default now). */
  captured_at?: string;
  app_commit?: string;
  /** The data-release ID (release.ts dataReleaseId of the page's stamps), when computed. */
  data_release?: string | null;
  /** What the page shows for the thing reported ("Secure · High fence", "BUILD", ...). */
  displayed_result?: string | null;
  metric_ids?: string[];
  view?: string | null;
}

const decode = (s: string): string => {
  try {
    return decodeURIComponent(s);
  } catch {
    return s;
  }
};
const upperIso3 = (s: string | undefined | null): string | null => (s && /^[A-Za-z]{3}$/.test(s) ? s.toUpperCase() : null);
const stageName = (s: string | null | undefined): string | null => (s ? supplyStageOfSlug(s.toLowerCase()) ?? normaliseStageTag(s) ?? s : null);

interface RouteMatch {
  route: string;
  view: string | null;
  country?: string | null;
  tech?: string | null;
  stage?: string | null;
  pair?: string | null;
  sid?: string | null;
}

/**
 * The route pattern and its params of an app pathname, as main's routes define them (routes.tsx and the
 * module routes): Sectors (/sector, /sector/:tech, /sector/:tech/:stage[/:iso3], explore / concentration /
 * minerals / sources), Countries (/countries, /countries/:iso3[/memo]), the Allied Industrial Base (/aib,
 * /aib/partnerships|roles|diversification, /compacts, /compacts/workstreams|about, /compacts/s/:sid,
 * /compacts/:id), About and the legacy views. /partnerships/:pair and /compacts/brief/:sid are not routes on
 * main; they are parsed anyway so a packet from a build that has them keeps its pair / compact.
 */
export function matchRoute(pathname: string): RouteMatch {
  const segs = pathname.replace(/^\/+|\/+$/g, '').split('/').filter(Boolean).map(decode);
  const [a, b, c, d] = segs;
  if (!a) return { route: '/', view: 'home' };
  switch (a) {
    case 'countries':
      if (!b) return { route: '/countries', view: 'countries' };
      if (b === 'about') return { route: '/countries/about', view: 'countries-about' };
      return { route: c ? `/countries/:iso3/${c}` : '/countries/:iso3', view: c ? `country-${c}` : 'country', country: upperIso3(b) };
    case 'sector': {
      const reserved = ['explore', 'concentration', 'minerals', 'sources', 'country', 'data'];
      if (!b) return { route: '/sector', view: 'sector' };
      if (reserved.includes(b)) return { route: c ? `/sector/${b}/:param` : `/sector/${b}`, view: `sector-${b}`, country: b === 'country' ? upperIso3(c) : null };
      if (d) return { route: '/sector/:tech/:stage/:iso3', view: 'sector-country', tech: b, stage: stageName(c), country: upperIso3(d) };
      if (c) return { route: '/sector/:tech/:stage', view: 'sector-profile', tech: b, stage: stageName(c) };
      return { route: '/sector/:tech', view: 'sector-chain', tech: b };
    }
    case 'partnerships':
      return b ? { route: '/partnerships/:pair', view: 'partnership', pair: b.toUpperCase() } : { route: '/partnerships', view: 'partnerships' };
    case 'compacts':
      if (b === 'brief' && c) return { route: '/compacts/brief/:sid', view: 'compact-brief', sid: c };
      if (b === 's' && c) return { route: '/compacts/s/:sid', view: 'compact-strategic', sid: c };
      if (!b) return { route: '/compacts', view: 'compacts' };
      if (['workstreams', 'about', 's'].includes(b)) return { route: `/compacts/${b}`, view: `compacts-${b}` };
      return { route: '/compacts/:id', view: 'compact', sid: b };
    case 'aib':
      return { route: b ? `/aib/${b}` : '/aib', view: b ? `aib-${b}` : 'aib' };
    case 'review':
      return { route: b ? `/review/${b}` : '/review', view: b ? `review-${b}` : 'review' };
    case 'chart-of-the-day':
      return { route: b ? '/chart-of-the-day/:date' : '/chart-of-the-day', view: 'chart-of-the-day' };
    default:
      return { route: `/${a}${b ? '/*' : ''}`, view: a, country: ['country', 'strategic', 'partners'].includes(a) ? upperIso3(b) : null };
  }
}

/** Routes whose pages show an AIB scenario: absent `scn` means Managed Interdependence (aib/AibLayout.tsx). */
const AIB_ROUTE = /^\/(aib|compacts)(\/|$)/;

/**
 * The page context of a submission. `params` are the router params (they win over the pathname parse when
 * given). The technology is written as its sector tag when it is one. Query keys read: node and pnode
 * (Tech|Stage: the Country profile's cell drawer and the Sector card), cell (tech-stage slug), layer,
 * country / c, tech, stage, scn / scenario. On the AIB pages an absent scn is recorded as 'managed', the
 * scenario those pages show by default.
 */
export function capturePageContext(loc: LocationLike, params: Readonly<Record<string, string | undefined>>, info: BuildInfo | null, extra: ContextExtra = {}): PageContext {
  const search = loc.search ? (loc.search.startsWith('?') ? loc.search : `?${loc.search}`) : '';
  const hash = loc.hash ? (loc.hash.startsWith('#') ? loc.hash : `#${loc.hash}`) : '';
  const q = new URLSearchParams(search);
  const m = matchRoute(loc.pathname);

  const country = upperIso3(params.iso3) ?? m.country ?? upperIso3(q.get('country')) ?? upperIso3(q.get('c')) ?? null;
  let tech = params.tech ? decode(params.tech) : (m.tech ?? q.get('tech') ?? null);
  let stage = stageName(params.stage ?? null) ?? m.stage ?? stageName(q.get('stage')) ?? null;
  const pair = params.pair ? params.pair.toUpperCase() : (m.pair ?? null);
  const sid = params.sid ?? m.sid ?? null;

  // ?node=Tech|Stage (the Country profile's cell drawer) or ?pnode=Tech|Stage (the Sector posture card)
  const nodeRaw = q.get('node') ?? q.get('pnode');
  let node: string | null = nodeRaw && nodeRaw.trim() ? nodeRaw.trim() : null;
  if (node && node.includes('|')) {
    const [t, s] = node.split('|');
    tech ??= t || null;
    stage ??= stageName(s);
  }
  // ?cell=<tech>-<stage> (the Country profile's selected cell)
  const cell = q.get('cell');
  const parsed = cell ? parseCellSlug(cell.toLowerCase()) : null;
  if (parsed && !parsed.legacy) {
    tech ??= parsed.techSlug;
    stage ??= parsed.stage;
    node ??= cell;
  }

  const scn = q.get('scn') ?? q.get('scenario');
  const scenario = scn && scn.trim() ? scn.trim() : AIB_ROUTE.test(m.route) ? 'managed' : null;

  const stamps: PageContext['bundle_stamps'] = {
    posture: info?.posture ? { created: info.posture.created, git_commit: info.posture.git_commit } : null,
    country: info?.country ? { created: info.country.created, git_commit: info.country.git_commit } : null,
    sector: info?.sector ? { created: info.sector.created, git_commit: info.sector.git_commit, bundle_id: info.sector.bundle_id } : null,
    aib: info?.aib ? { created: info.aib.created, git_commit: info.aib.git_commit ?? null } : null,
    strategic: info?.strategic ? { created: info.strategic.created, git_commit: info.strategic.git_commit, bundle_id: info.strategic.bundle_id } : null,
    memos: info?.memos ? { created: null, git_commit: null, bundle_id: info.memos.manifest_sha256 } : null,
  };

  return {
    url: `${loc.pathname}${search}${hash}`,
    route: m.route,
    view: extra.view ?? m.view,
    country,
    // the technology as its sector tag when it is one (route slugs and ?node= display names agree)
    tech: tech ? (normaliseSectorTag(tech) ?? tech) : null,
    stage,
    node,
    layer: q.get('layer') || null,
    pair,
    sid,
    scenario,
    displayed_result: extra.displayed_result ?? null,
    metric_ids: [...(extra.metric_ids ?? [])],
    data_release: extra.data_release ?? null,
    bundle_stamps: stamps,
    app_commit: extra.app_commit ?? 'unknown',
    captured_at: extra.captured_at ?? new Date().toISOString(),
  };
}

/**
 * sha256 of the canonical profile JSON without the name (and with tags normalised), so exports can join on
 * expertise. The UX v2 review mode is a working preference, not expertise: it is left out so switching it does
 * not change the digest (and a v1 profile's digest is unchanged).
 */
export function profileDigest(p: ReviewerProfile): string {
  const { name: _name, review_mode: _mode, ...rest } = normaliseProfile(p);
  void _name;
  void _mode;
  return sha256HexSync(canonicalJson(rest));
}

/** The reviewer stamp a submission carries. An anonymous reviewer's name never travels. */
export function reviewerStamp(p: ReviewerProfile): ReviewerStamp {
  const n = normaliseProfile(p);
  return {
    reviewer_id: n.reviewer_id,
    name: n.anonymous ? null : typeof n.name === 'string' && n.name.trim() ? n.name.trim() : null,
    anonymous: n.anonymous,
    profile_digest: profileDigest(p),
    expertise: { sectors: [...n.sectors], countries: [...n.countries], stages: [...n.stages], methods: [...n.methods], confidence: { ...n.confidence } },
  };
}

export interface NewSubmissionOptions {
  profile: ReviewerProfile;
  campaign_id: string | null;
  page_context?: PageContext | null;
  release: { data_release: string | null; app_commit: string };
  /** ISO time of creation (the caller's clock; default now). */
  now?: string;
  /** Whether it is being submitted now (submitted_at = now) or saved as a draft (null). Default true. */
  submitted?: boolean;
  /** A random UUID source; default crypto.randomUUID. */
  uuid?: () => string;
}

function defaultUuid(): string {
  const c = (globalThis as { crypto?: { randomUUID?: () => string } }).crypto;
  if (c?.randomUUID) return c.randomUUID();
  throw new Error('no random UUID source: pass opts.uuid');
}

/** A submission envelope around a payload. The submission_id ('SUB-<uuid>') never changes once made. */
export function newSubmission<K extends SubmissionKind>(kind: K, payload: PayloadByKind[K], opts: NewSubmissionOptions): Submission<K> {
  const id = (opts.uuid ?? defaultUuid)();
  const now = opts.now ?? new Date().toISOString();
  return {
    submission_id: `SUB-${id}`,
    schema: SUBMISSION_SCHEMA,
    schema_version: SUBMISSION_SCHEMA_VERSION,
    campaign_id: opts.campaign_id,
    kind,
    reviewer: reviewerStamp(opts.profile),
    created_at: now,
    submitted_at: opts.submitted === false ? null : now,
    app_commit: opts.release.app_commit,
    data_release: opts.release.data_release,
    page_context: opts.page_context ?? null,
    payload,
  };
}
