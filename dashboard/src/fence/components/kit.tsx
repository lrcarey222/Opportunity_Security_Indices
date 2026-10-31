// Draw the Fence: small shared pieces. Yard identity (glyph + name + fence, never colour alone),
// provenance badges (DATA / ASSESSMENT / YOUR JUDGMENT / SCENARIO / UNKNOWN), and EvidenceMetric.
// The yards' verbs and definitions follow the Explorer's shared vocabulary (content/vocabulary.ts, the NEIS copy
// refresh of 2026-10-30): Small yard "Secure it", Medium yard "Build or preserve it", Large yard "Keep it open"
// (the Large yard was "Deploy it": deployment priority is a Medium-yard choice in the Explorer's mapping).

import { useId, useState, type ReactNode } from 'react';
import type { Confidence, Provenance, YardOutcome } from '../engine/index.ts';
import type { YFEvidenceRecord } from '../data/contract.ts';
import { YARD_DEFINITION } from '../../content/vocabulary.ts';

export const YARD_COPY: Record<YardOutcome, { name: string; fence: string; verb: string; definition: string; question: string; objective: string }> = {
  small: {
    name: 'Small Yard',
    fence: 'High fence',
    verb: 'Secure it',
    definition: YARD_DEFINITION.small,
    question: 'Could foreign control of this system or input create acute harm that markets cannot remedy in time?',
    objective: 'Identify the specific vulnerability and keep working until it is isolated, replaced, diversified, secured, stockpiled or otherwise made resilient.',
  },
  medium: {
    name: 'Medium Yard',
    fence: 'Medium, permeable fence',
    verb: 'Build or preserve it',
    definition: YARD_DEFINITION.medium,
    question: 'Is this a capability the country has a credible reason and pathway to compete in or preserve for future innovation?',
    objective: 'Build industrial capability, learning and competitiveness without needlessly cutting off useful foreign technology or expertise.',
  },
  large: {
    name: 'Large Yard',
    fence: 'Low fence',
    verb: 'Keep it open',
    definition: YARD_DEFINITION.large,
    question: 'Would insisting on domestic production cost more than the strategic benefit it creates?',
    objective: 'Keep access affordable and diversified so deployment can proceed, and manage genuine concentration risks with the lightest effective intervention.',
  },
  mixed: { name: 'Mixed', fence: 'Different fences for different layers', verb: '', definition: '', question: '', objective: '' },
  indeterminate: { name: 'Indeterminate', fence: 'Not enough evidence to draw a fence', verb: '', definition: '', question: '', objective: '' },
};

/** A yard drawn as a plot and a fence: small plot / tall fence; medium plot / gated fence; large plot / low fence. */
export function YardGlyph({ yard, className = 'h-8 w-12', title }: { yard: YardOutcome; className?: string; title?: string }) {
  const c = `var(--yard-${yard === 'small' || yard === 'medium' || yard === 'large' ? yard : 'small'})`;
  if (yard === 'mixed')
    return (
      <svg viewBox="0 0 48 32" className={className} role={title ? 'img' : undefined} aria-hidden={title ? undefined : true}>
        {title && <title>{title}</title>}
        <rect x="2" y="20" width="12" height="10" fill="none" stroke="var(--yard-small)" strokeWidth="2" />
        <path d="M2 20V6M8 20V6M14 20V6" stroke="var(--yard-small)" strokeWidth="2" />
        <rect x="17" y="18" width="14" height="12" fill="none" stroke="var(--yard-medium)" strokeWidth="2" />
        <path d="M17 18V11M24 18V11M31 18V11" stroke="var(--yard-medium)" strokeWidth="2" />
        <rect x="34" y="16" width="12" height="14" fill="none" stroke="var(--yard-large)" strokeWidth="2" />
        <path d="M34 16V13M40 16V13M46 16V13" stroke="var(--yard-large)" strokeWidth="2" />
      </svg>
    );
  if (yard === 'indeterminate')
    return (
      <svg viewBox="0 0 48 32" className={className} role={title ? 'img' : undefined} aria-hidden={title ? undefined : true}>
        {title && <title>{title}</title>}
        <rect x="4" y="14" width="40" height="16" fill="none" stroke="var(--color-paper-mute)" strokeWidth="2" strokeDasharray="3 3" />
        <text x="24" y="27" textAnchor="middle" fontSize="12" fill="var(--color-paper-mute)">?</text>
      </svg>
    );
  const spec = {
    small: { x: 16, w: 16, h: 9, post: 22 },
    medium: { x: 9, w: 30, h: 12, post: 11 },
    large: { x: 2, w: 44, h: 15, post: 4 },
  }[yard];
  const top = 30 - spec.h;
  const posts = yard === 'small' ? [0, 0.5, 1] : yard === 'medium' ? [0, 0.33, 0.66, 1] : [0, 0.25, 0.5, 0.75, 1];
  return (
    <svg viewBox="0 0 48 32" className={className} role={title ? 'img' : undefined} aria-hidden={title ? undefined : true}>
      {title && <title>{title}</title>}
      <rect x={spec.x} y={top} width={spec.w} height={spec.h} fill="none" stroke={c} strokeWidth="2" />
      {posts.map((f, i) => (
        <path key={i} d={`M${spec.x + f * spec.w} ${top}V${top - spec.post}`} stroke={c} strokeWidth="2" />
      ))}
      {yard === 'small' && <path d={`M${spec.x} ${top - spec.post + 3}H${spec.x + spec.w}`} stroke={c} strokeWidth="2" />}
      {yard === 'medium' && <path d={`M${spec.x} ${top - spec.post + 3}H${spec.x + spec.w * 0.4}M${spec.x + spec.w * 0.6} ${top - spec.post + 3}H${spec.x + spec.w}`} stroke={c} strokeWidth="2" />}
    </svg>
  );
}

export function yardClass(y: YardOutcome): string {
  return `yf-yard-${y}`;
}

export function YardBadge({ yard, size = 'md', sub }: { yard: YardOutcome; size?: 'sm' | 'md' | 'lg'; sub?: ReactNode }) {
  const c = YARD_COPY[yard];
  const big = size === 'lg';
  return (
    <span className={`${yardClass(yard)} inline-flex items-center gap-2.5`}>
      <YardGlyph yard={yard} className={big ? 'h-12 w-[4.5rem]' : size === 'sm' ? 'h-5 w-8' : 'h-8 w-12'} />
      <span className="flex flex-col">
        <span className={`yf-yard-text font-semibold uppercase tracking-[0.12em] ${big ? 'text-2xl' : size === 'sm' ? 'text-[0.6875rem]' : 'text-sm'}`}>{c.name}</span>
        {size !== 'sm' && <span className="text-xs text-paper-mute">{sub ?? c.fence}</span>}
      </span>
    </span>
  );
}

const PROV_TEXT: Record<Provenance, [string, string, string]> = {
  data: ['Data', '■', 'Measured: from the Opportunity & Security Indices or another quantitative source'],
  assessment: ['Assessment', '◆', 'A curated NEIS Yards & Fences assumption, with its source and confidence'],
  judgment: ['Your judgment', '●', 'An answer you supplied'],
  scenario: ['Scenario', '◇', 'An assumption you set in the scenario lab, not observed data'],
  unknown: ['Unknown', '?', 'Evidence is insufficient: the engine brackets this input instead of guessing'],
};

export function ProvBadge({ p, className = '' }: { p: Provenance; className?: string }) {
  const [label, glyph, title] = PROV_TEXT[p];
  return (
    <span className={`yf-prov yf-prov-${p} ${className}`} title={title}>
      <span aria-hidden="true">{glyph}</span>
      {label}
    </span>
  );
}

export function ConfidencePill({ c }: { c: Confidence }) {
  const bars = c === 'high' ? 3 : c === 'moderate' ? 2 : 1;
  return (
    <span className="inline-flex items-center gap-2" title="Classification confidence">
      <span className="flex gap-0.5" aria-hidden="true">
        {[1, 2, 3].map((i) => (
          <span key={i} className={`block h-3 w-1.5 ${i <= bars ? 'bg-paper' : 'bg-ink-600'}`} />
        ))}
      </span>
      <span className="text-sm capitalize text-paper">{c}</span>
    </span>
  );
}

export function Completeness({ value }: { value: number }) {
  const pct = Math.round(value * 100);
  return (
    <span className="inline-flex items-center gap-2" title="Evidence completeness: share of relevant inputs that are known">
      <span className="yf-meter block w-20" aria-hidden="true">
        <span style={{ width: `${pct}%` }} />
      </span>
      <span className="tabular text-sm text-paper">{pct}%</span>
    </span>
  );
}

export function SectionHead({ eyebrow, title, id, children }: { eyebrow?: string; title: ReactNode; id?: string; children?: ReactNode }) {
  return (
    <div className="mb-4">
      {eyebrow && <div className="eyebrow mb-2">{eyebrow}</div>}
      <h2 id={id} className="rule-neis-sm text-xl font-medium text-paper">
        {title}
      </h2>
      {children && <div className="mt-2 max-w-3xl text-sm leading-relaxed text-paper-dim">{children}</div>}
    </div>
  );
}

// ── Evidence ─────────────────────────────────────────────────────────────────────────────────────

export function fmtRecordValue(r: YFEvidenceRecord): string {
  const v = r.normalized ? r.normalizedValue : r.rawValue;
  if (v === null || v === undefined || v === '') return 'n/a';
  if (typeof v === 'string') return v;
  switch (r.unit) {
    case 'share':
      return `${(v * 100).toFixed(v < 0.1 && v > 0 ? 1 : 0)}%`;
    case 'hhi':
      return `HHI ${Math.round(v * 10000).toLocaleString('en-US')}`;
    case 'usd': {
      const a = Math.abs(v);
      const s = v < 0 ? '−' : '';
      return a >= 1e9 ? `${s}$${(a / 1e9).toFixed(1)} bn` : `${s}$${(a / 1e6).toFixed(0)} m`;
    }
    case 'usd_bn':
      return `$${v.toFixed(1)} bn`;
    case 'rca':
      return `RCA ${v.toFixed(2)}`;
    case 'index01':
      return v.toFixed(2);
    case 'pct_change':
      return `${v >= 0 ? '+' : '−'}${Math.abs(v * 100).toFixed(0)}%`;
    case 'pct_pa':
      return `${v >= 0 ? '+' : '−'}${Math.abs(v * 100).toFixed(1)}% a year`;
    case 'trl':
      return `TRL ${v.toFixed(1)}`;
    case 'trl_points':
      return `${v >= 0 ? '+' : '−'}${Math.abs(v).toFixed(1)} TRL`;
  }
}

const ord = (n: number) => {
  const s = ['th', 'st', 'nd', 'rd'];
  const v = n % 100;
  return n + (s[(v - 20) % 10] || s[v] || s[0]);
};

/** One measured fact: raw value first, its relative standing second, then year, source and meaning. */
export function EvidenceMetric({ r, scopeNote, compact }: { r: YFEvidenceRecord; scopeNote?: string; compact?: boolean }) {
  const [open, setOpen] = useState(false);
  const id = useId();
  const pct = typeof r.percentile === 'number' ? r.percentile : null;
  const fill = r.unit === 'share' && typeof r.rawValue === 'number' ? r.rawValue : pct !== null ? pct / 100 : null;
  return (
    <div className="border-b hairline py-3 last:border-b-0">
      <div className="flex flex-wrap items-baseline justify-between gap-x-3 gap-y-1">
        <div className="min-w-0 text-sm text-paper-dim">{r.label}</div>
        <ProvBadge p="data" />
      </div>
      <div className="mt-1 flex flex-wrap items-baseline gap-x-3 gap-y-1">
        <span className="tabular text-lg font-medium text-paper">
          {fmtRecordValue(r)}
          {r.subject && r.unit === 'share' && !r.label.includes(r.subject) && <span className="ml-1.5 text-sm font-normal text-paper-dim">{r.subject}</span>}
        </span>
        {pct !== null && (
          <span className="text-sm text-paper-mute">
            {ord(Math.round(pct))} percentile{typeof r.rank === 'number' && typeof r.nEconomies === 'number' ? ` · rank ${r.rank} of ${r.nEconomies}` : ''}
          </span>
        )}
        {r.normalized && <span className="tag">normalised, relative</span>}
      </div>
      {fill !== null && (
        <div className="yf-meter mt-2" aria-hidden="true">
          <span className="yf-grow" style={{ width: `${Math.max(0, Math.min(1, fill)) * 100}%` }} />
        </div>
      )}
      <div className="mt-1.5 flex flex-wrap items-center gap-x-3 text-xs text-paper-mute">
        <span>{String(r.year ?? '')}</span>
        <span>{r.source}</span>
        {scopeNote && <span className="tag">{scopeNote}</span>}
        {!compact && (
          <button type="button" className="link text-xs" aria-expanded={open} aria-controls={id} onClick={() => setOpen(!open)}>
            {open ? 'Hide meaning' : 'What it means'}
          </button>
        )}
      </div>
      {(open || compact) && r.explanation && (
        <p id={id} className="mt-1.5 text-sm leading-relaxed text-paper-dim">
          {r.explanation}
          {r.caveats && r.caveats.length > 0 && !compact && <span className="mt-1 block text-xs text-paper-mute">Caveat: {r.caveats.join(' ')}</span>}
        </p>
      )}
    </div>
  );
}

export function Pill({ children, tone = 'mute' }: { children: ReactNode; tone?: 'mute' | 'signal' }) {
  return <span className={`tag ${tone === 'signal' ? 'tag-framework' : ''}`}>{children}</span>;
}
