// The cell drawer of the Country headline chart: what the decision engine says about one node. Partner:
// the recommended trusted partners and why (or the partner gap); Secure: the rationale and the fence;
// Deploy: why restraint is the recommendation; every posture: the quadrant axes, policy alignment, the
// policy design and the trade evidence with its periods. A side panel on wide screens, a bottom sheet on
// phones; Escape or the close button dismisses it.

import { useEffect, useRef } from 'react';
import type { DetailCell, PostureDetail } from './contract.ts';
import { pct } from './derive.ts';
import { ALIGNMENT_LABEL, MODE_LABEL, MOMENTUM_ARROW, MOMENTUM_UNAVAILABLE, POSTURE_COLOR, POSTURE_FROM_WORD, POSTURE_LABEL, QUADRANT_LABEL, RATIONALE_LABEL, RATIONALE_WORDS } from './grammar.ts';
import type { Alignment, PostureCore, SecurityRationale } from './contract.ts';
import { postureMomentum } from './momentum.ts';

function Row({ k, v }: { k: string; v: React.ReactNode }) {
  return (
    <div className="flex justify-between gap-3 py-1 text-sm">
      <dt className="text-paper-mute">{k}</dt>
      <dd className="text-right tabular text-paper-dim">{v}</dd>
    </div>
  );
}

const usd = (v: number | undefined) => (v === undefined ? '—' : v >= 1e9 ? `$${(v / 1e9).toFixed(1)}bn` : v >= 1e6 ? `$${(v / 1e6).toFixed(0)}m` : `$${Math.round(v / 1e3)}k`);

function restraint(c: DetailCell): string {
  if (c.quadrant === 'watch') return 'Neither Strategic Need nor Industrial Opportunity is high here: open, affordable deployment serves the country best, and the node is watched for change.';
  if (c.quadrant === 'insufficient_data') return 'Need or opportunity is not measured for this node: no case for intervention is established, so the default is open deployment.';
  if (c.stage === 'Deployment') return 'Deployment is where the technology is used: its supply risks are assessed at the producing stages and in the control layers, and the posture here is open, affordable deployment.';
  return 'No Secure route is met and the quadrant does not call for Compete.';
}

function Section({ title, children }: { title: string; children: React.ReactNode }) {
  return (
    <section className="mt-4">
      <h4 className="eyebrow mb-1">{title}</h4>
      {children}
    </section>
  );
}

export function CellDrawer({ detail, cell, names, core, onClose }: { detail: PostureDetail | null; cell: DetailCell | null; names: Record<string, string>; core: PostureCore; onClose: () => void }) {
  const ref = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const k = (e: KeyboardEvent) => e.key === 'Escape' && onClose();
    window.addEventListener('keydown', k);
    ref.current?.focus();
    return () => window.removeEventListener('keydown', k);
  }, [onClose, cell]);
  if (!cell) return null;
  const code = POSTURE_FROM_WORD[cell.posture];
  const meta = core.meta;
  const tr = cell.trade;
  const t = tr ? { ...tr, benchmark_period: meta.benchmark_period ?? undefined, momentum_period: meta.momentum_period ?? undefined, momentum_previous_period: meta.momentum_previous_period ?? undefined } : undefined;
  const mom = postureMomentum(t?.momentum_raw);
  const design = core.design[`${cell.variant}|${cell.delivery ?? ''}`];
  cell = design ? { ...cell, fence: design.fence, gate: design.gate, build: design.build, benchmark: design.benchmark, exit: design.exit } : cell;
  return (
    <div
      ref={ref}
      tabIndex={-1}
      role="dialog"
      aria-label={`${cell.tech} ${cell.stage} posture detail`}
      className="fixed inset-x-0 bottom-0 z-40 max-h-[75vh] overflow-y-auto rounded-t-xl border border-white/10 bg-ink-850 p-5 shadow-2xl outline-none sm:inset-x-auto sm:bottom-auto sm:right-4 sm:top-20 sm:max-h-[calc(100vh-6rem)] sm:w-[26rem] sm:rounded-xl"
      data-testid="cell-drawer"
    >
      <div className="flex items-start justify-between gap-3">
        <div>
          <p className="eyebrow">{detail?.country}</p>
          <h3 className="text-lg font-medium text-paper">
            {cell.tech} · {cell.stage}
          </h3>
          <p className="mt-1 inline-flex items-center gap-2 text-sm text-paper-dim">
            <span className="inline-block h-3 w-3 rounded-sm" style={{ background: POSTURE_COLOR[code] }} aria-hidden="true" />
            {POSTURE_LABEL[code]}
            {cell.mode ? ` · ${MODE_LABEL[cell.mode]}` : ''}
            {cell.posture === 'deploy' && cell.quadrant ? ` · ${QUADRANT_LABEL[cell.quadrant]}` : ''}
            {cell.confidence ? <span className="text-paper-mute"> · {cell.confidence} confidence</span> : null}
          </p>
        </div>
        <button type="button" className="btn btn-ghost btn-sm" onClick={onClose} aria-label="Close the detail">
          ✕
        </button>
      </div>

      {cell.posture === 'secure' && cell.security_rationale && (
        <Section title="Why Secure">
          <p className="text-sm text-paper-dim">
            <strong className="text-paper">{RATIONALE_LABEL[cell.security_rationale as SecurityRationale]}:</strong> {RATIONALE_WORDS[cell.security_rationale as SecurityRationale]}.
            {cell.security_routes && cell.security_routes.includes(';') ? ` Routes met: ${cell.security_routes.split(';').map((r) => RATIONALE_LABEL[r as SecurityRationale] ?? r).join(', ')}.` : ''}
          </p>
          {cell.reason && <p className="mt-1 text-xs text-paper-dim">{cell.reason}</p>}
          <p className="mt-1 text-xs text-paper-mute">Secure does not mean reshoring: delivery is {cell.delivery?.replace(/_/g, ' ') ?? '—'}.</p>
        </Section>
      )}
      {cell.mode === 'partner' && (
        <Section title="Trusted partners">
          {cell.partner_gap ? (
            <p className="text-sm text-signal">Partner gap: no credible trusted supplier.</p>
          ) : (
            <ol className="space-y-1 text-sm">
              {(cell.partners ?? []).map((p, i) => (
                <li key={p.iso3} className="flex justify-between gap-2">
                  <span className="text-paper">
                    {i + 1}. {names[p.iso3] ?? p.iso3}
                  </span>
                  <span className="tabular text-xs text-paper-mute">
                    holds {pct(p.capability)} · position {pct(p.cp)} · relationship {p.relationship !== undefined ? p.relationship.toFixed(2) : '—'}
                  </span>
                </li>
              ))}
            </ol>
          )}
          {cell.partner_reason && <p className="mt-1 text-xs text-paper-mute">{cell.partner_reason}</p>}
          <p className="mt-1 text-xs text-paper-mute">Evidence: {cell.partner_evidence ?? '—'} ({cell.partner_source === 'aib_position_share' ? 'Allied Industrial Base positions' : 'world supply shares'}).</p>
        </Section>
      )}
      {cell.posture === 'deploy' && (
        <Section title="Why restraint">
          <p className="text-sm text-paper-dim">{restraint(cell)}</p>
          {cell.reason && <p className="mt-1 text-xs text-paper-mute">{cell.reason}</p>}
        </Section>
      )}

      <Section title="Need × opportunity">
        <dl>
          <Row k="Strategic Need" v={pct(cell.need)} />
          <Row k="Industrial Opportunity" v={`${pct(cell.opportunity)}${cell.opportunity_basis ? ` (among ${cell.opportunity_basis})` : ''}`} />
          <Row k="Competitive Position" v={pct(cell.cp)} />
          <Row k="World supply share" v={pct(cell.home_share, 1)} />
          {cell.borderline && <Row k="Borderline" v={cell.borderline} />}
          {cell.consistency_flag && <Row k="Consistency check" v={cell.consistency_flag.replace(/_/g, ' ')} />}
        </dl>
      </Section>

      <Section title="Policy">
        <dl>
          <Row k="Alignment" v={cell.alignment ? ALIGNMENT_LABEL[cell.alignment as Alignment] ?? cell.alignment : '—'} />
          {cell.alignment_flag && <Row k="Note" v={cell.alignment_flag.replace(/_/g, ' ')} />}
        </dl>
        {cell.fence && <p className="mt-1 text-xs text-paper-mute"><strong className="text-paper-dim">Fence:</strong> {cell.fence}</p>}
        {cell.gate && <p className="mt-1 text-xs text-paper-mute"><strong className="text-paper-dim">Gate:</strong> {cell.gate}</p>}
        {cell.build && <p className="mt-1 text-xs text-paper-mute"><strong className="text-paper-dim">Build:</strong> {cell.build}</p>}
      </Section>

      <Section title="Trade evidence">
        <dl>
          <Row k={`Benchmark ${t?.benchmark_period ?? ''}`} v={`${pct(t?.benchmark_share, 1)} of world exports · RCA ${t?.benchmark_rca !== undefined ? t.benchmark_rca.toFixed(2) : '—'}`} />
          <Row k="Benchmark exports (avg.)" v={usd(t?.benchmark_exports)} />
          {mom ? (
            <Row k={`Momentum ${t?.momentum_period ?? ''}`} v={`${MOMENTUM_ARROW[mom]} ${t?.momentum_raw !== undefined ? `${((Math.exp(t.momentum_raw) - 1) * 100).toFixed(0)}%` : ''} vs ${t?.momentum_previous_period ?? 'the year before'}`} />
          ) : (
            <Row k="Momentum" v={<span className="text-paper-mute">{MOMENTUM_UNAVAILABLE}</span>} />
          )}
        </dl>
        {t?.momentum_period && <p className="mt-1 text-xs text-paper-mute">Data through {t.momentum_period.replace(/^.* - /, '')}.</p>}
      </Section>
    </div>
  );
}
