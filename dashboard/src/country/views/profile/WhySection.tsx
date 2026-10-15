// S4 of the Country profile (spec_country.md A4.2, "Why these sectors"): for the high-priority sectors
// (Upgrade sectors in the Strategic Index sample, else the top Strategic Index sectors, else the highest
// Economic Opportunity) and any other placed sector the reader picks: which kinds of data pull Energy
// Security down and push Economic Opportunity up compared with the middle economy of the sector, the raw
// figures behind the main ones, and context that is not a cause (imports by supplier, factory projects,
// policy records).

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Term } from '../../../components/Term.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Loading, Notice, Segmented, Select } from '../../../components/ui.tsx';
import { cellOf, type CellFacts, type Driver } from '../../contract.ts';
import { WhyBars } from '../../charts/WhyBars.tsx';
import { driverBars, driversOf } from '../../derive/model.ts';
import { joinAnd, whyIntro, whySentence, proseName, poss, cap, rawNoteLine, euRegionSentence } from '../../derive/text.ts';
import { csvWithSource, Section, useProfile } from './common.tsx';

const PILLAR = { es: 'Energy Security', eo: 'Economic Opportunity' } as const;

export function WhySection() {
  const { sheet, cell, setCell } = useProfile();
  const name = proseName(sheet.meta.country);
  const prio = sheet.priority_sectors.map((k) => cellOf(sheet, k)).filter((c): c is CellFacts => !!c);
  const placed = sheet.cells.filter((c) => c.placed);
  const current = cellOf(sheet, cell);
  const showing = current && current.placed ? current : (prio[0] ?? null);
  if (sheet.priority_rule === 'none' || !placed.length) {
    return (
      <Section id="why" title="Why these sectors">
        <Notice kind="info">{whyIntro(sheet)}</Notice>
      </Section>
    );
  }
  const inPrio = showing && prio.some((c) => c.key === showing.key);
  return (
    <Section id="why" title="Why these sectors" intro={whyIntro(sheet)}>
      <div className="flex flex-wrap items-end gap-4">
        {prio.length > 0 && (
          <Segmented
            label={sheet.priority_rule === 'upgrade' ? 'High-priority sectors' : sheet.priority_rule === 'top_eo' ? 'Highest Economic Opportunity' : 'Top Strategic Index sectors'}
            value={inPrio && showing ? showing.key : ''}
            options={prio.map((c) => ({ value: c.key, label: c.label }))}
            onChange={(k) => setCell(k)}
          />
        )}
        <Select
          label="Another sector"
          value={showing && !inPrio ? showing.key : ''}
          options={[{ value: '', label: 'Another sector…', disabled: true }, ...placed.map((c) => ({ value: c.key, label: c.label }))]}
          onChange={(k) => k && setCell(k)}
          className="min-w-[16rem]"
        />
      </div>
      {showing ? <WhyPanel c={showing} /> : null}
      <p className="text-sm text-paper-mute">
        Bars compare {name} with the middle economy of the same sector. The figures under them are the data behind the main bars, in their own units and years; see
        how each is measured in <Link className="link" to={`/sector?c=${sheet.iso3}`}>Sector Insights</Link>.
      </p>
    </Section>
  );
}

function VarList({ d, pillar }: { d: Driver; pillar: 'es' | 'eo' }) {
  const { sheet } = useProfile();
  const name = proseName(sheet.meta.country);
  const vars = d.vars.filter((v) => !v.constant);
  if (!vars.length) return <p className="text-xs text-paper-mute">No variable breakdown for this category.</p>;
  return (
    <ul className="space-y-2">
      {vars.map((v) => (
        <li key={v.key} className="text-sm">
          <div className="text-paper-dim">{v.label}</div>
          {v.raw && v.raw.v !== null ? (
            <div className="text-paper">
              <span className="tabular">{v.raw.d}</span>
              <span className="text-xs text-paper-mute">
                {' '}
                ({[v.raw.year, v.raw.source_label].filter(Boolean).join(', ')})
              </span>
            </div>
          ) : (
            <div className="text-xs text-paper-mute">Index component; no single raw figure (score {v.score.d})</div>
          )}
          {v.pct.v !== null && (
            <div className="text-xs text-paper-mute">
              On this measure {name} scores higher than {v.pct.d} of the other economies in the sector (higher means {pillar === 'es' ? 'more secure' : 'more opportunity'}).
            </div>
          )}
          {(v.imputed || v.rule_d) && <div className="text-xs text-paper-mute">Filled-in data: {v.rule_d ?? 'missing for this economy and filled in'}.</div>}
          {rawNoteLine(v.raw, name) && (
            <div className={`text-xs ${v.raw?.note === 'eu_region_share' ? 'text-paper-dim' : 'text-paper-mute'}`}>{rawNoteLine(v.raw, name)}</div>
          )}
          {v.sector_link && (
            <Link className="link text-xs" to={v.sector_link}>
              See the data in Sector Insights &rarr;
            </Link>
          )}
        </li>
      ))}
    </ul>
  );
}

function Evidence({ c }: { c: CellFacts }) {
  const { sheet } = useProfile();
  const name = proseName(sheet.meta.country);
  const lines: string[] = [];
  const t = c.trade;
  if (t?.has_basket && t.sources.length) {
    const top = t.sources.slice(0, 3).map((s) => `${s.name} (${s.share.d}${s.status === 'mirror' ? ', read from the supplier’s report' : ''})`);
    lines.push(`The largest suppliers of ${poss(name)} imports in this sector in ${t.year} were ${joinAnd(top)} (UN Comtrade, reconciled; ${t.imports.d} in all).`);
  }
  if (c.fdi && ((c.fdi.in_n.v ?? 0) > 0 || (c.fdi.out_n.v ?? 0) > 0)) {
    lines.push(`${c.fdi.in_n.d} cross-border factory projects in ${name} in this sector, ${c.fdi.in_china_n.d} with a parent headquartered in China, Hong Kong or Macao; ${c.fdi.out_n.d} by ${poss(name)} firms abroad (Cleantech Manufacturing FDI dataset).`);
  }
  if (c.policy) lines.push(`${c.policy.records_specific.d} International Energy Agency policy records tied to this sector.`);
  if (!lines.length) return null;
  return (
    <div className="mt-4 border-t hairline pt-3">
      <div className="eyebrow mb-1">For context (not inputs of the scores)</div>
      <ul className="list-disc space-y-1 pl-5 text-sm text-paper-dim">
        {lines.map((l) => (
          <li key={l}>{l}</li>
        ))}
      </ul>
      <p className="mt-1 text-xs text-paper-mute">
        The paper&apos;s Energy Security does not measure how concentrated a country&apos;s own suppliers are; its trade input is the world market&apos;s concentration, the same for every economy in a sector.
      </p>
    </div>
  );
}

function WhyPanel({ c }: { c: CellFacts }) {
  const { sheet, driversDoc, driversStatus, retryDrivers } = useProfile();
  const name = proseName(sheet.meta.country);
  const dr = driversOf(sheet, c.key, driversDoc);
  const bars = useMemo(() => (dr ? { es: driverBars(dr.es, 'es'), eo: driverBars(dr.eo, 'eo') } : null), [dr]);
  if (!dr || !bars) {
    if (driversStatus === 'error')
      return (
        <Notice kind="error" title="The data behind this sector could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={retryDrivers}>Retry</button>}>
          Try again, or pick another sector.
        </Notice>
      );
    return <Loading label={`Loading the data behind ${c.label}`} />;
  }
  const max = Math.max(1e-9, ...[...bars.es, ...bars.eo].map((d) => Math.abs(d.impact.v ?? 0)));
  // a top driver resting on the EU's regional figure (raw note eu_region_share): said above the bars
  const euLine = euRegionSentence(dr, name);
  const topOf = (pl: 'es' | 'eo') => bars[pl].filter((d) => d.top && !d.constant);
  type Row = { pillar: string; category: string; score: string; median: string; weight: string; impact: string; imputed: string; rule: string };
  const twinRows: Row[] = (['es', 'eo'] as const).flatMap((pl) =>
    bars[pl].map((d) => ({
      pillar: PILLAR[pl], category: d.category, score: d.score.d, median: d.median.d, weight: d.weight.d, impact: d.impact.d, imputed: d.imputed ? 'yes' : 'no', rule: d.rule_d ?? '',
    })),
  );
  const csv = () => ({
    name: `${sheet.meta.country} ${c.label} drivers`,
    text: csvWithSource(
      (['es', 'eo'] as const).flatMap((pl) =>
        bars[pl].flatMap((d) => [
          { pillar: PILLAR[pl], category: d.category, variable: null, score: d.score.v, median: d.median.v, weight: d.weight.v, impact: d.impact.v, imputed: d.imputed, rule: d.rule, raw_value: null, raw_unit: null, raw_year: null, raw_source: null },
          ...d.vars.map((v) => ({
            pillar: PILLAR[pl], category: d.category, variable: v.label, score: v.score.v, median: v.median.v, weight: v.weight.v, impact: null, imputed: v.imputed, rule: v.rule,
            raw_value: v.raw?.v ?? null, raw_unit: v.raw?.unit ?? null, raw_year: v.raw?.year ?? null, raw_source: v.raw?.source_label ?? v.raw?.source ?? null,
          })),
        ]),
      ),
      'Opportunity Security Indices, paper baseline (category and variable contributions); raw figures from their sources',
      sheet.meta.as_of,
    ),
  });
  return (
    <ChartCard
      headingLevel={3}
      title={c.label}
      sub={whySentence(c, dr)}
      summary={`${cap(name)}, ${c.label}: the categories behind Energy Security and Economic Opportunity compared with the middle economy.`}
      table={() => (
        <TableView<Row>
          rows={twinRows}
          cols={[
            { id: 'pillar', header: 'Score', value: (r) => r.pillar },
            { id: 'category', header: 'Category', value: (r) => r.category },
            { id: 'score', header: 'Category score', value: (r) => r.score, num: true },
            { id: 'median', header: 'Middle economy', value: (r) => r.median, num: true },
            { id: 'weight', header: 'Weight share', value: (r) => r.weight, num: true },
            { id: 'impact', header: 'Impact', value: (r) => r.impact, num: true },
            { id: 'imputed', header: 'Filled-in', value: (r) => r.imputed },
            { id: 'rule', header: 'Fill rule', value: (r) => r.rule },
          ]}
          caption={`${cap(name)}, ${c.label}: categories behind the scores`}
          pageSize={30}
        />
      )}
      csv={csv}
      legend={
        <p className="text-xs text-paper-dim">
          <span className="mr-1 inline-block h-2.5 w-3.5 align-middle" style={{ background: '#f58229' }} aria-hidden="true" /> Orange: below the middle economy ·{' '}
          <span className="mr-1 inline-block h-2.5 w-3.5 align-middle" style={{ background: '#1889b0' }} aria-hidden="true" /> Blue: above · Hollow: filled-in data. Bar length = the
          category&apos;s share of the score&apos;s weight × its distance from the middle economy.
        </p>
      }
    >
      {euLine && <p className="mb-3 text-sm text-paper-dim">{euLine}</p>}
      <div className="grid gap-6 lg:grid-cols-2">
        {(['es', 'eo'] as const).map((pl) => (
          <div key={pl}>
            <h4 className="mb-2 text-sm font-medium text-paper">{pl === 'es' ? 'What pulls Energy Security down or up' : 'What pushes Economic Opportunity up or down'}</h4>
            <p className="mb-2 text-xs text-paper-mute">
              {c[pl] ? (
                <>
                  <Term id={pl} /> {c[pl]!.d}; the middle economy {c[pl]!.med.d}.
                </>
              ) : (
                'No score.'
              )}
            </p>
            <WhyBars drivers={bars[pl]} max={max} label={`${PILLAR[pl]} categories of ${c.label}`} />
            {topOf(pl).length > 0 && (
              <div className="mt-4">
                <div className="eyebrow mb-1">What&apos;s behind it</div>
                {topOf(pl).map((d) => (
                  <div key={d.key} className="mt-2">
                    <div className="text-sm font-medium text-paper-dim">{d.category}</div>
                    <VarList d={d} pillar={pl} />
                  </div>
                ))}
              </div>
            )}
          </div>
        ))}
      </div>
      <Evidence c={c} />
    </ChartCard>
  );
}
