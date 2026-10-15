// S2 of the Country profile (spec_country.md A4.2): "Security and opportunity, sector by sector" (the
// quadrant scatter with its table twin, legend, the sectors not placed and the bridges to Strategic
// Insights) and "The four corners" (the categorisation in words, readable without the chart).

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { InfoTip } from '../../../components/InfoTip.tsx';
import { Term } from '../../../components/Term.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { EmptyState } from '../../../components/ui.tsx';
import { cellOf, QUADRANTS, type CellFacts } from '../../contract.ts';
import { QuadrantScatter, ScatterLegend } from '../../charts/QuadrantScatter.tsx';
import { countryScoresLink, spaceLink } from '../../derive/links.ts';
import { cellsByQuadrant, scatterPoints } from '../../derive/model.ts';
import { notPlacedLine, notPlacedReason, quadrantWord, proseName, poss, cap } from '../../derive/text.ts';
import { csvWithSource, LinkRow, Section, useProfile } from './common.tsx';

const yes = (b: boolean) => (b ? 'yes' : 'no');

export function SpaceSection() {
  const { sheet, cell, setCell } = useProfile();
  const name = proseName(sheet.meta.country);
  const pts = useMemo(() => scatterPoints(sheet), [sheet]);
  const sel = cellOf(sheet, cell);
  const placedSel = sel && sel.placed ? sel : null;
  const notPlaced = notPlacedLine(sheet);
  const byQ = useMemo(() => cellsByQuadrant(sheet), [sheet]);

  const cols: Col<CellFacts>[] = [
    { id: 'sector', header: 'Sector', value: (c) => c.tech },
    { id: 'stage', header: 'Stage', value: (c) => c.stage },
    { id: 'es', header: 'Energy Security', value: (c) => c.es?.v ?? null, cell: (c) => c.es?.d ?? 'no score', num: true },
    { id: 'es_pct', header: 'Beats (%)', value: (c) => c.es?.pct.v ?? null, cell: (c) => `${c.es?.pct.d ?? '–'}${c.es && (c.es.n.v ?? 99) < 10 ? ` (only ${c.es.n.d} economies)` : ''}`, num: true },
    { id: 'es_med', header: 'Median', value: (c) => c.es?.med.v ?? null, cell: (c) => c.es?.med.d ?? '–', num: true },
    { id: 'eo', header: 'Economic Opportunity', value: (c) => c.eo?.v ?? null, cell: (c) => c.eo?.d ?? 'no score', num: true },
    { id: 'eo_pct', header: 'Beats (%)', value: (c) => c.eo?.pct.v ?? null, cell: (c) => c.eo?.pct.d ?? '–', num: true },
    { id: 'eo_med', header: 'Median', value: (c) => c.eo?.med.v ?? null, cell: (c) => c.eo?.med.d ?? '–', num: true },
    { id: 'corner', header: 'Corner', value: (c) => (c.quadrant ? quadrantWord(sheet, c.quadrant) : null) },
    { id: 'si_rank', header: 'Strategic Index rank', value: (c) => (c.si.in_sample ? c.si.rank.v : null), cell: (c) => (c.si.in_sample ? c.si.rank.d : 'not in the index'), num: true },
    { id: 'placed', header: 'Placed', value: (c) => (c.placed ? 'yes' : notPlacedReason(c)) },
  ];
  const csv = () => ({
    name: `${sheet.meta.country} security and opportunity by sector`,
    text: csvWithSource(
      sheet.cells.map((c) => ({
        sector: c.tech, stage: c.stage, energy_security: c.es?.v ?? null, energy_security_beats_pct: c.es?.pct.v ?? null, energy_security_median: c.es?.med.v ?? null,
        economies_compared: c.es?.n.v ?? null, economic_opportunity: c.eo?.v ?? null, economic_opportunity_beats_pct: c.eo?.pct.v ?? null,
        economic_opportunity_median: c.eo?.med.v ?? null, corner: c.quadrant ? quadrantWord(sheet, c.quadrant) : null,
        strategic_index_rank: c.si.in_sample ? c.si.rank.v : null, placed: yes(c.placed), not_placed_reason: c.placed ? null : notPlacedReason(c),
      })),
      'Opportunity Security Indices, paper baseline',
      sheet.meta.as_of,
    ),
  });
  const summary = `${pts.length} of ${poss(name)} sectors placed by Energy Security and Economic Opportunity percentiles: ${QUADRANTS.map((q) => `${q.label} ${byQ[q.id].length}`).join(', ')}.`;
  return (
    <Section
      id="space"
      title="Security and opportunity, sector by sector"
      intro={
        <>
          Each sector is one technology at one stage of its supply chain. <Term id="es" /> asks how well {name} can rely on what it needs there; <Term id="eo" /> asks how well placed it is to create value there. Both compare {name} with every other economy scored in the same sector, so the chart shows the share of those economies it beats.
        </>
      }
    >
      <ChartCard
        headingLevel={3}
        title="Security and opportunity, sector by sector"
        sub={`Each dot is one of ${poss(name)} sectors: a technology at one stage of its supply chain. Across: Energy Security, shown as the share of other economies ${name} beats in that sector. Up: Economic Opportunity, read the same way. The dashed lines mark the middle economy, so each corner is a different situation.`}
        summary={summary}
        table={() => <TableView rows={sheet.cells} cols={cols} caption={`${cap(name)}: Energy Security and Economic Opportunity by sector`} rowKey={(c) => c.key} onRowClick={(c) => setCell(c.key)} selectedKey={cell} pageSize={33} />}
        csv={csv}
        legend={<ScatterLegend name={name} />}
        notes={notPlaced ?? undefined}
      >
        {pts.length ? (
          <QuadrantScatter sheet={sheet} points={pts} selected={placedSel?.key ?? null} onSelect={(k) => setCell(k)} ariaLabel={summary} />
        ) : (
          <EmptyState>
            None of {poss(name)} sectors can be placed: {sheet.meta.status_reason ?? 'no score is comparable with other economies'}. The table lists every sector with its values.
          </EmptyState>
        )}
        {placedSel && (
          <p className="mt-2 text-sm">
            <a className="link" href="#why">
              Why {placedSel.label}? &darr;
            </a>
          </p>
        )}
      </ChartCard>
      <LinkRow>
        <Link className="link" to={spaceLink(placedSel ?? sel)}>
          Compare all economies in one sector: Security vs opportunity &rarr;
        </Link>
        <Link className="link" to={countryScoresLink(sheet.iso3)}>
          Change the weighting in Strategic Insights &rarr;
        </Link>
      </LinkRow>
      <QuadrantList byQ={byQ} />
    </Section>
  );
}

function QuadrantList({ byQ }: { byQ: ReturnType<typeof cellsByQuadrant> }) {
  const { sheet, cell, setCell } = useProfile();
  return (
    <section className="card p-5" aria-labelledby="corners-h">
      <h3 id="corners-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        The four corners
      </h3>
      <p className="mt-1 text-sm text-paper-mute">
        Each corner is a different situation. The descriptions in brackets are the postures the RMI (2025) framework associates with each corner; they describe, they do not prescribe.
      </p>
      <div className="relative mt-3 overflow-x-auto">
        <table className="data-table">
          <caption className="sr-only">The four corners and {sheet.meta.country}&apos;s sectors in each</caption>
          <thead>
            <tr>
              <th scope="col">Corner</th>
              <th scope="col">What it means</th>
              <th scope="col" className="num">
                Sectors
              </th>
              <th scope="col">Which (by Strategic Index rank)</th>
            </tr>
          </thead>
          <tbody>
            {QUADRANTS.map((q) => (
              <tr key={q.id}>
                <td className="whitespace-nowrap text-paper">
                  {quadrantWord(sheet, q.id)} <InfoTip id={q.id} term={q.label} />
                </td>
                <td className="text-paper-dim">
                  {q.meaning} ({q.posture})
                </td>
                <td className="num tabular">{byQ[q.id].length}</td>
                <td>
                  <div className="flex flex-wrap gap-1.5">
                    {byQ[q.id].map((c) => (
                      <button
                        key={c.key}
                        type="button"
                        className="tag"
                        aria-pressed={cell === c.key}
                        aria-label={`${c.tech}, ${c.stage}: ${quadrantWord(sheet, q.id)}${c.si.in_sample ? `, Strategic Index rank ${c.si.rank.d}` : ''}`}
                        onClick={() => setCell(c.key)}
                      >
                        {c.label}
                      </button>
                    ))}
                    {!byQ[q.id].length && <span className="text-paper-mute">none</span>}
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </section>
  );
}
