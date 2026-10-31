// S2 of the Country profile (spec_country.md A4.2; IA refactor 2026-10-16: "Strategic position", the
// profile's central visual; since 2026-10-28 "Security and opportunity", the section after Strategic
// posture, with id "position" and the alias "space" kept for old links; since schema 2 the 66 six-stage
// sectors, at most 58 of them placed):
// the quadrant scatter with its table twin, legend and the sectors not placed;
// the filled dots are the top five Strategic Index sectors under the reader's Perspective (the only
// preference-adjusted mark; positions are facts); "Compare with other economies" opens the embedded
// Security vs Opportunity view (the model); "The four corners" puts the categorisation in words.
// Since 2026-10-29 (spec XVIII, design D10) this is a Legacy OSI block: it sits in Evidence & Methodology ->
// Legacy OSI framework (id legacy-space, heading level 3; the old #position and #space links open it there).

import { useMemo } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { InfoTip } from '../../../components/InfoTip.tsx';
import { Term } from '../../../components/Term.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { EmptyState } from '../../../components/ui.tsx';
import { cellOf, LEGACY_CORNERS, type CellFacts } from '../../contract.ts';
import { QuadrantScatter, ScatterLegend } from '../../charts/QuadrantScatter.tsx';
import SpaceView from '../../../views/Space/SpaceView.tsx';
import { siOrder } from '../../derive/framing.ts';
import { cellsByLegacyCorner, scatterPoints } from '../../derive/model.ts';
import { notPlacedLine, notPlacedReason, legacyCornerWord, proseName, poss, cap } from '../../derive/text.ts';
import { BasisTag, csvWithSource, Fold, perspectiveNote, Section, usePerspectiveSi, useProfile } from './common.tsx';

const yes = (b: boolean) => (b ? 'yes' : 'no');

export function SpaceSection({ id = 'position', level = 2 }: { id?: string; level?: 2 | 3 } = {}) {
  const { sheet, cell, setCell, reveal, setStrategicView } = useProfile();
  const si = usePerspectiveSi();
  const name = proseName(sheet.meta.country);
  const pts = useMemo(() => {
    const base = scatterPoints(sheet);
    if (!si.map) return base;
    // the Perspective's top five Strategic Index sectors are the filled dots
    const top = new Set(siOrder(sheet, si.map).slice(0, 5).map((c) => c.key));
    return base.map((p) => ({ ...p, top5: top.has(p.key) }));
  }, [sheet, si.map]);
  const sel = cellOf(sheet, cell);
  const placedSel = sel && sel.placed ? sel : null;
  const notPlaced = notPlacedLine(sheet);
  const byQ = useMemo(() => cellsByLegacyCorner(sheet), [sheet]);

  const cols: Col<CellFacts>[] = [
    { id: 'sector', header: 'Sector', value: (c) => c.tech },
    { id: 'stage', header: 'Stage', value: (c) => c.stage },
    { id: 'es', header: 'Energy Security', value: (c) => c.es?.v ?? null, cell: (c) => c.es?.d ?? 'no score', num: true },
    { id: 'es_pct', header: 'Beats (%)', value: (c) => c.es?.pct.v ?? null, cell: (c) => `${c.es?.pct.d ?? '–'}${c.es && (c.es.n.v ?? 99) < 10 ? ` (only ${c.es.n.d} economies)` : ''}`, num: true },
    { id: 'es_med', header: 'Median', value: (c) => c.es?.med.v ?? null, cell: (c) => c.es?.med.d ?? '–', num: true },
    { id: 'eo', header: 'Economic Opportunity', value: (c) => c.eo?.v ?? null, cell: (c) => c.eo?.d ?? 'no score', num: true },
    { id: 'eo_pct', header: 'Beats (%)', value: (c) => c.eo?.pct.v ?? null, cell: (c) => c.eo?.pct.d ?? '–', num: true },
    { id: 'eo_med', header: 'Median', value: (c) => c.eo?.med.v ?? null, cell: (c) => c.eo?.med.d ?? '–', num: true },
    { id: 'corner', header: 'Corner', value: (c) => (c.legacy_corner ? legacyCornerWord(c.legacy_corner) : null) },
    { id: 'si_rank', header: 'Strategic Index rank', value: (c) => (c.si.in_sample ? c.si.rank.v : null), cell: (c) => (c.si.in_sample ? c.si.rank.d : 'not in the index'), num: true },
    { id: 'placed', header: 'Placed', value: (c) => (c.placed ? 'yes' : notPlacedReason(c)) },
  ];
  const csv = () => ({
    name: `${sheet.meta.country} security and opportunity by sector`,
    text: csvWithSource(
      sheet.cells.map((c) => ({
        sector: c.tech, stage: c.stage, energy_security: c.es?.v ?? null, energy_security_beats_pct: c.es?.pct.v ?? null, energy_security_median: c.es?.med.v ?? null,
        economies_compared: c.es?.n.v ?? null, economic_opportunity: c.eo?.v ?? null, economic_opportunity_beats_pct: c.eo?.pct.v ?? null,
        economic_opportunity_median: c.eo?.med.v ?? null, corner: c.legacy_corner ? legacyCornerWord(c.legacy_corner) : null,
        strategic_index_rank: c.si.in_sample ? c.si.rank.v : null, placed: yes(c.placed), not_placed_reason: c.placed ? null : notPlacedReason(c),
      })),
      "Security and opportunity scores computed by the Explorer's authors (baseline)",
      sheet.meta.as_of,
    ),
  });
  const cm = sheet.summary.country_median;
  const summary = `${pts.length} of ${poss(name)} sectors placed by Economic Opportunity and Energy Security percentiles, split at ${poss(name)} median sector (${cm?.eo.d ?? '–'} and ${cm?.es.d ?? '–'}): ${LEGACY_CORNERS.map((q) => `${q.label} ${byQ[q.id].length}`).join(', ')}.`;
  return (
    <Section
      id={id}
      level={level}
      title="Security and opportunity"
      basis={
        <>
          <BasisTag kind="facts" note="positions" />
          <BasisTag kind="prefs" note={`filled dots, ${perspectiveNote(si)}`} />
        </>
      }
      intro={
        <>
          Each sector is one technology at one of the six <Term id="stage">supply-chain stages</Term> (extraction, refining, components, assembly, deployment, recovery), as in the
          Country Strategy Map. <Term id="es" /> asks how well {name} can rely on what it needs there; <Term id="eo" /> asks how well placed it is to create value there. Both compare {name} with every other economy scored in the same sector, so the chart shows the share of those economies it beats. The four corners then sort {poss(name)} own sectors: the dashed lines are its median sector, so the corners show where {name} is relatively strong or weak.
        </>
      }
    >
      <ChartCard
        headingLevel={3}
        title="Security and opportunity, sector by sector"
        sub={`Each dot is one of ${poss(name)} sectors: a technology at one stage of its supply chain. Across: Economic Opportunity, shown as the share of other economies ${name} beats in that sector. Up: Energy Security, read the same way. The dashed lines mark ${poss(name)} median sector on each axis (${cm?.eo.d ?? '–'} across, ${cm?.es.d ?? '–'} up), so the corners sort its relatively strong and weak sectors; the axes still compare ${name} with the other economies.`}
        summary={summary}
        table={() => <TableView rows={sheet.cells} cols={cols} caption={`${cap(name)}: Energy Security and Economic Opportunity by sector`} rowKey={(c) => c.key} onRowClick={(c) => setCell(c.key)} selectedKey={cell} pageSize={66} />}
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
            <button type="button" className="link" onClick={() => reveal?.('why')}>
              Why {placedSel.label}? &darr;
            </button>
          </p>
        )}
      </ChartCard>
      <QuadrantList byQ={byQ} />
      <Fold
        id="compare"
        engine
        summary="Compare with other economies: every economy in one sector (Security vs opportunity)"
        onOpen={() => {
          const c = placedSel ?? sel;
          if (c) setStrategicView?.([['tech', c.tech], ['stage', c.stage]]);
        }}
      >
        <SpaceView embedded iso3={sheet.iso3} />
      </Fold>
    </Section>
  );
}

function QuadrantList({ byQ }: { byQ: ReturnType<typeof cellsByLegacyCorner> }) {
  const { sheet, cell, setCell } = useProfile();
  return (
    <section className="card p-5" aria-labelledby="corners-h">
      <h3 id="corners-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        The four corners
      </h3>
      <p className="mt-1 text-sm text-paper-mute">
        Each corner is a different situation. High and low are against {poss(proseName(sheet.meta.country))} own median sector (a sector at the median counts as high), so the corners show its relatively strong and weak sectors. The descriptions in brackets are the strategies the RMI (2025) framework associates with each corner (not the Secure / Compete / Deploy postures of the Strategic posture section); they describe, they do not prescribe.
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
            {LEGACY_CORNERS.map((q) => (
              <tr key={q.id}>
                <td className="whitespace-nowrap text-paper">
                  {legacyCornerWord(q.id)} <InfoTip id={q.glossary} term={q.label} />
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
                        aria-label={`${c.tech}, ${c.stage}: ${legacyCornerWord(q.id)}${c.si.in_sample ? `, Strategic Index rank ${c.si.rank.d}` : ''}`}
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
