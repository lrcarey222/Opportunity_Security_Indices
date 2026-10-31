// "Opportunities & risks" of the Country profile (IA refactor 2026-10-16, brief s.12A; plan.md 5.3 item 2):
// three short lists built from existing outputs, no new score (derive/framing.ts):
//   Make bets                    Grow or Upgrade corner, by Strategic Index rank (preference-adjusted)
//   Avoid dangerous dependencies Upgrade or Exposed corner (key secure), lowest Energy Security first, with
//                                the largest supplier and any compact where the economy is a demand anchor (facts)
//   Capabilities to keep         Grow corner, and the economy's supplier / technology roles in the compacts
//                                ("Preserve capabilities" until 2026-10-28, renamed so it is not read as the
//                                Strategic posture section's Compete · Preserve mode)
// The corners (recomputed on the 66 six-stage sectors) take their words from LEGACY_CORNERS (contract.ts).
// Since 2026-10-29 (spec XIV, XVIII; design D10) the three lists are a Legacy OSI reading: Strategic Choices
// replaces them in the canonical page, and this block sits in Evidence & Methodology -> Legacy OSI framework
// (id legacy-opportunities, heading level 3), with its Why and Strategic Index disclosures.
// Behind disclosures: "Why these sectors" (the existing drivers section) and "How priorities are built"
// (the existing Strategic Index table at the NEIS baseline; the embedded Strategic Index view under the
// reader's Perspective).

import { useMemo, type ReactNode } from 'react';
import { Link } from 'react-router';
import { Term } from '../../../components/Term.tsx';
import { Notice, RankChip } from '../../../components/ui.tsx';
import { compactHref } from '../../../aib/nav.ts';
import { countryRoleRow } from '../../../aib/derive/countryRoles.ts';
import StrategicView from '../../../views/Strategic/StrategicView.tsx';
import type { CellFacts } from '../../contract.ts';
import { capabilities, dependencies, makeBets, siRankOf } from '../../derive/framing.ts';
import { sectorProfileLink } from '../../derive/links.ts';
import { cap, legacyCornerWord, poss, proseName } from '../../derive/text.ts';
import { PriorityTable } from './PrioritiesSection.tsx';
import { BasisTag, Fold, perspectiveNote, Section, usePerspectiveSi, useProfile, type Basis } from './common.tsx';
import { WhySection } from './WhySection.tsx';

function FrameCard({ id, title, question, basis, note, children }: { id: string; title: string; question: string; basis: Basis; note?: string; children: ReactNode }) {
  return (
    <section className="card flex flex-col p-5" aria-labelledby={`${id}-h`} data-testid={id}>
      <div className="mb-2">
        <BasisTag kind={basis} note={note} />
      </div>
      <h3 id={`${id}-h`} className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        {title}
      </h3>
      <p className="mt-1 text-sm text-paper-mute">{question}</p>
      <div className="mt-3 flex-1 text-sm text-paper-dim">{children}</div>
    </section>
  );
}

function SectorLink({ c, iso3 }: { c: CellFacts; iso3: string }) {
  return (
    <Link className="link text-paper" to={sectorProfileLink(c, iso3)}>
      {c.label}
    </Link>
  );
}

export function OpportunitiesSection({ id = 'opportunities', level = 2, title = 'Opportunities & risks' }: { id?: string; level?: 2 | 3; title?: string } = {}) {
  const { sheet, aib } = useProfile();
  const si = usePerspectiveSi();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const doc = aib?.doc ?? null;
  const names = aib?.names;
  const bets = useMemo(() => makeBets(sheet, si.map), [sheet, si.map]);
  const deps = useMemo(() => dependencies(sheet, doc), [sheet, doc]);
  const roleRow = useMemo(() => (doc && names ? countryRoleRow(doc, names, iso) : null), [doc, names, iso]);
  const caps = useMemo(() => capabilities(sheet, roleRow), [sheet, roleRow]);
  const corner = (c: CellFacts) => (c.legacy_corner ? legacyCornerWord(c.legacy_corner) : 'not placed');
  return (
    <Section
      id={id}
      level={level}
      title={title}
      intro={
        <>
          Three readings of the index scores of {poss(name)} sectors: where {name} could build on an opportunity, where it depends most on others, and which capabilities it already has. The corners split{' '}
          {poss(name)} sectors at its own median <Term id="eo" /> and <Term id="es" />; the order of the first list follows the <Term id="si" /> under your perspective.
        </>
      }
    >
      <div className="grid gap-4 lg:grid-cols-3">
        <FrameCard id="make-bets" title="Make bets" question={`Where does ${name} have compelling economic and industrial opportunities?`} basis="prefs" note={perspectiveNote(si)}>
          {bets.length ? (
            <ol className="space-y-2">
              {bets.map((c) => {
                const rank = siRankOf(c, si.map);
                const o = si.map?.get(c.key);
                return (
                  <li key={c.key}>
                    <SectorLink c={c} iso3={iso} />
                    <span className="block text-xs text-paper-mute">
                      {corner(c)} corner · Economic Opportunity beats {c.eo?.pct.d ?? '–'} of economies · {rank !== null ? `Strategic Index rank ${rank}` : 'not in the Strategic Index'}{' '}
                      {o && <RankChip change={o.delta} isNew={o.isNew} />}
                    </span>
                  </li>
                );
              })}
            </ol>
          ) : (
            <p className="text-paper-mute">No placed sector of {name} sits in the {legacyCornerWord('legacy_grow')} or {legacyCornerWord('legacy_upgrade')} corner.</p>
          )}
        </FrameCard>
        <FrameCard id="dependencies" title="Avoid dangerous dependencies" question={`Where are ${poss(name)} largest security vulnerabilities?`} basis="facts">
          {deps.length ? (
            <ol className="space-y-2">
              {deps.map((d) => (
                <li key={d.cell.key}>
                  <SectorLink c={d.cell} iso3={iso} />
                  <span className="block text-xs text-paper-mute">
                    Energy Security beats {d.cell.es?.pct.d ?? '–'} of economies
                    {d.supplier ? ` · largest supplier ${d.supplier.name} (${d.supplier.share.d} of imports, ${d.cell.trade?.year ?? sheet.meta.trade_year})` : ''}
                  </span>
                  {d.compacts.length > 0 && (
                    <span className="block text-xs">
                      Demand anchor in{' '}
                      {d.compacts.map((cp, i) => (
                        <span key={cp.id}>
                          {i > 0 && ', '}
                          <Link className="link" to={compactHref(cp.id)}>
                            {cp.name}
                          </Link>
                        </span>
                      ))}
                    </span>
                  )}
                </li>
              ))}
            </ol>
          ) : (
            <p className="text-paper-mute">No placed sector of {name} sits in the {legacyCornerWord('legacy_upgrade')} or {legacyCornerWord('legacy_exposed')} corner.</p>
          )}
        </FrameCard>
        <FrameCard id="capabilities" title="Capabilities to keep" question={`Where does ${name} hold specialised capabilities worth keeping?`} basis="facts">
          {caps.cells.length ? (
            <ul className="space-y-2">
              {caps.cells.map((c) => (
                <li key={c.key}>
                  <SectorLink c={c} iso3={iso} />
                  <span className="block text-xs text-paper-mute">
                    Economic Opportunity beats {c.eo?.pct.d ?? '–'}, Energy Security beats {c.es?.pct.d ?? '–'} of economies
                  </span>
                </li>
              ))}
            </ul>
          ) : (
            <p className="text-paper-mute">No placed sector of {name} sits in the {legacyCornerWord('legacy_grow')} corner.</p>
          )}
          {caps.roles.length > 0 && (
            <p className="mt-3 border-t hairline pt-2 text-xs">
              In the Allied Industrial Base compacts {name} is a{' '}
              {caps.roles.map((r, i) => (
                <span key={r.role}>
                  {i > 0 && (i === caps.roles.length - 1 ? ' and a ' : ', a ')}
                  <span className="text-paper">{r.role.toLowerCase()}</span> in {r.compacts.length} compact{r.compacts.length === 1 ? '' : 's'}
                </span>
              ))}
              .{' '}
              <a className="link" href="#aib-role">
                Its role &darr;
              </a>
            </p>
          )}
        </FrameCard>
      </div>
      <div>
        <Fold id="why" summary={`Why these sectors: the measures behind ${poss(name)} scores`}>
          <WhySection level={3} />
        </Fold>
        <Fold id="si" summary="How priorities are built: the Strategic Index and its five parts">
          {sheet.meta.has_si ? (
            <PriorityTable />
          ) : (
            <Notice kind="info" title="No Strategic Index">
              {cap(name)} has no Policy index (no International Energy Agency Policies and Measures records), so no Strategic Index is computed for it.
            </Notice>
          )}
          <Fold id="si-model" engine summary="The Strategic Index under your perspective: top ten, the five weighted parts and the allied picks (Legacy OSI)">
            <StrategicView embedded />
          </Fold>
        </Fold>
      </div>
    </Section>
  );
}
