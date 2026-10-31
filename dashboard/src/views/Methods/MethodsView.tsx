// View 7: Methods (spec_react.md 8.6). Since 2026-10-29 it opens with the canonical strategic decision
// framework (need, prize, position, choice, policy alignment; #framework; since the NEIS copy refresh of
// 2026-10-30 it leads with what the strategy is for, and the build details sit in a disclosure), then the Legacy OSI framework
// (the paper baseline; #legacy-osi): the indices, the dials in plain language and the exact formulas
// (content/methods.md, Markdown + KaTeX, loaded with this route only), with the constants of this data
// build filled in; panels for the data vintage and the paper. A link to #framework or #legacy-osi scrolls
// there once the text is in.

import { useEffect, useMemo } from 'react';
import { Link, useLocation } from 'react-router';
import { marked } from 'marked';
import katex from 'katex';
import 'katex/dist/katex.min.css';
import methodsMd from '../../content/methods.md?raw';
import { useTitle } from '../../App.tsx';
import { GlossaryList } from '../../components/GlossaryList.tsx';
import { Disclosure, KeyValue, PageHead } from '../../components/ui.tsx';
import { MODULE_QUESTION } from '../../content/vocabulary.ts';
import { DASH, fmtDate, yesNo } from '../../lib/format.ts';
import { useApp, useEngine } from '../../state/EngineProvider.tsx';
import { buildFacts, fillPlaceholders, methodsValues, renderMath } from './methodsFill.ts';

export default function MethodsView() {
  useTitle('Methods');
  const app = useApp();
  const { state } = useEngine();
  const m = app.manifest;
  const html = useMemo(() => {
    const facts = buildFacts({
      countries: app.countries,
      eligibility: app.baseline.policy.eligibility,
      siCells: app.baseline.si.cells,
      allied: app.baseline.allied,
    });
    const md = fillPlaceholders(methodsMd, methodsValues(app.bundleLike.baselineInputs, m, facts));
    return renderMath(
      md,
      (s) => marked.parse(s, { async: false, gfm: true }) as string,
      (t, display) => katex.renderToString(t, { displayMode: display, throwOnError: false, output: 'htmlAndMathml' }),
    );
  }, [app, m]);
  const { hash } = useLocation();
  // the article is set as HTML, so a link to one of its chapters scrolls there once it is in
  useEffect(() => {
    const id = decodeURIComponent(hash.replace(/^#/, ''));
    if (id) document.getElementById(id)?.scrollIntoView?.({ block: 'start' });
  }, [hash, html]);
  const fl = m.flags ?? {};
  const paper = m.paper;
  return (
    <div>
      <PageHead
        eyebrow="About · Methods"
        title="Methods"
        lede={`${MODULE_QUESTION.methods}: what the country needs, how valuable the capability is, how strong its current position is, what action follows, and whether policy matches. Then the security and opportunity scores, a descriptive view beside the choices, with their indices, dials and formulas, and a glossary.`}
      />
      <div className="grid gap-8 xl:grid-cols-[minmax(0,2fr)_minmax(0,1fr)]">
        {/* the content is this app's own Markdown, filled with numbers from the data build */}
        <div className="min-w-0">
          <article className="prose-neis min-w-0" dangerouslySetInnerHTML={{ __html: html }} />
          <section className="mt-10" aria-labelledby="methods-glossary">
            <h2 id="methods-glossary" className="rule-neis-sm mb-2 text-xl font-medium text-paper">
              Glossary
            </h2>
            <p className="mb-4 text-sm text-paper-dim">
              The strategic choices and the terms of the indices and the dials in plain words. The glossary of the whole Explorer is on the{' '}
              <Link className="link" to="/glossary">
                Glossary page
              </Link>
              .
            </p>
            <GlossaryList module="strategic" />
          </section>
        </div>
        <aside className="flex flex-col gap-6 self-start">
          <section className="card p-5">
            <h2 className="rule-neis-sm mb-3 text-base font-medium text-paper">Data vintage</h2>
            <KeyValue rows={[['Exported', fmtDate(m.created)]]} />
            {/* build identifiers stay out of the first read (the NEIS copy refresh): one click away */}
            <Disclosure summary="Build details">
              <KeyValue
                rows={[
                  ['Build', m.bundle_id.slice(0, 8)],
                  ['Git commit', m.git_commit ? m.git_commit.slice(0, 10) : DASH],
                  ['Branch', m.osi_branch ?? DASH],
                  ['Engine', state.engine ?? DASH],
                  ['Stored pillars apply the rankability rule', yesNo(fl.pillar_rule)],
                  ['Friendshore includes geopolitical alignment', yesNo(fl.geo_alignment_in_fs)],
                  ['Friendshore reads factory projects (CM-FDI) and Chinese-parent ownership', yesNo(fl.fs_cmfdi)],
                  ['Partnerships recompute with scenario pillars', yesNo(fl.partner_pillar_consistent)],
                ]}
              />
            </Disclosure>
            {(m.notes ?? []).length > 0 && (
              <ul className="mt-3 list-disc space-y-1.5 pl-5 text-xs text-paper-mute">
                {m.notes.map((n) => (
                  <li key={n}>{n}</li>
                ))}
              </ul>
            )}
          </section>
          <section className="card p-5">
            <h2 className="rule-neis-sm mb-3 text-base font-medium text-paper">Background paper</h2>
            <p className="text-sm text-paper-dim">{paper?.title ?? 'Opportunity, Security, and the Allied Industrial Base'}</p>
            <ul className="mt-3 list-disc space-y-1 pl-5 text-sm text-paper-mute">
              <li>The framework and the equations for Energy Security, Economic Opportunity, the Strategic Index and the Partnership Strength Index</li>
              <li>The opportunity&ndash;security space</li>
              <li>Policy activity and the Strategic Index</li>
              <li>The partnership indices</li>
              <li>The Allied Industrial Base: how it tests supply disruptions and concentration, when trade is enough, where new capacity is needed, and the Sectoral Compacts</li>
            </ul>
            {(paper?.url || paper?.pdf) && (
              <p className="mt-3 flex flex-wrap gap-4 text-sm">
                {paper?.url && (
                  <a className="link" href={paper.url}>
                    Read it online
                  </a>
                )}
                {paper?.pdf && (
                  <a className="link" href={`${import.meta.env.BASE_URL}${paper.pdf.replace(/^\/+/, '')}`}>
                    Draft (PDF)
                  </a>
                )}
              </p>
            )}
          </section>
        </aside>
      </div>
    </div>
  );
}
