// MemoDocument (spec_country.md A4.3, B7.2): renders the memo's structured document (the AST the PDF and
// the Word file are built from, so the three are word-for-word the same) as React elements: no HTML
// injection, no Markdown parser at runtime. Summary box with the short orange rule, numbered h2
// sections, superscript source numbers linking to the numbered source list. With `trace`, every value
// that came from the data is outlined and says its path and source in a tooltip.
//
// A country strategy brief (format 'brief', 2026-11-02) has no summary box and adds sub-headings, the box
// of strategic choices (each item topped by its choice colour), tables (source numbers once, under the
// table), figures (the published SVG, through `fileUrl`) and a technical appendix after the sources.

import type { MemoBlock, MemoDoc, MemoInline, MemoItem } from '../contract.ts';
import { VERB_COLOR } from '../../posture/grammar.ts';

type SrcOf = (n: number | null | undefined) => string | null;

function Inlines({ xs, trace, srcOf }: { xs: MemoInline[]; trace: boolean; srcOf: SrcOf }) {
  return (
    <>
      {xs.map((n, i) => {
        if (n.t === 'strong') return <strong key={i} className="font-medium text-paper">{n.v}</strong>;
        if (n.t === 'em') return <em key={i}>{n.v}</em>;
        if (n.t === 'value') {
          if (!trace) return <span key={i}>{n.v}</span>;
          const src = srcOf(n.src);
          return (
            <span key={i} className="border-b border-dotted border-signal" title={`${n.path}${src ? ` · ${src}` : ''}`} data-path={n.path}>
              {n.v}
            </span>
          );
        }
        return <span key={i}>{n.v}</span>;
      })}
    </>
  );
}

function Refs({ refs }: { refs: number[] }) {
  if (!refs.length) return null;
  return (
    <sup className="ml-0.5 text-[0.7em] text-paper-mute">
      {refs.map((r, i) => (
        <span key={r}>
          {i > 0 && ','}
          <a href={`#src-${r}`} className="link" aria-label={`Source ${r}`}>
            {r}
          </a>
        </span>
      ))}
    </sup>
  );
}

function Item({ it, trace, srcOf }: { it: MemoItem; trace: boolean; srcOf: SrcOf }) {
  return (
    <>
      <Inlines xs={it.inlines} trace={trace} srcOf={srcOf} />
      <Refs refs={it.refs} />
    </>
  );
}

/** The colour that tops a choice-box item (the dashboard's choice colours; Watch keeps its outline grey). */
const KIND_COLOR: Record<string, string> = { ...VERB_COLOR, watch: '#9ca3af' };

function Blocks({ blocks, trace, srcOf, fileUrl }: { blocks: MemoBlock[]; trace: boolean; srcOf: SrcOf; fileUrl: (f: string) => string }) {
  return (
    <>
      {blocks.map((b, j) => {
        switch (b.type) {
          case 'ul':
            return (
              <ul key={j} className="mb-4 list-disc space-y-1.5 pl-5">
                {b.items.map((it, k) => (
                  <li key={k}>
                    <Item it={it} trace={trace} srcOf={srcOf} />
                  </li>
                ))}
              </ul>
            );
          case 'h3':
            return (
              <h3 key={j} className="mb-2 mt-6 text-lg font-medium text-paper">
                {b.text}
              </h3>
            );
          case 'choices':
            return (
              <div key={j} className="mb-6 grid gap-3 sm:grid-cols-2" data-testid="memo-choices">
                {b.items.map((it, k) => (
                  <div key={k} className="panel p-4 text-sm" style={{ borderTopWidth: 4, borderTopColor: KIND_COLOR[it.kind] ?? KIND_COLOR.watch }}>
                    <div className="eyebrow mb-1 text-paper">{it.label}</div>
                    <Item it={it} trace={trace} srcOf={srcOf} />
                  </div>
                ))}
              </div>
            );
          case 'table':
            return (
              <div key={j} className="mb-6 overflow-x-auto">
                <table className="w-full min-w-[36rem] border-collapse text-sm">
                  <thead>
                    <tr>
                      {b.head.map((h, k) => (
                        <th key={k} scope="col" className="border-b border-paper-mute py-2 pr-3 text-left text-xs font-medium uppercase tracking-wide text-paper-mute">
                          {h}
                        </th>
                      ))}
                    </tr>
                  </thead>
                  <tbody>
                    {b.rows.map((r, i) => (
                      <tr key={i} className="border-b hairline align-top">
                        {r.map((c, k) => (
                          <td key={k} className="py-2 pr-3">
                            <Item it={c} trace={trace} srcOf={srcOf} />
                          </td>
                        ))}
                      </tr>
                    ))}
                  </tbody>
                </table>
                {b.refs.length > 0 && (
                  <p className="mt-1 text-xs text-paper-mute">
                    Sources: <Refs refs={b.refs} />
                  </p>
                )}
              </div>
            );
          case 'figure':
            return (
              <figure key={j} className="mb-6" aria-labelledby={`memo-fig-${b.n}`}>
                <div id={`memo-fig-${b.n}`} className="mb-2 font-medium text-paper">
                  Figure {b.n}. {b.title}
                </div>
                {b.svg ? (
                  <img src={fileUrl(b.svg)} alt={b.alt ?? b.title} className="w-full rounded bg-white" />
                ) : (
                  <p className="text-sm text-paper-mute">The figure is not available.</p>
                )}
                <figcaption className="mt-2 text-sm text-paper-mute">
                  <Item it={b.caption} trace={trace} srcOf={srcOf} />
                </figcaption>
              </figure>
            );
          default:
            return (
              <p key={j} className="mb-4">
                <Item it={b} trace={trace} srcOf={srcOf} />
              </p>
            );
        }
      })}
    </>
  );
}

export function MemoDocument({ doc, trace = false, fileUrl = (f: string) => f }: { doc: MemoDoc; trace?: boolean; fileUrl?: (f: string) => string }) {
  const byN = new Map(doc.sources.map((s) => [s.n, s.text]));
  const srcOf: SrcOf = (n) => (n ? (byN.get(n) ?? null) : null);
  return (
    <article className="max-w-3xl text-base leading-relaxed text-paper-dim" aria-label={doc.title}>
      {doc.summary.length > 0 && (
        <section className="panel mb-8 p-5" aria-labelledby="memo-summary-h">
          <h2 id="memo-summary-h" className="rule-neis-sm mb-3 text-lg font-medium text-paper">
            Summary
          </h2>
          <ul className="list-disc space-y-2 pl-5">
            {doc.summary.map((it, i) => (
              <li key={i}>
                <Item it={it} trace={trace} srcOf={srcOf} />
              </li>
            ))}
          </ul>
        </section>
      )}
      {doc.sections.map((s) => (
        <section key={s.n} className="mb-8" aria-labelledby={`memo-s${s.n}`}>
          <h2 id={`memo-s${s.n}`} className="mb-3 text-xl font-medium text-paper">
            {s.n}. {s.title}
          </h2>
          <Blocks blocks={s.blocks} trace={trace} srcOf={srcOf} fileUrl={fileUrl} />
        </section>
      ))}
      <section aria-labelledby="memo-sources-h" className="border-t hairline pt-5">
        <h2 id="memo-sources-h" className="mb-3 text-base font-medium text-paper">
          Sources
        </h2>
        <ol className="list-decimal space-y-1.5 pl-5 text-sm">
          {doc.sources.map((s) => (
            <li key={s.n} id={`src-${s.n}`} value={s.n}>
              {s.text}
            </li>
          ))}
        </ol>
      </section>
      {doc.appendix && (
        <section aria-labelledby="memo-appendix-h" className="mt-10 border-t hairline pt-5">
          <h2 id="memo-appendix-h" className="mb-3 text-xl font-medium text-paper">
            {doc.appendix.title}
          </h2>
          <Blocks blocks={doc.appendix.blocks} trace={trace} srcOf={srcOf} fileUrl={fileUrl} />
        </section>
      )}
      <p className="mt-6 border-t hairline pt-3 text-sm italic text-paper-mute">
        {doc.disclosure ?? doc.label}
        {doc.footer && <span className="block not-italic">{doc.footer}</span>}
      </p>
    </article>
  );
}
