// The Explore notes card (spec_sector.md 4.6 section 7): every caveat of the measure, the source
// citation with its edition, licence and attribution, how the export transforms it, and "Used in the
// indices" from the dictionary's `feeds` (the ES / EO category the measure enters, as S-curved
// within-sector ranks). No index value is shown; the Strategic bridge link is not part of this build.

import type { VarInfo } from '../../data/model.ts';
import { useModel } from '../../data/SectorDataProvider.tsx';
import { ExternalLink } from '../common.tsx';
import { safeUrl } from '../../derive/policy.ts';
import { lc } from '../../format.ts';

/** "ES: Trade" -> "Energy Security: Trade". */
function feedWords(f: string): string {
  const [pillar, ...rest] = f.split(':');
  const name = rest.join(':').trim();
  const P: Record<string, string> = { ES: 'Energy Security', EO: 'Economic Opportunity', Policy: 'Policy', Partners: 'Partnerships', Coalition: 'Coalition', PSI: 'Partnerships' };
  return name ? `${P[pillar.trim()] ?? pillar.trim()}: ${name}` : f;
}

export function NotesCard({ v, extra }: { v: VarInfo; extra?: string[] }) {
  const model = useModel();
  const caveats = [...model.caveatTexts(v), ...(extra ?? [])];
  const src = v.source;
  const url = safeUrl(src.url);
  return (
    <section className="card p-5" aria-labelledby="explore-notes-h">
      <h2 id="explore-notes-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        Notes on {lc(v.label)}
      </h2>
      <div className="mt-3 grid gap-6 text-sm text-paper-dim md:grid-cols-2">
        <div className="space-y-3">
          {v.definition && <p>{v.definition}</p>}
          {caveats.length > 0 ? (
            <ul className="list-disc space-y-1 pl-5">
              {caveats.map((c) => (
                <li key={c}>{c}</li>
              ))}
            </ul>
          ) : (
            <p className="text-paper-mute">No caveats recorded for this measure.</p>
          )}
          {v.sign_note && <p>{v.sign_note}</p>}
        </div>
        <div className="space-y-3">
          <p>
            <span className="text-paper">Source:</span> {src.citation || src.short}
            {src.edition ? ` (${src.edition})` : ''}. {url && <ExternalLink href={url}>{src.publisher ?? src.short}</ExternalLink>}
          </p>
          <p>
            <span className="text-paper">Licence:</span> {src.licence === 'attribution' ? 'open, with attribution' : 'open'}
            {src.attribution ? `. ${src.attribution}` : ''}
          </p>
          {src.transform && (
            <p>
              <span className="text-paper">How the export handles it:</span> {src.transform}
            </p>
          )}
          {src.issues && (
            <p>
              <span className="text-paper">Known issues:</span> {src.issues}
            </p>
          )}
          <div>
            <p className="text-paper">Used in the indices</p>
            {v.feeds.length ? (
              <p>
                The country indices use this measure in {v.feeds.map(feedWords).join('; ')}
                {v.feeds.every((f) => /^\s*(ES|EO)\s*:/.test(f)) ? ', as S-curved ranks within each sector' : v.feeds.some((f) => /^\s*(ES|EO)\s*:/.test(f)) ? ' (the Energy Security and Economic Opportunity inputs as S-curved ranks within each sector)' : ' (see Methods for how it enters)'}. No index values are shown here.
              </p>
            ) : (
              <p>Not used by the country indices: shown here as context.</p>
            )}
          </div>
        </div>
      </div>
    </section>
  );
}
