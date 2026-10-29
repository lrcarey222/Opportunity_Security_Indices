// "Strategic posture" of the Country profile (design L2, 2026-10-29): the first section of the sections
// column, after the "at a glance" tiles (whose first tile carries the Secure / Compete / Deploy counts).
// A real Section (id "posture", the h2 the section nav links to) with the posture basis tag and intro,
// and the posture module's CountryPostureCard (heading level 3) inside it. The card reads the posture
// bundle (scripts/91), not the fact sheet: the framework's thresholds are fixed, so neither the
// Perspective dials nor the AIB scenario change it. The open node is held on the page (ProfileCtx.pcell),
// so it survives the model mounting (the body remounts inside EngineScope); a node's drawer links to its
// six-stage Sector profile with the economy highlighted, at that profile's Strategic posture section.
// Economies without a posture keep the section: the card shows its "No posture" notice.

import { CountryPostureCard, CountryPostureIntro, PostureBasisTag } from '../../../posture/index.ts';
import { postureSectorLink } from '../../derive/links.ts';
import { cap, proseName } from '../../derive/text.ts';
import { Section, useProfile } from './common.tsx';

export function PostureSection() {
  const { sheet, pcell, setPcell } = useProfile();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  return (
    <Section id="posture" title="Strategic posture" basis={<PostureBasisTag />} intro={<CountryPostureIntro name={name} />}>
      <CountryPostureCard
        iso3={iso}
        name={cap(name)}
        {...(setPcell ? { selected: pcell ?? null, onSelect: setPcell } : {})}
        sectorHref={(tech, stage) => postureSectorLink(tech, stage, iso)}
      />
    </Section>
  );
}
