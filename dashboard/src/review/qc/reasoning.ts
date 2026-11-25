// The reasoning reveal (UX v2; NEIS brief sections 7-8; docs/qc_audit/QC_EVIDENCE_CATALOGUE.md 2D, 3D, 4.2, 5D,
// 6D): shown only AFTER the blind answer. For every case it states the Explorer's conclusion in one sentence, the
// analytical chain step by step (a level word, the value against its bar, one plain sentence and the drivers),
// the "Therefore" rule that was actually applied, and at least one neighbouring alternative with why it was not
// chosen, what would have to change for it, and the margin. Alternatives are computed, never authored: posture
// cells replay the canonical choice rule (qc/choiceRule.ts spChoiceRule) with one input flipped at a time; AIB
// nodes replay the class rule (R aib_classify_nodes) with one input moved across its bar and add the shipped
// variants; compacts replay the materiality rule (R aib_strategic missions: 25% of the lost supply or US$1bn);
// sector chains compare the lead stage with the other stages and the chokepoint view; partnerships read the
// memo's four boxes. technical_path is the Explorer deep link (Level 3). Pure: no IO.

import { tradeWhen, type AibDoc, type AibNode, type AibStrategicCompactScenario } from '../../aib/contract.ts';
import { AIB_CLASS_NAME, CHOICE_ACTION, CHOICE_NAME, RATIONALE_NAME } from '../../content/vocabulary.ts';
import { DEPLOYMENT_STAGE, isSupplyStage, STAGE_DISPLAY, type SupplyStage } from '../../lib/stages.ts';
import type { PostureCell, SecurityRationale, Verb } from '../../posture/contract.ts';
import { plainReason, type NodeDetail } from '../../posture/detail.ts';
import { largeYardOf, splitSignals } from '../../posture/yards.ts';
import type { AlternativeExplanation, AibScenarioKey, CaseDefinition, CaseReasoning, CaseSnapshot, ChainStep, ReasoningStep } from '../model.ts';
import {
  choiceOptionLabel,
  conclusionLabel,
  NO_STAGE,
  NO_STAGE_LABEL,
  PARTNERSHIP_BOX_LABEL,
  STRATEGIC_COMPACT_DECISION_LABEL,
  type PartnershipBoxKind,
  type StrategicCompactDecision,
} from './answerOptions.ts';
import type { DecodedBundles } from './bundles.ts';
import { cappedNodes, cpHighUnder, decisionPath, extractChoiceInputs, spChoiceRule, thresholdsFromCore, type Choice, type ChoiceInputs, type ChoiceThresholds } from './choiceRule.ts';
import { countryNames, nn, parsePrizeNote, annualRate, pct, productionFlagCore, usd, cleanNote } from './evidence.ts';
import { chainAnalysis, INTERVENTION_CLASSES, type ChainAnalysis } from './sectorChain.ts';
import { memoChoices } from './snapshot.ts';

/** English possessive: "Japan's", "United States'". */
const poss = (n: string): string => (n.endsWith('s') ? n + "'" : n + "'s");
const f3 = (v: number | null): string => (v === null ? 'not measured' : v.toFixed(3));
const signed3 = (v: number): string => `${v >= 0 ? '+' : '−'}${Math.abs(v).toFixed(3)}`;
const choiceLabel = (c: string): string => (c === 'not_assessed' ? conclusionLabel('posture_cell', c) : choiceOptionLabel(c as Verb));
const aibLabel = (c: string): string => AIB_CLASS_NAME[c as keyof typeof AIB_CLASS_NAME] ?? c;

// ---- posture cells -------------------------------------------------------------------------------------------------

interface Levels {
  secure: boolean;
  hn: boolean;
  hp: boolean;
  ch: boolean;
  pw: string | null;
  ly: boolean;
  dn: boolean;
}

function ruleOf(inp: ChoiceInputs, lv: Levels, th: Pick<ChoiceThresholds, 'positionFirst'>): Choice {
  const dep = inp.stage === DEPLOYMENT_STAGE;
  return spChoiceRule(
    {
      stage: inp.stage,
      secure: lv.secure,
      need: inp.need === null ? null : lv.hn ? 1 : 0,
      prize: inp.prize === null ? null : lv.hp ? 1 : 0,
      cpHigh: lv.ch,
      pathway: lv.pw,
      dn: dep ? (inp.dn === null ? null : lv.dn ? 1 : 0) : null,
      large: lv.ly,
      capped: false,
    },
    { need: 1, prize: 1, deployment: 1, positionFirst: th.positionFirst },
  );
}

const ROUTE_WORDS: Readonly<Record<string, string>> = {
  coercive_chokepoint:
    'one rival supplier holds half or more of world supply, a quarter or more of imports come from rival suppliers, substitution is hard and trusted supply could not be restored in time',
  operational_control: 'a control-layer or dual-use step (enrichment, isotope separation, rare-earth separation, magnets) with a quarter or more of imports from rival suppliers',
  systemic_resilience: 'an essential function, dependent on imports, taking a year or more to rebuild, where trusted partners hold under half of world supply',
};

const FAMILY_WORDS: Readonly<Record<string, string>> = {
  importDependence: 'import dependence',
  supplierConcentration: 'concentration of its own suppliers',
  riskyExposure: 'imports from China, Russia and the other UN-voting group',
  trustedAlternatives: 'scarcity of same-group alternative suppliers',
};

function postureReasoning(def: Pick<CaseDefinition, 'key' | 'explorer_path'>, b: DecodedBundles, s: CaseSnapshot): CaseReasoning | null {
  const p = b.posture;
  const { iso3 = '', tech = '', stage = '' } = def.key;
  const c: PostureCell | undefined = p?.core.cells.find((x) => x.iso3 === iso3 && x.tech === tech && x.stage === stage);
  const d: NodeDetail | undefined = p?.details[iso3]?.cells.find((x) => x.tech === tech && x.stage === stage);
  if (!p || !c || !d) return null;
  const name = countryNames(b)(iso3);
  const meta = p.core.meta;
  const th = thresholdsFromCore(p.core);
  const capped = cappedNodes(p.core).get(`${tech}|${stage}`) ?? null;
  const lyc = largeYardOf(meta, tech, stage, d);
  const yf = meta.yf_nodes[`${tech}|${stage}|stage`] ?? null;
  const inp = extractChoiceInputs(d, stage, { capped, cpThreshold: th.cp });
  const shipped = s.conclusion.value as Choice;
  const path = decisionPath(inp, shipped);
  const dep = stage === DEPLOYMENT_STAGE;
  const stageName = isSupplyStage(stage) ? STAGE_DISPLAY[stage] : stage;
  const phrase = meta.phrases[`${tech}|${stage}`] ?? `${tech} ${stageName}`;
  const t = inp.traced;
  const chKnown = cpHighUnder(inp, th);
  const lv: Levels = {
    secure: inp.secure,
    hn: t.need === 'high' || t.need === 'low' ? t.need === 'high' : inp.need !== null && inp.need >= th.need,
    hp: t.prize === 'high' || t.prize === 'low' ? t.prize === 'high' : inp.prize !== null && inp.prize >= th.prize && !inp.capped,
    ch: t.position !== null ? t.position === 'high' : chKnown === true,
    pw: inp.pathway,
    ly: inp.large === true,
    dn: t.deployment === 'high' || t.deployment === 'low' ? t.deployment === 'high' : inp.dn !== null && inp.dn >= th.deployment,
  };
  const base = shipped === 'not_assessed' ? 'not_assessed' : ruleOf(inp, lv, th);
  const steps: ReasoningStep[] = [];
  const sc = d.supplyChain;

  // ---- steps
  const secStep = d.trace.find((x) => x.test === 'secure_override');
  if (secStep) {
    const routes = d.routes.length ? d.routes : typeof secStep.value === 'string' ? secStep.value.split(';').filter(Boolean) : [];
    steps.push({
      id: 'secure', label: 'Security test', level: inp.secure ? 'Yes' : 'No', value: null, threshold: null,
      sentence: inp.secure
        ? `An acute vulnerability was found through the ${routes.map((r) => RATIONALE_NAME[r as SecurityRationale]?.toLowerCase() ?? r).join(' and ')} route${routes.length > 1 ? 's' : ''}: ${routes.map((r) => ROUTE_WORDS[r] ?? r).join('; ')}.`
        : `None of the three security routes found an acute vulnerability, so ${dep ? 'deployment need decides' : 'value and position decide'}.`,
      drivers: inp.secure
        ? [
            sc?.coerciveTop ? `${countryNames(b)(sc.coerciveTop)} holds ${pct(sc.coerciveTopShare)} of world supply (route line: half)` : null,
            sc?.coerciveImportShare !== null && sc?.coerciveImportShare !== undefined ? `${pct(sc.coerciveImportShare)} of ${poss(name)} imports come from rival suppliers (route line: a quarter)` : null,
            sc?.trustedShare !== null && sc?.trustedShare !== undefined ? `same-group economies hold ${pct(sc.trustedShare)} of world supply (route line: half)` : null,
            sc?.substitutability ? `substitutability: ${sc.substitutability} (curated)` : null,
            sc?.reconstitution ? `time to rebuild: ${sc.reconstitution.replace('m6to12', '6 to 12 months').replace('gt3y', 'more than 3 years').replace('y1to3', '1 to 3 years')} (curated)` : null,
            'The consequence, qualification-time and stockpile legs of the test are not in this release, so the full test cannot be replayed here.',
          ].filter((x): x is string => !!x)
        : [],
    });
  }
  if (!dep) {
    const ly = d.trace.find((x) => x.test === 'large_yard_case');
    if (ly) {
      steps.push({
        id: 'large_yard', label: 'Commodity test', level: inp.large === true ? 'Yes' : inp.large === false ? 'No' : 'Undecided', value: inp.largeN, threshold: inp.largeMin,
        sentence: `${inp.largeN ?? 'n/a'} commodity signals hold; the test needs ${inp.largeMin ?? 5}${inp.large === true && path === 'large_yard' ? ', so the step is treated as a commodity unless the position is top-quarter' : ''}.`,
        drivers: lyc
          ? [
              `required signals that hold (all are needed; not counted): ${splitSignals(lyc.signals).required.join(', ').replace(/_/g, ' ') || 'none'}`,
              `counted signals that hold (${lyc.n ?? 'n/a'} of ${lyc.bar ?? inp.largeMin ?? 5} needed): ${splitSignals(lyc.signals).commodity.join(', ').replace(/_/g, ' ') || 'none'}`,
              `not observed: ${lyc.unknown.join(', ').replace(/_/g, ' ') || 'none'}`,
            ]
          : [],
      });
    }
    const need = inp.need;
    const fams: [string, number | null][] = sc ? [['importDependence', sc.importDependence], ['supplierConcentration', sc.supplierConcentration], ['riskyExposure', sc.riskyExposure], ['trustedAlternatives', sc.trustedAlternatives]] : [];
    // self-supply guard (R/strategic_position/supply_chain_need.R): at 10% or more of world supply the
    // import-composition families carry no need, so only import dependence can drive the score
    const selfSupplied = (inp.home_share ?? 0) >= 0.1;
    const scored = (k: string) => !selfSupplied || k === 'importDependence';
    const topFam = [...fams].filter(([k, v]) => v !== null && scored(k)).sort((x, y) => (y[1] ?? 0) - (x[1] ?? 0))[0];
    const needWord = need === null ? 'Not measured' : need === 0.61 ? 'High (capped by the score floor)' : need >= th.need ? 'High' : need >= 0.33 ? 'Moderate' : 'Low';
    steps.push({
      id: 'need', label: 'Need: import vulnerability', level: needWord, value: need, threshold: th.need,
      sentence: need === null
        ? 'Import vulnerability is not measured for this step.'
        : `${name} ranks above ${Math.min(99, Math.round(need * 100))}% of economies on import vulnerability for this step${topFam ? `, driven most by ${FAMILY_WORDS[topFam[0]]}` : ''}${selfSupplied ? '; its import mix is not scored because it holds 10% or more of world supply' : ''}.${path === 'secure_override' || path === 'large_yard' ? ' (Not used: an earlier test decided.)' : ''}`,
      drivers: [
        ...fams.map(([k, v]) => `${FAMILY_WORDS[k]}: ${v === null ? 'not measured' : `rank ${f3(v)}`}${v !== null && !scored(k) ? ' (not scored: self-supply)' : ''}`),
        sc?.topSupplier ? `largest supplier ${countryNames(b)(sc.topSupplier)} ${pct(sc.topSupplierShare)} of imports` : null,
        sc?.importsToGdp !== null && sc?.importsToGdp !== undefined ? `gross imports ${(sc.importsToGdp * 100).toFixed(3)}% of GDP` : null,
      ].filter((x): x is string => !!x),
    });
    const prize = inp.prize;
    const out = parsePrizeNote(d.prize.note);
    const prizeWord = prize === null ? 'Not measured' : inp.capped ? 'Held below the bar: market size alone' : prize >= th.prize ? 'High' : prize >= 0.4 ? 'Moderate' : 'Low';
    steps.push({
      id: 'prize', label: 'Prize: value of the industry', level: prizeWord, value: prize, threshold: th.prize,
      sentence: prize === null
        ? 'The value of this step is not measured.'
        : `${out && out.totalPct !== null ? `World demand grows about ${annualRate(out.totalPct, out.toYear - out.fromYear).toFixed(1)}% a year to ${out.toYear}` : 'World demand outlook not stated'}; industrial know-how scores ${f3(yf?.commons ?? null)} and future option ${f3(yf?.option ?? null)}${inp.capped ? ', neither reaching 0.5, so market size alone holds the value below the bar' : ''}.${path === 'secure_override' || path === 'large_yard' ? ' (Not used: an earlier test decided.)' : ''}`,
      drivers: [
        `market part ${f3(d.prize.market)}`,
        `know-how (industrial commons) ${f3(yf?.commons ?? null)}`,
        `future option ${f3(yf?.option ?? null)}`,
        d.prize.homeMarket !== null ? `home-market growth ${f3(d.prize.homeMarket)} (can only lift the value)` : null,
        d.prize.investmentMomentum !== null ? `investment momentum ${f3(d.prize.investmentMomentum)}` : null,
      ].filter((x): x is string => !!x),
    });
    const cp = inp.cp;
    const posRes = t.position;
    const posWord = posRes === 'high' ? 'High (top quarter)' : posRes === 'not_top_quarter' ? 'Moderate (a contender below the top quarter)' : posRes === 'capability_floor_not_met' ? 'Top-quarter rank, below the capability floor' : posRes === 'not_a_contender' ? 'Low (under 0.5% of world supply)' : cp === null ? 'Not measured' : 'Not tested';
    const famSorted = [...d.cpFamilies].sort((x, y) => y.value - x.value);
    steps.push({
      id: 'position', label: 'Position: competitive standing', level: posWord, value: cp, threshold: th.cp,
      sentence: `${name} ranks ${f3(cp)} among the economies holding at least 0.5% of world supply; it holds ${pct(inp.home_share)} of world supply and ${pct(nn(d.trade?.benchmark_share))} of world exports (RCA ${nn(d.trade?.benchmark_rca)?.toFixed(2) ?? 'n/a'}).${posRes === null ? ' (Not used: an earlier test decided.)' : ''}`,
      drivers: famSorted.length ? [`strongest: ${famSorted[0].label} ${f3(famSorted[0].value)}`, `weakest: ${famSorted[famSorted.length - 1].label} ${f3(famSorted[famSorted.length - 1].value)}`, ...(famSorted.every((x) => x.id.startsWith('export_position')) ? ['read from trade only'] : [])] : [],
    });
    if (t.pathway !== null || (lv.hp && !lv.ch)) {
      steps.push({
        id: 'pathway', label: 'Credible base to build from', level: inp.pathway === 'yes' ? 'Yes' : inp.pathway === 'no' ? 'No' : 'Unknown', value: null, threshold: null,
        sentence: inp.pathway === 'yes' ? `${name} has a capability anchor plus a corroborating signal.` : `No capability anchor with a corroborating signal was found.`,
        drivers: d.pathway.basis.map((x) => x.replace(/_/g, ' ')),
      });
    }
  } else {
    const dp = d.deployment;
    const dn = inp.dn;
    const fams: [string, { value: number | null; note: string | null } | undefined][] = [['demand under current policies', dp?.demand], ['gap to 2030 national targets', dp?.policyGap], ['system constraints', dp?.system]];
    steps.push({
      id: 'deployment', label: 'Deployment need', level: dn === null ? 'Not measured' : dn >= th.deployment ? 'High' : dn >= 0.4 ? 'Moderate' : 'Low', value: dn, threshold: th.deployment,
      sentence: dn === null ? 'Deployment need is not measured.' : `Deployment need is ${f3(dn)} against the bar of ${th.deployment}${fams.some(([, f]) => !f || f.value === null) ? `; not measured: ${fams.filter(([, f]) => !f || f.value === null).map(([k]) => k).join(', ')}` : ''}.`,
      drivers: fams.map(([k, f]) => `${k}: ${f && f.value !== null ? f3(f.value) : 'not measured'}${f?.note ? ` (${cleanNote(f.note)})` : ''}`),
    });
  }

  // ---- the rule actually applied
  let rule: string;
  if (path === 'secure_override') {
    rule = `Before weighing value or position, the Explorer checks for an acute vulnerability. It found one, so the answer is ${CHOICE_NAME.secure} whatever the value or position.`;
  } else if (path === 'not_assessed') {
    rule = `The Explorer makes no call: ${plainReason(d.notAssessedReason) ?? 'an input is not measured'}. A call needs trade data for at least 5 comparable importers${dep ? ', or a deployment-need series' : ''}.`;
  } else if (path === 'deployment') {
    rule = `At Deployment & Use the only question is whether the energy system needs much more of the technology: deployment need at or above ${th.deployment} means ${CHOICE_NAME.accelerate}, below means ${choiceOptionLabel('watch')}. It says nothing about where equipment is made.`;
  } else if (path === 'large_yard') {
    rule = `This step looks like a commodity: ${inp.largeN ?? 'n/a'} commodity signals hold (${inp.largeMin ?? 5} needed). The Explorer then prefers affordable, diversified access (${choiceOptionLabel('watch')}) unless ${name} already ranks in the top quarter of producers.`;
  } else if (lv.hp && lv.ch) rule = `High value and a top-quarter position: keep and strengthen it (${CHOICE_NAME.preserve}).`;
  else if (lv.hp && lv.pw === 'yes') rule = `High value, a position below the top quarter, and a credible base to build from: build it (${CHOICE_NAME.catch_up}).`;
  else if (lv.hp && lv.hn) rule = `Valuable and vulnerable, but no strong position and no credible domestic base: rely on trusted partners (${CHOICE_NAME.partner}).`;
  else if (lv.hp) rule = `Valuable, but neither vulnerable nor positioned to build: keep it open (${choiceOptionLabel('watch')}).`;
  else if (lv.hn && lv.ch) rule = `Vulnerable and already strong, though the value is not high: keep it (${CHOICE_NAME.preserve}).`;
  else if (lv.hn) rule = `Vulnerable, without high value or a strong position: rely on trusted partners or diversified supply (${CHOICE_NAME.partner}).`;
  else rule = `Neither vulnerable nor high value: keep it open (${choiceOptionLabel('watch')}); a strong position alone does not justify intervention.`;
  rule = `${rule} The rule takes the first test that matches, in the order shown.`;

  // ---- alternatives: flip one input at a time
  type Flip = { input: string; alt: Choice; dist: number; why: string; need: string; margin: string };
  const flips: Flip[] = [];
  const tryFlip = (input: string, patch: Partial<Levels>, dist: number, why: string, need: string, margin: string) => {
    if (shipped === 'not_assessed') return;
    const alt = ruleOf(inp, { ...lv, ...patch }, th);
    if (alt !== base) flips.push({ input, alt, dist, why, need, margin });
  };
  if (inp.secure) {
    // a Secure trace stops before the commodity test: read the economy's own case, else the node's (meta.yf_nodes)
    const lyNode = inp.large !== undefined ? inp.large === true : (d.largeYard?.case ?? yf?.largeYard ?? false) === true;
    const tri = ruleOf(inp, { ...lv, secure: false, hn: inp.need !== null && inp.need >= th.need, hp: inp.prize !== null && inp.prize >= th.prize && !inp.capped, ch: chKnown === true, ly: lyNode }, th);
    flips.push({
      input: 'secure', alt: tri, dist: 0,
      why: `The security test found an acute vulnerability, and that test comes first.`,
      need: `${choiceLabel(tri)} would follow if the security test found no acute vulnerability (for example if rival suppliers fell below a quarter of ${poss(name)} imports, or trusted supply rose toward half of world supply); the rest of the rule would then give ${choiceLabel(tri)}.`,
      margin: [sc?.coerciveImportShare != null ? `rival suppliers ${pct(sc.coerciveImportShare)} of imports vs a quarter` : null, sc?.trustedShare != null ? `same-group world supply ${pct(sc.trustedShare)} vs half` : null].filter(Boolean).join('; ') || 'route legs not in this release',
    });
  }
  if (!dep && !inp.secure) {
    if (inp.need !== null)
      tryFlip('need', { hn: !lv.hn }, Math.abs(inp.need - th.need),
        `Import vulnerability is ${lv.hn ? 'high' : 'not high'} (${f3(inp.need)} against ${th.need}).`,
        `import vulnerability ${lv.hn ? `fell below ${th.need}` : `reached ${th.need}`} (now ${f3(inp.need)})`,
        `Need ${f3(inp.need)} vs bar ${th.need} (${signed3(inp.need - th.need)})`);
    if (inp.prize !== null)
      tryFlip('prize', { hp: !lv.hp }, inp.capped && !lv.hp ? 0.5 : Math.abs(inp.prize - th.prize),
        inp.capped && !lv.hp ? 'The value is held below the bar because only market size supports it.' : `The value of the step is ${lv.hp ? 'high' : 'not high'} (${f3(inp.prize)} against ${th.prize}).`,
        inp.capped && !lv.hp ? 'the value were not capped as "market size alone" (industrial know-how or a future option scoring 0.5 or more)' : `the value of the step ${lv.hp ? `fell below ${th.prize}` : `reached ${th.prize}`} (now ${f3(inp.prize)})`,
        inp.capped && !lv.hp ? `Prize ${f3(inp.prize)}: capped (know-how ${f3(yf?.commons ?? null)}, option ${f3(yf?.option ?? null)} vs 0.5)` : `Prize ${f3(inp.prize)} vs bar ${th.prize} (${signed3(inp.prize - th.prize)})`);
    if (inp.cp !== null || t.position !== null) {
      const floor = t.position === 'capability_floor_not_met';
      const nonc = t.position === 'not_a_contender' || (inp.home_share !== null && inp.home_share < 0.005);
      tryFlip('position', { ch: !lv.ch }, floor ? 0.02 : nonc ? (Math.abs((inp.home_share ?? 0) - 0.005) < 0.001 ? 0.01 : 0.5) : Math.abs((inp.cp ?? 0) - th.cp),
        floor ? `${name} ranks in the top quarter (${f3(inp.cp)}) but its capability case is below the floor.` : `${poss(name)} position is ${lv.ch ? 'top-quarter' : 'below the top quarter'} (${f3(inp.cp)} against ${th.cp}).`,
        floor ? `${poss(name)} capability case met the floor (it already ranks ${f3(inp.cp)})` : nonc ? `${name} held at least 0.5% of world supply (now ${pct(inp.home_share)})` : `${name} ranked ${lv.ch ? 'below' : 'in'} the top quarter of contenders (now ${f3(inp.cp)} against ${th.cp})`,
        floor ? `Position ${f3(inp.cp)}: top-quarter rank, capability floor not met` : `Position ${f3(inp.cp)} vs bar ${th.cp} (${signed3((inp.cp ?? 0) - th.cp)})`);
    }
    // a pathway resting on a world-supply share that sits on the 0.5% anchor line is as fragile as a value on a bar
    const onAnchor = inp.home_share !== null && Math.abs(inp.home_share - 0.005) < 0.0005 && d.pathway.basis.includes('direct_supply_share');
    tryFlip('pathway', { pw: lv.pw === 'yes' ? 'no' : 'yes' }, onAnchor ? 0.0001 : 0.3,
      `${lv.pw === 'yes' ? 'A' : 'No'} credible base to build from was found${onAnchor ? `; one of its anchors is a ${pct(inp.home_share, 2)} share of world supply, on the 0.5% line` : ''}.`,
      lv.pw === 'yes' ? `no credible base to build from existed${onAnchor ? ` (for example if ${poss(name)} share of world supply fell below 0.5% and no other anchor held)` : ''}` : 'a credible base to build from existed (a capability anchor plus one corroborating signal)',
      onAnchor ? `world-supply share ${pct(inp.home_share, 2)} vs the 0.5% anchor line` : `Pathway: ${inp.pathway ?? 'unknown'}`);
    if (inp.large !== undefined)
      tryFlip('large_yard', { ly: !lv.ly }, lv.ly ? 0.05 : Math.max((inp.largeMin ?? 5) - (inp.largeN ?? 0), 0.5) * 0.05,
        `${inp.largeN ?? 'n/a'} commodity signals hold (${inp.largeMin ?? 5} needed): the step is ${lv.ly ? '' : 'not '}treated as a commodity.`,
        lv.ly ? `fewer than ${inp.largeMin ?? 5} commodity signals held (now ${inp.largeN ?? 'n/a'})` : `${inp.largeMin ?? 5} or more commodity signals held (now ${inp.largeN ?? 'n/a'})`,
        `Commodity signals ${inp.largeN ?? 'n/a'} of ${inp.largeMin ?? 5} needed`);
  }
  if (dep && inp.dn !== null)
    tryFlip('deployment', { dn: !lv.dn }, Math.abs(inp.dn - th.deployment),
      `Deployment need is ${lv.dn ? 'at or above' : 'below'} the bar (${f3(inp.dn)} against ${th.deployment}).`,
      `deployment need ${lv.dn ? `fell below ${th.deployment}` : `reached ${th.deployment}`} (now ${f3(inp.dn)})`,
      `Deployment need ${f3(inp.dn)} vs bar ${th.deployment} (${signed3(inp.dn - th.deployment)})`);
  // not assessed: what a call would be with the missing input measured
  if (shipped === 'not_assessed') {
    const missingNeed = dep ? inp.dn === null : inp.need === null;
    const which = dep ? 'deployment need' : missingNeed ? 'import vulnerability' : 'the value of the step';
    for (const hi of [true, false]) {
      const patch: Partial<Levels> = dep ? { dn: hi } : missingNeed ? { hn: hi } : { hp: hi };
      const fake: ChoiceInputs = { ...inp, need: !dep && missingNeed ? 0 : inp.need, prize: !dep && !missingNeed && inp.prize === null ? 0 : inp.prize, dn: dep ? 0 : inp.dn };
      const alt = ruleOf(fake, { ...lv, ...patch }, th);
      if (!flips.some((f) => f.alt === alt))
        flips.push({ input: which, alt, dist: hi ? 0 : 1, why: `The Explorer makes no call because ${which} is not measured.`, need: `${which} were measured ${hi ? 'at or above' : 'below'} its bar`, margin: `${which}: not measured` });
    }
  }
  const possible = (d.choicePossible ?? '').split('|').filter((x) => x && x !== shipped);
  // the engine's own pivotal input comes first
  const PIVOT: Readonly<Record<string, string>> = { large_yard_case: 'large_yard', cp_contender_pct: 'position', supply_chain_need: 'need', strategic_industrial_value: 'prize', deployment_need: 'deployment', pathway_credible: 'pathway' };
  const pivot = d.pivotal ? PIVOT[d.pivotal] : null;
  for (const f of flips) if (pivot && f.input === pivot) f.dist = -1;
  const best = new Map<string, Flip>();
  for (const f of flips) {
    const cur = best.get(f.alt);
    if (!cur || f.dist < cur.dist) best.set(f.alt, f);
  }
  const ordered = [...best.values()].sort((x, y) => Number(possible.includes(y.alt)) - Number(possible.includes(x.alt)) || x.dist - y.dist);
  const alternatives: AlternativeExplanation[] = ordered.slice(0, 3).map((f) => ({
    value: f.alt,
    label: choiceLabel(f.alt),
    why_not: `${f.why} So the rule gives ${choiceLabel(shipped)}, not ${choiceLabel(f.alt)}.`,
    would_need: f.need.startsWith(choiceLabel(f.alt)) ? f.need : `${choiceLabel(f.alt)} would follow if ${f.need}.`,
    margin: f.margin,
  }));
  const summary =
    path === 'secure_override'
      ? `${poss(name)} ${phrase}: an acute vulnerability, so ${CHOICE_ACTION.secure.toLowerCase()}.`
      : path === 'not_assessed'
        ? `${poss(name)} ${phrase}: the data cannot support a call.`
        : `${poss(name)} ${phrase}: ${CHOICE_ACTION[shipped as Verb]?.toLowerCase() ?? shipped}.`;
  return {
    conclusion: { value: shipped, label: s.conclusion.label, summary },
    steps,
    rule,
    alternatives,
    compact_chain: null,
    partnership: null,
    sector: null,
    technical_path: def.explorer_path,
  };
}

// ---- AIB nodes ---------------------------------------------------------------------------------------------------------

interface AibInputs {
  ok: boolean;
  exposure: number | null;
  residual: number | null;
  largest: number | null;
  top3: number | null;
  loss: number | null;
  existing: number | null;
  dividend: number | null;
}

/** R aib_classify_nodes, first match wins; returns the class and the branch that decided it. */
export function aibClassRule(x: AibInputs, t: Record<string, number>): { cls: string; branch: string } {
  const concentrated = (x.largest ?? 0) >= t.manage_largest_share || (x.top3 ?? 0) >= (t.manage_top3_share ?? Infinity) || (x.loss ?? 0) >= t.manage_coverage_loss;
  if (!x.ok || x.exposure === null) return { cls: 'INSUFFICIENT DATA', branch: 'insufficient' };
  if (x.exposure < t.low_exposure) return { cls: 'LOW INTERVENTION', branch: 'low_exposure' };
  if ((x.residual ?? 0) >= t.build_gap) return { cls: 'BUILD', branch: 'build_gap' };
  if (concentrated) return { cls: 'MANAGE SPECIALIZATION', branch: 'concentrated' };
  if ((x.existing ?? 0) >= t.adequate_coverage) return { cls: 'LOW INTERVENTION', branch: 'adequate_existing' };
  if ((x.dividend ?? 0) >= t.coordinate_dividend) return { cls: 'COORDINATE', branch: 'dividend' };
  return { cls: 'LOW INTERVENTION', branch: 'default_low' };
}

function aibInputs(n: AibNode, scenario: AibScenarioKey): AibInputs {
  if (scenario === 'managed') {
    const dv = n.diversify;
    return { ok: !!dv && dv.cls !== 'INSUFFICIENT DATA', exposure: nn(dv?.excess), residual: nn(dv?.residual), largest: nn(dv?.largestShare), top3: nn(dv?.top3Share), loss: null, existing: nn(dv?.existing), dividend: nn(dv?.dividend) };
  }
  const f1 = n.frontier.find((p) => p.cap === 1) ?? null;
  const f4 = n.frontier.find((p) => Math.abs(p.cap - 0.4) < 1e-9) ?? null;
  const loss = f1 && f4 && nn(f1.coverage) !== null && nn(f4.coverage) !== null ? (f1.coverage as number) - (f4.coverage as number) : null;
  return { ok: n.cls !== 'INSUFFICIENT DATA', exposure: nn(n.exposure), residual: nn(n.residual), largest: nn(n.largestShare), top3: null, loss, existing: nn(n.existing), dividend: nn(n.dividend) };
}

const VARIANT_WORDS: Readonly<Record<string, string>> = {
  china_50: 'China supply halved',
  largest_subsector: 'the largest supplier of the product removed',
  largest_stage: 'the largest supplier of the stage removed',
  largest_technology: 'the largest supplier of the technology removed',
  diversify_benchmark: 'Managed Interdependence',
  conservative: 'a conservative supply scenario (no redirection of exports)',
  release: 'China also stops buying (two-way cut)',
  production_constrained: 'supply limited by production data',
  trade_only: 'trade data only (no production limit)',
};

function aibNodeReasoning(def: Pick<CaseDefinition, 'key' | 'explorer_path'>, b: DecodedBundles, s: CaseSnapshot): CaseReasoning | null {
  const doc = b.aib?.doc;
  const n = doc?.nodes.find((x) => x.id === def.key.node);
  if (!doc || !n) return null;
  const scenario = def.key.scenario ?? 'decoupling';
  const t = doc.meta.thresholds;
  const x = aibInputs(n, scenario);
  const name = countryNames(b);
  const applied = aibClassRule(x, t);
  const shipped = s.conclusion.value;
  const largestWho = scenario === 'managed' ? n.diversify?.largest : n.largest;
  const steps: ReasoningStep[] = [
    {
      id: 'exposure', label: scenario === 'managed' ? 'Supply that has to shift' : 'Supply exposed to the disruption', value: x.exposure, threshold: t.low_exposure,
      level: x.exposure === null ? 'Not measured' : x.exposure >= t.high_severity ? 'High' : x.exposure >= t.low_exposure ? 'Moderate' : 'Low',
      sentence: `${pct(x.exposure)} of allied supply ${scenario === 'managed' ? 'has to shift under the cap' : 'is lost'}; below ${pct(t.low_exposure, 0)} the product needs little intervention whatever else holds.`,
      drivers: [`allied imports ${usd(nn(n.demandUsd))} a year`, `${scenario === 'managed' ? 'to shift' : 'lost'}: ${usd(scenario === 'managed' ? nn(n.diversify?.lostUsd) : nn(n.lostUsd))} a year`],
    },
    {
      id: 'gap', label: 'Gap left after coordinated trade', value: x.residual, threshold: t.build_gap,
      level: x.residual === null ? 'Not measured' : x.residual >= t.build_gap ? 'Material' : 'Small',
      sentence: `After coordinated trade (new and expanded links), ${pct(x.residual)} of allied demand is still unmet; ${pct(t.build_gap, 0)} or more means new capacity is required.`,
      drivers: [`existing links restore ${pct(x.existing)}`, `coordinated trade adds ${pct(x.dividend)}`],
    },
    {
      id: 'concentration', label: 'Concentration after coordinated trade', value: x.largest, threshold: t.manage_largest_share,
      level: (x.largest ?? 0) >= t.manage_largest_share || (x.top3 ?? 0) >= t.manage_top3_share || (x.loss ?? 0) >= t.manage_coverage_loss ? 'Concentrated' : 'Not concentrated',
      sentence: `The largest supplier after coordinated trade (${name(largestWho ?? null)}) holds ${pct(x.largest)}; at ${pct(t.manage_largest_share, 0)} or more (or three suppliers at ${pct(t.manage_top3_share, 0)}, or a ${pct(t.manage_coverage_loss, 0)} coverage cost of capping suppliers at ${pct(t.manage_cap, 0)}) the reliance needs managing.`,
      drivers: [x.top3 === null ? 'top-3 share after the shock is not in this release' : `top three after: ${pct(x.top3)}`, x.loss === null ? 'coverage cost of a 40% cap: not in this release' : `coverage cost of a 40% supplier cap: ${pct(x.loss)}`],
    },
    {
      id: 'coverage', label: 'Existing links alone', value: x.existing, threshold: t.adequate_coverage,
      level: x.existing === null ? 'Not measured' : x.existing >= t.adequate_coverage ? 'Adequate' : 'Not enough',
      sentence: `Existing links (each at most doubling) restore ${pct(x.existing)} of allied demand; ${pct(t.adequate_coverage, 0)} or more is treated as adequate.`,
      drivers: [],
    },
    {
      id: 'dividend', label: 'Gain from coordinating trade', value: x.dividend, threshold: t.coordinate_dividend,
      level: x.dividend === null ? 'Not measured' : x.dividend >= t.coordinate_dividend ? 'Material' : 'Small',
      sentence: `New or expanded relationships add ${pct(x.dividend)} of coverage over existing links; ${pct(t.coordinate_dividend, 0)} or more is material.`,
      drivers: n.partners ? [`largest new or expanded links: ${n.partners}`] : [],
    },
  ];
  const BRANCH_RULE: Record<string, string> = {
    insufficient: 'The trade data are too thin for the analysis, so no response type is given.',
    low_exposure: `The disruption removes under ${pct(t.low_exposure, 0)} of allied supply, so the product needs little intervention.`,
    build_gap: `Exposed, and coordinated trade still leaves a gap of ${pct(t.build_gap, 0)} or more: new capacity is required.`,
    concentrated: 'Exposed, trade can close the gap, but the solution leans on one or a few suppliers: keep the specialisation and manage the reliance.',
    adequate_existing: `Exposed, but existing links alone restore ${pct(t.adequate_coverage, 0)} or more: little intervention is needed.`,
    dividend: `Exposed, existing links fall short, and coordinating new trade adds ${pct(t.coordinate_dividend, 0)} or more: coordinate trade.`,
    default_low: 'Exposed, but neither new capacity, concentration nor coordination adds enough: little intervention is needed.',
  };
  const rule = `The Explorer tests, in order: thin data; exposure under ${pct(t.low_exposure, 0)}; a gap of ${pct(t.build_gap, 0)} or more after coordinated trade; concentration after; existing links restoring ${pct(t.adequate_coverage, 0)}; a coordination gain of ${pct(t.coordinate_dividend, 0)}. The first that matches decides. Here: ${BRANCH_RULE[applied.branch]}`;
  // neighbours: each input moved just across its bar
  const moves: [keyof AibInputs, number, string][] = [
    ['exposure', t.low_exposure, 'exposure'],
    ['residual', t.build_gap, 'gap after coordinated trade'],
    ['largest', t.manage_largest_share, 'largest supplier after'],
    ['existing', t.adequate_coverage, 'existing-link coverage'],
    ['dividend', t.coordinate_dividend, 'coordination gain'],
  ];
  const alts = new Map<string, AlternativeExplanation & { dist: number }>();
  for (const [k, bar, words] of moves) {
    const v = x[k] as number | null;
    if (v === null) continue;
    const moved = v >= bar ? bar - 0.001 : bar;
    const r = aibClassRule({ ...x, [k]: moved }, t);
    if (r.cls === shipped || r.cls === 'INSUFFICIENT DATA') continue;
    const dist = Math.abs(v - bar);
    const cur = alts.get(r.cls);
    if (cur && cur.dist <= dist) continue;
    alts.set(r.cls, {
      value: r.cls, label: aibLabel(r.cls), dist,
      why_not: `The ${words} is ${pct(v)}, ${v >= bar ? 'at or above' : 'below'} the ${pct(bar, 0)} bar, so the rule gives ${aibLabel(shipped)}.`,
      would_need: `${aibLabel(r.cls)} would follow if the ${words} were ${v >= bar ? 'below' : 'at or above'} ${pct(bar, 0)} (now ${pct(v)}).`,
      margin: `${words} ${pct(v)} vs bar ${pct(bar, 0)}`,
    });
  }
  const variantNotes: string[] = [];
  for (const [k, cls] of Object.entries(n.variants ?? {})) {
    if (!cls || cls === shipped) continue;
    variantNotes.push(`${VARIANT_WORDS[k] ?? k}: ${aibLabel(cls)}`);
    if (!alts.has(cls) && cls !== 'INSUFFICIENT DATA')
      alts.set(cls, { value: cls, label: aibLabel(cls), dist: 1, why_not: `Under the headline settings the rule gives ${aibLabel(shipped)}.`, would_need: `${aibLabel(cls)} is what the Explorer finds under ${VARIANT_WORDS[k] ?? k}.`, margin: `variant: ${VARIANT_WORDS[k] ?? k}` });
  }
  for (const g of n.growth ?? []) if (g.cls && g.cls !== shipped && scenario === 'decoupling') variantNotes.push(`existing links growing ${g.label}: ${aibLabel(g.cls)}`);
  if (variantNotes.length) steps.push({ id: 'variants', label: 'Under other settings', level: 'Differs', value: null, threshold: null, sentence: 'The shipped variants that give another response type.', drivers: variantNotes });
  else steps.push({ id: 'variants', label: 'Under other settings', level: 'Same', value: null, threshold: null, sentence: 'Every shipped variant (other shocks, supply scenarios, growth bounds) gives the same response type.', drivers: [] });
  if (n.warning) steps.push({ id: 'physical', label: 'Production data check', level: 'Not validated', value: null, threshold: null, sentence: n.physicalValidation ?? 'Physical capability is not validated for this result.', drivers: (n.flags ?? []).filter((f) => /production/.test(f)) });
  const alternatives = [...alts.values()].sort((a, b2) => a.dist - b2.dist).slice(0, 3).map(({ dist: _d, ...a }) => a);
  if (!alternatives.length)
    alternatives.push({ value: shipped, label: aibLabel(shipped), why_not: 'No single input moved across its bar changes the response type.', would_need: 'Several inputs would have to change at once.', margin: null });
  return {
    conclusion: { value: shipped, label: s.conclusion.label, summary: `${n.label}${scenario === 'managed' ? ' under Managed Interdependence' : ' if China stops supplying'}: ${BRANCH_RULE[applied.branch].replace(/:.*$/, '').replace(/\.$/, '').toLowerCase()}.` },
    steps,
    rule,
    alternatives,
    compact_chain: null,
    partnership: null,
    sector: null,
    technical_path: def.explorer_path,
  };
}

// ---- compacts ----------------------------------------------------------------------------------------------------------

const M_SHARE = 0.25;
const M_USD = 1e9;
/** US$ in billions to three decimals (the materiality margin is often a few tens of millions). */
const usdBn = (v: number): string => `US$${(v / 1e9).toFixed(3)}bn`;

function compactReasoning(def: Pick<CaseDefinition, 'key' | 'explorer_path'>, b: DecodedBundles, s: CaseSnapshot): CaseReasoning | null {
  const doc = b.aib?.doc;
  const scenario = def.key.scenario ?? 'decoupling';
  const an = doc && def.key.sid ? chainAnalysis(doc, def.key.sid, scenario) : null;
  if (!doc || !an || !an.rec) return null;
  const rec: AibStrategicCompactScenario = an.rec;
  const name = countryNames(b);
  const shipped = s.conclusion.value as StrategicCompactDecision;
  const m = an.mission;
  const k = rec.counts;
  const lost = nn(rec.lostUsd);
  const steps: ReasoningStep[] = [
    {
      id: 'classes', label: 'What each product needs', level: `${k.build + k.coordinate + k.manage} of ${an.nodes.length} need action`, value: null, threshold: null,
      sentence: `Each product is classed with the stress-test rule: ${k.build} need new capacity, ${k.coordinate} coordinated trade, ${k.manage} managed reliance, ${k.low} little intervention${k.notStressed ? `, ${k.notStressed} not stressed` : ''}${k.insufficient ? `, ${k.insufficient} too thin to test` : ''}.`,
      drivers: an.nodes.filter((n) => INTERVENTION_CLASSES.includes(n.status)).map((n) => `${n.label}: ${aibLabel(n.status)} (${usd(n.lostUsd)})`),
    },
    {
      id: 'materiality', label: 'Size of the lead mission', level: !m ? 'No mission' : m.targeted ? 'Below the floor' : 'Material', value: m?.share ?? null, threshold: M_SHARE,
      sentence: m ? `The lead mission (${aibLabel(m.cls)}) carries ${pct(m.share)} of the chain's ${scenario === 'managed' ? 'supply to shift' : 'lost supply'} (${usd(m.lostUsd)}); it is material at ${pct(M_SHARE, 0)} or US$1bn.` : 'No product needs action, so there is no mission.',
      drivers: m ? [`share: ${pct(m.share)} vs ${pct(M_SHARE, 0)}`, `amount: ${usd(m.lostUsd)} vs US$1.0bn`, m.text] : [],
    },
    {
      id: 'coverage', label: 'Coverage of allied demand', level: 'Estimate', value: nn(rec.coverage.coordinated), threshold: null,
      sentence: `Right after the stress ${pct(nn(rec.coverage.static))}; with existing links ${pct(nn(rec.coverage.existing))}; with coordinated trade ${pct(nn(rec.coverage.coordinated))}; after simulated new capacity ${pct(nn(rec.coverage.postInvestment))}.`,
      drivers: ['After new capacity = coordinated trade + the simulated increment, capped at the gap: arithmetic by construction, not a finding.'],
    },
  ];
  const rule = `The Explorer classes every product of the chain, forms missions from the products needing action (new capacity, managed reliance, coordinated trade) ordered by the supply they carry, and asks whether the lead mission is material: ${pct(M_SHARE, 0)} of the chain's ${scenario === 'managed' ? 'supply to shift' : 'lost supply'} or US$1bn. Material: ${STRATEGIC_COMPACT_DECISION_LABEL.sectoral}. Action needed but below both: ${STRATEGIC_COMPACT_DECISION_LABEL.narrower}. No product needs action: ${STRATEGIC_COMPACT_DECISION_LABEL.none}.`;
  const alternatives: AlternativeExplanation[] = [];
  if (shipped === 'sectoral' && m) {
    const usdGap = m.lostUsd - M_USD;
    alternatives.push({
      value: 'narrower', label: STRATEGIC_COMPACT_DECISION_LABEL.narrower,
      why_not: `The lead mission carries ${pct(m.share)} of the ${scenario === 'managed' ? 'supply to shift' : 'lost supply'} and ${usd(m.lostUsd)}: ${(m.share ?? 0) >= M_SHARE ? `the share clears ${pct(M_SHARE, 0)}` : ''}${(m.share ?? 0) >= M_SHARE && m.lostUsd >= M_USD ? ' and ' : ''}${m.lostUsd >= M_USD ? 'the amount clears US$1bn' : ''}.`,
      would_need: `${STRATEGIC_COMPACT_DECISION_LABEL.narrower} would follow if the lead mission fell below both ${pct(M_SHARE, 0)} and US$1bn${m.lostUsd >= M_USD && (m.share ?? 0) < M_SHARE ? ` (it is ${usd(usdGap)} above the US$1bn line)` : ''}.`,
      margin: `share ${pct(m.share)} vs ${pct(M_SHARE, 0)}; ${usdBn(m.lostUsd)} vs US$1.000bn`,
    });
  } else if (shipped === 'narrower' && m) {
    alternatives.push({
      value: 'sectoral', label: STRATEGIC_COMPACT_DECISION_LABEL.sectoral,
      why_not: `The lead mission carries ${pct(m.share)} and ${usd(m.lostUsd)}, below both lines.`,
      would_need: `${STRATEGIC_COMPACT_DECISION_LABEL.sectoral} would follow if the lead mission reached ${pct(M_SHARE, 0)} of the chain's supply at risk or US$1bn.`,
      margin: `share ${pct(m.share)} vs ${pct(M_SHARE, 0)}; ${usdBn(m.lostUsd)} vs US$1.000bn`,
    });
  }
  if (shipped !== 'none')
    alternatives.push({
      value: 'none', label: STRATEGIC_COMPACT_DECISION_LABEL.none,
      why_not: `${k.build + k.coordinate + k.manage} ${k.build + k.coordinate + k.manage === 1 ? 'product needs' : 'products need'} action under this scenario.`,
      would_need: `${STRATEGIC_COMPACT_DECISION_LABEL.none} would follow if existing trade covered every product (no gap of 10% or more after coordinated trade, no excessive reliance, no material coordination gain).`,
      margin: `${k.build} new capacity, ${k.coordinate} coordinated trade, ${k.manage} managed reliance`,
    });
  else
    alternatives.push({
      value: 'narrower', label: STRATEGIC_COMPACT_DECISION_LABEL.narrower,
      why_not: 'No product of the chain needs action under this scenario.',
      would_need: `${STRATEGIC_COMPACT_DECISION_LABEL.narrower} would follow if at least one product showed a gap, excessive reliance or a material coordination gain.`,
      margin: `exposure ${pct(lost !== null && nn(rec.demandUsd) ? lost / (rec.demandUsd as number) : null)}`,
    });
  const cov = rec.coverage;
  const co = nn(cov.coordinated);
  const post = nn(cov.postInvestment);
  const ex = nn(cov.existing);
  const buildNodes = an.nodes.filter((n) => n.status === 'BUILD');
  const chain: ChainStep[] = [
    { id: 'coordinated_trade', label: 'What coordinated trade adds', estimate: true, display: `${pct(co)} (${ex !== null && co !== null ? `${co - ex >= 0 ? '+' : ''}${((co - ex) * 100).toFixed(1)} points over existing links` : 'n/a'})`, text: 'New and expanded supplier-buyer relationships among allied and other suppliers.' },
    { id: 'new_capacity', label: 'New capacity required', estimate: true, display: co === null ? 'not measured' : `${pct(1 - co)} of allied demand`, text: buildNodes.length ? `Products needing new capacity: ${buildNodes.map((n) => n.label).join(', ')}.` : 'No product needs new capacity.' },
    { id: 'hosts', label: 'Countries capable of adding it', estimate: true, display: rec.buildHosts.length ? rec.buildHosts.map((i) => name(i)).join(', ') : 'none named', text: rec.buildHostTies ? `${rec.buildHostTies} ${rec.buildHostTies === 1 ? 'product has' : 'products have'} hosts tied with the leading one, so no host is named for them.` : 'The leading host of each product needing new capacity (a host is named only when no other ties with it).' },
    {
      id: 'coalition', label: 'Proposed coalition', estimate: true,
      display: rec.coalition === null ? 'no coalition under this scenario' : rec.coalition.length ? `${rec.coalition.length} economies: ${rec.coalition.map((i) => name(i)).join(', ')}` : 'empty',
      text: [rec.criticalExternal?.length ? `Critical non-allied suppliers: ${rec.criticalExternal.map((i) => name(i)).join(', ')}.` : null, rec.nonAlliedReliedOn?.length ? `Non-allied suppliers the coalition still relies on: ${rec.nonAlliedReliedOn.map((i) => name(i)).join(', ')}.` : null, rec.coalition && !rec.coalition.length && shipped === 'sectoral' ? 'The coalition is empty although a standing arrangement is the conclusion.' : null].filter(Boolean).join(' ') || 'The fewest economies that together meet the coverage targets.',
    },
    { id: 'resilience_after', label: 'Expected resilience after', estimate: true, display: post === null ? 'no investment simulation under this scenario' : `${pct(post)} of allied demand met; ${pct(1 - post)} still unmet`, text: 'After new capacity = coordinated trade plus the simulated increment, capped at the gap: arithmetic by construction, not a finding. Build times are not modelled.' },
  ];
  return {
    conclusion: { value: shipped, label: s.conclusion.label, summary: m ? `${an.compact.name}: the lead mission (${aibLabel(m.cls).toLowerCase()}) carries ${pct(m.share)} of the supply at risk (${usd(m.lostUsd)}), so the Explorer proposes: ${s.conclusion.label}.` : `${an.compact.name}: no product needs action.` },
    steps,
    rule,
    alternatives,
    compact_chain: chain,
    partnership: null,
    sector: null,
    technical_path: def.explorer_path,
  };
}

// ---- sector chains -----------------------------------------------------------------------------------------------------

function sectorChainReasoning(def: Pick<CaseDefinition, 'key' | 'explorer_path'>, b: DecodedBundles, s: CaseSnapshot): CaseReasoning | null {
  const doc = b.aib?.doc;
  const scenario = def.key.scenario ?? 'decoupling';
  const an: ChainAnalysis | null = doc && def.key.sid ? chainAnalysis(doc, def.key.sid, scenario) : null;
  if (!doc || !an || !an.rec) return null;
  const name = countryNames(b);
  const dec = scenario === 'decoupling';
  const lostAt = (st: SupplyStage) => an.nodes.filter((n) => n.stage === st && INTERVENTION_CLASSES.includes(n.status)).reduce((x, n) => x + (n.lostUsd ?? 0), 0);
  const steps: ReasoningStep[] = [];
  for (const st of [...new Set(an.nodes.map((n) => n.stage).filter((x): x is SupplyStage => !!x))]) {
    const ns = an.nodes.filter((n) => n.stage === st);
    const act = ns.filter((n) => INTERVENTION_CLASSES.includes(n.status));
    steps.push({
      id: `stage_${st}`, label: STAGE_DISPLAY[st], level: act.length ? 'Needs allied action' : 'No action needed', value: lostAt(st), threshold: null,
      sentence: act.length ? `${act.length} of ${ns.length} product${ns.length > 1 ? 's' : ''} ${act.length > 1 ? 'need' : 'needs'} action, carrying ${usd(lostAt(st))} of supply at risk.` : `${ns.length > 1 ? `None of its ${ns.length} products needs` : 'Its one product does not need'} action under this scenario.`,
      drivers: ns.map((n) => `${n.label}: ${aibLabel(n.status) === n.status ? n.status.toLowerCase() : aibLabel(n.status)}; ${n.lostUsd === null || n.lostUsd === 0 ? 'nothing at risk' : `${usd(n.lostUsd)} at risk`}; ${dec ? `China ${pct(n.chinaShare)}` : `${name(n.largest)} ${pct(n.largestShare)}`} of allied supply before the stress`),
    });
  }
  const cp = an.chokepoint;
  const divs = an.nodes.filter((n) => n.productionFlags.length);
  const chokeNote = [
    cp ? `Highest ${cp.basis === 'china_share' ? 'China' : 'single-supplier'} share before the stress: ${cp.label} (${cp.stage ? STAGE_DISPLAY[cp.stage] : 'no stage'}), ${pct(cp.share)} of allied supply.` : null,
    ...divs.map((n) => `${n.label} (${n.stage ? STAGE_DISPLAY[n.stage] : 'no stage'}): production is more concentrated than trade.`),
  ].filter(Boolean).join(' ') || null;
  const rule = `The Explorer classes every product of the chain with the stress-test rule. A stage needs allied action when it holds a product needing new capacity, managed reliance or coordinated trade. The lead mission's steps are ranked by the dollars of supply at risk, and the lead stage is where that mission's largest amount sits. Dollar ranking favours high-value downstream stages over small upstream chokepoints, so the chokepoint view is shown beside it.`;
  const lead = an.leadStage;
  const alternatives: AlternativeExplanation[] = [];
  // one alternative per other stage that a reviewer could defensibly name first: the chokepoint stage, the other
  // stages needing action, and stages holding a product whose production is more concentrated than its trade
  const STATUS_WORDS: Readonly<Record<string, string>> = { 'NOT STRESSED': 'already as diversified as comparable products', 'NOT TESTED': 'not tested', 'NO SHOCK': 'no shock applied' };
  const statusWords = (st: string) => STATUS_WORDS[st] ?? aibLabel(st).toLowerCase();
  const chainStages = [...new Set(an.nodes.map((n) => n.stage).filter((x): x is SupplyStage => !!x))];
  const candidates = [...new Set([cp?.stage ?? null, ...divs.map((n) => n.stage), ...an.explorerStages, ...chainStages].filter((x): x is SupplyStage => !!x && x !== lead))];
  for (const st of candidates) {
    const needs = an.explorerStages.includes(st);
    const dvs = divs.filter((n) => n.stage === st);
    const isCp = cp?.stage === st;
    const here = an.nodes.filter((n) => n.stage === st);
    const why = [
      needs
        ? `${STAGE_DISPLAY[st]} also needs allied action, but carries ${usd(lostAt(st))} of supply at risk in products needing action, against ${lead ? usd(lostAt(lead)) : 'none'} at the lead stage; the Explorer ranks by dollars.`
        : `No product at ${STAGE_DISPLAY[st]} needs allied action under this scenario (${here.map((n) => `${n.label}: ${statusWords(n.status)}`).join('; ')}).`,
      isCp && cp ? `It holds the most concentrated product before the stress (${cp.label}: ${cp.basis === 'china_share' ? 'China' : name(cp.supplier)} ${pct(cp.share)} of allied supply).` : null,
      ...dvs.map((n) => `${n.label} is read from trade (${dec ? `China ${pct(n.chinaShare)}` : `${name(n.largest)} ${pct(n.largestShare)}`} of allied supply) and classed ${aibLabel(n.status).toLowerCase()}, although ${flagFirst(n.productionFlags[0])}.`),
    ].filter(Boolean).join(' ');
    const need = [
      needs ? 'its supply at risk exceeded the lead stage’s' : null,
      isCp ? 'stages were ranked by concentration rather than by dollars at risk' : null,
      dvs.length ? 'production capacity rather than trade set the exposure' : null,
    ].filter(Boolean);
    alternatives.push({
      value: st, label: STAGE_DISPLAY[st],
      why_not: why,
      would_need: `${STAGE_DISPLAY[st]} would come first if ${need.join(', or if ') || 'a product there needed allied action'}.`,
      margin: [needs ? `${usd(lostAt(st))} vs ${lead ? usd(lostAt(lead)) : 'n/a'}` : null, isCp && cp ? `${cp.label} ${pct(cp.share)}` : null, ...dvs.map((n) => flagFirst(n.productionFlags[0]))].filter(Boolean).join('; ') || null,
    });
  }
  // the most telling first: a stage with a production flag or the chokepoint, then stages needing action by dollars
  // at risk, then the rest by the size of their allied imports; three at most, then "no stage"
  const tell = (v: string) => Number(divs.some((n) => n.stage === v) || cp?.stage === v);
  const imports = (v: string) => an.nodes.filter((n) => n.stage === v).reduce((x, n) => x + (n.demandUsd ?? 0), 0);
  alternatives.sort((x, y) => tell(y.value) - tell(x.value) || lostAt(y.value as SupplyStage) - lostAt(x.value as SupplyStage) || imports(y.value) - imports(x.value));
  alternatives.splice(3);
  if (lead)
    alternatives.push({
      value: NO_STAGE, label: NO_STAGE_LABEL,
      why_not: `${an.explorerStages.length} ${an.explorerStages.length === 1 ? 'stage holds' : 'stages hold'} products existing trade cannot cover.`,
      would_need: `${NO_STAGE_LABEL} would follow if every product were covered by existing or coordinated trade without excessive reliance.`,
      margin: an.mission ? `lead mission ${usd(an.mission.lostUsd)} (${pct(an.mission.share)})` : null,
    });
  return {
    conclusion: { value: s.conclusion.value, label: s.conclusion.label, summary: lead ? `${an.compact.name}: allied action is needed at ${an.explorerStages.map((x) => STAGE_DISPLAY[x]).join(', ')}; the largest dollar gap is at ${STAGE_DISPLAY[lead]} (${an.leadNode?.label ?? ''}).` : `${an.compact.name}: no stage needs allied action under this scenario.` },
    steps,
    rule,
    alternatives,
    compact_chain: null,
    partnership: null,
    sector: { explorer_stages: an.explorerStages.map((x) => STAGE_DISPLAY[x]), lead_stage: lead ? STAGE_DISPLAY[lead] : null, chokepoint_note: chokeNote },
    technical_path: def.explorer_path,
  };
}

/** "CHN holds 97% of GCIM operational capacity: Solar Polysilicon vs 10% of exports, ..." -> "China holds 97% of operational production capacity, against 10% of exports" */
function flagFirst(f: string | undefined): string {
  if (!f) return '';
  return productionFlagCore(f, (i) => (i === 'CHN' ? 'China' : i)) ?? f.replace(/^production more concentrated than trade: /, '').replace(/\bCHN\b/, 'China');
}

// ---- partnerships --------------------------------------------------------------------------------------------------------

function partnershipReasoning(def: Pick<CaseDefinition, 'key' | 'explorer_path'>, b: DecodedBundles, s: CaseSnapshot): CaseReasoning | null {
  const memo = b.partnerships?.memos[def.key.pair ?? ''] ?? null;
  if (!memo) return null;
  const items = memoChoices(memo);
  const boxes = items.map((i) => ({ kind: i.kind, label: i.label, items: i.text.split(/(?<=\.)\s+(?=[A-Z])/).map((x) => x.trim()).filter(Boolean) }));
  const doc = b.aib?.doc as AibDoc | undefined;
  const byLabel = (doc?.nodes ?? []).filter((n) => n.label.length > 4);
  const inBox = (kind: string) => {
    const tx = (items.find((i) => i.kind === kind)?.text ?? '').toLowerCase();
    return byLabel.filter((n) => tx.includes(n.label.toLowerCase()));
  };
  const build = inBox('preserve');
  const trade = inBox('partner');
  const both = build.filter((n) => trade.some((t2) => t2.id === n.id));
  const changes = build.map((n) => `${n.label}: existing links restore ${pct(nn(n.existing))} of the allied supply lost if China stops supplying; coordinated trade ${pct(nn(n.optimized))}; ${pct(nn(n.residual))} needs new capacity (${aibLabel(n.cls).toLowerCase()}).`);
  const steps: ReasoningStep[] = boxes.map((x) => ({ id: `box_${x.kind}`, label: x.label, level: x.items.length ? `${x.items.length} item${x.items.length > 1 ? 's' : ''}` : 'Empty', value: null, threshold: null, sentence: x.items[0] ?? 'Nothing named.', drivers: x.items.slice(1) }));
  if (both.length)
    steps.push({ id: 'consistency', label: 'Consistency check', level: 'Check', value: null, threshold: null, sentence: `${both.length} product${both.length > 1 ? 's appear' : ' appears'} in both the first and the second box: ${both.map((n) => n.label).join(', ')}.`, drivers: [] });
  const head = s.conclusion.value as PartnershipBoxKind;
  const alternatives: AlternativeExplanation[] = [
    {
      value: 'partner', label: PARTNERSHIP_BOX_LABEL.partner,
      why_not: `The memo names ${build.length} products for joint new capacity, where the allied stress test leaves a gap or both sides are exposed.`,
      would_need: `${PARTNERSHIP_BOX_LABEL.partner} would lead if allied capacity were adequate on those products (no gap after coordinated trade), so one side could supply the other through trade.`,
      margin: build.length ? build.map((n) => `${n.label}: gap ${pct(nn(n.residual))}`).join('; ') : null,
    },
    {
      value: 'watch', label: PARTNERSHIP_BOX_LABEL.watch,
      why_not: 'The memo finds complementary strengths and shared exposures, not only competition.',
      would_need: `${PARTNERSHIP_BOX_LABEL.watch} would lead if both sides were strong producers of the same products (competition) or the strategic case were weak.`,
      margin: null,
    },
  ].filter((a) => a.value !== head);
  return {
    conclusion: { value: head, label: s.conclusion.label, summary: `The memo leads with ${PARTNERSHIP_BOX_LABEL[head]?.toLowerCase() ?? head}: ${build.length ? build.map((n) => n.label).join(', ') : 'no product named'}.` },
    steps,
    rule: 'The memo applies fixed rules: build together where trusted allied supply is short (a stress-test gap) or both sides are exposed; trade and standards where one side supplies what the other needs and allied supply is adequate; stay out where both are strong (competition) or the strategic case is low. Its first box is always the joint-build list, so the headline is the same for every pair with any joint build.',
    alternatives,
    compact_chain: null,
    partnership: { coordination_changes: changes, memo_choices: boxes },
    sector: null,
    technical_path: def.explorer_path,
  };
}

// ---- entry point ---------------------------------------------------------------------------------------------------------

/** The reasoning reveal of a case (null when the subject is not in the release). */
export function buildCaseReasoning(def: Pick<CaseDefinition, 'kind' | 'key' | 'explorer_path'>, b: DecodedBundles, snapshot: CaseSnapshot): CaseReasoning | null {
  if (snapshot.conclusion.value === 'missing') return null;
  switch (def.kind) {
    case 'posture_cell':
    case 'sector':
      return postureReasoning(def, b, snapshot);
    case 'aib_node':
      return aibNodeReasoning(def, b, snapshot);
    case 'compact':
      return compactReasoning(def, b, snapshot);
    case 'sector_chain':
      return sectorChainReasoning(def, b, snapshot);
    case 'partnership':
      return partnershipReasoning(def, b, snapshot);
  }
}

/** For tests and the admin: the trade period words of the AIB bundle. */
export const aibTradePeriod = (doc: AibDoc): string => tradeWhen(doc.meta);
