#!/usr/bin/env node
// Writes the SYNTHETIC partnership-agreements bundle (src/agreements/__fixtures__/synthetic/data/agreements/):
// a manifest, the global file and the files of four economies (JPN, AUS, USA, DEU) in the shape of
// docs/partnership_agreements.md section 9, for the dashboard's tests and dev. The agreements are invented
// but realistic (real ISO3 codes, titles in the style of the sources, made-up record ids and dates); every
// field of the contract is present, every depth class 0-5 occurs, codes come at specific and text grade,
// some agreements carry delivery evidence, EU agreements reach member states (via_eu), several agreements
// have two or more sources, and each file has gap and execution-gap rows. The pair roll-up and the depth
// classes are computed here with a simplified version of the section 7 rules (it does not replace R).
// The additive fields are emitted too: vocab.specific_bases, vocab.established_instrument_bases (a title keyword
// never establishes an instrument), vocab.compacts, the global pairs' years, years_ei, igroups, legal, status and
// stages, an agreement with an official page but no primary_url, and a gap row whose complementarity is not_assessed.
//
//   node src/agreements/__fixtures__/make-synthetic.mjs        (from dashboard/)

import { createHash } from 'node:crypto';
import { mkdirSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const OUT = join(dirname(fileURLToPath(import.meta.url)), 'synthetic', 'data', 'agreements');
const AS_OF = '2026-11-06';
const FOCAL = ['JPN', 'AUS', 'USA', 'DEU'];
const EU_MEMBERS = ['DEU', 'FRA', 'ITA'];

const vocab = {
  agreement_types: {
    treaty: 'Treaty', fta: 'FTA / Trade Agreement', epa: 'Economic Partnership Agreement', strategic_partnership: 'Strategic Partnership', framework: 'Framework Agreement', mou: 'MOU',
    joint_declaration: 'Joint Declaration', joint_statement: 'Joint Statement', sectoral_partnership: 'Sectoral Partnership', ministerial_initiative: 'Ministerial Initiative',
    plurilateral_forum: 'Plurilateral forum or club', financing_agreement: 'Financing Agreement', other: 'Other',
  },
  partnership_labels: {
    comprehensive_strategic: 'Comprehensive strategic partnership', global_strategic: 'Global strategic partnership', special_strategic: 'Special strategic partnership',
    strategic: 'Strategic partnership', comprehensive: 'Comprehensive partnership', other_partnership: 'Other named partnership',
  },
  legal_statuses: { binding: 'Legally binding', non_binding: 'Political commitment / non-binding', mixed: 'Mixed / unclear', unknown: 'Unknown' },
  implementation_statuses: {
    announced: 'Announced', signed: 'Signed', in_force: 'In force', implementation_underway: 'Implementation underway', operational: 'Implemented / operational',
    inactive: 'Inactive / expired', cancelled: 'Cancelled', unknown: 'Unknown',
  },
  delivery_statuses: ['implementation_underway', 'operational'],
  coding_bases: {
    source_coded: 'coded by the source dataset (a DESTA provision, an IISD topic)', curated: 'recorded by hand from the official text',
    type_implied: "implied by the source's agreement type (a free-trade agreement reduces tariffs)", text_title: 'keyword in the title',
    text_body: 'keyword in a sentence of the published text that also speaks of cooperation',
  },
  confidence_levels: {
    high: 'primary government or agreement text, or two or more independent sources', medium: 'one reputable structured dataset', low: 'a secondary source only, or uncertain entity or record matching',
  },
  depth_classes: { 0: 'No identified partnership', 1: 'Dialogue', 2: 'Framework', 3: 'Sectoral cooperation', 4: 'Operational partnership', 5: 'Integrated partnership' },
  instrument_groups: {
    trade: 'Trade', finance: 'Finance', investment: 'Investment', offtake: 'Offtake and demand', supply: 'Supply security', manufacturing: 'Manufacturing', rd: 'R&D and technology',
    standards: 'Standards and regulation', procurement: 'Procurement', dialogue: 'Dialogue and working groups', other: 'Other',
  },
  sectors: [
    ['critical_minerals', 'Critical minerals', true, ['Batteries', 'Electric Vehicles', 'Electric Grid', 'Wind', 'Solar']],
    ['solar', 'Solar', true, ['Solar']], ['wind', 'Wind', true, ['Wind']], ['batteries', 'Batteries', true, ['Batteries']], ['evs', 'EVs', true, ['Electric Vehicles']],
    ['nuclear', 'Nuclear', true, ['Nuclear']], ['grid_equipment', 'Grid equipment', true, ['Electric Grid']], ['transformers', 'Transformers', true, ['Electric Grid']],
    ['transmission', 'Transmission', true, ['Electric Grid']], ['power_electronics', 'Power electronics', true, ['Electric Grid', 'Solar']], ['semiconductors', 'Semiconductors', true, []],
    ['hydrogen', 'Hydrogen', true, ['Green Hydrogen']], ['geothermal', 'Geothermal', true, ['Geothermal']], ['ccs', 'CCS', true, []], ['industrial_heat', 'Industrial heat', true, []],
    ['steel', 'Steel', true, []], ['aluminum', 'Aluminum', true, []], ['cement', 'Cement', true, []], ['data_centers', 'Data centers / compute infrastructure', true, []],
    ['general_clean_energy', 'General clean energy', true, []], ['fossil_fuels', 'Oil, gas and coal', false, ['Oil', 'Gas', 'Coal']],
    ['general_industrial', 'General industrial cooperation', false, []], ['trade', 'Trade', false, []], ['science_rd', 'Science / R&D', false, []],
  ].map(([id, label, ei, techs]) => ({ id, label, ei, techs })),
  instruments: [
    ['trade_preference', 'Trade preference', 'trade', true], ['tariff_reduction', 'Tariff reduction', 'trade', true], ['market_access', 'Market access', 'trade', true],
    ['investment_facilitation', 'Investment facilitation', 'investment', true], ['fdi_framework', 'FDI framework', 'investment', true], ['public_finance', 'Public finance', 'finance', true],
    ['export_credit', 'Export credit', 'finance', true], ['development_finance', 'Development finance', 'finance', true], ['loan_guarantee', 'Loan guarantee', 'finance', true],
    ['joint_investment_fund', 'Joint investment fund', 'investment', true], ['offtake', 'Offtake / purchase commitment', 'offtake', true], ['demand_guarantee', 'Demand guarantee', 'offtake', true],
    ['price_support', 'Price support', 'offtake', true], ['critical_mineral_supply', 'Critical-mineral supply agreement', 'supply', true], ['long_term_supply', 'Long-term supply arrangement', 'supply', true],
    ['joint_venture', 'Joint venture', 'manufacturing', true], ['co_production', 'Co-production', 'manufacturing', true], ['manufacturing_cooperation', 'Manufacturing cooperation', 'manufacturing', true],
    ['rd_cooperation', 'R&D cooperation', 'rd', true], ['technology_cooperation', 'Technology cooperation', 'rd', false], ['technology_transfer', 'Technology transfer', 'rd', true],
    ['standards_cooperation', 'Standards cooperation', 'standards', true], ['regulatory_cooperation', 'Regulatory cooperation', 'standards', true], ['mutual_recognition', 'Mutual recognition', 'standards', true],
    ['workforce_skills', 'Workforce / skills cooperation', 'other', false], ['permitting_coordination', 'Permitting / regulatory coordination', 'standards', false],
    ['data_sharing', 'Data / information sharing', 'dialogue', false], ['government_procurement', 'Government procurement', 'procurement', true],
    ['supply_chain_monitoring', 'Supply-chain monitoring', 'supply', false], ['early_warning', 'Early-warning mechanism', 'supply', false], ['strategic_dialogue', 'Strategic dialogue', 'dialogue', false],
    ['working_group', 'Working group', 'dialogue', false], ['other', 'Other', 'other', false],
  ].map(([id, label, group, operational]) => ({ id, label, group, operational })),
  sources: [
    { id: 'gsp', label: 'Global Strategic Partnership Dataset', short: 'GSP', citation: 'Chang-Liao, N., Fu, Lin (2025). Global Strategic Partnership Dataset, 1990-2020, V1.0. Harvard Dataverse, doi:10.7910/DVN/UJD7LW (CC0 1.0).', url: 'https://doi.org/10.7910/DVN/UJD7LW', licence: 'CC0-1.0' },
    { id: 'desta', label: 'Design of Trade Agreements (DESTA)', short: 'DESTA', citation: 'Dür, A., Baccini, L. and Elsig, M. (2014). The Design of International Trade Agreements. Review of International Organizations 9(3): 353-375. DESTA version 2.3.', url: 'https://www.designoftradeagreements.org/downloads/', licence: 'none published (citation requested); redistribution of derived facts only' },
    { id: 'memberships', label: 'Economic-security agreement memberships (project reference)', short: 'Memberships', citation: 'data/reference/economic_security_agreements.csv (USTR, IPEF, MSP, G7, NATO pages; accessed 2026-10-08)', url: 'https://ustr.gov/trade-agreements/free-trade-agreements', licence: 'project-authored from official pages' },
    { id: 'curated', label: 'Curated government agreements', short: 'Curated', citation: 'data/reference/partnership_agreements/curated_agreements.csv (one official source per row)', url: '', licence: 'project-authored; each row cites its official source' },
    { id: 'pmemo', label: 'Partnership memo evidence (US-Japan)', short: 'Memo evidence', citation: 'data/reference/partnership_evidence/ (bilateral_mechanisms.csv, policy_evidence.csv)', url: '', licence: 'project-authored; each row cites its source' },
    { id: 'iisd', label: 'IISD Trade & Climate Tracker', short: 'IISD', citation: 'IISD (2026). Trade & Climate Tracker. International Institute for Sustainable Development.', url: 'https://www.iisd.org/trade-climate-tracker/', licence: 'All rights reserved (IISD); ingest only an export IISD has shared for this use' },
  ],
};

const names = {
  AUS: 'Australia', CAN: 'Canada', DEU: 'Germany', EUU: 'European Union', FRA: 'France', GBR: 'United Kingdom', IND: 'India', ITA: 'Italy', JPN: 'Japan', KOR: 'Korea',
  NOR: 'Norway', NZL: 'New Zealand', USA: 'United States',
};

// ---- the canonical agreements --------------------------------------------------------------------------
// s: sectors [code, basis]; i: instruments [code, basis]; src: [dataset, record_id, url]; d: delivery [text, status, url]
const A = [
  { id: 'agr-1a2b3c4d5e', title: 'Agreement between Japan and Australia for an Economic Partnership', parties: ['AUS', 'JPN'], type: 'epa', legal: 'binding', legal_basis: 'source_document_type',
    date: '2014-07-08', signature_date: '2014-07-08', effective_date: '2015-01-15', status: 'in_force', category: 'trade_economic',
    s: [['trade', 'type_implied'], ['critical_minerals', 'source_coded']], i: [['tariff_reduction', 'source_coded'], ['market_access', 'source_coded'], ['investment_facilitation', 'source_coded']],
    src: [['desta', 'desta-0912', null], ['curated', 'cur-jaepa', 'https://www.dfat.gov.au/trade/agreements/in-force/jaepa']], primary_url: 'https://www.dfat.gov.au/trade/agreements/in-force/jaepa', confidence: 'high', tdi: 6, d: [] },
  { id: 'agr-2b3c4d5e6f', title: 'Joint Statement: Special Strategic Partnership for the 21st Century', parties: ['AUS', 'JPN'], type: 'strategic_partnership', label: 'special_strategic', legal: 'non_binding', legal_basis: 'source_document_type',
    date: '2014-07-08', status: 'signed', category: 'overall_relationship',
    s: [['critical_minerals', 'text_body'], ['general_clean_energy', 'text_body']], i: [['strategic_dialogue', 'text_body']],
    src: [['gsp', 'gsp-AUS-JPN-2014-07', 'https://www.mofa.go.jp/a_o/ocn/au/page3e_000213.html']], primary_url: 'https://www.mofa.go.jp/a_o/ocn/au/page3e_000213.html', confidence: 'high', d: [] },
  { id: 'agr-3c4d5e6f7a', title: 'Japan-Australia Joint Declaration on Security Cooperation', parties: ['AUS', 'JPN'], type: 'joint_declaration', label: 'special_strategic', legal: 'non_binding', legal_basis: 'source_document_type',
    date: '2022-10-22', status: 'signed', category: 'overall_relationship',
    s: [['critical_minerals', 'text_body'], ['general_clean_energy', 'text_body']], i: [['strategic_dialogue', 'text_body'], ['data_sharing', 'text_body']],
    src: [['curated', 'cur-jadsc-2022', 'https://www.mofa.go.jp/fp/nsp/page1e_000483.html']], primary_url: 'https://www.mofa.go.jp/fp/nsp/page1e_000483.html', confidence: 'high', d: [] },
  { id: 'agr-4d5e6f7a8b', title: 'Australia-Japan Critical Minerals Partnership', parties: ['AUS', 'JPN'], type: 'sectoral_partnership', legal: 'non_binding', legal_basis: 'curated',
    date: '2022-10', status: 'implementation_underway', category: 'sectoral',
    s: [['critical_minerals', 'curated']], i: [['critical_mineral_supply', 'curated'], ['export_credit', 'curated'], ['offtake', 'text_body']],
    src: [['curated', 'cur-ajcmp', 'https://www.dfat.gov.au/geo/japan/australia-japan-critical-minerals-partnership'], ['pmemo', 'pm-ajcmp', null]], primary_url: 'https://www.dfat.gov.au/geo/japan/australia-japan-critical-minerals-partnership', confidence: 'high',
    d: [['JOGMEC and Sojitz equity in Lynas rare-earth separation, with offtake to Japan', 'implementation_underway', 'https://www.jogmec.go.jp/english/news/release/news_10_00038.html'], ['JBIC loan to an Australian battery-grade nickel project', 'implementation_underway', null]] },
  { id: 'agr-5e6f7a8b9c', title: 'Memorandum of Cooperation on the Hydrogen Energy Supply Chain pilot', parties: ['AUS', 'JPN'], type: 'mou', legal: 'non_binding', legal_basis: 'curated',
    date: '2018', status: 'operational', category: 'sectoral',
    s: [['hydrogen', 'curated']], i: [['rd_cooperation', 'curated'], ['co_production', 'text_body'], ['long_term_supply', 'text_title']],
    src: [['curated', 'cur-hesc', 'https://www.energy.gov.au/government-priorities/energy-and-climate-change-ministerial-council']], primary_url: null, pa: true, confidence: 'low',
    d: [['Liquefied hydrogen carrier delivered the first cargo from Victoria to Kobe', 'operational', 'https://www.hystra.or.jp/en/project/']] },
  { id: 'agr-6f7a8b9c0d', title: 'Japan-Australia Green Iron and Steel Partnership (announced)', parties: ['AUS', 'JPN'], type: 'sectoral_partnership', legal: 'unknown', legal_basis: 'none',
    date: '2025-09', status: 'announced', category: 'sectoral',
    s: [['steel', 'text_title'], ['hydrogen', 'text_body']], i: [],
    src: [['curated', 'cur-ajgreeniron', null]], primary_url: null, confidence: 'low', d: [] },
  { id: 'agr-7a8b9c0d1e', title: 'Agreement on Commerce between Australia and Japan', parties: ['AUS', 'JPN'], type: 'treaty', legal: 'binding', legal_basis: 'source_document_type',
    date: '1957-07-06', signature_date: '1957-07-06', effective_date: '1957-12-04', status: 'inactive', category: 'trade_economic',
    s: [['trade', 'type_implied']], i: [['market_access', 'source_coded']],
    src: [['desta', 'desta-0071', null]], primary_url: null, confidence: 'medium', tdi: 1, d: [] },
  { id: 'agr-8b9c0d1e2f', title: 'Agreement between Japan and the United States on Strengthening Critical Minerals Supply Chains', parties: ['JPN', 'USA'], type: 'sectoral_partnership', legal: 'binding', legal_basis: 'curated',
    date: '2023-03-28', signature_date: '2023-03-28', effective_date: '2023-03-28', status: 'in_force', category: 'sectoral',
    s: [['critical_minerals', 'curated'], ['batteries', 'curated'], ['evs', 'curated']], i: [['standards_cooperation', 'curated'], ['supply_chain_monitoring', 'curated'], ['investment_facilitation', 'text_body']],
    src: [['curated', 'cur-jus-cma', 'https://ustr.gov/sites/default/files/2023-03/US%20Japan%20Critical%20Minerals%20Agreement%202023%2003%2028.pdf'], ['pmemo', 'pm-jus-cma', null]],
    primary_url: 'https://ustr.gov/sites/default/files/2023-03/US%20Japan%20Critical%20Minerals%20Agreement%202023%2003%2028.pdf', confidence: 'high', d: [] },
  { id: 'agr-9c0d1e2f3a', title: "Japan-U.S. Joint Leaders' Statement: Global Partners for the Future", parties: ['JPN', 'USA'], type: 'joint_statement', label: 'global_strategic', legal: 'non_binding', legal_basis: 'source_document_type',
    date: '2024-04-10', status: 'signed', category: 'overall_relationship',
    s: [['nuclear', 'text_body'], ['semiconductors', 'text_body'], ['hydrogen', 'text_body']], i: [['working_group', 'text_body'], ['joint_investment_fund', 'text_body']],
    src: [['pmemo', 'pm-jus-2024-04', 'https://www.whitehouse.gov/briefing-room/statements-releases/2024/04/10/']], primary_url: 'https://www.whitehouse.gov/briefing-room/statements-releases/2024/04/10/', confidence: 'high', d: [] },
  { id: 'agr-0d1e2f3a4b', title: 'Agreement between the United States of America and Japan concerning Trade', parties: ['JPN', 'USA'], type: 'fta', legal: 'binding', legal_basis: 'source_document_type',
    date: '2019-10-07', signature_date: '2019-10-07', effective_date: '2020-01-01', status: 'in_force', category: 'trade_economic',
    s: [['trade', 'type_implied']], i: [['tariff_reduction', 'type_implied'], ['market_access', 'type_implied']],
    src: [['desta', 'desta-1102', null]], primary_url: null, confidence: 'medium', tdi: 2, d: [] },
  { id: 'agr-1e2f3a4b5c', title: 'Australia-United States Free Trade Agreement', parties: ['AUS', 'USA'], type: 'fta', legal: 'binding', legal_basis: 'source_document_type',
    date: '2004-05-18', signature_date: '2004-05-18', effective_date: '2005-01-01', status: 'in_force', category: 'trade_economic',
    s: [['trade', 'type_implied']], i: [['tariff_reduction', 'type_implied'], ['government_procurement', 'source_coded'], ['investment_facilitation', 'source_coded']],
    src: [['desta', 'desta-0566', null], ['memberships', 'mem-USFTA-AUS', 'https://ustr.gov/trade-agreements/free-trade-agreements/australian-fta']], primary_url: 'https://ustr.gov/trade-agreements/free-trade-agreements/australian-fta', confidence: 'high', tdi: 7, d: [] },
  { id: 'agr-2f3a4b5c6d', title: 'Australia-United States Climate, Critical Minerals and Clean Energy Transformation Compact', parties: ['AUS', 'USA'], type: 'framework', label: 'other_partnership', legal: 'non_binding', legal_basis: 'curated',
    date: '2023-05-20', status: 'signed', category: 'sectoral',
    s: [['critical_minerals', 'curated'], ['general_clean_energy', 'curated'], ['batteries', 'text_body']], i: [['critical_mineral_supply', 'curated'], ['public_finance', 'text_body']],
    src: [['curated', 'cur-aus-usa-compact', 'https://www.whitehouse.gov/briefing-room/statements-releases/2023/05/20/']], primary_url: 'https://www.whitehouse.gov/briefing-room/statements-releases/2023/05/20/', confidence: 'high', d: [] },
  { id: 'agr-3a4b5c6d7e', title: 'Agreement between the European Union and Japan for an Economic Partnership', parties: ['EUU', 'JPN'], type: 'epa', legal: 'binding', legal_basis: 'source_document_type',
    date: '2018-07-17', signature_date: '2018-07-17', effective_date: '2019-02-01', status: 'in_force', category: 'trade_economic',
    s: [['trade', 'type_implied'], ['general_industrial', 'source_coded']], i: [['tariff_reduction', 'source_coded'], ['standards_cooperation', 'source_coded'], ['mutual_recognition', 'source_coded'], ['government_procurement', 'source_coded']],
    src: [['desta', 'desta-1043', null], ['iisd', 'iisd-eu-jpn-epa', 'https://www.iisd.org/trade-climate-tracker/']], primary_url: 'https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:22018A1227(01)', confidence: 'high', tdi: 7, d: [] },
  { id: 'agr-4b5c6d7e8f', title: 'EU-Japan Green Alliance', parties: ['EUU', 'JPN'], type: 'joint_declaration', legal: 'non_binding', legal_basis: 'source_document_type',
    date: '2021-05-27', status: 'signed', category: 'sectoral',
    s: [['hydrogen', 'text_body'], ['wind', 'text_body'], ['general_clean_energy', 'text_title']], i: [['regulatory_cooperation', 'text_body'], ['rd_cooperation', 'text_body']],
    src: [['curated', 'cur-eu-jpn-green', 'https://ec.europa.eu/commission/presscorner/detail/en/ip_21_2745']], primary_url: 'https://ec.europa.eu/commission/presscorner/detail/en/ip_21_2745', confidence: 'high', d: [] },
  { id: 'agr-5c6d7e8f9a', title: 'Joint Statement of the Seventh German-Japanese Intergovernmental Consultations', parties: ['DEU', 'JPN'], type: 'joint_statement', legal: 'non_binding', legal_basis: 'source_document_type',
    date: '2023-03-18', status: 'signed', category: 'overall_relationship',
    s: [['semiconductors', 'text_body'], ['critical_minerals', 'text_body'], ['hydrogen', 'text_body']], i: [['strategic_dialogue', 'text_body'], ['working_group', 'text_body']],
    src: [['curated', 'cur-deu-jpn-2023', 'https://www.bundesregierung.de/breg-en/news/german-japanese-government-consultations-2172154']], primary_url: 'https://www.bundesregierung.de/breg-en/news/german-japanese-government-consultations-2172154', confidence: 'high', d: [] },
  { id: 'agr-6d7e8f9a0b', title: 'German-Australian Hydrogen Accord', parties: ['AUS', 'DEU'], type: 'sectoral_partnership', legal: 'non_binding', legal_basis: 'curated',
    date: '2021-06-13', status: 'implementation_underway', category: 'sectoral',
    s: [['hydrogen', 'curated']], i: [['rd_cooperation', 'curated'], ['joint_investment_fund', 'curated']],
    src: [['curated', 'cur-hygate', 'https://www.dcceew.gov.au/energy/hydrogen/hygate']], primary_url: 'https://www.dcceew.gov.au/energy/hydrogen/hygate', confidence: 'high',
    d: [['HyGATE: four joint hydrogen supply-chain projects funded by both governments', 'implementation_underway', 'https://www.dcceew.gov.au/energy/hydrogen/hygate']] },
  { id: 'agr-7e8f9a0b1c', title: 'U.S.-Germany Climate and Energy Partnership', parties: ['DEU', 'USA'], type: 'sectoral_partnership', legal: 'non_binding', legal_basis: 'curated',
    date: '2021-07-15', status: 'signed', category: 'sectoral',
    s: [['general_clean_energy', 'curated'], ['hydrogen', 'curated'], ['evs', 'text_body']], i: [['strategic_dialogue', 'curated'], ['rd_cooperation', 'text_body']],
    src: [['curated', 'cur-us-deu-climate', 'https://www.state.gov/u-s-germany-climate-and-energy-partnership/']], primary_url: 'https://www.state.gov/u-s-germany-climate-and-energy-partnership/', confidence: 'high', d: [] },
  { id: 'agr-8f9a0b1c2d', title: 'EU-U.S. Trade and Technology Council', parties: ['EUU', 'USA'], type: 'ministerial_initiative', legal: 'non_binding', legal_basis: 'source_document_type',
    date: '2021-06-15', status: 'unknown', status_raw: 'meetings paused', category: 'trade_economic',
    s: [['semiconductors', 'curated']], i: [['working_group', 'curated'], ['supply_chain_monitoring', 'curated'], ['early_warning', 'curated']],
    src: [['curated', 'cur-ttc', 'https://ec.europa.eu/commission/presscorner/detail/en/ip_21_2990']], primary_url: 'https://ec.europa.eu/commission/presscorner/detail/en/ip_21_2990', confidence: 'medium', d: [] },
  { id: 'agr-9a0b1c2d3e', title: 'Transatlantic Trade and Investment Partnership (negotiations ended)', parties: ['EUU', 'USA'], type: 'fta', legal: 'unknown', legal_basis: 'none',
    date: '2013-07', status: 'cancelled', category: 'trade_economic',
    s: [['trade', 'type_implied']], i: [['tariff_reduction', 'type_implied']],
    src: [['curated', 'cur-ttip', null]], primary_url: null, confidence: 'low', d: [] },
  { id: 'agr-0b1c2d3e4f', title: 'Japan-India Vision Statement: Special Strategic and Global Partnership', parties: ['IND', 'JPN'], type: 'strategic_partnership', label: 'special_strategic', legal: 'non_binding', legal_basis: 'source_document_type',
    date: '2018-10-29', status: 'signed', category: 'overall_relationship',
    s: [['general_clean_energy', 'text_body']], i: [['strategic_dialogue', 'text_body']],
    src: [['gsp', 'gsp-IND-JPN-2018-10', 'https://www.mofa.go.jp/s_sa/sw/in/page3e_000233.html']], primary_url: 'https://www.mofa.go.jp/s_sa/sw/in/page3e_000233.html', confidence: 'high', d: [] },
  { id: 'agr-1c2d3e4f5a', title: 'Agreement on Commerce between Japan and New Zealand', parties: ['JPN', 'NZL'], type: 'treaty', legal: 'binding', legal_basis: 'source_document_type',
    date: '1958-09-09', signature_date: '1958-09-09', effective_date: '1958-12-01', status: 'inactive', category: 'trade_economic',
    s: [['trade', 'type_implied']], i: [],
    src: [['desta', 'desta-0089', null]], primary_url: null, confidence: 'medium', tdi: 0, d: [] },
  { id: 'agr-2d3e4f5a6b', title: 'Minerals Security Partnership', parties: ['AUS', 'CAN', 'DEU', 'EUU', 'FRA', 'GBR', 'IND', 'ITA', 'JPN', 'KOR', 'NOR', 'USA'], type: 'plurilateral_forum', legal: 'non_binding', legal_basis: 'source_document_type',
    date: '2022-06', status: 'signed', category: 'plurilateral_club',
    s: [['critical_minerals', 'text_body']], i: [],
    src: [['memberships', 'mem-MSP', 'https://www.state.gov/minerals-security-partnership/']], primary_url: 'https://www.state.gov/minerals-security-partnership/', confidence: 'high', d: [] },
  { id: 'agr-3e4f5a6b7c', title: 'Group of Seven (G7)', parties: ['CAN', 'DEU', 'EUU', 'FRA', 'GBR', 'ITA', 'JPN', 'USA'], type: 'plurilateral_forum', legal: 'non_binding', legal_basis: 'source_document_type',
    date: '1975', status: 'signed', category: 'plurilateral_club',
    s: [], i: [],
    src: [['memberships', 'mem-G7', null]], primary_url: null, confidence: 'medium', d: [] },
].map((a) => ({
  label: null, signature_date: null, effective_date: null, status_raw: null, tdi: null, ...a,
}));

const sectorById = new Map(vocab.sectors.map((s) => [s.id, s]));
const instById = new Map(vocab.instruments.map((s) => [s.id, s]));
const SPECIFIC = ['source_coded', 'curated', 'type_implied', 'text_title'];
/** an instrument is established at these bases only (a title keyword never establishes one) */
const INST_ESTABLISHED = ['source_coded', 'curated', 'type_implied'];
const isSpecific = (b) => SPECIFIC.includes(b);
const isInstEstablished = (b) => INST_ESTABLISHED.includes(b);
const precision = (d) => (/^\d{4}-\d{2}-\d{2}$/.test(d) ? 'day' : /^\d{4}-\d{2}$/.test(d) ? 'month' : /^\d{4}$/.test(d) ? 'year' : 'none');
const isEi = (a) => a.s.some(([c]) => sectorById.get(c)?.ei);
const yearOf = (d) => (d ? Number(d.slice(0, 4)) : null);
const ASY = yearOf(AS_OF);

function techsOf(a) {
  return [...new Set(a.s.filter(([, b]) => isSpecific(b)).flatMap(([c]) => sectorById.get(c)?.techs ?? []))].sort();
}
const STAGES = { critical_minerals: ['Extraction', 'Refining', 'Recovery'], grid_equipment: ['Components', 'Assembly'], transformers: ['Components', 'Assembly'], transmission: ['Components', 'Assembly', 'Deployment'], power_electronics: ['Components'] };
function segmentsOf(a) {
  return [...new Set(a.s.filter(([, b]) => isSpecific(b)).flatMap(([c]) => STAGES[c] ?? []))];
}

/** an agreement record of a focal economy's file */
function record(a, focal) {
  return {
    id: a.id, title: a.title, parties: a.parties, type: a.type, label: a.label, legal: a.legal, legal_basis: a.legal_basis,
    date: a.date, date_precision: precision(a.date), signature_date: a.signature_date, effective_date: a.effective_date,
    status: a.status, status_raw: a.status_raw ?? a.status, category: a.category,
    sectors: a.s, instruments: a.i, techs: techsOf(a), segments: segmentsOf(a), ei: isEi(a), via_eu: !a.parties.includes(focal) && a.parties.includes('EUU'),
    sources: a.src.map(([dataset, record_id, url]) => ({ dataset, record_id, url })),
    primary_url: a.primary_url, primary_available: a.pa ?? !!a.primary_url, confidence: a.confidence, source_count: new Set(a.src.map((s) => s[0])).size,
    delivery: a.d.map(([text, status, url]) => ({ text, status, url })), trade_depth_index: a.tdi,
  };
}

// ---- pairs ---------------------------------------------------------------------------------------------
const key = (x, y) => (x < y ? `${x}|${y}` : `${y}|${x}`);
const pairs = new Map(); // key -> { a, b, items: [{ agr, viaEu }] }
const addPair = (x, y, agr, viaEu) => {
  if (x === y) return;
  const k = key(x, y);
  const [a, b] = k.split('|');
  const p = pairs.get(k) ?? { a, b, items: [] };
  if (!p.items.some((it) => it.agr.id === agr.id)) p.items.push({ agr, viaEu });
  pairs.set(k, p);
};
for (const a of A) {
  for (const x of a.parties) for (const y of a.parties) if (x < y) addPair(x, y, a, false);
  if (a.parties.includes('EUU')) {
    for (const m of EU_MEMBERS) {
      if (a.parties.includes(m)) continue;
      for (const y of a.parties) if (y !== 'EUU' && y !== m) addPair(m, y, a, true);
    }
  }
}

const isActive = (a) => {
  if (a.status === 'cancelled' || a.status === 'inactive') return false;
  if (a.status === 'in_force' || vocab.delivery_statuses.includes(a.status)) return true;
  const y = yearOf(a.date);
  return y !== null && ASY - y <= 10;
};
const FRAMEWORK = ['treaty', 'fta', 'epa', 'strategic_partnership', 'framework', 'mou', 'sectoral_partnership', 'financing_agreement'];
const SP_LABELS = ['comprehensive_strategic', 'global_strategic', 'special_strategic', 'strategic'];
const SECTORAL_EXCLUDE = ['trade', 'science_rd', 'general_clean_energy'];

/** pair sector grades: specific from specific bases; text when two or more agreements mention it in the text (the fixture's stand-in for two sentences) */
function sectorGrades(items) {
  const out = new Map();
  const textHits = new Map();
  for (const { agr } of items)
    for (const [c, b] of agr.s) {
      if (isSpecific(b)) out.set(c, 'specific');
      else textHits.set(c, (textHits.get(c) ?? 0) + 1);
    }
  for (const [c, n] of textHits) if (!out.has(c) && n >= 2) out.set(c, 'text');
  return out;
}

/**
 * The depth class and its rule, the rule worded as R's agr_depth_classes() words it (R/agreements/agr_pairs.R):
 * "<class> <label> (<scope>): <evidence> in <agreement id>", the evidence of the newest agreement that meets it.
 */
function depth(items, scope) {
  const head = (d) => `${d} ${vocab.depth_classes[d]} (${scope})`;
  const act = items.filter(({ agr }) => isActive(agr));
  if (!act.length) return { d: 0, rule: head(0) };
  const deep = act.filter(({ agr }) => agr.parties.length <= 20);
  const grades = sectorGrades(deep);
  const sectoral = [...grades.keys()].some((c) => !SECTORAL_EXCLUDE.includes(c));
  const opOf = (agr) => agr.i.filter(([c, b]) => isInstEstablished(b) && instById.get(c)?.operational);
  const groupsOf = (agr) => [...new Set(opOf(agr).map(([c]) => instById.get(c).group))].sort();
  const opGroups = new Set(deep.flatMap(({ agr }) => groupsOf(agr)));
  const inEffect = (agr) => (agr.legal === 'binding' && agr.status === 'in_force') || vocab.delivery_statuses.includes(agr.status);
  const effect = deep.some(({ agr }) => inEffect(agr));
  const first = (ok) => deep.find(({ agr }) => ok(agr))?.agr ?? null;
  const secOf = (agr) => agr.s.filter(([c]) => !SECTORAL_EXCLUDE.includes(c) && grades.has(c)).map(([c]) => `${c}:${grades.get(c)}`);
  const fiman = ['finance', 'investment', 'manufacturing'];
  const c4 = opGroups.size > 0;
  if (c4 && opGroups.has('trade') && fiman.some((g) => opGroups.has(g)) && sectoral && effect) {
    const et = first((a) => groupsOf(a).includes('trade'));
    const ef = first((a) => groupsOf(a).some((g) => fiman.includes(g)));
    const e3 = first((a) => secOf(a).length > 0);
    const ee = first(inEffect);
    return {
      d: 5,
      rule: `${head(5)}: trade instrument in ${et.id}; finance, investment or manufacturing (${groupsOf(ef).join('|')}) in ${ef.id}; sectoral ${secOf(e3).join(', ')} in ${e3.id}; in effect: ${ee.id}`,
    };
  }
  if (c4) {
    const e4 = first((a) => opOf(a).length > 0);
    return { d: 4, rule: `${head(4)}: ${opOf(e4).map(([c, b]) => `${c} [${b}]`).join(', ')} in ${e4.id}` };
  }
  if (sectoral) {
    const e3 = first((a) => secOf(a).length > 0);
    return { d: 3, rule: `${head(3)}: ${secOf(e3).join(', ')} in ${e3.id}` };
  }
  const e2 = first((a) => FRAMEWORK.includes(a.type) || SP_LABELS.includes(a.label));
  if (e2) return { d: 2, rule: `${head(2)}: ${FRAMEWORK.includes(e2.type) ? e2.type : 'strategic partnership label'} in ${e2.id}` };
  const e1 = act[0].agr;
  return { d: 1, rule: `${head(1)}: ${deep.length ? e1.type : 'only plurilaterals above max_parties_for_depth'} in ${e1.id}` };
}

const newestOf = (items) => items.map(({ agr }) => agr.date).filter(Boolean).sort().at(-1) ?? null;
const oldestActiveOf = (items) => items.filter(({ agr }) => isActive(agr)).map(({ agr }) => agr.date).filter(Boolean).sort()[0] ?? null;
const confRank = { high: 3, medium: 2, low: 1 };

function profile(p) {
  const items = [...p.items].sort((x, y) => (y.agr.date ?? '').localeCompare(x.agr.date ?? ''));
  const eiItems = items.filter(({ agr }) => isEi(agr));
  const all = depth(items, 'all agreements');
  const ei = depth(eiItems, 'electro-industrial agreements');
  const grades = sectorGrades(items);
  const inst = {};
  for (const { agr } of items)
    for (const [c, b] of agr.i) {
      const g = instById.get(c).group;
      inst[g] = Math.max(inst[g] ?? 0, isInstEstablished(b) ? 2 : 1);
    }
  const n = items.length;
  const count = (st) => items.filter(({ agr }) => agr.status === st).length;
  const nDelivery = items.filter(({ agr }) => vocab.delivery_statuses.includes(agr.status)).length;
  const tradeDepth = items.filter(({ agr }) => agr.tdi !== null && agr.status === 'in_force').map(({ agr }) => agr.tdi);
  return {
    n, n_bilateral: items.filter(({ agr }) => agr.parties.length === 2).length, n_plurilateral: items.filter(({ agr }) => agr.parties.length > 2).length,
    n_via_eu: items.filter((it) => it.viaEu).length, n_active: items.filter(({ agr }) => isActive(agr)).length,
    n_recent: items.filter(({ agr }) => agr.date && ASY - yearOf(agr.date) <= 5).length,
    oldest_active: oldestActiveOf(items), newest: newestOf(items),
    has_sp: items.some(({ agr }) => agr.type === 'strategic_partnership' || SP_LABELS.includes(agr.label)),
    has_trade: items.some(({ agr }) => ['fta', 'epa'].includes(agr.type) && ['in_force', 'signed'].includes(agr.status)),
    has_sectoral: [...grades].some(([c, g]) => g === 'specific' && !SECTORAL_EXCLUDE.includes(c)),
    sectors: [...grades].sort(([x], [y]) => x.localeCompare(y)),
    ei_sectors: [...grades.keys()].filter((c) => sectorById.get(c)?.ei).sort(),
    techs: [...new Set(items.flatMap(({ agr }) => techsOf(agr)))].sort(),
    segments: [...new Set(items.flatMap(({ agr }) => segmentsOf(agr)))],
    inst,
    n_announced: count('announced'), n_signed: count('signed'), n_in_force: count('in_force'), n_delivery: nDelivery,
    share_delivery: n ? Math.round((nDelivery / n) * 1000) / 1000 : null,
    trade_depth_index: tradeDepth.length ? Math.max(...tradeDepth) : null,
    depth_all: all.d, depth_all_rule: all.rule, depth_ei: ei.d, depth_ei_rule: ei.rule,
    sources: [...new Set(items.flatMap(({ agr }) => agr.src.map((s) => s[0])))].sort(),
    max_confidence: items.map(({ agr }) => agr.confidence).sort((x, y) => confRank[y] - confRank[x])[0] ?? null,
    agreement_ids: items.map(({ agr }) => agr.id),
  };
}
const profiles = new Map([...pairs].map(([k, p]) => [k, { ...p, ...profile(p) }]));

// ---- gaps (invented components, as scripts/36 would join them) ---------------------------------------------
// complementarity: high (recommended, or need and capability both high), not_assessed (need or capability missing:
// quadrant none), else low; architecture: strong only when an agreement covers the technology at an established
// level (R's strong_depth is null by default)
const G = (partner, tech, side, need, capability, recommended, friendshore, aib_shared, covered_grade) => {
  const high = recommended > 0 || (need !== null && capability !== null && need >= 0.67 && capability >= 0.75);
  const complementarity = high ? 'high' : need === null || capability === null ? 'not_assessed' : 'low';
  const architecture = covered_grade === 'specific' ? 'strong' : 'weak';
  const quadrant = complementarity === 'not_assessed' ? 'none' : high ? (architecture === 'strong' ? 'established' : 'opportunity') : architecture === 'strong' ? 'legacy' : 'none';
  return { partner, tech, side, need, capability, recommended, friendshore, aib_shared, complementarity, covered_grade, architecture, quadrant };
};
const GAPS = {
  JPN: [
    G('AUS', 'Batteries', 'needs', 0.81, 0.93, 3, 0.71, ['SC-BATTERIES'], 'specific'),
    G('AUS', 'Green Hydrogen', 'needs', 0.74, 0.81, 1, 0.64, [], 'specific'),
    G('USA', 'Nuclear', 'needs', 0.7, 0.88, 2, 0.66, ['SC-NUCLEAR'], 'text'),
    G('KOR', 'Batteries', 'needs', 0.81, 0.97, 3, 0.62, ['SC-BATTERIES', 'SC-UNNAMED'], 'none'),
    G('IND', 'Solar', 'needs', 0.72, 0.79, 2, 0.48, [], 'none'),
    G('IND', 'Electric Grid', 'supplies', 0.85, 0.9, 1, null, [], 'none'),
    G('USA', 'Oil', 'needs', 0.3, 0.95, 0, 0.55, [], 'specific'),
    G('ITA', 'Wind', 'needs', 0.2, 0.41, 0, 0.3, [], 'none'),
    G('NZL', 'Wind', 'needs', 0.7, null, 0, null, [], 'none'),
  ],
  AUS: [
    G('JPN', 'Batteries', 'supplies', 0.81, 0.93, 3, 0.71, ['SC-BATTERIES'], 'specific'),
    G('KOR', 'Batteries', 'supplies', 0.78, 0.93, 2, 0.6, ['SC-BATTERIES'], 'none'),
    G('USA', 'Solar', 'needs', 0.69, 0.77, 1, 0.58, [], 'none'),
    G('DEU', 'Green Hydrogen', 'supplies', 0.72, 0.81, 2, 0.52, [], 'specific'),
  ],
  USA: [
    G('JPN', 'Nuclear', 'supplies', 0.7, 0.88, 2, 0.66, ['SC-NUCLEAR'], 'text'),
    G('KOR', 'Batteries', 'needs', 0.88, 0.97, 3, 0.7, ['SC-BATTERIES'], 'none'),
    G('AUS', 'Electric Vehicles', 'needs', 0.4, 0.6, 0, 0.5, [], 'specific'),
  ],
  DEU: [
    G('JPN', 'Electric Grid', 'needs', 0.71, 0.86, 1, 0.61, [], 'none'),
    G('AUS', 'Green Hydrogen', 'needs', 0.72, 0.81, 2, 0.52, [], 'specific'),
    G('USA', 'Nuclear', 'needs', 0.2, 0.9, 0, 0.58, [], 'none'),
  ],
};
const EXEC = {
  JPN: [
    { tech: 'Solar', stages: ['Components', 'Assembly'], priority_basis: 'policy_priority', priority: 0.86, suppliers_without_coverage: ['IND', 'KOR'] },
    { tech: 'Electric Grid', stages: ['Components'], priority_basis: 'secure', priority: null, suppliers_without_coverage: [] },
  ],
  AUS: [{ tech: 'Solar', stages: ['Refining'], priority_basis: 'policy_priority|partner', priority: 0.91, suppliers_without_coverage: ['USA'] }],
  USA: [{ tech: 'Batteries', stages: ['Assembly'], priority_basis: 'secure', priority: 0.95, suppliers_without_coverage: ['KOR'] }],
  DEU: [],
};

// ---- write -----------------------------------------------------------------------------------------------
const h8 = (s) => createHash('md5').update(s).digest('hex').slice(0, 8);
mkdirSync(OUT, { recursive: true });
for (const f of readdirSync(OUT)) rmSync(join(OUT, f));

const countryFiles = {};
for (const iso of FOCAL) {
  const mine = [...profiles.values()].filter((p) => p.a === iso || p.b === iso);
  const partners = mine
    .map((p) => {
      const { a, b, items, ...rest } = p;
      void items;
      return { partner: a === iso ? b : a, ...rest };
    })
    .sort((x, y) => y.depth_all - x.depth_all || y.n - x.n || x.partner.localeCompare(y.partner));
  const ids = new Set(partners.flatMap((p) => p.agreement_ids));
  const agreements = A.filter((a) => ids.has(a.id)).sort((x, y) => (y.date ?? '').localeCompare(x.date ?? '')).map((a) => record(a, iso));
  const doc = { iso3: iso, agreements, partners, gaps: GAPS[iso], execution_gaps: EXEC[iso] };
  const text = JSON.stringify(doc);
  const file = `agreements-${iso}.${h8(text)}.json`;
  writeFileSync(join(OUT, file), text);
  countryFiles[iso] = file;
}

const all = [...profiles.values()].sort((x, y) => x.a.localeCompare(y.a) || x.b.localeCompare(y.b));
const joinDistinct = (xs) => [...new Set(xs.filter(Boolean))].sort().join('|');
const col = (f) => all.map(f);
const economies = [...new Set(all.flatMap((p) => [p.a, p.b]))].sort();
const glob = {
  pairs: {
    a: col((p) => p.a), b: col((p) => p.b), n: col((p) => p.n), n_recent: col((p) => p.n_recent), newest: col((p) => p.newest),
    depth_all: col((p) => p.depth_all), depth_ei: col((p) => p.depth_ei), has_sp: col((p) => p.has_sp), has_trade: col((p) => p.has_trade), has_sectoral: col((p) => p.has_sectoral),
    n_delivery: col((p) => p.n_delivery), ei_sectors: col((p) => p.ei_sectors.join('|')),
    types: col((p) => [...new Set(p.items.map(({ agr }) => agr.type))].sort().join('|')), sources: col((p) => p.sources.join('|')),
    // the additive columns of the all-economies filters ("|"-joined, "" for none)
    years: col((p) => joinDistinct(p.items.map(({ agr }) => agr.date?.slice(0, 4)))),
    years_ei: col((p) => joinDistinct(p.items.filter(({ agr }) => isEi(agr)).map(({ agr }) => agr.date?.slice(0, 4)))),
    igroups: col((p) => joinDistinct(Object.entries(p.inst).filter(([, l]) => l === 2).map(([g]) => g))),
    legal: col((p) => joinDistinct(p.items.map(({ agr }) => agr.legal))),
    status: col((p) => joinDistinct(p.items.map(({ agr }) => agr.status))),
    stages: col((p) => joinDistinct(p.segments)),
  },
  countries: (() => {
    const rows = economies.map((iso) => {
      const ps = all.filter((p) => p.a === iso || p.b === iso);
      const agrs = new Set(ps.flatMap((p) => p.agreement_ids));
      const list = A.filter((a) => agrs.has(a.id));
      return { iso3: iso, n_partners: ps.length, n_partners_d3: ps.filter((p) => p.depth_all >= 3).length, n_agreements: agrs.size, n_ei_agreements: list.filter(isEi).length, newest: list.map((a) => a.date).sort().at(-1) ?? null };
    });
    return Object.fromEntries(['iso3', 'n_partners', 'n_partners_d3', 'n_agreements', 'n_ei_agreements', 'newest'].map((k) => [k, rows.map((r) => r[k])]));
  })(),
};
const gtext = JSON.stringify(glob);
const gfile = `agreements-global.${h8(gtext)}.json`;
writeFileSync(join(OUT, gfile), gtext);

const bySource = {};
for (const a of A) for (const [s] of a.src) bySource[s] = (bySource[s] ?? 0) + 1;
const manifest = {
  version: 1,
  created: '2026-11-06T12:00:00Z',
  as_of: AS_OF,
  git_commit: 'synthetic',
  files: { global: gfile, countries: countryFiles },
  vocab: {
    ...vocab,
    sources: vocab.sources.map((s) => ({ ...s, n_records: bySource[s.id] ?? 0, last_updated: s.id === 'gsp' ? '2025-06-30' : s.id === 'desta' ? '2025-09-15' : '2026-11-01' })),
    specific_bases: SPECIFIC,
    established_instrument_bases: INST_ESTABLISHED,
    // strategic compact names (config/aib_strategic_compacts.yml); SC-UNNAMED is left out on purpose (shown by its id)
    compacts: { 'SC-BATTERIES': 'Battery Materials & Cells', 'SC-NUCLEAR': 'Nuclear Fuel & Reactor Systems', 'SC-GRID': 'Grid Equipment & Conductors', 'SC-SOLAR': 'Solar Manufacturing' },
  },
  names,
  counts: { records: A.reduce((n, a) => n + a.src.length, 0), canonical: A.length, pairs: all.length, countries: economies.length, by_source: bySource },
  caveats: [
    'There is no comprehensive global registry of non-binding MOUs, joint statements and joint initiatives: an empty cell means no agreement was found in these sources, not that no cooperation exists.',
    'SYNTHETIC data for tests: invented records in the shape of the real export.',
  ],
};
writeFileSync(join(OUT, 'manifest.json'), JSON.stringify(manifest, null, 1));
console.log(`wrote ${OUT}: ${A.length} agreements, ${all.length} pairs, files ${Object.values(countryFiles).join(', ')}, ${gfile}`);
const depths = [...new Set(all.flatMap((p) => [p.depth_all, p.depth_ei]))].sort();
console.log(`depth classes present: ${depths.join(', ')}`);
for (const iso of FOCAL) console.log(iso, all.filter((p) => p.a === iso || p.b === iso).map((p) => `${p.a === iso ? p.b : p.a}:${p.depth_all}/${p.depth_ei}`).join(' '));
