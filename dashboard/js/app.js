/**
 * Energy Transition Intelligence Hub — Application Logic v2
 * NEIS Centre palette · Tech × SC picker · Scatter-first panel
 * Partnership world map · Region zoom · Top-3 tooltips
 */
'use strict';

// ─── Aliases ────────────────────────────────────────────────────────────────
const D = window.OSI_DATA;
const C = window.NEIS;           // colour constants

// ─── App State ───────────────────────────────────────────────────────────────
const STATE = {
  view:            'world',      // 'world' | 'partnerships'
  index:           'es',         // 'es' | 'eo' | 'psi'
  activeTech:      null,         // null = All
  activeSC:        null,         // null = All
  region:          'All',
  selectedCountry: null,
  panelTab:        'scatter',
  techDetailMode:  'es',
  partnerCountry:  null,
  partnerType:     'friendshore',
  selectedPartner: null,
  userWeights: {
    es: Object.fromEntries(D.ES_CATEGORIES.map(c => [c.key, c.weight])),
    eo: Object.fromEntries(D.EO_CATEGORIES.map(c => [c.key, c.weight])),
  },
};

// ─── Chart instances cache ────────────────────────────────────────────────────
const CHARTS = {};
function destroyChart(id) {
  if (CHARTS[id]) { try { CHARTS[id].destroy(); } catch(e){} delete CHARTS[id]; }
}

// ─── Helpers ─────────────────────────────────────────────────────────────────
function fmt(v) { return (typeof v === 'number' && !isNaN(v)) ? v.toFixed(2) : '—'; }
function fmtPct(v) { return (typeof v === 'number' && !isNaN(v)) ? (v*100).toFixed(0)+'%' : '—'; }

function getCountry(iso3) { return D.COUNTRIES.find(c => c.iso3 === iso3); }

// Return the score for a country under current STATE.index/tech/sc
function getScore(country, index, tech, sc) {
  index = index || STATE.index;
  tech  = (tech  !== undefined) ? tech  : STATE.activeTech;
  sc    = (sc    !== undefined) ? sc    : STATE.activeSC;
  if (!country) return NaN;
  const key = index === 'psi' ? 'psi' : index;
  if (tech && sc) {
    const row = country.techScores?.find(r => r.tech === tech && r.sc === sc);
    if (row) return key === 'psi' ? row.psi : (key === 'es' ? row.es : row.eo);
    // fallback to aggregate
    return country[key] ?? NaN;
  }
  if (tech) {
    const rows = country.techScores?.filter(r => r.tech === tech) || [];
    if (rows.length) return rows.reduce((a,r) => a + (key === 'es' ? r.es : key === 'eo' ? r.eo : r.psi), 0) / rows.length;
    return country[key] ?? NaN;
  }
  return country[key] ?? NaN;
}

function indexColor(idx) {
  return idx === 'es' ? C.esColor : idx === 'eo' ? C.eoColor : C.psiColor;
}
function indexLabel(idx) {
  return idx === 'es' ? 'Energy Security' : idx === 'eo' ? 'Econ. Opportunity' : 'Partnership';
}

// D3 colour scale for choropleth — uses actual data range for better contrast
function makeColorScale(scores, idx) {
  const color = indexColor(idx);
  // Collect all country scores for the active index/tech/sc to normalize the domain
  const vals = D.COUNTRIES.map(c => getScore(c, idx, STATE.activeTech, STATE.activeSC))
                           .filter(v => !isNaN(v));
  const lo_val = vals.length ? Math.max(0, d3.quantile(vals.sort(d3.ascending), 0.05)) : 0;
  const hi_val = vals.length ? Math.min(1, d3.quantile(vals.sort(d3.ascending), 0.95)) : 1;
  const domLo  = Math.max(0, lo_val - 0.05);
  const domHi  = Math.min(1, hi_val + 0.05);
  return d3.scaleLinear()
           .domain([domLo, domHi])
           .range(['#152D56', color])
           .clamp(true);
}

// ─── D3 Map Setup ─────────────────────────────────────────────────────────────
let svgWorld, gMap, projection, pathGen, zoomBehaviour, currentTransform;
let partnerSvg, gPartnerMap;

const ISO_NUM_TO_A3 = {};   // populated after TopoJSON loaded
let topoData;

function initWorldMap(containerId, svgId) {
  const container = document.getElementById(containerId);
  const svg = d3.select('#' + svgId);
  const w = container.clientWidth;
  const h = container.clientHeight;

  svg.attr('width', w).attr('height', h);

  const proj = d3.geoNaturalEarth1()
    .scale(w / 6.5)
    .translate([w / 2, h / 2]);
  const path = d3.geoPath().projection(proj);

  const g = svg.append('g');

  const zoom = d3.zoom()
    .scaleExtent([0.5, 12])
    .on('zoom', ev => {
      g.attr('transform', ev.transform);
      if (containerId === 'map-container') currentTransform = ev.transform;
    });

  svg.call(zoom).on('dblclick.zoom', null);

  // Ocean background
  svg.insert('rect', ':first-child')
    .attr('width', w).attr('height', h)
    .attr('fill', '#050F1E');

  return { svg, g, proj, path, zoom, w, h };
}

async function loadTopoAndRender() {
  try {
    topoData = await d3.json(
      'https://cdn.jsdelivr.net/npm/world-atlas@2/countries-110m.json'
    );

    // Build ISO numeric → A3 map from OSI data (field is isoN in data.js)
    D.COUNTRIES.forEach(c => {
      if (c.isoN) ISO_NUM_TO_A3[String(c.isoN)] = c.iso3;
    });

    // ── World view map ──
    const wm = initWorldMap('map-container', 'world-map');
    svgWorld = wm.svg; gMap = wm.g;
    projection = wm.proj; pathGen = wm.path; zoomBehaviour = wm.zoom;

    const countries = topojson.feature(topoData, topoData.objects.countries);
    gMap.selectAll('.country-path')
      .data(countries.features)
      .join('path')
      .attr('class', 'country-path')
      .attr('d', pathGen)
      .on('mousemove', onCountryMousemove)
      .on('mouseleave', onCountryMouseleave)
      .on('click', (ev, d) => {
        const iso3 = ISO_NUM_TO_A3[String(d.id)];
        if (iso3) openCountryPanel(iso3);
      });

    // ── Partnerships map ──
    const pm = initWorldMap('partners-map-container', 'partners-world-map');
    partnerSvg = pm.svg; gPartnerMap = pm.g;

    const pCountries = topojson.feature(topoData, topoData.objects.countries);
    gPartnerMap.selectAll('.country-path')
      .data(pCountries.features)
      .join('path')
      .attr('class', 'country-path')
      .attr('d', pm.path)
      .on('click', (ev, d) => {
        const iso3 = ISO_NUM_TO_A3[String(d.id)];
        if (iso3) {
          document.getElementById('partner-home-select').value = iso3;
          STATE.partnerCountry = iso3;
          renderPartners();
        }
      });

    renderMapColors();
    hideLoading();
  } catch(e) {
    console.error('Failed to load TopoJSON:', e);
    hideLoading();
  }
}

function hideLoading() {
  const el = document.getElementById('loading');
  if (el) { el.classList.add('hidden'); setTimeout(() => el.remove(), 500); }
}

// ─── Map Colour Render ────────────────────────────────────────────────────────
function renderMapColors() {
  if (!gMap) return;
  const scale = makeColorScale(null, STATE.index);
  gMap.selectAll('.country-path').attr('fill', d => {
    const iso3 = ISO_NUM_TO_A3[String(d.id)];
    if (!iso3) return '#1C2F4A';
    const c = getCountry(iso3);
    if (!c) return '#1C2F4A';
    const v = getScore(c);
    return isNaN(v) ? '#1A2E4A' : scale(v);
  });
  // Legend label
  const lbl = document.getElementById('legend-index-label');
  if (lbl) lbl.textContent = indexLabel(STATE.index);
  const bar = document.getElementById('legend-bar');
  if (bar) { bar.className = 'legend-bar ' + STATE.index; }
}

function renderPartnerMapColors(homeIso, topPartnerIsos) {
  if (!gPartnerMap) return;
  const scale = makeColorScale(null, 'psi');
  const top1 = topPartnerIsos[0];
  const top5set = new Set(topPartnerIsos.slice(0, 5));

  gPartnerMap.selectAll('.country-path').each(function(d) {
    const iso3 = ISO_NUM_TO_A3[String(d.id)];
    const el = d3.select(this);
    el.classed('partner-top1', iso3 === top1)
      .classed('partner-top5', iso3 !== top1 && top5set.has(iso3))
      .classed('highlighted', iso3 === homeIso)
      .classed('dimmed', iso3 && iso3 !== homeIso && !top5set.has(iso3));
    const c = getCountry(iso3);
    if (!c) { el.attr('fill', '#1C2F4A'); return; }
    if (iso3 === homeIso) { el.attr('fill', C.gold); return; }
    if (top5set.has(iso3)) { el.attr('fill', C.psiColor); return; }
    el.attr('fill', '#1C2F4A');
  });

  const banner = document.getElementById('partner-map-legend');
  if (banner) {
    banner.style.display = 'block';
    banner.textContent = '⭐ Top-5 ' + STATE.partnerType + ' partners highlighted';
  }
}

// ─── Region Zoom ──────────────────────────────────────────────────────────────
function zoomToRegion(region) {
  if (!svgWorld || !zoomBehaviour) return;
  if (!region || region === 'All') {
    svgWorld.transition().duration(700).call(zoomBehaviour.transform, d3.zoomIdentity);
    return;
  }
  const bounds = D.REGION_BOUNDS[region];
  if (!bounds) return;
  const [lon0, lat0, lon1, lat1] = bounds;
  const container = document.getElementById('map-container');
  const w = container.clientWidth;
  const h = container.clientHeight;

  const p0 = projection([lon0, lat1]);   // top-left corner [x,y]
  const p1 = projection([lon1, lat0]);   // bottom-right

  if (!p0 || !p1) return;
  const dx = p1[0] - p0[0];
  const dy = p1[1] - p0[1];
  const cx = (p0[0] + p1[0]) / 2;
  const cy = (p0[1] + p1[1]) / 2;
  const scale = Math.min(8, 0.85 / Math.max(dx / w, dy / h));
  const tx = w / 2 - scale * cx;
  const ty = h / 2 - scale * cy;

  svgWorld.transition().duration(750)
    .call(zoomBehaviour.transform, d3.zoomIdentity.translate(tx, ty).scale(scale));
}

// ─── Tooltip ──────────────────────────────────────────────────────────────────
function onCountryMousemove(event, d) {
  const iso3 = ISO_NUM_TO_A3[String(d.id)];
  const country = iso3 ? getCountry(iso3) : null;
  const tt = document.getElementById('map-tooltip');
  if (!tt) return;

  if (!country) { tt.classList.remove('visible'); return; }

  const esV  = getScore(country, 'es', null, null);
  const eoV  = getScore(country, 'eo', null, null);
  const psiV = getScore(country, 'psi', null, null);

  const topEO  = D.topIndustries(country, 'eo',  3);
  const topES  = D.topIndustries(country, 'es',  3);
  const topPol = D.topIndustries(country, 'policy', 3);

  function industryRows(arr) {
    return arr.map((r, i) =>
      `<div class="tt-industry-row">
        <span class="rank">${i+1}.</span>
        <span>${r.icon} ${r.tech} · ${r.sc}</span>
        <span class="score">${fmt(r.score)}</span>
      </div>`
    ).join('');
  }

  tt.innerHTML = `
    <div class="tt-country">${country.name}</div>
    <div style="font-size:0.62rem;color:var(--text-muted);margin-bottom:5px">${country.region}</div>
    <div class="tt-scores">
      <span class="tt-score-pill" style="background:${C.esColor}22;color:${C.esColor}">ES ${fmt(esV)}</span>
      <span class="tt-score-pill" style="background:${C.eoColor}22;color:${C.eoColor}">EO ${fmt(eoV)}</span>
      <span class="tt-score-pill" style="background:${C.psiColor}22;color:${C.psiColor}">PSI ${fmt(psiV)}</span>
    </div>
    <div class="tt-section-label">Top EO Industries</div>
    ${industryRows(topEO)}
    <div class="tt-section-label">Top ES Industries</div>
    ${industryRows(topES)}
    <div class="tt-section-label">Top Policy Industries</div>
    ${industryRows(topPol)}
  `;
  tt.classList.add('visible');

  const rect = document.getElementById('map-container').getBoundingClientRect();
  let x = event.clientX - rect.left + 14;
  let y = event.clientY - rect.top  - 14;
  if (x + 275 > rect.width)  x = event.clientX - rect.left - 285;
  if (y + 300 > rect.height) y = rect.height - 310;
  tt.style.left = x + 'px';
  tt.style.top  = y + 'px';
}
function onCountryMouseleave() {
  const tt = document.getElementById('map-tooltip');
  if (tt) tt.classList.remove('visible');
}

// ─── Tech × SC Picker ─────────────────────────────────────────────────────────
function buildTechPicker() {
  const picker = document.getElementById('tech-picker');
  if (!picker) return;
  picker.innerHTML = '';

  const groups = [
    { label: 'Fossil',    keys: ['Coal','Oil','Gas'] },
    { label: 'Clean',     keys: ['Solar','Wind','Nuclear','Green Hydrogen','Geothermal'] },
    { label: 'Clean Tech',keys: ['Electric Vehicles','Batteries','Electric Grid'] },
  ];

  groups.forEach(grp => {
    const section = document.createElement('div');
    section.className = 'picker-section';

    const lbl = document.createElement('span');
    lbl.className = 'picker-label';
    lbl.textContent = grp.label;
    section.appendChild(lbl);

    const group = document.createElement('div');
    group.className = 'picker-group';

    grp.keys.forEach(key => {
      const tech = D.TECHNOLOGIES.find(t => t.key === key);
      if (!tech) return;
      const btn = document.createElement('button');
      btn.className = 'tech-btn' + (STATE.activeTech === key ? ' active' : '');
      btn.dataset.tech = key;
      btn.innerHTML = `<span class="t-icon">${tech.icon}</span>${tech.label}`;
      btn.title = tech.label;
      btn.addEventListener('click', () => {
        if (STATE.activeTech === key) {
          STATE.activeTech = null;
        } else {
          STATE.activeTech = key;
        }
        updateTechPicker();
        renderMapColors();
        updatePickerLabel();
        if (STATE.selectedCountry) rerenderPanel();
      });
      group.appendChild(btn);
    });
    section.appendChild(group);
    picker.appendChild(section);
  });

  // Supply chain section
  const scSection = document.createElement('div');
  scSection.className = 'picker-section';
  const scLbl = document.createElement('span');
  scLbl.className = 'picker-label';
  scLbl.textContent = 'Chain';
  scSection.appendChild(scLbl);

  const scGroup = document.createElement('div');
  scGroup.className = 'picker-group';
  D.SUPPLY_CHAINS.forEach(sc => {
    const btn = document.createElement('button');
    btn.className = 'sc-btn' + (STATE.activeSC === sc.key ? ' active' : '');
    btn.dataset.sc = sc.key;
    btn.innerHTML = `<span class="sc-icon">${sc.icon}</span>${sc.label}`;
    btn.title = sc.desc;
    btn.addEventListener('click', () => {
      if (STATE.activeSC === sc.key) {
        STATE.activeSC = null;
      } else {
        STATE.activeSC = sc.key;
      }
      updateTechPicker();
      renderMapColors();
      updatePickerLabel();
      if (STATE.selectedCountry) rerenderPanel();
    });
    scGroup.appendChild(btn);
  });
  scSection.appendChild(scGroup);
  picker.appendChild(scSection);

  // Label
  const labelSpan = document.createElement('span');
  labelSpan.id = 'picker-selection-label';
  picker.appendChild(labelSpan);
  updatePickerLabel();
}

function updateTechPicker() {
  document.querySelectorAll('.tech-btn').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.tech === STATE.activeTech);
  });
  document.querySelectorAll('.sc-btn').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.sc === STATE.activeSC);
  });
}

function updatePickerLabel() {
  const el = document.getElementById('picker-selection-label');
  if (!el) return;
  const parts = [];
  if (STATE.activeTech) {
    const t = D.TECHNOLOGIES.find(x => x.key === STATE.activeTech);
    if (t) parts.push(t.icon + ' ' + t.label);
  }
  if (STATE.activeSC) {
    const s = D.SUPPLY_CHAINS.find(x => x.key === STATE.activeSC);
    if (s) parts.push(s.icon + ' ' + s.label);
  }
  el.textContent = parts.length ? '→ ' + parts.join(' · ') : 'All Industries';
}

// ─── Country Panel ────────────────────────────────────────────────────────────
function openCountryPanel(iso3) {
  const country = getCountry(iso3);
  if (!country) return;
  STATE.selectedCountry = iso3;

  // Highlight on map
  gMap.selectAll('.country-path').classed('highlighted', d => ISO_NUM_TO_A3[String(d.id)] === iso3);

  // Header
  document.getElementById('panel-country-name').textContent = country.name;
  document.getElementById('panel-country-meta').textContent = country.region + ' · ' + iso3;

  // Score badges
  document.querySelector('#badge-es .badge-value').textContent  = fmt(country.es);
  document.querySelector('#badge-eo .badge-value').textContent  = fmt(country.eo);
  document.querySelector('#badge-psi .badge-value').textContent = fmt(country.psi);

  // Switch to scatter tab
  switchPanelTab('scatter');

  // Open panel
  document.getElementById('detail-panel').classList.add('open');
}

function closeCountryPanel() {
  STATE.selectedCountry = null;
  document.getElementById('detail-panel').classList.remove('open');
  gMap?.selectAll('.country-path').classed('highlighted', false);
}

function rerenderPanel() {
  if (!STATE.selectedCountry) return;
  switchPanelTab(STATE.panelTab);
}

function switchPanelTab(tabName) {
  STATE.panelTab = tabName;
  document.querySelectorAll('.panel-tab').forEach(b => b.classList.toggle('active', b.dataset.tab === tabName));
  document.querySelectorAll('.tab-pane').forEach(p => p.classList.toggle('active', p.dataset.tab === tabName));

  const country = getCountry(STATE.selectedCountry);
  if (!country) return;

  if      (tabName === 'scatter')   renderScatterTab(country);
  else if (tabName === 'es')        renderESTab(country);
  else if (tabName === 'eo')        renderEOTab(country);
  else if (tabName === 'tech')      renderTechTab(country);
  else if (tabName === 'advanced')  renderAdvancedTab(country);
  else if (tabName === 'method')    renderMethodTab();
}

// ─── Scatter Tab (primary) ────────────────────────────────────────────────────
function renderScatterTab(country) {
  const wrap = document.getElementById('scatter-chart-wrap');
  if (!wrap) return;

  // Build points: one per tech × SC combination
  const points = [];
  const techColors = {};
  D.TECHNOLOGIES.forEach(t => { techColors[t.key] = t.color; });

  D.TECHNOLOGIES.forEach(tech => {
    D.SUPPLY_CHAINS.forEach(sc => {
      const row = country.techScores?.find(r => r.tech === tech.key && r.sc === sc.key);
      if (!row) return;
      const esV  = row.es;
      const eoV  = row.eo;
      if (isNaN(esV) || isNaN(eoV)) return;
      points.push({
        x: eoV,
        y: 1 - esV,           // (1-ES): higher = more vulnerable / needs imports
        label: tech.icon + ' ' + tech.label + ' · ' + sc.key,
        color: tech.color,
        tech: tech.key,
        sc: sc.key,
        r: 6,
      });
    });
  });

  destroyChart('scatter');
  const canvas = wrap.querySelector('canvas') || (() => {
    const c = document.createElement('canvas');
    wrap.appendChild(c);
    return c;
  })();

  CHARTS['scatter'] = new Chart(canvas, {
    type: 'bubble',
    data: {
      datasets: points.map(p => ({
        label: p.label,
        data: [{ x: p.x, y: p.y, r: p.r }],
        backgroundColor: p.color + 'BB',
        borderColor: p.color,
        borderWidth: 1.5,
      }))
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: { duration: 300 },
      plugins: {
        legend: { display: false },
        tooltip: {
          callbacks: {
            label: ctx => {
              const d = ctx.raw;
              return `${ctx.dataset.label}  EO: ${d.x.toFixed(2)}  ES-need: ${d.y.toFixed(2)}`;
            }
          },
          backgroundColor: '#152D56',
          borderColor: '#1E3A6A',
          borderWidth: 1,
          titleColor: '#EEF2F7',
          bodyColor: '#8DAABF',
          padding: 8,
        },
      },
      scales: {
        x: {
          title: { display: true, text: 'Economic Opportunity  →', color: C.eoColor, font: { size: 10 } },
          min: 0, max: 1,
          grid: { color: '#1E3A6A' },
          ticks: { color: '#4A6A88', font: { size: 9 } },
        },
        y: {
          title: { display: true, text: '← Low ES need    High ES need →', color: C.esColor, font: { size: 10 } },
          min: 0, max: 1,
          grid: { color: '#1E3A6A' },
          ticks: {
            color: '#4A6A88', font: { size: 9 },
            callback: v => (1-v).toFixed(1),
          },
        },
      },
    }
  });

  // Legend below chart
  let legendEl = wrap.nextElementSibling;
  if (legendEl && legendEl.id === 'scatter-legend') legendEl.remove();
  const legend = document.createElement('div');
  legend.id = 'scatter-legend';
  legend.style.cssText = 'display:flex;flex-wrap:wrap;gap:4px;margin-bottom:10px';
  const seenTechs = new Set();
  points.forEach(p => {
    if (seenTechs.has(p.tech)) return;
    seenTechs.add(p.tech);
    const tech = D.TECHNOLOGIES.find(t => t.key === p.tech);
    if (!tech) return;
    const span = document.createElement('span');
    span.style.cssText = `font-size:0.64rem;padding:2px 6px;border-radius:3px;border:1px solid ${tech.color}44;color:${tech.color};background:${tech.color}18`;
    span.textContent = tech.icon + ' ' + tech.label;
    legend.appendChild(span);
  });
  wrap.after(legend);
}

// ─── ES Tab ───────────────────────────────────────────────────────────────────
function renderESTab(country) {
  // Donut chart for weights
  destroyChart('es-donut');
  const donutCanvas = document.getElementById('es-donut');
  if (donutCanvas) {
    const cats = D.ES_CATEGORIES;
    const weights = cats.map(c => STATE.userWeights.es[c.key] ?? c.weight);
    CHARTS['es-donut'] = new Chart(donutCanvas, {
      type: 'doughnut',
      data: {
        labels: cats.map(c => c.key),
        datasets: [{ data: weights, backgroundColor: cats.map((_, i) => `hsl(${195 + i*22},60%,${45+i*2}%)`) }]
      },
      options: {
        responsive: true, maintainAspectRatio: false,
        cutout: '62%',
        plugins: {
          legend: { display: false },
          tooltip: {
            backgroundColor: '#152D56', borderColor: '#1E3A6A', borderWidth: 1,
            titleColor: '#EEF2F7', bodyColor: '#8DAABF',
            callbacks: { label: ctx => `${ctx.label}: ${ctx.parsed.toFixed(2)}` }
          }
        }
      }
    });
  }

  // Horizontal bars
  const barsEl = document.getElementById('es-bars');
  if (!barsEl) return;
  barsEl.innerHTML = '';
  D.ES_CATEGORIES.forEach(cat => {
    const v = country.esCategories?.[cat.key] ?? 0;
    const div = document.createElement('div');
    div.className = 'hbar-item';
    div.innerHTML = `
      <div class="hbar-label">
        <span class="hbar-name">${cat.key}</span>
        <span class="hbar-val">${fmt(v)}</span>
      </div>
      <div class="hbar-track">
        <div class="hbar-fill" style="width:${v*100}%;background:${C.esColor}"></div>
      </div>`;
    barsEl.appendChild(div);
  });
}

// ─── EO Tab ───────────────────────────────────────────────────────────────────
function renderEOTab(country) {
  destroyChart('eo-bar');
  const canvas = document.getElementById('eo-bar');
  if (canvas) {
    const cats = D.EO_CATEGORIES;
    const vals = cats.map(c => country.eoCategories?.[c.key] ?? 0);
    CHARTS['eo-bar'] = new Chart(canvas, {
      type: 'bar',
      data: {
        labels: cats.map(c => c.key),
        datasets: [{ data: vals, backgroundColor: C.eoColor + '99', borderColor: C.eoColor, borderWidth: 1.5, borderRadius: 3 }]
      },
      options: {
        indexAxis: 'y',
        responsive: true, maintainAspectRatio: false,
        plugins: {
          legend: { display: false },
          tooltip: {
            backgroundColor: '#152D56', borderColor: '#1E3A6A', borderWidth: 1,
            titleColor: '#EEF2F7', bodyColor: '#8DAABF',
          }
        },
        scales: {
          x: { min: 0, max: 1, grid: { color: '#1E3A6A' }, ticks: { color: '#4A6A88', font: { size: 9 } } },
          y: { grid: { display: false }, ticks: { color: '#8DAABF', font: { size: 9 } } }
        }
      }
    });
  }

  const barsEl = document.getElementById('eo-bars');
  if (!barsEl) return;
  barsEl.innerHTML = '';
  D.EO_CATEGORIES.forEach(cat => {
    const v = country.eoCategories?.[cat.key] ?? 0;
    const div = document.createElement('div');
    div.className = 'hbar-item';
    div.innerHTML = `
      <div class="hbar-label">
        <span class="hbar-name">${cat.key}</span>
        <span class="hbar-val">${fmt(v)}</span>
      </div>
      <div class="hbar-track">
        <div class="hbar-fill" style="width:${v*100}%;background:${C.eoColor}"></div>
      </div>`;
    barsEl.appendChild(div);
  });
}

// ─── Tech Heatmap Tab ─────────────────────────────────────────────────────────
function renderTechTab(country) {
  const mode = STATE.techDetailMode;
  const color = mode === 'es' ? C.esColor : C.eoColor;

  // Update toggle buttons
  document.querySelectorAll('.heatmap-btn').forEach(b => b.classList.toggle('active', b.dataset.mode === mode));

  // Build heatmap table
  const container = document.getElementById('tech-heatmap');
  if (!container) return;
  container.innerHTML = '';

  const scs = D.SUPPLY_CHAINS.map(s => s.key);
  const table = document.createElement('table');
  table.className = 'heatmap-table';

  // Header
  const thead = document.createElement('thead');
  const hrow = document.createElement('tr');
  hrow.innerHTML = '<th>Technology</th>' + scs.map(s => `<th>${s}</th>`).join('');
  thead.appendChild(hrow);
  table.appendChild(thead);

  // Body
  const tbody = document.createElement('tbody');
  D.TECHNOLOGIES.forEach(tech => {
    const tr = document.createElement('tr');
    tr.innerHTML = `<td class="row-label">${tech.icon} ${tech.label}</td>`;
    scs.forEach(sc => {
      const row = country.techScores?.find(r => r.tech === tech.key && r.sc === sc);
      const v = row ? (mode === 'es' ? row.es : row.eo) : null;
      const td = document.createElement('td');
      if (v !== null && !isNaN(v)) {
        const alpha = Math.round(v * 180).toString(16).padStart(2,'0');
        td.style.background = color + alpha;
        td.style.color = v > 0.6 ? '#071628' : '#EEF2F7';
        td.textContent = (v * 100).toFixed(0);
      } else {
        td.style.color = '#4A6A88';
        td.textContent = '—';
      }
      tr.appendChild(td);
    });
    tbody.appendChild(tr);
  });
  table.appendChild(tbody);
  container.appendChild(table);

  // Bar chart: average by tech
  destroyChart('tech-bar');
  const canvas = document.getElementById('tech-bar');
  if (canvas) {
    const labels = D.TECHNOLOGIES.map(t => t.icon + ' ' + t.label);
    const vals = D.TECHNOLOGIES.map(tech => {
      const rows = country.techScores?.filter(r => r.tech === tech.key) || [];
      if (!rows.length) return 0;
      return rows.reduce((s, r) => s + (mode === 'es' ? r.es : r.eo), 0) / rows.length;
    });
    CHARTS['tech-bar'] = new Chart(canvas, {
      type: 'bar',
      data: {
        labels,
        datasets: [{
          data: vals,
          backgroundColor: D.TECHNOLOGIES.map(t => t.color + '99'),
          borderColor:      D.TECHNOLOGIES.map(t => t.color),
          borderWidth: 1.5, borderRadius: 3,
        }]
      },
      options: {
        responsive: true, maintainAspectRatio: false,
        plugins: {
          legend: { display: false },
          tooltip: { backgroundColor: '#152D56', borderColor: '#1E3A6A', borderWidth: 1, titleColor: '#EEF2F7', bodyColor: '#8DAABF' }
        },
        scales: {
          x: { grid: { display: false }, ticks: { color: '#8DAABF', font: { size: 8 } } },
          y: { min: 0, max: 1, grid: { color: '#1E3A6A' }, ticks: { color: '#4A6A88', font: { size: 9 } } }
        }
      }
    });
  }
}

// ─── Advanced Tab ─────────────────────────────────────────────────────────────
function renderAdvancedTab(country) {
  const tbody = document.getElementById('advanced-table-body');
  if (!tbody) return;
  tbody.innerHTML = '';

  D.ES_CATEGORIES.forEach(cat => {
    const v = country.esCategories?.[cat.key];
    const w = STATE.userWeights.es[cat.key] ?? cat.weight;
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>Energy Security</td><td>${cat.key}</td>
      <td class="mono">${fmt(v)}</td><td class="mono">${w.toFixed(2)}</td>`;
    tbody.appendChild(tr);
  });
  D.EO_CATEGORIES.forEach(cat => {
    const v = country.eoCategories?.[cat.key];
    const w = STATE.userWeights.eo[cat.key] ?? cat.weight;
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>Econ. Opportunity</td><td>${cat.key}</td>
      <td class="mono">${fmt(v)}</td><td class="mono">${w.toFixed(2)}</td>`;
    tbody.appendChild(tr);
  });
}

// ─── Methodology Tab ──────────────────────────────────────────────────────────
function renderMethodTab() {
  const esText = document.getElementById('method-es-text');
  if (esText) esText.textContent =
    'The Energy Security Index aggregates 9 categories: ' +
    D.ES_CATEGORIES.map(c => c.key).join(', ') +
    '. Each category is normalised 0–1 (higher = better energy security) and combined as a weighted mean.';

  const eoText = document.getElementById('method-eo-text');
  if (eoText) eoText.textContent =
    'The Economic Opportunity Index aggregates 10 categories: ' +
    D.EO_CATEGORIES.map(c => c.key).join(', ') +
    '. Reflects market potential, production capacity, trade flows, and investment climate.';

  const psiText = document.getElementById('method-psi-text');
  if (psiText) psiText.textContent =
    'PSI = 0.4 × Friendshore + 0.4 × Export Partner + 0.2 × Development. ' +
    'Friendshore weights alignment, ES need (1 − home ES), and partner EO. ' +
    'Export partner weights demand size and trade compatibility. ' +
    'Development weights partner need, resource potential, and home capacity.';

  // ES weights
  const wEs = document.getElementById('method-weights-es');
  if (wEs) {
    wEs.innerHTML = '';
    const total = D.ES_CATEGORIES.reduce((s,c) => s + c.weight, 0);
    D.ES_CATEGORIES.forEach(cat => {
      const w = cat.weight / total;
      const row = document.createElement('div');
      row.className = 'method-weight-row';
      row.innerHTML = `
        <span class="method-weight-name">${cat.key}</span>
        <span class="method-weight-val">${(w*100).toFixed(0)}%</span>
        <div class="method-weight-bar"><div class="method-weight-fill" style="width:${w*100}%"></div></div>`;
      wEs.appendChild(row);
    });
  }

  const wEo = document.getElementById('method-weights-eo');
  if (wEo) {
    wEo.innerHTML = '';
    const total = D.EO_CATEGORIES.reduce((s,c) => s + c.weight, 0);
    D.EO_CATEGORIES.forEach(cat => {
      const w = cat.weight / total;
      const row = document.createElement('div');
      row.className = 'method-weight-row';
      row.innerHTML = `
        <span class="method-weight-name">${cat.key}</span>
        <span class="method-weight-val">${(w*100).toFixed(0)}%</span>
        <div class="method-weight-bar"><div class="method-weight-fill" style="width:${w*100}%"></div></div>`;
      wEo.appendChild(row);
    });
  }
}

// ─── Partnerships View ────────────────────────────────────────────────────────
function renderPartners() {
  const homeIso = STATE.partnerCountry;
  const home    = homeIso ? getCountry(homeIso) : null;
  const ptype   = STATE.partnerType;

  const list = document.getElementById('partners-list');
  if (!list) return;
  list.innerHTML = '';

  if (!home) {
    list.innerHTML = '<div class="empty-state"><div class="empty-icon">🌍</div><div class="empty-desc">Select a home country above.</div></div>';
    return;
  }

  // Compute partner scores — use data.js computePartnership (has proper ALIGN_COMPAT table)
  const scored = D.COUNTRIES
    .filter(c => c.iso3 !== homeIso)
    .map(partner => {
      const ps = D.computePartnership(home, partner);
      return { country: partner, score: ps?.[ptype] ?? NaN, ps };
    })
    .filter(x => !isNaN(x.score) && x.score > 0)
    .sort((a, b) => b.score - a.score)
    .slice(0, 20);

  // Highlight top-5 on map
  const top5 = scored.slice(0, 5).map(x => x.country.iso3);
  renderPartnerMapColors(homeIso, top5);

  // Render list
  scored.forEach((item, i) => {
    const { country, score } = item;
    const el = document.createElement('div');
    el.className = 'partner-item' + (STATE.selectedPartner === country.iso3 ? ' selected' : '');
    el.innerHTML = `
      <div class="partner-rank ${i===0?'top1':i<3?'top3':''}">${i+1}</div>
      <div class="partner-info">
        <div class="partner-name">${country.name}</div>
        <div class="partner-region">${country.region}</div>
      </div>
      <div class="partner-score">${fmt(score)}</div>`;
    el.addEventListener('click', () => selectPartner(country.iso3, scored));
    list.appendChild(el);
  });
}

function computePartnerScore(home, partner, ptype) {
  if (ptype === 'friendshore') {
    // Inspired by safer_friendshore.R: es_need, eo_partner, alignment, trade
    const esNeed     = 1 - (home.es ?? 0.5);
    const eoPartner  = partner.eo ?? 0.5;
    const alignment  = 1 - Math.abs((home.es ?? 0.5) - (partner.es ?? 0.5));
    // Fix: parenthesise each operand of ?? to avoid precedence collision with +
    const tradeComp  = ((home.eoCategories?.trade ?? 0.5) + (partner.eoCategories?.trade ?? 0.5)) / 2;
    return 0.3*alignment + 0.3*eoPartner + 0.25*esNeed + 0.15*tradeComp;
  }
  if (ptype === 'export_partner') {
    // prosperous_opportunity.R: trade_index, econ_opp_index, energy_security_index
    const demand    = partner.eoCategories?.technology_demand ?? partner.eo ?? 0.5;
    const tradeFit  = partner.eoCategories?.trade ?? 0.5;
    const cap       = home.eoCategories?.production ?? home.eo ?? 0.5;
    return 0.45*demand + 0.35*tradeFit + 0.20*cap;
  }
  // development
  const partnerNeed = 1 - (partner.es ?? 0.5);
  const potential   = partner.esCategories?.reserves ?? partner.es ?? 0.5;
  const homeCap     = home.eo ?? 0.5;
  return 0.4*partnerNeed + 0.3*potential + 0.3*homeCap;
}

function selectPartner(iso3, scored) {
  STATE.selectedPartner = iso3;
  document.querySelectorAll('.partner-item').forEach((el, i) => {
    el.classList.toggle('selected', scored[i]?.country.iso3 === iso3);
  });

  const partner = getCountry(iso3);
  const home    = getCountry(STATE.partnerCountry);
  if (!partner || !home) return;

  // Show detail panel
  document.getElementById('partner-detail-placeholder').style.display = 'none';
  const content = document.getElementById('partner-detail-content');
  content.style.display = 'flex';

  document.getElementById('partner-detail-name').textContent   = partner.name;
  document.getElementById('partner-detail-region').textContent = partner.region;

  // Score badges
  const badgesRow = document.getElementById('partner-scores-row');
  if (badgesRow) {
    badgesRow.innerHTML = `
      <div class="score-badge"><div class="badge-label">Partner ES</div><div class="badge-value es-val mono">${fmt(partner.es)}</div></div>
      <div class="score-badge"><div class="badge-label">Partner EO</div><div class="badge-value eo-val mono">${fmt(partner.eo)}</div></div>
      <div class="score-badge"><div class="badge-label">PSI</div><div class="badge-value psi-val mono">${fmt(D.computePartnership(home,partner)?.[STATE.partnerType] ?? 0)}</div></div>
    `;
  }

  renderPartnerCharts(home, partner);
}

function renderPartnerCharts(home, partner) {
  // Chart 1: Partnership radar
  destroyChart('partner-radar');
  const radarCanvas = document.getElementById('partner-radar');
  if (radarCanvas) {
    const labels = ['Alignment','EO','ES Need','Trade','Production'];
    const esNeed = 1 - (home.es ?? 0.5);
    const alignment = 1 - Math.abs((home.es??0.5)-(partner.es??0.5));
    CHARTS['partner-radar'] = new Chart(radarCanvas, {
      type: 'radar',
      data: {
        labels,
        datasets: [{
          label: home.name,
          data: [alignment, home.eo??0, esNeed, home.eoCategories?.trade??0.5, home.eoCategories?.production??0.5],
          backgroundColor: C.esColor+'33', borderColor: C.esColor, borderWidth: 2, pointRadius: 3,
        },{
          label: partner.name,
          data: [alignment, partner.eo??0, 1-(partner.es??0), partner.eoCategories?.trade??0.5, partner.eoCategories?.production??0.5],
          backgroundColor: C.eoColor+'33', borderColor: C.eoColor, borderWidth: 2, pointRadius: 3,
        }]
      },
      options: {
        responsive: true, maintainAspectRatio: false,
        plugins: {
          legend: { labels: { color: '#8DAABF', font: { size: 9 } } },
          tooltip: { backgroundColor: '#152D56', borderColor: '#1E3A6A', borderWidth: 1, titleColor: '#EEF2F7', bodyColor: '#8DAABF' }
        },
        scales: {
          r: {
            min: 0, max: 1, grid: { color: '#1E3A6A' },
            ticks: { display: false },
            pointLabels: { color: '#8DAABF', font: { size: 8 } }
          }
        }
      }
    });
  }

  // Chart 2: Global EO vs ES scatter (all countries) with home + partner highlighted
  destroyChart('partner-scatter');
  const scatterCanvas = document.getElementById('partner-scatter');
  if (scatterCanvas) {
    const allPts = D.COUNTRIES.map(c => ({
      x: c.eo ?? 0, y: c.es ?? 0, label: c.name,
      isHome: c.iso3 === home.iso3, isPartner: c.iso3 === partner.iso3,
    }));
    CHARTS['partner-scatter'] = new Chart(scatterCanvas, {
      type: 'bubble',
      data: {
        datasets: [{
          label: 'Countries',
          data: allPts.filter(p => !p.isHome && !p.isPartner).map(p => ({x:p.x,y:p.y,r:3,label:p.label})),
          backgroundColor: '#8DAABF22', borderColor: '#8DAABF55', borderWidth: 0.5,
        },{
          label: partner.name,
          data: [{ x: partner.eo??0, y: partner.es??0, r: 7, label: partner.name }],
          backgroundColor: C.eoColor+'BB', borderColor: C.eoColor, borderWidth: 2,
        },{
          label: home.name,
          data: [{ x: home.eo??0, y: home.es??0, r: 8, label: home.name }],
          backgroundColor: C.gold+'BB', borderColor: C.gold, borderWidth: 2,
        }]
      },
      options: {
        responsive: true, maintainAspectRatio: false,
        plugins: {
          legend: { labels: { color: '#8DAABF', font: { size: 9 }, boxWidth: 8 } },
          tooltip: {
            backgroundColor: '#152D56', borderColor: '#1E3A6A', borderWidth: 1,
            titleColor: '#EEF2F7', bodyColor: '#8DAABF',
            callbacks: { label: ctx => ctx.raw.label || ctx.dataset.label }
          }
        },
        scales: {
          x: { min:0, max:1, title:{ display:true, text:'EO', color: C.eoColor, font:{size:9}}, grid:{color:'#1E3A6A'}, ticks:{color:'#4A6A88',font:{size:8}}},
          y: { min:0, max:1, title:{ display:true, text:'ES', color: C.esColor, font:{size:9}}, grid:{color:'#1E3A6A'}, ticks:{color:'#4A6A88',font:{size:8}}}
        }
      }
    });
  }

  // Chart 3: ES category comparison bar
  destroyChart('partner-cat-bar');
  const esBarCanvas = document.getElementById('partner-cat-bar');
  if (esBarCanvas) {
    const labels = D.ES_CATEGORIES.map(c => c.key.substring(0,8));
    CHARTS['partner-cat-bar'] = new Chart(esBarCanvas, {
      type: 'bar',
      data: {
        labels,
        datasets: [
          { label: home.name,    data: D.ES_CATEGORIES.map(c => home.esCategories?.[c.key]??0),    backgroundColor: C.esColor+'88', borderColor: C.esColor, borderWidth: 1, borderRadius: 2 },
          { label: partner.name, data: D.ES_CATEGORIES.map(c => partner.esCategories?.[c.key]??0), backgroundColor: C.gold+'88',    borderColor: C.gold,    borderWidth: 1, borderRadius: 2 },
        ]
      },
      options: barOptions(false),
    });
  }

  // Chart 4: EO category comparison bar
  destroyChart('partner-eo-bar');
  const eoBarCanvas = document.getElementById('partner-eo-bar');
  if (eoBarCanvas) {
    const labels = D.EO_CATEGORIES.map(c => c.key.substring(0,8));
    CHARTS['partner-eo-bar'] = new Chart(eoBarCanvas, {
      type: 'bar',
      data: {
        labels,
        datasets: [
          { label: home.name,    data: D.EO_CATEGORIES.map(c => home.eoCategories?.[c.key]??0),    backgroundColor: C.eoColor+'88', borderColor: C.eoColor, borderWidth: 1, borderRadius: 2 },
          { label: partner.name, data: D.EO_CATEGORIES.map(c => partner.eoCategories?.[c.key]??0), backgroundColor: C.psiColor+'88',borderColor: C.psiColor,borderWidth: 1, borderRadius: 2 },
        ]
      },
      options: barOptions(false),
    });
  }
}

function barOptions(showLegend) {
  return {
    responsive: true, maintainAspectRatio: false,
    plugins: {
      legend: { display: showLegend, labels: { color: '#8DAABF', font: { size: 8 }, boxWidth: 8 } },
      tooltip: { backgroundColor: '#152D56', borderColor: '#1E3A6A', borderWidth: 1, titleColor: '#EEF2F7', bodyColor: '#8DAABF' }
    },
    scales: {
      x: { grid: { display: false }, ticks: { color: '#8DAABF', font: { size: 7 }, maxRotation: 45 } },
      y: { min: 0, max: 1, grid: { color: '#1E3A6A' }, ticks: { color: '#4A6A88', font: { size: 8 } } }
    }
  };
}

// ─── Weight Modal ─────────────────────────────────────────────────────────────
function buildWeightModal() {
  buildWeightRows('es');
  buildWeightRows('eo');
}
function buildWeightRows(index) {
  const container = document.getElementById(`weight-${index}-rows`);
  if (!container) return;
  container.innerHTML = '';
  const cats = index === 'es' ? D.ES_CATEGORIES : D.EO_CATEGORIES;
  cats.forEach(cat => {
    const w = STATE.userWeights[index][cat.key] ?? cat.weight;
    const row = document.createElement('div');
    row.className = 'weight-row';
    row.innerHTML = `
      <span class="weight-name">${cat.key}</span>
      <input type="range" class="weight-slider" data-index="${index}" data-key="${cat.key}"
             min="0" max="1" step="0.05" value="${w}">
      <span class="weight-val" id="wval-${index}-${cat.key.replace(/\s/g,'_')}">${w.toFixed(2)}</span>`;
    container.appendChild(row);
  });
}

function applyWeights() {
  document.querySelectorAll('.weight-slider').forEach(slider => {
    const idx = slider.dataset.index;
    const key = slider.dataset.key;
    STATE.userWeights[idx][key] = parseFloat(slider.value);
  });
  // Recalculate ES/EO scores
  recomputeScores();
  renderMapColors();
  if (STATE.selectedCountry) {
    const country = getCountry(STATE.selectedCountry);
    if (country) {
      document.querySelector('#badge-es .badge-value').textContent  = fmt(country.es);
      document.querySelector('#badge-eo .badge-value').textContent  = fmt(country.eo);
      rerenderPanel();
    }
  }
  closeWeightModal();
}

function recomputeScores() {
  D.COUNTRIES.forEach(country => {
    const esW = STATE.userWeights.es;
    const eoW = STATE.userWeights.eo;
    const esTotal = Object.values(esW).reduce((a,b)=>a+b,0)||1;
    const eoTotal = Object.values(eoW).reduce((a,b)=>a+b,0)||1;
    let esSum = 0, eoSum = 0;
    D.ES_CATEGORIES.forEach(c => { esSum += (country.esCategories?.[c.key]??0) * (esW[c.key]??c.weight); });
    D.EO_CATEGORIES.forEach(c => { eoSum += (country.eoCategories?.[c.key]??0) * (eoW[c.key]??c.weight); });
    country.es = esSum / esTotal;
    country.eo = eoSum / eoTotal;
  });
}

function openWeightModal() {
  buildWeightRows('es');
  buildWeightRows('eo');
  document.getElementById('weight-modal').classList.add('open');

  document.querySelectorAll('.weight-slider').forEach(slider => {
    slider.addEventListener('input', () => {
      const key = slider.dataset.key.replace(/\s/g,'_');
      const idx = slider.dataset.index;
      const valEl = document.getElementById(`wval-${idx}-${key}`);
      if (valEl) valEl.textContent = parseFloat(slider.value).toFixed(2);
    });
  });
}
function closeWeightModal() {
  document.getElementById('weight-modal').classList.remove('open');
}

// ─── Filters & Search ─────────────────────────────────────────────────────────
function populateFilters() {
  // Index buttons handled via HTML
  // Region filter
  const regionSel = document.getElementById('filter-region');
  if (regionSel) {
    regionSel.innerHTML = '<option value="All">All Regions</option>';
    D.REGIONS.forEach(r => {
      const opt = document.createElement('option');
      opt.value = opt.textContent = r;
      regionSel.appendChild(opt);
    });
  }
  // Partnership home country
  const homeSel = document.getElementById('partner-home-select');
  if (homeSel) {
    homeSel.innerHTML = '<option value="">— Select home country —</option>';
    [...D.COUNTRIES].sort((a,b)=>a.name.localeCompare(b.name)).forEach(c => {
      const opt = document.createElement('option');
      opt.value = c.iso3;
      opt.textContent = c.name;
      homeSel.appendChild(opt);
    });
  }
}

function filterMapByRegion(region) {
  STATE.region = region;
  if (!gMap) return;
  if (region === 'All') {
    gMap.selectAll('.country-path').classed('dimmed', false);
  } else {
    gMap.selectAll('.country-path').classed('dimmed', d => {
      const iso3 = ISO_NUM_TO_A3[String(d.id)];
      const c = iso3 ? getCountry(iso3) : null;
      return c ? c.region !== region : true;
    });
  }
  zoomToRegion(region);
}

function filterMapBySearch(query) {
  if (!gMap) return;
  if (!query) { gMap.selectAll('.country-path').classed('dimmed', false); return; }
  const q = query.toLowerCase();
  gMap.selectAll('.country-path').classed('dimmed', d => {
    const iso3 = ISO_NUM_TO_A3[String(d.id)];
    const c = iso3 ? getCountry(iso3) : null;
    return c ? !c.name.toLowerCase().includes(q) : true;
  });
}

// ─── CSV Export ───────────────────────────────────────────────────────────────
function exportAllCSV() {
  const cols = ['iso3','name','region','es','eo','psi'];
  const rows = [cols.join(',')];
  D.COUNTRIES.forEach(c => {
    rows.push([c.iso3, `"${c.name}"`, c.region, fmt(c.es), fmt(c.eo), fmt(c.psi)].join(','));
  });
  downloadCSV(rows.join('\n'), 'osi_all_countries.csv');
}

function exportCountryCSV(iso3) {
  const country = getCountry(iso3);
  if (!country) return;
  const cols = ['pillar','category','score','weight'];
  const rows = [cols.join(',')];
  D.ES_CATEGORIES.forEach(c => {
    rows.push(['Energy Security', c.key, fmt(country.esCategories?.[c.key]), c.weight].join(','));
  });
  D.EO_CATEGORIES.forEach(c => {
    rows.push(['Econ. Opportunity', c.key, fmt(country.eoCategories?.[c.key]), c.weight].join(','));
  });
  downloadCSV(rows.join('\n'), `osi_${iso3}.csv`);
}

function downloadCSV(content, filename) {
  const a = document.createElement('a');
  a.href = 'data:text/csv;charset=utf-8,' + encodeURIComponent(content);
  a.download = filename;
  a.click();
}

// ─── View Switching ───────────────────────────────────────────────────────────
function switchView(view) {
  STATE.view = view;
  document.querySelectorAll('.view').forEach(v => v.classList.toggle('active', v.id === 'view-' + view));
  document.querySelectorAll('.nav-tab').forEach(b => b.classList.toggle('active', b.dataset.view === view));

  // Show/hide partnerships toolbar
  const ptb = document.getElementById('partnerships-toolbar');
  if (ptb) ptb.style.display = view === 'partnerships' ? 'flex' : 'none';

  // Show/hide world-view tech picker
  const picker = document.getElementById('tech-picker');
  if (picker) picker.style.display = view === 'world' ? 'flex' : 'none';

  // Show/hide world toolbar
  const toolbar = document.getElementById('toolbar');
  if (toolbar) toolbar.style.display = view === 'world' ? 'flex' : 'none';

  if (view === 'partnerships') {
    // Trigger map initialisation for partner map if not done yet
    if (!partnerSvg) loadTopoAndRender();
    else renderPartners();
    // Reset partner map banner
    const banner = document.getElementById('partner-map-legend');
    if (banner) banner.style.display = 'none';
  }
}

// ─── Event Wiring ─────────────────────────────────────────────────────────────
function wireEvents() {
  // Nav tabs
  document.querySelectorAll('.nav-tab').forEach(btn => {
    btn.addEventListener('click', () => switchView(btn.dataset.view));
  });

  // Index toggle
  document.querySelectorAll('.idx-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      document.querySelectorAll('.idx-btn').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      STATE.index = btn.dataset.index;
      renderMapColors();
    });
  });

  // Region filter
  const regionSel = document.getElementById('filter-region');
  if (regionSel) {
    regionSel.addEventListener('change', () => filterMapByRegion(regionSel.value));
  }

  // Country search
  const search = document.getElementById('country-search');
  if (search) {
    search.addEventListener('input', () => filterMapBySearch(search.value.trim()));
    search.addEventListener('keydown', e => {
      if (e.key === 'Enter') {
        const q = search.value.trim().toLowerCase();
        const found = D.COUNTRIES.find(c => c.name.toLowerCase().includes(q) || c.iso3.toLowerCase() === q);
        if (found) openCountryPanel(found.iso3);
      }
    });
  }

  // Close panel
  document.getElementById('btn-close-panel')?.addEventListener('click', closeCountryPanel);

  // Panel tabs
  document.querySelectorAll('.panel-tab').forEach(btn => {
    btn.addEventListener('click', () => switchPanelTab(btn.dataset.tab));
  });

  // Zoom controls
  document.getElementById('btn-zoom-in')?.addEventListener('click', () => {
    svgWorld?.transition().duration(300).call(zoomBehaviour.scaleBy, 1.5);
  });
  document.getElementById('btn-zoom-out')?.addEventListener('click', () => {
    svgWorld?.transition().duration(300).call(zoomBehaviour.scaleBy, 0.67);
  });
  document.getElementById('btn-zoom-reset')?.addEventListener('click', () => {
    svgWorld?.transition().duration(500).call(zoomBehaviour.transform, d3.zoomIdentity);
    STATE.region = 'All';
    const regionSel = document.getElementById('filter-region');
    if (regionSel) regionSel.value = 'All';
    gMap?.selectAll('.country-path').classed('dimmed', false);
  });

  // Export buttons
  document.getElementById('btn-export-all')?.addEventListener('click', exportAllCSV);
  document.getElementById('btn-export-csv')?.addEventListener('click', () => {
    if (STATE.selectedCountry) exportCountryCSV(STATE.selectedCountry);
  });

  // Weight modal
  document.getElementById('btn-weights')?.addEventListener('click', openWeightModal);
  document.getElementById('btn-weights-close')?.addEventListener('click', closeWeightModal);
  document.getElementById('btn-weights-apply')?.addEventListener('click', applyWeights);
  document.getElementById('weight-modal')?.addEventListener('click', e => {
    if (e.target.id === 'weight-modal') closeWeightModal();
  });

  // Tech heatmap mode toggle
  document.querySelectorAll('.heatmap-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      STATE.techDetailMode = btn.dataset.mode;
      if (STATE.selectedCountry) renderTechTab(getCountry(STATE.selectedCountry));
    });
  });

  // Partnerships
  document.getElementById('partner-home-select')?.addEventListener('change', e => {
    STATE.partnerCountry = e.target.value || null;
    STATE.selectedPartner = null;
    document.getElementById('partner-detail-placeholder').style.display = 'flex';
    document.getElementById('partner-detail-content').style.display = 'none';
    renderPartners();
  });

  document.querySelectorAll('.pship-tab').forEach(btn => {
    btn.addEventListener('click', () => {
      document.querySelectorAll('.pship-tab').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      STATE.partnerType = btn.dataset.ptype;
      STATE.selectedPartner = null;
      document.getElementById('partner-detail-placeholder').style.display = 'flex';
      document.getElementById('partner-detail-content').style.display = 'none';
      renderPartners();
    });
  });
}

// ─── Init ─────────────────────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', () => {
  populateFilters();
  buildTechPicker();
  buildWeightModal();
  wireEvents();
  loadTopoAndRender();

  // Default: partnerships toolbar hidden
  const ptb = document.getElementById('partnerships-toolbar');
  if (ptb) ptb.style.display = 'none';
});
