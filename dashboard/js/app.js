/**
 * Energy Transition Intelligence Hub — Application Logic
 * Handles world map (D3+TopoJSON), country detail panel (Chart.js),
 * and partnerships view.
 */

'use strict';

// ─── Aliases ───────────────────────────────────────────────────────────────
const D = window.OSI_DATA;

// ─── App State ─────────────────────────────────────────────────────────────
const STATE = {
  view:         'world',       // 'world' | 'partnerships'
  index:        'es',          // 'es' | 'eo' | 'psi'
  tech:         'All',
  supplyChain:  'All',
  region:       'All',
  selectedCountry: null,
  techDetailMode: 'es',        // heatmap toggle
  partnerCountry:  null,       // home country for partnerships
  partnerType:    'friendshore',
  selectedPartner: null,
  userWeights: {
    es: Object.fromEntries(D.ES_CATEGORIES.map(c => [c.key, c.weight])),
    eo: Object.fromEntries(D.EO_CATEGORIES.map(c => [c.key, c.weight])),
  },
  compareList: [],
};

// Chart.js instances (kept for destruction on re-render)
const CHARTS = {};

// ─── Colour helpers ─────────────────────────────────────────────────────────
const INDEX_COLORS = {
  es:  { scale: d3.scaleSequential([0,1], d3.interpolateBlues).domain([0.1, 0.95]), css: '#4fc3f7' },
  eo:  { scale: d3.scaleSequential([0,1], d3.interpolateGreens).domain([0.1, 0.95]), css: '#69f0ae' },
  psi: { scale: d3.scaleSequential([0,1], d3.interpolatePurples).domain([0.1, 0.95]), css: '#ce93d8' },
};

function indexColor(idx, val) {
  const palettes = {
    es:  d3.interpolateRgb('#1a2a45', '#4fc3f7'),
    eo:  d3.interpolateRgb('#0d2a1a', '#69f0ae'),
    psi: d3.interpolateRgb('#1a1030', '#ce93d8'),
  };
  return palettes[idx](Math.max(0, Math.min(1, val)));
}

function catColor(val) {
  if (val >= 0.70) return '#69f0ae';
  if (val >= 0.50) return '#f7c34a';
  if (val >= 0.30) return '#e05470';
  return '#a03050';
}

// ─── Score for currently selected index/tech/supply chain ─────────────────
function getScore(country) {
  const c = typeof country === 'string' ? D.COUNTRY_MAP[country] : country;
  if (!c) return null;
  const { tech, supplyChain, index } = STATE;

  if (tech === 'All' || supplyChain === 'All') {
    return c[index === 'es' ? 'es' : index === 'eo' ? 'eo' : 'psi'];
  }
  if (index === 'psi') return c.psi; // PSI not tech-disaggregated in sample
  return c.techBreakdown?.[tech]?.[supplyChain]?.[index] ?? c[index];
}

// ─── World Map ──────────────────────────────────────────────────────────────
let worldTopology = null;

async function initMap() {
  const svg = d3.select('#world-map');
  const container = document.getElementById('map-container');
  const tooltip = document.getElementById('map-tooltip');

  const W = container.clientWidth;
  const H = container.clientHeight;

  svg.attr('width', W).attr('height', H);

  const projection = d3.geoNaturalEarth1()
    .scale(W / 6.5)
    .translate([W / 2, H / 2]);

  const path = d3.geoPath().projection(projection);

  // Fetch world topology
  if (!worldTopology) {
    try {
      worldTopology = await d3.json('https://cdn.jsdelivr.net/npm/world-atlas@2/countries-110m.json');
    } catch (e) {
      console.warn('Could not load world topology. Map will be blank.', e);
      document.getElementById('map-container').innerHTML =
        '<div class="empty-state"><div class="empty-icon">🌐</div><div class="empty-title">Map unavailable</div><div class="empty-desc">Internet connection required to load the world map. All other features work offline.</div></div>';
      return;
    }
  }

  const countries = topojson.feature(worldTopology, worldTopology.objects.countries);

  // Graticule
  svg.append('path')
    .datum(d3.geoGraticule()())
    .attr('class', 'graticule')
    .attr('d', path)
    .attr('fill', 'none')
    .attr('stroke', '#1a2a45')
    .attr('stroke-width', 0.4);

  // Sphere
  svg.append('path')
    .datum({ type: 'Sphere' })
    .attr('d', path)
    .attr('fill', '#0b1628');

  // Country paths
  svg.append('g')
    .attr('id', 'countries-g')
    .selectAll('path')
    .data(countries.features)
    .join('path')
    .attr('d', path)
    .attr('class', 'country-path')
    .attr('stroke', '#0d1825')
    .attr('stroke-width', 0.5)
    .on('mouseenter', (event, d) => {
      const country = D.ISONUM_MAP[+d.id];
      if (!country) return;
      const score = getScore(country);
      tooltip.style.display = 'block';
      const sl = D.scoreLabel(score);
      tooltip.innerHTML = `
        <div class="tt-name">${country.name}</div>
        <div class="tt-row"><span>${STATE.index.toUpperCase()} Score</span><span class="${sl.cls}">${D.fmtScore(score)}</span></div>
        <div class="tt-row"><span>Region</span><span>${country.region}</span></div>
        <div class="tt-row"><span>ES</span><span>${D.fmtScore(country.es)}</span></div>
        <div class="tt-row"><span>EO</span><span>${D.fmtScore(country.eo)}</span></div>
        <div class="tt-row"><span style="font-size:0.65rem;color:var(--text-muted)">Click to explore ↗</span><span></span></div>
      `;
      d3.select(event.currentTarget).attr('stroke', '#fff').attr('stroke-width', 1.2);
    })
    .on('mousemove', event => {
      const tx = Math.min(event.offsetX + 14, container.clientWidth - 180);
      const ty = Math.min(event.offsetY + 14, container.clientHeight - 160);
      tooltip.style.left = tx + 'px';
      tooltip.style.top  = ty + 'px';
    })
    .on('mouseleave', (event, d) => {
      tooltip.style.display = 'none';
      const isSelected = D.ISONUM_MAP[+d.id]?.iso3 === STATE.selectedCountry;
      d3.select(event.currentTarget)
        .attr('stroke', isSelected ? '#fff' : '#0d1825')
        .attr('stroke-width', isSelected ? 1.8 : 0.5);
    })
    .on('click', (event, d) => {
      const country = D.ISONUM_MAP[+d.id];
      if (country) openCountryPanel(country.iso3);
    });

  renderMapColors();
  updateLegend();

  // Zoom behaviour
  const zoom = d3.zoom()
    .scaleExtent([0.8, 8])
    .on('zoom', event => {
      svg.selectAll('g, path.graticule').attr('transform', event.transform);
    });
  svg.call(zoom);

  // Zoom controls
  document.getElementById('btn-zoom-in').onclick  = () => svg.transition().call(zoom.scaleBy, 1.4);
  document.getElementById('btn-zoom-out').onclick = () => svg.transition().call(zoom.scaleBy, 0.7);
  document.getElementById('btn-zoom-reset').onclick = () => svg.transition().call(zoom.transform, d3.zoomIdentity);
}

function renderMapColors() {
  d3.selectAll('.country-path').attr('fill', function(d) {
    const country = D.ISONUM_MAP[+d.id];
    if (!country) return '#141f35';
    const score = getScore(country);
    if (score == null) return '#141f35';
    return indexColor(STATE.index, score);
  });
}

function updateLegend() {
  const bar = document.getElementById('legend-bar');
  const label = document.getElementById('legend-index-label');
  if (!bar || !label) return;
  bar.className = `legend-bar ${STATE.index}`;
  const names = { es: 'Energy Security', eo: 'Economic Opportunity', psi: 'Partnership Strength' };
  label.textContent = names[STATE.index];
}

// ─── Country Detail Panel ───────────────────────────────────────────────────
function openCountryPanel(iso3) {
  STATE.selectedCountry = iso3;
  const country = D.COUNTRY_MAP[iso3];
  if (!country) return;

  // Highlight on map
  d3.selectAll('.country-path')
    .attr('stroke', d => D.ISONUM_MAP[+d.id]?.iso3 === iso3 ? '#ffffff' : '#0d1825')
    .attr('stroke-width', d => D.ISONUM_MAP[+d.id]?.iso3 === iso3 ? 1.8 : 0.5);

  // Header
  document.getElementById('panel-country-name').textContent = country.name;
  document.getElementById('panel-country-meta').textContent = `${country.region} · ${country.align}`;

  // Badges
  const esBadge = D.scoreLabel(country.es);
  const eoBadge = D.scoreLabel(country.eo);
  const psiBadge = D.scoreLabel(country.psi);
  document.getElementById('badge-es').innerHTML  = `<div class="badge-label">Energy Security</div><div class="badge-value es-val mono">${D.fmtScore(country.es)}</div><div class="badge-qual ${esBadge.cls}">${esBadge.text}</div>`;
  document.getElementById('badge-eo').innerHTML  = `<div class="badge-label">Econ. Opportunity</div><div class="badge-value eo-val mono">${D.fmtScore(country.eo)}</div><div class="badge-qual ${eoBadge.cls}">${eoBadge.text}</div>`;
  document.getElementById('badge-psi').innerHTML = `<div class="badge-label">Partnership</div><div class="badge-value psi-val mono">${D.fmtScore(country.psi)}</div><div class="badge-qual ${psiBadge.cls}">${psiBadge.text}</div>`;

  // Populate tabs
  renderOverviewTab(country);
  renderESTab(country);
  renderEOTab(country);
  renderTechTab(country);
  renderAdvancedTab(country);
  renderMethodTab();

  // Open panel
  document.getElementById('detail-panel').classList.add('open');

  // Default to Overview tab
  switchPanelTab('overview');
}

function closePanel() {
  document.getElementById('detail-panel').classList.remove('open');
  STATE.selectedCountry = null;
  d3.selectAll('.country-path').attr('stroke', '#0d1825').attr('stroke-width', 0.5);
}

function switchPanelTab(tab) {
  document.querySelectorAll('.panel-tab').forEach(b => b.classList.toggle('active', b.dataset.tab === tab));
  document.querySelectorAll('.tab-pane').forEach(p => p.classList.toggle('active', p.dataset.tab === tab));
}

// ── Overview Tab ────────────────────────────────────────────────────────────
function renderOverviewTab(country) {
  // Combined radar: ES + EO (first 6 ES categories)
  const radarLabels = D.ES_CATEGORIES.slice(0, 8).map(c => c.label);
  const radarES = D.ES_CATEGORIES.slice(0, 8).map(c => country.esCategories[c.key]);
  const radarEO = D.EO_CATEGORIES.slice(0, 8).map(c => country.eoCategories[c.key]);

  destroyChart('radar-overview');
  CHARTS['radar-overview'] = new Chart(document.getElementById('radar-overview'), {
    type: 'radar',
    data: {
      labels: radarLabels,
      datasets: [
        {
          label: 'Energy Security',
          data: radarES,
          borderColor: '#4fc3f7',
          backgroundColor: 'rgba(79,195,247,0.15)',
          pointBackgroundColor: '#4fc3f7',
          pointRadius: 3,
          borderWidth: 2,
        },
        {
          label: 'Econ. Opportunity',
          data: radarEO,
          borderColor: '#69f0ae',
          backgroundColor: 'rgba(105,240,174,0.10)',
          pointBackgroundColor: '#69f0ae',
          pointRadius: 3,
          borderWidth: 2,
        },
      ],
    },
    options: {
      responsive: true,
      maintainAspectRatio: true,
      scales: {
        r: {
          min: 0, max: 1,
          ticks: { display: false, stepSize: 0.25 },
          grid: { color: '#1a2d4a' },
          angleLines: { color: '#1a2d4a' },
          pointLabels: { color: '#8a9bbf', font: { size: 9 } },
        },
      },
      plugins: {
        legend: { labels: { color: '#8a9bbf', font: { size: 10 }, boxWidth: 10 } },
        tooltip: { callbacks: { label: ctx => `${ctx.dataset.label}: ${D.fmtScore(ctx.raw)}` } },
      },
    },
  });

  // Insight cards
  const bestES = D.ES_CATEGORIES.reduce((a, b) => country.esCategories[b.key] > country.esCategories[a.key] ? b : a);
  const worstES = D.ES_CATEGORIES.reduce((a, b) => country.esCategories[b.key] < country.esCategories[a.key] ? b : a);
  const bestEO = D.EO_CATEGORIES.reduce((a, b) => country.eoCategories[b.key] > country.eoCategories[a.key] ? b : a);
  const worstEO = D.EO_CATEGORIES.reduce((a, b) => country.eoCategories[b.key] < country.eoCategories[a.key] ? b : a);

  document.getElementById('insights-content').innerHTML = `
    <div class="insight-card good">
      <strong>Top ES strength: ${bestES.label}</strong><br>
      Score ${D.fmtScore(country.esCategories[bestES.key])} — ${bestES.description}
    </div>
    <div class="insight-card risk">
      <strong>ES vulnerability: ${worstES.label}</strong><br>
      Score ${D.fmtScore(country.esCategories[worstES.key])} — ${worstES.description}
    </div>
    <div class="insight-card good">
      <strong>Top EO opportunity: ${bestEO.label}</strong><br>
      Score ${D.fmtScore(country.eoCategories[bestEO.key])} — ${bestEO.description}
    </div>
    <div class="insight-card warn">
      <strong>EO gap: ${worstEO.label}</strong><br>
      Score ${D.fmtScore(country.eoCategories[worstEO.key])} — ${worstEO.description}
    </div>
  `;
}

// ── Energy Security Tab ─────────────────────────────────────────────────────
function renderESTab(country) {
  const weights = STATE.userWeights.es;
  const totalW = Object.values(weights).reduce((a,b) => a+b, 0);

  const sorted = [...D.ES_CATEGORIES].sort((a,b) =>
    country.esCategories[b.key] - country.esCategories[a.key]
  );

  document.getElementById('es-bars').innerHTML = sorted.map(cat => {
    const val = country.esCategories[cat.key];
    const pct = (val * 100).toFixed(1);
    const wPct = ((weights[cat.key] / totalW) * 100).toFixed(0);
    const sl = D.scoreLabel(val);
    return `
      <div class="hbar-item">
        <div class="hbar-label-row">
          <span class="hbar-name">${cat.label} <span class="tooltip-info" title="${cat.description}">?</span></span>
          <span class="hbar-score ${sl.cls}">${pct}</span>
        </div>
        <div class="hbar-track">
          <div class="hbar-fill es" style="width:${pct}%"></div>
        </div>
        <div class="hbar-weight">Weight: ${weights[cat.key]}× (${wPct}% of index)</div>
      </div>`;
  }).join('');

  // Weighted contribution donut
  const donutData = D.ES_CATEGORIES.map(c => ({
    label: c.label,
    val: country.esCategories[c.key],
    weight: weights[c.key],
  }));
  const contribs = donutData.map(d => d.val * d.weight);

  destroyChart('es-donut');
  CHARTS['es-donut'] = new Chart(document.getElementById('es-donut'), {
    type: 'doughnut',
    data: {
      labels: donutData.map(d => d.label),
      datasets: [{
        data: contribs,
        backgroundColor: [
          '#4fc3f7','#29b6f6','#0288d1','#01579b',
          '#00bcd4','#009688','#26a69a','#80cbc4','#b2dfdb',
        ],
        borderColor: '#0b0f1a',
        borderWidth: 2,
      }],
    },
    options: {
      responsive: true,
      cutout: '58%',
      plugins: {
        legend: { display: false },
        tooltip: {
          callbacks: {
            label: ctx => `${ctx.label}: ${(ctx.raw / contribs.reduce((a,b)=>a+b) * 100).toFixed(1)}%`,
          },
        },
      },
    },
  });
}

// ── Economic Opportunity Tab ─────────────────────────────────────────────────
function renderEOTab(country) {
  const weights = STATE.userWeights.eo;
  const totalW = Object.values(weights).reduce((a,b) => a+b, 0);

  const sorted = [...D.EO_CATEGORIES].sort((a,b) =>
    country.eoCategories[b.key] - country.eoCategories[a.key]
  );

  document.getElementById('eo-bars').innerHTML = sorted.map(cat => {
    const val = country.eoCategories[cat.key];
    const pct = (val * 100).toFixed(1);
    const wPct = ((weights[cat.key] / totalW) * 100).toFixed(0);
    const sl = D.scoreLabel(val);
    return `
      <div class="hbar-item">
        <div class="hbar-label-row">
          <span class="hbar-name">${cat.label} <span class="tooltip-info" title="${cat.description}">?</span></span>
          <span class="hbar-score ${sl.cls}">${pct}</span>
        </div>
        <div class="hbar-track">
          <div class="hbar-fill eo" style="width:${pct}%"></div>
        </div>
        <div class="hbar-weight">Weight: ${weights[cat.key]}× (${wPct}% of index)</div>
      </div>`;
  }).join('');

  // Horizontal bar chart using Chart.js
  destroyChart('eo-bar');
  CHARTS['eo-bar'] = new Chart(document.getElementById('eo-bar'), {
    type: 'bar',
    data: {
      labels: D.EO_CATEGORIES.map(c => c.label),
      datasets: [{
        label: 'EO Score',
        data: D.EO_CATEGORIES.map(c => country.eoCategories[c.key]),
        backgroundColor: D.EO_CATEGORIES.map(c => catColor(country.eoCategories[c.key])),
        borderRadius: 4,
        borderSkipped: false,
      }],
    },
    options: {
      indexAxis: 'y',
      responsive: true,
      maintainAspectRatio: false,
      scales: {
        x: { min: 0, max: 1, grid: { color: '#1a2d4a' }, ticks: { color: '#8a9bbf', font: { size: 9 } } },
        y: { grid: { display: false }, ticks: { color: '#8a9bbf', font: { size: 9 } } },
      },
      plugins: {
        legend: { display: false },
        tooltip: { callbacks: { label: ctx => `Score: ${D.fmtScore(ctx.raw)}` } },
      },
    },
  });
}

// ── Technology Tab ──────────────────────────────────────────────────────────
function renderTechTab(country, mode) {
  mode = mode || STATE.techDetailMode;
  STATE.techDetailMode = mode;

  document.querySelectorAll('.heatmap-btn').forEach(b =>
    b.classList.toggle('active', b.dataset.mode === mode)
  );

  const colorFn = v => indexColor(mode === 'es' ? 'es' : 'eo', v);
  const label = mode === 'es' ? 'ES' : 'EO';

  const techs = D.TECHNOLOGIES;
  const scs = D.SUPPLY_CHAINS;

  const rows = techs.map(tech => {
    const cells = scs.map(sc => {
      const val = country.techBreakdown[tech.key]?.[sc.key]?.[mode] ?? 0;
      const bg = colorFn(val);
      const textCol = val > 0.55 ? '#000' : '#e8eaf2';
      return `<td style="background:${bg};color:${textCol}" title="${tech.label} ${sc.label}: ${label}=${D.fmtScore(val)}">${D.fmtScore(val)}</td>`;
    }).join('');
    return `<tr><td class="row-label">${tech.label}</td>${cells}</tr>`;
  }).join('');

  const header = scs.map(sc => `<th>${sc.label}</th>`).join('');

  document.getElementById('tech-heatmap').innerHTML = `
    <table class="heatmap-table">
      <thead><tr><th></th>${header}</tr></thead>
      <tbody>${rows}</tbody>
    </table>
  `;

  // Bar chart: Overall ES/EO per technology (All supply chains avg)
  const techAvgES = techs.map(t => {
    const vals = scs.map(sc => country.techBreakdown[t.key]?.[sc.key]?.es ?? 0);
    return vals.reduce((a,b)=>a+b,0) / vals.length;
  });
  const techAvgEO = techs.map(t => {
    const vals = scs.map(sc => country.techBreakdown[t.key]?.[sc.key]?.eo ?? 0);
    return vals.reduce((a,b)=>a+b,0) / vals.length;
  });

  destroyChart('tech-bar');
  CHARTS['tech-bar'] = new Chart(document.getElementById('tech-bar'), {
    type: 'bar',
    data: {
      labels: techs.map(t => t.label),
      datasets: [
        { label: 'Energy Security', data: techAvgES, backgroundColor: 'rgba(79,195,247,0.75)', borderRadius: 4, borderSkipped: false },
        { label: 'Econ. Opportunity', data: techAvgEO, backgroundColor: 'rgba(105,240,174,0.75)', borderRadius: 4, borderSkipped: false },
      ],
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      scales: {
        x: { grid: { color: '#1a2d4a' }, ticks: { color: '#8a9bbf', font: { size: 9 } } },
        y: { min: 0, max: 1, grid: { color: '#1a2d4a' }, ticks: { color: '#8a9bbf', font: { size: 9 } } },
      },
      plugins: {
        legend: { labels: { color: '#8a9bbf', font: { size: 10 }, boxWidth: 10 } },
        tooltip: { callbacks: { label: ctx => `${ctx.dataset.label}: ${D.fmtScore(ctx.raw)}` } },
      },
    },
  });
}

// ── Advanced Tab ─────────────────────────────────────────────────────────────
function renderAdvancedTab(country) {
  const rows = [
    ...D.ES_CATEGORIES.map(c => ({ cat: 'ES', name: c.label, key: c.key, val: country.esCategories[c.key], weight: STATE.userWeights.es[c.key] })),
    ...D.EO_CATEGORIES.map(c => ({ cat: 'EO', name: c.label, key: c.key, val: country.eoCategories[c.key], weight: STATE.userWeights.eo[c.key] })),
  ];
  const tbody = rows.map(r => {
    const sl = D.scoreLabel(r.val);
    return `<tr>
      <td>${r.cat}</td>
      <td>${r.name}</td>
      <td class="mono ${sl.cls}">${D.fmtScore(r.val)}</td>
      <td class="mono text-muted">${r.weight}×</td>
    </tr>`;
  }).join('');

  document.getElementById('advanced-table-body').innerHTML = tbody;

  // CSV export
  document.getElementById('btn-export-csv').onclick = () => {
    const lines = ['Pillar,Category,Score,Weight'];
    rows.forEach(r => lines.push(`${r.cat},${r.name},${r.val.toFixed(4)},${r.weight}`));
    downloadBlob(lines.join('\n'), `${country.iso3}_scores.csv`, 'text/csv');
  };
}

// ── Methodology Tab ───────────────────────────────────────────────────────
function renderMethodTab() {
  document.getElementById('method-es-text').textContent   = D.METHODOLOGY.es;
  document.getElementById('method-eo-text').textContent   = D.METHODOLOGY.eo;
  document.getElementById('method-psi-text').textContent  = D.METHODOLOGY.psi;
  document.getElementById('method-weights-es').textContent = D.METHODOLOGY.weights_es;
  document.getElementById('method-weights-eo').textContent = D.METHODOLOGY.weights_eo;
}

// ─── Partnerships View ──────────────────────────────────────────────────────
function initPartnershipsView() {
  // Populate country selector
  const sel = document.getElementById('partner-home-select');
  sel.innerHTML = '<option value="">— Select home country —</option>' +
    D.COUNTRIES.map(c => `<option value="${c.iso3}">${c.name}</option>`).join('');

  sel.onchange = () => {
    STATE.partnerCountry = sel.value || null;
    renderPartnersList();
  };
}

function renderPartnersList() {
  const { partnerCountry, partnerType } = STATE;
  const list = document.getElementById('partners-list');
  const detailPlaceholder = document.getElementById('partner-detail-placeholder');
  const detailContent = document.getElementById('partner-detail-content');

  if (!partnerCountry) {
    list.innerHTML = '<div class="empty-state"><div class="empty-icon">🌍</div><div class="empty-desc">Select a home country above to see ranked partners.</div></div>';
    if (detailPlaceholder) detailPlaceholder.style.display = '';
    if (detailContent) detailContent.style.display = 'none';
    return;
  }

  const partners = D.getPartners(partnerCountry, partnerType);
  const fillClass = partnerType === 'friendshore' ? 'fr-fill' : partnerType === 'export_partner' ? 'exp-fill' : 'dev-fill';

  list.innerHTML = partners.map((p, i) => {
    const score = p.scores[partnerType];
    const sl = D.scoreLabel(score);
    const rankClass = i < 3 ? 'top3' : '';
    const barW = Math.round(score * 100);
    return `
      <div class="partner-item" data-iso3="${p.iso3}" onclick="selectPartner('${p.iso3}')">
        <div class="partner-rank ${rankClass}">${i+1}</div>
        <div class="partner-name-col">
          <div class="partner-name">${p.name}</div>
          <div class="partner-region">${p.region}</div>
        </div>
        <div class="partner-score-col">
          <div class="partner-score-val ${sl.cls}">${D.fmtScore(score)}</div>
          <div class="partner-mini-bar"><div class="partner-mini-bar-fill ${fillClass}" style="width:${barW}%"></div></div>
        </div>
      </div>`;
  }).join('');

  // Auto-select top partner
  if (partners.length > 0) selectPartner(partners[0].iso3);
}

function selectPartner(iso3) {
  STATE.selectedPartner = iso3;
  document.querySelectorAll('.partner-item').forEach(el =>
    el.classList.toggle('selected', el.dataset.iso3 === iso3)
  );

  const home    = D.COUNTRY_MAP[STATE.partnerCountry];
  const partner = D.COUNTRY_MAP[iso3];
  if (!home || !partner) return;

  const scores = D.computePartnership(home, partner);
  const detailPlaceholder = document.getElementById('partner-detail-placeholder');
  const detailContent     = document.getElementById('partner-detail-content');
  if (detailPlaceholder) detailPlaceholder.style.display = 'none';
  if (detailContent)     detailContent.style.display = 'flex';

  // Header
  document.getElementById('partner-detail-name').textContent = partner.name;
  document.getElementById('partner-detail-region').textContent = partner.region;

  // Score badges
  document.getElementById('partner-scores-row').innerHTML = `
    <div class="score-badge" style="min-width:90px">
      <div class="badge-label">Friendshore</div>
      <div class="badge-value" style="color:var(--fr-color)" class="mono">${D.fmtScore(scores.friendshore)}</div>
    </div>
    <div class="score-badge" style="min-width:90px">
      <div class="badge-label">Export Partner</div>
      <div class="badge-value" style="color:var(--exp-color)" class="mono">${D.fmtScore(scores.export_partner)}</div>
    </div>
    <div class="score-badge" style="min-width:90px">
      <div class="badge-label">Development</div>
      <div class="badge-value" style="color:var(--dev-color)" class="mono">${D.fmtScore(scores.development)}</div>
    </div>
    <div class="score-badge" style="min-width:90px">
      <div class="badge-label">Composite PSI</div>
      <div class="badge-value" style="color:var(--psi-color)" class="mono">${D.fmtScore(scores.composite)}</div>
    </div>
  `;

  // Chart 1: Partnership breakdown radar (home vs partner)
  const pscores = [scores.friendshore, scores.export_partner, scores.development, scores.composite];
  destroyChart('partner-radar');
  CHARTS['partner-radar'] = new Chart(document.getElementById('partner-radar'), {
    type: 'radar',
    data: {
      labels: ['Friendshore', 'Export Fit', 'Development', 'Composite'],
      datasets: [{
        label: partner.name,
        data: pscores,
        borderColor: '#f7c34a',
        backgroundColor: 'rgba(247,195,74,0.15)',
        pointBackgroundColor: '#f7c34a',
        borderWidth: 2, pointRadius: 4,
      }],
    },
    options: {
      responsive: true, maintainAspectRatio: false,
      scales: { r: {
        min: 0, max: 1, ticks: { display: false },
        grid: { color: '#1a2d4a' },
        angleLines: { color: '#1a2d4a' },
        pointLabels: { color: '#8a9bbf', font: { size: 9 } },
      }},
      plugins: {
        legend: { display: false },
        tooltip: { callbacks: { label: ctx => `${ctx.dataset.label}: ${D.fmtScore(ctx.raw)}` } },
      },
    },
  });

  // Chart 2: ES vs EO scatter (all countries, home & partner highlighted)
  const scatterData = D.COUNTRIES.map(c => ({
    x: c.eo, y: c.es, label: c.name, iso3: c.iso3,
    r: c.iso3 === home.iso3 ? 10 : c.iso3 === partner.iso3 ? 8 : 5,
  }));
  const colourFn = c => {
    if (c.iso3 === home.iso3)    return '#4fc3f7';
    if (c.iso3 === partner.iso3) return '#f7c34a';
    return 'rgba(138,155,191,0.35)';
  };

  destroyChart('partner-scatter');
  CHARTS['partner-scatter'] = new Chart(document.getElementById('partner-scatter'), {
    type: 'bubble',
    data: {
      datasets: [{
        label: 'Countries',
        data: scatterData,
        backgroundColor: scatterData.map(colourFn),
        borderColor: scatterData.map(c =>
          c.iso3 === home.iso3 || c.iso3 === partner.iso3 ? '#fff' : 'transparent'
        ),
        borderWidth: 1.5,
      }],
    },
    options: {
      responsive: true, maintainAspectRatio: false,
      scales: {
        x: { min: 0.1, max: 0.95, title: { display: true, text: 'Economic Opportunity →', color: '#8a9bbf', font:{size:9} }, grid: { color: '#1a2d4a' }, ticks: { color: '#8a9bbf', font:{size:9} } },
        y: { min: 0.1, max: 0.95, title: { display: true, text: 'Energy Security →', color: '#8a9bbf', font:{size:9} }, grid: { color: '#1a2d4a' }, ticks: { color: '#8a9bbf', font:{size:9} } },
      },
      plugins: {
        legend: { display: false },
        tooltip: { callbacks: {
          label: ctx => {
            const d = ctx.raw;
            const role = d.iso3===home.iso3 ? ' (HOME)' : d.iso3===partner.iso3 ? ' (PARTNER)' : '';
            return `${d.label}${role}: EO=${D.fmtScore(d.x)}, ES=${D.fmtScore(d.y)}`;
          },
        }},
      },
    },
  });

  // Chart 3: Category comparison bar (ES categories, home vs partner)
  const catLabels = D.ES_CATEGORIES.map(c => c.label);
  destroyChart('partner-cat-bar');
  CHARTS['partner-cat-bar'] = new Chart(document.getElementById('partner-cat-bar'), {
    type: 'bar',
    data: {
      labels: catLabels,
      datasets: [
        { label: home.name,    data: D.ES_CATEGORIES.map(c => home.esCategories[c.key]),    backgroundColor: 'rgba(79,195,247,0.7)',  borderRadius: 3, borderSkipped: false },
        { label: partner.name, data: D.ES_CATEGORIES.map(c => partner.esCategories[c.key]), backgroundColor: 'rgba(247,195,74,0.7)', borderRadius: 3, borderSkipped: false },
      ],
    },
    options: {
      indexAxis: 'y',
      responsive: true, maintainAspectRatio: false,
      scales: {
        x: { min: 0, max: 1, grid: { color: '#1a2d4a' }, ticks: { color: '#8a9bbf', font:{size:8} } },
        y: { grid: { display: false }, ticks: { color: '#8a9bbf', font:{size:8} } },
      },
      plugins: {
        legend: { labels: { color: '#8a9bbf', font:{size:9}, boxWidth:8 } },
        tooltip: { callbacks: { label: ctx => `${ctx.dataset.label}: ${D.fmtScore(ctx.raw)}` } },
      },
    },
  });

  // Chart 4: EO category comparison
  const eoCatLabels = D.EO_CATEGORIES.map(c => c.label);
  destroyChart('partner-eo-bar');
  CHARTS['partner-eo-bar'] = new Chart(document.getElementById('partner-eo-bar'), {
    type: 'bar',
    data: {
      labels: eoCatLabels,
      datasets: [
        { label: home.name,    data: D.EO_CATEGORIES.map(c => home.eoCategories[c.key]),    backgroundColor: 'rgba(79,195,247,0.7)',  borderRadius: 3, borderSkipped: false },
        { label: partner.name, data: D.EO_CATEGORIES.map(c => partner.eoCategories[c.key]), backgroundColor: 'rgba(105,240,174,0.7)', borderRadius: 3, borderSkipped: false },
      ],
    },
    options: {
      indexAxis: 'y',
      responsive: true, maintainAspectRatio: false,
      scales: {
        x: { min: 0, max: 1, grid: { color: '#1a2d4a' }, ticks: { color: '#8a9bbf', font:{size:8} } },
        y: { grid: { display: false }, ticks: { color: '#8a9bbf', font:{size:8} } },
      },
      plugins: {
        legend: { labels: { color: '#8a9bbf', font:{size:9}, boxWidth:8 } },
        tooltip: { callbacks: { label: ctx => `${ctx.dataset.label}: ${D.fmtScore(ctx.raw)}` } },
      },
    },
  });
}

// ─── Weight Editor Modal ────────────────────────────────────────────────────
function openWeightModal() {
  const modal = document.getElementById('weight-modal');
  const esRows = document.getElementById('weight-es-rows');
  const eoRows = document.getElementById('weight-eo-rows');

  esRows.innerHTML = D.ES_CATEGORIES.map(c => {
    const w = STATE.userWeights.es[c.key];
    return `<div class="weight-row">
      <label>${c.label}</label>
      <input type="range" min="0" max="10" step="0.5" value="${w}"
        oninput="this.nextElementSibling.textContent=this.value;STATE.userWeights.es['${c.key}']=+this.value">
      <span class="w-val">${w}</span>
    </div>`;
  }).join('');

  eoRows.innerHTML = D.EO_CATEGORIES.map(c => {
    const w = STATE.userWeights.eo[c.key];
    return `<div class="weight-row">
      <label>${c.label}</label>
      <input type="range" min="0" max="10" step="0.5" value="${w}"
        oninput="this.nextElementSibling.textContent=this.value;STATE.userWeights.eo['${c.key}']=+this.value">
      <span class="w-val">${w}</span>
    </div>`;
  }).join('');

  modal.classList.add('open');
}

function closeWeightModal() {
  document.getElementById('weight-modal').classList.remove('open');
  // If country open, re-render ES/EO tabs with new weights
  if (STATE.selectedCountry) {
    const country = D.COUNTRY_MAP[STATE.selectedCountry];
    renderESTab(country);
    renderEOTab(country);
  }
}

// ─── Chart destruction helper ───────────────────────────────────────────────
function destroyChart(id) {
  if (CHARTS[id]) {
    CHARTS[id].destroy();
    delete CHARTS[id];
  }
}

// ─── CSV download ───────────────────────────────────────────────────────────
function downloadBlob(content, filename, type) {
  const blob = new Blob([content], { type });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url; a.download = filename; a.click();
  URL.revokeObjectURL(url);
}

// ─── Export all country scores ──────────────────────────────────────────────
function exportAllScores() {
  const lines = ['iso3,country,region,es,eo,psi'];
  D.COUNTRIES.forEach(c => lines.push(`${c.iso3},${c.name},${c.region},${c.es.toFixed(4)},${c.eo.toFixed(4)},${c.psi.toFixed(4)}`));
  downloadBlob(lines.join('\n'), 'osi_scores_all.csv', 'text/csv');
}

// ─── Country search ─────────────────────────────────────────────────────────
function handleSearch(val) {
  const q = val.toLowerCase().trim();
  if (!q) { d3.selectAll('.country-path').attr('opacity', 1); return; }
  d3.selectAll('.country-path').attr('opacity', function(d) {
    const c = D.ISONUM_MAP[+d.id];
    if (!c) return 0.2;
    return c.name.toLowerCase().includes(q) || c.iso3.toLowerCase().includes(q) ? 1 : 0.25;
  });
}

// ─── Switch views ───────────────────────────────────────────────────────────
function switchView(view) {
  STATE.view = view;
  document.querySelectorAll('.view').forEach(v => v.classList.toggle('active', v.id === `view-${view}`));
  document.querySelectorAll('.nav-tab').forEach(t => t.classList.toggle('active', t.dataset.view === view));
  document.getElementById('toolbar').style.display = view === 'world' ? '' : 'none';
  if (view === 'partnerships') {
    document.getElementById('partnerships-toolbar').style.display = '';
  }
}

// ─── Global expose (called from onclick attributes) ─────────────────────────
window.selectPartner = selectPartner;

// ─── Initialise ────────────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', async () => {
  // Chart.js global defaults
  Chart.defaults.color = '#8a9bbf';
  Chart.defaults.borderColor = '#1a2d4a';
  Chart.defaults.backgroundColor = 'rgba(255,255,255,0.05)';

  // ── Nav tabs
  document.querySelectorAll('.nav-tab').forEach(btn => {
    btn.addEventListener('click', () => switchView(btn.dataset.view));
  });

  // ── Index toggle
  document.querySelectorAll('.idx-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      STATE.index = btn.dataset.index;
      document.querySelectorAll('.idx-btn').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      renderMapColors();
      updateLegend();
    });
  });

  // ── Filters
  document.getElementById('filter-tech').addEventListener('change', e => {
    STATE.tech = e.target.value;
    renderMapColors();
  });
  document.getElementById('filter-sc').addEventListener('change', e => {
    STATE.supplyChain = e.target.value;
    renderMapColors();
  });
  document.getElementById('filter-region').addEventListener('change', e => {
    STATE.region = e.target.value;
    d3.selectAll('.country-path').attr('opacity', function(d) {
      const c = D.ISONUM_MAP[+d.id];
      if (!c) return 0.3;
      return STATE.region === 'All' || c.region === STATE.region ? 1 : 0.2;
    });
  });

  // ── Country search
  document.getElementById('country-search').addEventListener('input', e => handleSearch(e.target.value));

  // ── Panel tabs
  document.querySelectorAll('.panel-tab').forEach(btn => {
    btn.addEventListener('click', () => switchPanelTab(btn.dataset.tab));
  });

  // ── Close panel
  document.getElementById('btn-close-panel').addEventListener('click', closePanel);

  // ── Heatmap mode toggle
  document.querySelectorAll('.heatmap-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      if (STATE.selectedCountry) {
        renderTechTab(D.COUNTRY_MAP[STATE.selectedCountry], btn.dataset.mode);
      }
    });
  });

  // ── Partnership type tabs
  document.querySelectorAll('.pship-tab').forEach(btn => {
    btn.addEventListener('click', () => {
      STATE.partnerType = btn.dataset.ptype;
      document.querySelectorAll('.pship-tab').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      renderPartnersList();
    });
  });

  // ── Weight editor
  document.getElementById('btn-weights').addEventListener('click', openWeightModal);
  document.getElementById('btn-weights-close').addEventListener('click', closeWeightModal);
  document.getElementById('btn-weights-apply').addEventListener('click', closeWeightModal);
  document.getElementById('weight-modal').addEventListener('click', e => {
    if (e.target === e.currentTarget) closeWeightModal();
  });

  // ── Export all
  document.getElementById('btn-export-all').addEventListener('click', exportAllScores);

  // ── Populate filters
  const regionSel = document.getElementById('filter-region');
  regionSel.innerHTML = '<option value="All">All regions</option>' +
    D.REGIONS.map(r => `<option value="${r}">${r}</option>`).join('');

  const techSel = document.getElementById('filter-tech');
  techSel.innerHTML = '<option value="All">All technologies</option>' +
    D.TECHNOLOGIES.map(t => `<option value="${t.key}">${t.label}</option>`).join('');

  const scSel = document.getElementById('filter-sc');
  scSel.innerHTML = '<option value="All">All supply chains</option>' +
    D.SUPPLY_CHAINS.map(s => `<option value="${s.key}">${s.label}</option>`).join('');

  // ── Initialise partnerships view
  initPartnershipsView();

  // ── Initialise map (after DOM ready)
  await initMap();

  // ── Hide loading screen
  const loading = document.getElementById('loading');
  loading.classList.add('hidden');
  setTimeout(() => { loading.style.display = 'none'; }, 450);

  // ── Start on world view
  switchView('world');
});
