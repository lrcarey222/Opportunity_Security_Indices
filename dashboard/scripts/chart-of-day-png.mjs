#!/usr/bin/env node
// Chart of the Day -> PNG, drawn by the Explorer itself (src/chartOfDay/ExportCard.tsx at
// /export/chart-of-the-day/:date) so the image is the dashboard's own chart in its NEIS styling.
//
// Starts a Vite dev server in-process, opens headless Microsoft Edge (or the browser named by --browser /
// OPSI_BROWSER) through the DevTools protocol, waits for the card's [data-cod-ready], and screenshots exactly the card
// at 2x. No extra npm packages: Node 24's WebSocket speaks the protocol.
//
//   node scripts/chart-of-day-png.mjs [--dates 2026-11-17,2026-11-18 | --all] [--out <dir>] [--scale 2]
//
// Default dates: every day in public/data/chart-of-day/index.json. Default --out: OPSI_COD_PNG_DIR if set (e.g. the
// team's shared folder), else <repo>/outputs/chart_of_day/png (gitignored). Files are <date>.png.

import { spawn } from 'node:child_process';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const dashboard = resolve(here, '..');
const repo = resolve(dashboard, '..');
const argv = process.argv.slice(2);
const opt = (name, dflt) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 ? argv[i + 1] : dflt;
};

const index = JSON.parse(readFileSync(join(dashboard, 'public', 'data', 'chart-of-day', 'index.json'), 'utf8'));
const allDates = index.days.map((d) => d.date).sort();
const dates = opt('dates') ? opt('dates').split(',').map((s) => s.trim()).filter(Boolean) : allDates;
const missing = dates.filter((d) => !allDates.includes(d));
if (missing.length) throw new Error(`not in index.json: ${missing.join(', ')}`);
const outDir = resolve(opt('out', process.env.OPSI_COD_PNG_DIR || join(repo, 'outputs', 'chart_of_day', 'png')));
const scale = Number(opt('scale', '2'));
const WIDTH = 1200;

const candidates = [
  opt('browser'),
  process.env.OPSI_BROWSER,
  'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe',
  'C:/Program Files/Microsoft/Edge/Application/msedge.exe',
  'C:/Program Files/Google/Chrome/Application/chrome.exe',
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  '/usr/bin/google-chrome',
  '/usr/bin/chromium',
].filter(Boolean);
const browserPath = candidates.find((p) => existsSync(p));
if (!browserPath) throw new Error('no Chromium-family browser found; pass --browser <path>');

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

/** A synced folder (OneDrive, SharePoint) briefly locks a file while it uploads (Windows reports EBUSY, EPERM, EACCES
 *  or UNKNOWN/-4094 when overwriting it): retry for up to ~30 s. */
async function writeWithRetry(file, data) {
  for (let i = 0; ; i++) {
    try {
      writeFileSync(file, data);
      return;
    } catch (e) {
      if (i >= 30 || !['EBUSY', 'EPERM', 'EACCES', 'UNKNOWN'].includes(e.code)) throw e;
      await sleep(1000);
    }
  }
}

/** Minimal DevTools-protocol client over one browser WebSocket (flattened sessions). */
class Cdp {
  constructor(url) {
    this.ws = new WebSocket(url);
    this.id = 0;
    this.pending = new Map();
    this.ws.addEventListener('message', (ev) => {
      const msg = JSON.parse(typeof ev.data === 'string' ? ev.data : Buffer.from(ev.data).toString());
      if (msg.id && this.pending.has(msg.id)) {
        const { ok, err } = this.pending.get(msg.id);
        this.pending.delete(msg.id);
        if (msg.error) err(new Error(`${msg.error.message} ${msg.error.data ?? ''}`));
        else ok(msg.result);
      }
    });
    this.open = new Promise((ok, err) => {
      this.ws.addEventListener('open', ok, { once: true });
      this.ws.addEventListener('error', err, { once: true });
    });
  }
  send(method, params = {}, sessionId) {
    const id = ++this.id;
    this.ws.send(JSON.stringify({ id, method, params, ...(sessionId ? { sessionId } : {}) }));
    return new Promise((ok, err) => this.pending.set(id, { ok, err }));
  }
}

async function launchBrowser() {
  const profile = mkdtempSync(join(tmpdir(), 'cod-png-'));
  const proc = spawn(browserPath, ['--headless=new', '--disable-gpu', '--hide-scrollbars', '--no-first-run', '--no-default-browser-check',
    '--remote-debugging-port=0', `--user-data-dir=${profile}`, 'about:blank'], { stdio: ['ignore', 'ignore', 'pipe'] });
  const wsUrl = await new Promise((ok, err) => {
    let buf = '';
    const t = setTimeout(() => err(new Error(`browser did not start: ${buf.slice(0, 400)}`)), 30000);
    proc.stderr.on('data', (c) => {
      buf += c.toString();
      const m = buf.match(/DevTools listening on (ws:\/\/\S+)/);
      if (m) { clearTimeout(t); ok(m[1]); }
    });
  });
  return { proc, wsUrl, profile };
}

async function main() {
  mkdirSync(outDir, { recursive: true });
  const { createServer } = await import('vite');
  const server = await createServer({ root: dashboard, logLevel: 'error', server: { port: 0, strictPort: false, open: false } });
  await server.listen();
  const base = (server.resolvedUrls?.local?.[0] ?? `http://localhost:${server.config.server.port}/`).replace(/\/$/, '');
  const { proc, wsUrl, profile } = await launchBrowser();
  const cdp = new Cdp(wsUrl);
  await cdp.open;
  const written = [];
  try {
    const { targetId } = await cdp.send('Target.createTarget', { url: 'about:blank' });
    const { sessionId } = await cdp.send('Target.attachToTarget', { targetId, flatten: true });
    const s = (m, p) => cdp.send(m, p, sessionId);
    await s('Page.enable');
    await s('Runtime.enable');
    await s('Emulation.setDeviceMetricsOverride', { width: WIDTH, height: 900, deviceScaleFactor: scale, mobile: false });
    await s('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-color-scheme', value: 'dark' }] });
    for (const date of dates) {
      await s('Page.navigate', { url: `${base}/export/chart-of-the-day/${date}` });
      let h = 0;
      for (let i = 0; i < 600; i++) {
        const r = await s('Runtime.evaluate', {
          expression: `(() => { const e = document.querySelector('[data-cod-ready]'); if (document.querySelector('[data-cod-error]')) return -1; return e ? Math.ceil(e.getBoundingClientRect().height) : 0; })()`,
          returnByValue: true,
        });
        h = r.result.value;
        if (h) break;
        await sleep(100);
      }
      if (h <= 0) throw new Error(`${date}: the export card did not become ready (${h === -1 ? 'error shown' : 'timeout'})`);
      await s('Emulation.setDeviceMetricsOverride', { width: WIDTH, height: h, deviceScaleFactor: scale, mobile: false });
      await sleep(250); // let Plot's ResizeObserver settle after the viewport change
      const r2 = await s('Runtime.evaluate', { expression: `Math.ceil(document.querySelector('[data-cod-ready]').getBoundingClientRect().height)`, returnByValue: true });
      h = r2.result.value;
      const shot = await s('Page.captureScreenshot', { format: 'png', clip: { x: 0, y: 0, width: WIDTH, height: h, scale: 1 }, captureBeyondViewport: true });
      const file = join(outDir, `${date}.png`);
      await writeWithRetry(file, Buffer.from(shot.data, 'base64'));
      written.push(file);
      console.log(`${date} -> ${file} (${WIDTH * scale} x ${h * scale})`);
    }
  } finally {
    try { await cdp.send('Browser.close'); } catch { proc.kill(); }
    await server.close();
    await sleep(300);
    try { rmSync(profile, { recursive: true, force: true }); } catch { /* the browser may still hold it briefly */ }
  }
  console.log(`wrote ${written.length} PNG(s) to ${outDir}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
