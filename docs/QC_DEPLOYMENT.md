# AIB Explorer expert review: deployment, storage and security

This document describes how the review pages are hosted, where reviewer submissions go, what must never
happen, and the backend options NEIS can choose from. **No backend is chosen in code**: the choice, and
the data-protection decisions that come with it, are NEIS's. Related: [QC_SYSTEM_DESIGN.md](QC_SYSTEM_DESIGN.md)
section 7, [QC_ADMIN_GUIDE.md](QC_ADMIN_GUIDE.md) (collecting and exporting submissions),
[QC_REVIEWER_GUIDE.md](QC_REVIEWER_GUIDE.md).

---

## 1. Architecture as built

- **A static site with no backend.** The Explorer, including `/review` and `/review/admin`, is a static
  React app (`dashboard/`). Everything a reviewer does runs in their browser. The site serves only
  public material: Explorer bundles, the built review data in `dashboard/public/data/review/` (case
  snapshots built from public outputs, question text, method cards, framework scenarios, a robustness
  map, a QC summary and clearly synthetic demo submissions). No submission, reviewer name or comment is
  ever part of the site.
- **Hosting.** The public site is published by hand to GitHub Pages from main, through the separate
  repository `lrcarey222/aib-explorer` (the Pages copy). The repository also contains a Netlify
  configuration, `dashboard/netlify.toml`, with a strict Content-Security-Policy (CSP):
  `default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self' data: blob:;
  font-src 'self'; connect-src 'self'; worker-src 'self'; frame-ancestors 'self' https://neiscenter.org
  https://*.neiscenter.org; base-uri 'self'; form-action 'none'`.
  - `connect-src 'self'` means the browser may only send requests to the site's own origin. A live
    endpoint on another origin is blocked until `connect-src` names it.
  - **GitHub Pages cannot set response headers**, so `netlify.toml` (CSP included) does not apply to the
    Pages copy, and `dashboard/index.html` has no CSP `<meta>` tag. As built, the Pages copy therefore runs
    **without** a CSP. If NEIS keeps Pages hosting for the review, decide whether to add a
    `<meta http-equiv="Content-Security-Policy">` to `index.html` (a meta CSP cannot carry
    `frame-ancestors`), or to host the review build on Netlify or another host that sets headers.
- **Reviewer state.** Profile, drafts and submissions are kept in the reviewer's browser
  (`localStorage`, keys prefixed `aibx-review:v1:`, with an in-memory fallback and a warning when storage
  is not durable). The profile never leaves the browser; each submission carries a random reviewer code,
  the reviewer's expertise tags and self-ratings, a digest of the profile, and the name only when given
  and not anonymous.
- **Admin.** `/review/admin` reads files the admin loads from disk, in the admin's browser. It never
  uploads anything. Adjudications are kept in that browser until downloaded.
- **Build identity.** `vite.config.ts` injects the app's git commit and build date
  (`VITE_APP_COMMIT`, `VITE_BUILD_DATE`; existing environment values win). Every submission records
  them with the data release.

## 2. The three storage modes

Chosen at build time (`dashboard/src/review/sink.ts`; flags read by `sinkMode()` and
`review/appInfo.ts`):

| Variable | Values | Effect |
|---|---|---|
| `VITE_REVIEW_SINK` | `local` (default; any unknown value), `packet`, `http` | Where a finished submission goes |
| `VITE_REVIEW_ENDPOINT` | an `https://` URL | The POST target in `http` mode. Without a valid `http(s)://` URL every send fails with "No review endpoint is configured for this build", and the submission stays in the browser |
| `VITE_REVIEW_UI` | `1` to switch on | Shows the review toolbar and the "Report an issue" buttons to **every** visitor. Unset, they show only in a browser holding a reviewer profile (`reviewUiEnabled()`); the public Explorer is otherwise unchanged |

- **`local`, "Demo"**: `LocalSink` stores the submission in the browser. Nothing is sent. The reviewer
  downloads all their answers as one packet from `/review` and sends it by hand.
- **`packet`**: `PacketSink` stores a copy and downloads a one-submission packet
  (`aib-review-R-XXXXXXXX-<time>.json`, schema `aib-review-packet`; the file name carries the
  pseudonymous code, never a name). On a case, the case review and its decision-use and threat answers
  come as one packet.
- **`http`, "Live"**: the submission is first stored locally, then POSTed as JSON to
  `VITE_REVIEW_ENDPOINT` with `Content-Type: application/json`, `credentials: 'omit'`, `mode: 'cors'`,
  `cache: 'no-store'`, `referrerPolicy: 'no-referrer'` and a 15-second timeout. No cookies, no
  credentials, no third-party SDK, and the payload is never logged. A failed or refused POST leaves the
  local copy; there is no automatic retry (the reviewer can still download a packet).

Because `application/json` is not a "simple" content type, browsers send a CORS **preflight**
(`OPTIONS`) before each POST: any endpoint must answer it, allowing the Explorer's origin, `POST` and the
`Content-Type` header.

Example builds (from `dashboard/`): `VITE_REVIEW_SINK=packet npm run build`; for live mode,
`VITE_REVIEW_SINK=http VITE_REVIEW_ENDPOINT=https://<endpoint> npm run build` plus the CSP change in
section 4. Every `VITE_*` value is compiled into the public JavaScript: it is visible to anyone.

## 3. What must never happen

- **Submissions in a public repository.** Not in this repository, not in `lrcarey222/aib-explorer`, not
  in any Pages or artifact copy. `qc/private/` and `**/review_submissions/` are git-ignored here;
  `export-review.mjs` refuses to write anywhere in a work tree except an ignored `qc/private/`. Never
  point a backend at a git repository as its store.
- **Credentials in the frontend.** No API key, password, service token or connection string in a
  `VITE_*` variable or in source. The live endpoint must be a public, write-only URL whose service does
  its own validation, rate limiting and access control. (A Power Automate trigger URL is a special case:
  see 4b.)
- **Relying on the `/review` URL being secret.** It is not, and nothing depends on it: the review pages
  hold only public outputs and question text, and `/review/admin` holds nothing until an admin loads
  files on their own computer. Private data exist only in reviewers' browsers, in the packets they send
  and in NEIS's store.
- **Reading back submissions from the site.** No endpoint may let the browser list or read stored
  submissions. Retrieval is an admin task outside the site.

## 4. Backend options (NEIS to choose)

Each option is compatible with the code as built unless stated. In every case NEIS keeps the raw
submission JSON verbatim, brings it into `qc/private/submissions/` (one file per submission, or arrays,
or packets) and runs `export-review.mjs` ([QC_ADMIN_GUIDE.md](QC_ADMIN_GUIDE.md) section 10).

Common privacy points for every option:

- **Reviewer names.** A name travels only when the reviewer gives one and does not choose anonymity.
  Exports drop names unless `--include-names`. Decide whether names should be collected at all.
- **Consent and notice.** The app has no consent checkbox. Put a short notice (purpose, who at NEIS sees
  the answers, retention, contact, how to withdraw) in the campaign `description`, which is shown at the
  top of `/review`, and in the reviewer guide. NEIS to write the text.
- **Retention.** Decide how long raw submissions, packets, exports and any code-to-person key are kept,
  and where (RMI's Microsoft 365 tenant is the natural home). Reviewers can delete their own browser
  copy by clearing site data; there is no in-app delete.
- **Validation.** Any receiving service should accept only JSON with `schema: "aib-review-submission"`,
  `schema_version: 1`, a known `kind` and a bounded size (a few tens of kB is ample), and reject
  everything else. The full validator is `validateSubmission` in `dashboard/src/review/manifest.ts`, and
  `export-review.mjs` re-validates on export in any case.

### (a) Packet mode plus email or SharePoint upload (zero infrastructure)

- **Build:** `VITE_REVIEW_SINK=packet`. No endpoint, no CSP change.
- **Flow:** each submit downloads a packet; reviewers email the files to a named NEIS mailbox or upload
  them to a SharePoint or OneDrive "file request" folder NEIS controls. The all-in-one download on
  `/review` is a safety net at the end.
- **Trade-offs:** nothing to run, secure or pay for; nothing reaches NEIS unless a reviewer sends it, so
  answers can be lost or arrive late; a handful of files per reviewer to collect by hand; duplicates are
  harmless (de-duplicated by `submission_id`).
- **Privacy:** data travel by email or upload inside RMI's tenant; set a mailbox or folder with
  restricted access; the files carry no name unless the reviewer gave one.
- **Admin retrieval:** save attachments or the upload folder's files into `qc/private/submissions/`.

### (b) Microsoft Power Automate HTTP-trigger flow to a SharePoint list or Dataverse

- **Build:** `VITE_REVIEW_SINK=http`, `VITE_REVIEW_ENDPOINT=<the flow's trigger URL>`; add the flow's host
  to `connect-src` (section 5).
- **Flow:** "When an HTTP request is received" trigger, a Parse JSON step with the submission schema,
  then a create-item step in a SharePoint list (store the raw JSON in a multi-line text column plus a few
  indexed columns: submission_id, kind, campaign_id, reviewer code, submitted_at) or a Dataverse table;
  respond 200/202, or 400 on a bad body.
- **Trade-offs:** fits RMI's Microsoft 365; data stay in the tenant; no server code. But:
  - The trigger URL carries a signature in its query string: it is a **bearer secret** that is compiled
    into the public bundle, so anyone can find it and post to the flow. Treat it as public and defend the
    flow itself.
  - **Rate limiting** is weak: flows have run and throughput limits, not per-client limits. Add
    validation that rejects bad bodies early, a size cap, and alerts on unusual volume.
  - **Rotation** means a new trigger URL, a rebuild and a redeploy. Plan for it (for example after the
    round, or if the URL is abused).
  - **CORS:** confirm that the trigger answers the browser's preflight from the Explorer's origin before
    choosing this option; if it does not, the browser blocks every POST.
  - The HTTP trigger is a premium connector: check licensing with RMI IT.
- **Privacy:** data in a SharePoint list or Dataverse in RMI's tenant, access by list permissions;
  retention via a retention label or a scheduled clean-up flow.
- **Admin retrieval:** export the list (or a view filtered by campaign) and write each row's raw JSON to
  a `.json` file in `qc/private/submissions/` (a small export flow or a script can do this; none is
  provided).

### (c) A small serverless function (Azure Functions or Cloudflare Workers)

- **Build:** `VITE_REVIEW_SINK=http`, `VITE_REVIEW_ENDPOINT=https://<function URL>`; add its origin to
  `connect-src`.
- **Function:** accept `POST` (and the `OPTIONS` preflight) only from the Explorer's origin; validate the
  body (schema, kind, size); add a server-side receive time; write it to a private store (Azure Blob or
  Table storage in an RMI subscription; Cloudflare R2, KV or D1); return 202. No read endpoint. Rate-limit
  per IP; a challenge such as Turnstile would need code and CSP changes not built here.
- **Trade-offs:** the most control (validation, rate limits, logging without payloads, rotation without
  rebuild because the URL holds no secret) for a little code to write, deploy and maintain. Azure keeps
  data in RMI's tenant and region; Cloudflare needs a vendor and data-location review.
- **Privacy:** storage access limited to named NEIS staff; retention by a lifecycle rule; logs must not
  record bodies.
- **Admin retrieval:** download the stored objects (each is one submission JSON) into
  `qc/private/submissions/`.

### (d) A hosted database or form service with row-level security (for example Supabase)

- **Build:** `VITE_REVIEW_SINK=http` and an endpoint, plus `connect-src` for the service's origin.
- **Fit with the code as built:** the browser sends only `Content-Type`. A table insert through
  Supabase's REST API also needs an `apikey` header, so either put an Edge Function (or similar) in front
  of the table that accepts the plain POST, or add a header to `HttpSink` (a code change; the
  "anon" key is public by design, so this is not a secret, but it must be paired with RLS).
- **Security:** row-level security with an insert-only policy for anonymous users and no select, update
  or delete; a check constraint or function validating the JSON; rate limits from the service.
- **Trade-offs:** quick to set up, with a dashboard and SQL export; but a third-party processor outside
  RMI's tenant: needs a vendor and security review, a data-processing agreement and a decision on the
  data region (data residency). Free tiers can pause or change terms.
- **Admin retrieval:** export rows (SQL or CSV) and write each raw JSON value to a file in
  `qc/private/submissions/`.

## 5. CSP edits for a live endpoint

For any `http` option, the site's CSP must allow the endpoint's origin, and only it:

```
connect-src 'self' https://<endpoint-origin>
```

- On Netlify: edit the `Content-Security-Policy` value in `dashboard/netlify.toml`. Leave
  `form-action 'none'` (the review uses `fetch`, not form posts) and every other directive as it is.
- On GitHub Pages: there is no header CSP (section 1). If NEIS adds a meta CSP to `index.html`, it must
  include the same `connect-src`.
- Change the CSP in the same commit as the build variables, and remove the origin when the endpoint is
  retired.

## 6. Local and demo use

- `npm run dev` (from `dashboard/`) serves the app with the default `local` sink: submissions stay in
  the browser; nothing is sent.
- `dashboard/public/data/review/fixtures/demo_submissions.json` holds **synthetic** submissions generated
  by `review/demo.ts` from a fixed seed (marked `synthetic: true` and `[synthetic]`). Load them in
  `/review/admin` with "Load the demo data (synthetic)": the page then warns that nothing shown is a real
  answer, and the CSV zip is named `-SYNTHETIC`. Never quote or count them as reviews.
- `build-review-data.mjs --no-demo` leaves the fixtures out (for example in a production build, if NEIS
  prefers).

## 7. Recommended default for round 1 (pending NEIS decision)

**Packet mode (option a)**: `VITE_REVIEW_SINK=packet`, no endpoint, no CSP change, packets returned to a
restricted NEIS mailbox or upload folder in RMI's tenant. It needs no infrastructure, puts no secret in
the bundle and keeps every answer inside RMI's systems; its cost is manual collection, which is
manageable for a first round of tens of reviewers. Revisit options (b) or (c) for a larger round. This is
a recommendation only; NEIS decides.
