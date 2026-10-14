/* Offline unit tests for the Alliance Architect scoring engine (no network).
   Run:  node scripts/test-coordination.mjs      (from fantasy-draft/)
   The same suite runs in the browser via coord-tests.html — see the README. */
import { createRequire } from "node:module";
const require = createRequire(import.meta.url);
const TESTS = require("../coord-tests.js");

console.log("Alliance Architect — scoring engine");
const { pass, fail } = TESTS.run((line) => console.log(line));
if (fail) { console.log(`FAILED (${fail})`); process.exit(1); }
console.log("All good.");
