/* ============================================================================
   FIREBASE CONFIG  —  fill this in to enable real multiplayer leagues.
   ----------------------------------------------------------------------------
   Until you paste a real config below, the game runs in LOCAL mode: multiplayer
   works only across tabs of the SAME browser (great for testing, not for friends
   on other devices). Solo (vs AI) always works with no setup.

   ONE-TIME SETUP (free, ~5 minutes):
   1. Go to https://console.firebase.google.com and click "Add project"
      (any name, e.g. "allied-draft"). Disable Analytics if asked.
   2. In the left sidebar open  Build → Realtime Database → "Create Database".
      Pick a location, then choose **"Start in test mode"** and Enable.
      (Test mode = open read/write. Fine for a game; see the note at the bottom
      if you want to lock it down later.)
   3. Click the gear icon → "Project settings" → scroll to "Your apps" →
      click the Web icon "</>" → register an app (any nickname).
   4. Firebase shows a `firebaseConfig = { ... }` object. Copy its values into
      FIREBASE_CONFIG below. Make sure `databaseURL` is included — if it isn't
      shown, it's  https://<your-project-id>-default-rtdb.firebaseio.com
   5. Commit + push. That's it — leagues now work across devices.

   Optional hardening (so only players can read/write leagues) — set these
   Realtime Database rules instead of test mode:
     { "rules": { "leagues": { "$id": { ".read": true, ".write": true } } } }
   (Still open, but scoped. For real auth you'd add Firebase Anonymous Auth.)
   ============================================================================ */
const FIREBASE_CONFIG = {
  apiKey: "PASTE_YOUR_API_KEY",
  authDomain: "PASTE_YOUR_PROJECT.firebaseapp.com",
  databaseURL: "https://PASTE_YOUR_PROJECT-default-rtdb.firebaseio.com",
  projectId: "PASTE_YOUR_PROJECT",
  appId: "PASTE_YOUR_APP_ID",
};
