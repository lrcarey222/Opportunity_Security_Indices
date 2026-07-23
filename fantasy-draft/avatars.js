/* ============================================================================
   FIGHTER PORTRAITS  ·  ally id -> image (data-URI or path)
   ----------------------------------------------------------------------------
   Populate each entry with the cropped Street-Fighter-style portrait for that
   ally. Data-URIs keep the game fully self-contained (works offline and as a
   published artifact). Until an entry exists, the SVG chibi mascot is used as
   a fallback, so the game always renders.

   Grid position in the source roster art (7 cols x 3 rows):
     row0: US CA MX UK DE FR IT
     row1: NL ES SE FI PL JP KR
     row2: AU IN TW NO NZ SG TR
   The 12 drafted allies are: US CA MX UK DE FR IT JP KR AU IN TW
   ============================================================================ */
const PORTRAITS = {
  // US: "data:image/webp;base64,....",
  // CA: "...",  MX: "...",  UK: "...",  DE: "...",  FR: "...",  IT: "...",
  // JP: "...",  KR: "...",  AU: "...",  IN: "...",  TW: "...",
};
