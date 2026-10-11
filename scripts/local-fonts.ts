// Post-Render: ersetzt Google-Fonts-Imports in der gerenderten Seite durch die
// lokal gehosteten Schriften (fonts/fonts.css).
//
// Betrifft Output, den R-Pakete selbst erzeugen und der in _freeze/ eingefroren ist
// (gt-Tabellen, dygraphs via dyCSS), die gt-HTML-Dateien in den pp1-iframes und den
// RSS-Feed (blog.xml enthält den Post-Inhalt).
// Läuft mit dem in Quarto gebündelten Deno, braucht also auch auf Netlify nichts extra.
//
// Ersetzt wird nur die URL selbst, damit die umgebenden Anführungszeichen erhalten
// bleiben (der dygraphs-Import steckt z. B. in einem JSON-String).

const outputDir = Deno.env.get("QUARTO_PROJECT_OUTPUT_DIR") ?? "_site";
const localFontsCss = "/fonts/fonts.css";

const googleFontsUrl = /https:\/\/fonts\.googleapis\.com\/[^"'()\s\\]*/g;
// mehrere gleiche Imports hintereinander (gt importiert jede Schrift einzeln) zusammenfassen
const duplicateImports =
  /(@import url\((["'])\/fonts\/fonts\.css\2\);?)(\s*@import url\((["'])\/fonts\/fonts\.css\4\);?)+/g;

async function* outputFiles(dir: string): AsyncGenerator<string> {
  for await (const entry of Deno.readDir(dir)) {
    const path = `${dir}/${entry.name}`;
    if (entry.isDirectory) {
      yield* outputFiles(path);
    } else if (entry.isFile && /\.(html|xml)$/.test(entry.name)) {
      yield path;
    }
  }
}

let changed = 0;
for await (const file of outputFiles(outputDir)) {
  const content = await Deno.readTextFile(file);
  if (!content.includes("fonts.googleapis.com")) continue;

  const fixed = content
    .replace(googleFontsUrl, localFontsCss)
    .replace(duplicateImports, "$1");
  await Deno.writeTextFile(file, fixed);
  changed++;
}

if (changed > 0) {
  console.log(`local-fonts: Google-Fonts-Imports in ${changed} Datei(en) durch ${localFontsCss} ersetzt`);
}
