// Pre-Render: holt die Daten für die Badges auf der Packages-Seite (CRAN-Version,
// Downloads im letzten Monat, Datum der letzten Veröffentlichung; für Packages nur auf
// GitHub die Version aus der DESCRIPTION) und schreibt sie nach data/pkg_badges.json.
// Der Shortcode {{< pkg-badge >}} liest die Datei beim Rendern.
//
// So werden die Badges selbst gehostet: Besucher*innen laden nichts von r-pkg.org,
// die Zahlen sind so aktuell wie das letzte Rendern/Deploy.
// Läuft nur, wenn pkgs.qmd mitgerendert wird. Schlägt eine Abfrage fehl, bleiben die
// alten Daten des Packages stehen (falls vorhanden).

const packages = ["PP"];
const githubPackages: Record<string, string> = { oxo: "manuelreif/oxo" };
const outFile = "data/pkg_badges.json";

const inputFiles = Deno.env.get("QUARTO_PROJECT_INPUT_FILES") ?? "";
if (inputFiles && !inputFiles.split("\n").some((f) => f.endsWith("pkgs.qmd"))) {
  Deno.exit(0);
}

async function getJson(url: string) {
  const res = await fetch(url, { signal: AbortSignal.timeout(10_000) });
  if (!res.ok) throw new Error(`${url}: HTTP ${res.status}`);
  return await res.json();
}

let data: Record<string, unknown> = {};
try {
  data = JSON.parse(await Deno.readTextFile(outFile));
} catch {
  // noch keine Daten vorhanden
}

for (const pkg of packages) {
  try {
    const [desc, downloads] = await Promise.all([
      getJson(`https://crandb.r-pkg.org/${pkg}`),
      getJson(`https://cranlogs.r-pkg.org/downloads/total/last-month/${pkg}`),
    ]);
    data[pkg] = {
      version: desc["Version"],
      published: String(desc["Date/Publication"]).slice(0, 10),
      downloads_month: downloads[0].downloads,
      downloads_end: downloads[0].end,
    };
  } catch (e) {
    console.warn(`pkg-badges: keine aktuellen Daten für ${pkg} (${e})`);
  }
}

for (const [pkg, repo] of Object.entries(githubPackages)) {
  try {
    const url = `https://raw.githubusercontent.com/${repo}/HEAD/DESCRIPTION`;
    const res = await fetch(url, { signal: AbortSignal.timeout(10_000) });
    if (!res.ok) throw new Error(`${url}: HTTP ${res.status}`);
    const version = (await res.text()).match(/^Version:\s*(\S+)/m)?.[1];
    if (!version) throw new Error(`${url}: keine Version gefunden`);
    data[pkg] = { github_version: version, repo };
  } catch (e) {
    console.warn(`pkg-badges: keine aktuellen Daten für ${pkg} (${e})`);
  }
}

await Deno.writeTextFile(outFile, JSON.stringify(data, null, 2) + "\n");
