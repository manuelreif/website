---
name: cv-pdf
description: >
  Baut das CV-PDF der Website (cv/reif_cv.pdf) aus cv/reif_cv.qmd bzw. data/cv_*.csv neu,
  mit System-TeX statt TinyTeX (dort fehlen raleway/fontawesome, Quarto versucht sonst per
  tlmgr nachzuinstallieren und scheitert), und prüft das Ergebnis. Nutzen bei: "CV-PDF neu
  bauen", "Lebenslauf aktualisieren", "reif_cv.pdf", nach Änderungen an data/cv_*.csv oder
  cv/reif_cv.qmd, oder wenn beim Rendern "finding package for raleway.sty" auftaucht.
---

# CV-PDF bauen

`cv/reif_cv.qmd` (Template modern-cv, in `cv/_extensions/`) liest `../data/cv_stationen.csv`
und wird beim Website-Render ausgelassen (`"!cv"` in `_quarto.yml`). Das PDF muss also
eigens gebaut werden und wird committet (`cv.qmd` verlinkt es).

## Bauen

Aus dem Projektverzeichnis:

```bash
quarto render cv/reif_cv.qmd
```

Das genügt, weil der YAML-Kopf von `reif_cv.qmd` unter `format: modern-cv-pdf:` die nötigen
Optionen setzt. So gelten sie auch für Preview/Render-Buttons in Positron/RStudio:

- `latex-tinytex: false`: System-TeX (`/usr/bin/xelatex`) statt `~/.TinyTeX`. TinyTeX fehlen
  `raleway` und `fontawesome`; ohne die Option hängt Quarto in „finding package for
  raleway.sty“ und das alte PDF ist danach weg, ohne dass ein neues entsteht.
- `latex-auto-install: false`: kein tlmgr-Versuch, Fehler kommen sofort.
- `pdf-engine: xelatex`: Das Template würde sonst LuaLaTeX nehmen; dann fehlt den Schriften
  die Unicode-Zuordnung (Text im PDF schlecht kopier-/durchsuchbar).

Taucht „finding package for raleway.sty“ trotzdem auf: prüfen, ob diese drei Zeilen noch im
YAML-Kopf stehen (und unter `modern-cv-pdf:`, nicht auf oberster Ebene).
Funktioniert mit dem System-Quarto (1.6.4) wie mit 1.10.19. Es bleiben keine
Build-Dateien liegen (`.tex`, `.log`, `reif_cv_files/`); falls doch, löschen.

## Prüfen

```bash
pdfinfo cv/reif_cv.pdf | grep -E "Pages|Creator"   # Creator: XeTeX, Zeitstempel von jetzt
pdffonts cv/reif_cv.pdf                            # Spalte uni = yes
pdftoppm -r 50 -png cv/reif_cv.pdf <scratchpad>/cv # Seiten ansehen (Read auf die PNGs)
```

Mit dem committeten Stand vergleichen: `git show HEAD:cv/reif_cv.pdf > <scratchpad>/old.pdf`,
dann Seitenzahl und Bilder gegenüberstellen. Der CV war bisher **eine Seite**; rutscht
etwas auf Seite 2, dem Nutzer melden (Inhalt kürzen ist seine Entscheidung), nicht
eigenmächtig Layout oder Text ändern.

## Bekannt, kein Fehler

- Im Log: `Font shape 'T1/Raleway(0)/...' undefined`, `pdffonts` zeigt `SFRM*`
  (Computer Modern Sans) statt Raleway. Das war schon in den früheren PDFs so; Raleway wird
  unter XeLaTeX vom Template nicht wirklich geladen. Nur erwähnen, nicht ungefragt reparieren
  (sichtbare Schriftänderung → vorher fragen).
- `cv/.~lock.work.xlsx#` ist eine LibreOffice-Sperrdatei (work.xlsx offen), nicht anfassen.

## Danach

Nicht von selbst committen. Wenn gewünscht: `cv/reif_cv.pdf` zusammen mit den geänderten
`data/cv_*.csv` committen (Branch beachten, keine Co-Authored-By-Zeile).
