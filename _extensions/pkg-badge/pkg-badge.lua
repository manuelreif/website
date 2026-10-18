-- Shortcode {{< pkg-badge PP version|downloads|release >}}: Badge mit CRAN-Daten,
-- verlinkt auf die CRAN-Seite des Packages.
-- {{< pkg-badge oxo github-version >}}: Version aus der DESCRIPTION auf GitHub,
-- verlinkt auf die NEWS.md im Repo (für Packages, die nicht auf CRAN sind).
-- Die Daten holt scripts/pkg-badges.ts vor dem Rendern nach data/pkg_badges.json.
-- Fehlen sie, gibt der Shortcode nichts aus (mit Warnung).

local function load_data()
  local path = pandoc.path.join({ quarto.project.directory or ".", "data/pkg_badges.json" })
  local f = io.open(path, "r")
  if not f then return {} end
  local content = f:read("a")
  f:close()
  return quarto.json.decode(content)
end

-- 1234567 -> "1.234.567"
local function tausender(n)
  local s = tostring(math.floor(n))
  return (s:reverse():gsub("(%d%d%d)", "%1."):reverse():gsub("^%.", ""))
end

-- "2026-10-14" -> "14.10.2026"
local function datum(iso)
  local y, m, d = iso:match("(%d+)-(%d+)-(%d+)")
  return d .. "." .. m .. "." .. y
end

local function badge(href, label, value, title)
  return pandoc.RawInline("html", string.format(
    '<a class="pkg-badge" href="%s" title="%s">' ..
    '<span class="pkg-badge-label">%s</span> %s</a>',
    href, title, label, value))
end

return {
  ["pkg-badge"] = function(args)
    if not quarto.doc.is_format("html") then return pandoc.Null() end

    local pkg = pandoc.utils.stringify(args[1])
    local what = pandoc.utils.stringify(args[2])
    local d = load_data()[pkg]
    if not d then
      quarto.log.warning("pkg-badge: keine Daten für " .. pkg)
      return pandoc.Null()
    end

    local cran = "https://cran.r-project.org/package=" .. pkg
    if what == "version" then
      return badge(cran, "CRAN", d.version, "Aktuelle Version auf CRAN")
    elseif what == "downloads" then
      return badge(cran, "Downloads", tausender(d.downloads_month) .. " / Monat",
        "Downloads in den 30 Tagen bis " .. datum(d.downloads_end) .. " (Posit-CRAN-Mirror)")
    elseif what == "release" then
      return badge(cran, "Release", datum(d.published), "Letzte Veröffentlichung auf CRAN")
    elseif what == "github-version" then
      return badge("https://github.com/" .. d.repo .. "/blob/HEAD/NEWS.md", "Version",
        d.github_version, "Aktuelle Version auf GitHub (noch nicht auf CRAN)")
    end
    quarto.log.warning("pkg-badge: unbekannter Badge-Typ " .. what)
    return pandoc.Null()
  end
}
