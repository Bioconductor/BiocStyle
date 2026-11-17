--[[
BiocStyle Quarto filter for document metadata (runs before Quarto's own
filters, after Quarto has normalized authors and affiliations).

* `package:` field -> `biocstyle-package` metadata ("Name version"), shown
  by the title block. The version comes from `package-version` metadata
  (set by the BiocStyle::quarto_html / quarto_pdf vignette engines) or,
  when previewing a vignette interactively, from the DESCRIPTION file in
  the parent directory of the document.
* Email marks (*, **, ...) for the title block.
* PDF author block: authors, affiliations and emails as `authblk`
  commands for Bioconductor.sty.
]]

local stringify = pandoc.utils.stringify

local function trim(s)
  return (s:gsub("^%s+", ""):gsub("%s+$", ""))
end

-- Return the value of `field` in a DCF (DESCRIPTION) text.
local function dcf_field(text, field)
  return text:match("^" .. field .. ":[ \t]*([^\r\n]+)")
    or text:match("\n" .. field .. ":[ \t]*([^\r\n]+)")
end

-- Version of `pkg` from a DESCRIPTION file next to or above the document.
local function description_version(pkg)
  local input = quarto.doc.input_file
  if input == nil then
    return nil
  end
  local dir = pandoc.path.directory(input)
  for _, rel in ipairs({ "..", "." }) do
    local path = pandoc.path.join({ dir, rel, "DESCRIPTION" })
    local fh = io.open(path, "r")
    if fh then
      local text = fh:read("a")
      fh:close()
      local name = dcf_field(text, "Package")
      local version = dcf_field(text, "Version")
      if name and version and trim(name) == pkg then
        return trim(version)
      end
    end
  end
  return nil
end

local function package_line(meta)
  if meta.package == nil then
    return nil
  end
  local pkg = trim(stringify(meta.package))
  if pkg == "" then
    return nil
  end
  -- Like BiocStyle::html_document, only append a version to something
  -- that looks like a package name.
  if not pkg:match("^[%w.]*%w$") then
    return pkg
  end
  local version = nil
  if meta["package-version"] ~= nil then
    version = trim(stringify(meta["package-version"]))
  end
  if version == nil or version == "" then
    version = description_version(pkg)
  end
  if version == nil or version == "" then
    return pkg
  end
  return pkg .. " " .. version
end

local function latex(inlines)
  return pandoc.write(pandoc.Pandoc({ pandoc.Plain(inlines) }), "latex")
end

-- \author / \affil commands for authblk, mirroring auth_affil_latex() in
-- R/auth_affil.R.
local function latex_authors(meta)
  local authors = meta["by-author"]
  if authors == nil or #authors == 0 then
    return nil
  end
  local affiliations = meta["by-affiliation"] or {}
  local lines = {}
  local names = {}
  for _, author in ipairs(authors) do
    local name = latex(author.name.literal)
    if author.email ~= nil then
      name = name .. "\\thanks{\\ttfamily " .. latex(author.email) .. "}"
    end
    local marks = {}
    for _, affil in ipairs(author.affiliations or {}) do
      table.insert(marks, stringify(affil.number))
    end
    if #affiliations > 0 then
      table.insert(lines, "\\author[" .. table.concat(marks, ",") .. "]{" .. name .. "}")
    else
      table.insert(names, name)
    end
  end
  if #affiliations > 0 then
    for _, affil in ipairs(affiliations) do
      table.insert(lines, "\\affil[" .. stringify(affil.number) .. "]{" .. latex(affil.name) .. "}")
    end
  else
    local n = #names
    local joined = names[1]
    if n > 1 then
      joined = table.concat(names, ", ", 1, n - 1) .. " and " .. names[n]
    end
    table.insert(lines, "\\author{" .. joined .. "}")
  end
  return table.concat(lines, "\n")
end

-- Mark authors' emails with *, **, ... as auth_affil_html() does; the title
-- block templates read the mark from `emailmark`.
local function mark_emails(meta)
  local authors = meta["by-author"]
  if authors == nil then
    return
  end
  local marks = {}
  local n = 0
  for _, author in ipairs(authors) do
    if author.email ~= nil then
      local email = stringify(author.email)
      if marks[email] == nil then
        n = n + 1
        marks[email] = string.rep("*", n)
      end
      author.emailmark = pandoc.MetaString(marks[email])
    end
  end
  meta["by-author"] = authors
end

function Meta(meta)
  mark_emails(meta)
  local line = package_line(meta)
  if line ~= nil then
    meta["biocstyle-package"] = pandoc.MetaInlines(pandoc.Inlines(line))
  end
  if quarto.doc.is_format("latex") then
    local authors = latex_authors(meta)
    if authors ~= nil then
      meta["biocstyle-latex-authors"] = pandoc.MetaBlocks({ pandoc.RawBlock("latex", authors) })
    end
  end
  return meta
end
