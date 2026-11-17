--[[
BiocStyle Quarto filter (runs after Quarto's own filters).

* HTML: wrap tables in a `horizontal-scroll` container so wide tables
  scroll instead of overflowing the page, as BiocStyle::html_document does.
* LaTeX: Bioconductor.sty highlights inline code with `soul`, which cannot
  handle pandoc's control spaces (`\ `) inside `\texttt`; replace them with
  plain spaces, as BiocStyle::pdf_document's post-processor does.
]]

function Table(el)
  if quarto.doc.is_format("html") then
    return pandoc.Div({ el }, pandoc.Attr("", { "horizontal-scroll" }))
  end
end

function Code(el)
  if quarto.doc.is_format("latex") and #el.classes == 0 then
    local tex = pandoc.write(pandoc.Pandoc({ pandoc.Plain({ el }) }), "latex")
    tex = tex:gsub("\n$", ""):gsub("\\ ", " ")
    return pandoc.RawInline("latex", tex)
  end
end
