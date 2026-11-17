--[[
BiocStyle Quarto filter for the figure size classes `fig-small` and
`fig-wide`, the counterparts of the R Markdown chunk options `fig.small`
and `fig.wide`. Runs at `pre-ast`, before Quarto turns figures into
cross-referenceable floats, so that `fig-wide` can be mapped to Quarto's
`column-page-right` layout class on figure divs, cells and images.

* fig-wide: extends into the right margin (HTML and PDF).
* fig-small: narrower than the text and centered; in HTML through the
  SCSS rules for `.fig-small`, in PDF by setting the image width.
]]

local small_width = "60%"

local function set_image_width(el, width)
  return el:walk({
    Image = function(img)
      if img.attributes.width == nil then
        img.attributes.width = width
      end
      return img
    end,
  })
end

function Div(el)
  if el.classes:includes("fig-wide") then
    -- Reuse Quarto's page layout: the figure extends into the right
    -- margin, as BiocStyle's wide figures do.
    if not el.classes:includes("column-page-right") then
      el.classes:insert("column-page-right")
    end
    return el
  end
  if el.classes:includes("fig-small") and quarto.doc.is_format("latex") then
    return set_image_width(el, small_width)
  end
end

function Image(img)
  if img.classes:includes("fig-wide") then
    if not img.classes:includes("column-page-right") then
      img.classes:insert("column-page-right")
    end
    return img
  end
  if img.classes:includes("fig-small") and quarto.doc.is_format("latex") then
    if img.attributes.width == nil then
      img.attributes.width = small_width
    end
    return img
  end
end
