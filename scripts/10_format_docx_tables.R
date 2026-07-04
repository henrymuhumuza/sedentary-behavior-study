args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript scripts/10_format_docx_tables.R path/to/document.docx")
}

docx_path <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
tmp_dir <- tempfile("docx_tables_")
dir.create(tmp_dir, recursive = TRUE)
zip_path <- file.path(tmp_dir, "document.zip")
invisible(file.copy(docx_path, zip_path, overwrite = TRUE))
utils::unzip(zip_path, exdir = tmp_dir)

document_xml <- file.path(tmp_dir, "word", "document.xml")
xml <- xml2::read_xml(document_xml)
ns <- xml2::xml_ns(xml)

set_attr <- function(node, name, value) xml2::xml_set_attr(node, name, value)

ensure_child <- function(parent, child_name) {
  child <- xml2::xml_find_first(parent, paste0("./w:", child_name), ns)
  if (inherits(child, "xml_missing")) {
    xml2::xml_add_child(parent, paste0("w:", child_name))
    child <- xml2::xml_find_first(parent, paste0("./w:", child_name), ns)
  }
  child
}

for (tbl in xml2::xml_find_all(xml, ".//w:tbl", ns)) {
  tbl_pr <- ensure_child(tbl, "tblPr")

  tbl_w <- ensure_child(tbl_pr, "tblW")
  set_attr(tbl_w, "w:w", "5000")
  set_attr(tbl_w, "w:type", "pct")

  tbl_layout <- ensure_child(tbl_pr, "tblLayout")
  set_attr(tbl_layout, "w:type", "fixed")

  cell_mar <- ensure_child(tbl_pr, "tblCellMar")
  for (side in c("top", "left", "bottom", "right")) {
    margin <- ensure_child(cell_mar, side)
    set_attr(margin, "w:w", "40")
    set_attr(margin, "w:type", "dxa")
  }

  for (p in xml2::xml_find_all(tbl, ".//w:p", ns)) {
    p_pr <- ensure_child(p, "pPr")
    spacing <- ensure_child(p_pr, "spacing")
    set_attr(spacing, "w:before", "0")
    set_attr(spacing, "w:after", "0")
    set_attr(spacing, "w:line", "240")
    set_attr(spacing, "w:lineRule", "auto")
  }

  for (r in xml2::xml_find_all(tbl, ".//w:r", ns)) {
    r_pr <- ensure_child(r, "rPr")
    sz <- ensure_child(r_pr, "sz")
    set_attr(sz, "w:val", "18")
    sz_cs <- ensure_child(r_pr, "szCs")
    set_attr(sz_cs, "w:val", "18")
  }
}

xml2::write_xml(xml, document_xml)
old_wd <- setwd(tmp_dir)
on.exit(setwd(old_wd), add = TRUE)

unlink(zip_path)
files <- list.files(tmp_dir, all.files = TRUE, no.. = TRUE)
if (file.exists(docx_path)) unlink(docx_path)
utils::zip(zipfile = docx_path, files = files)
