# Assemble the final accessible Word report.
# test_content_objects: list (or list of lists) returned by build_test_content()
# intro_docx: path to the rendered intro Word document from Quarto
# Saves the final report to reports/Final_Technical_Report_2026.docx and
# returns the output path invisibly.
create_accessible_yearbook <- function(test_content_objects, intro_docx) {
  doc <- officer::read_docx(intro_docx)

  # Normalize: allow a single content object or a list of objects
  if (!is.null(test_content_objects[["ft"]])) {
    test_content_objects <- list(test_content_objects)
  }

  for (content in test_content_objects) {
    doc <- flextable::body_add_flextable(doc, content$ft)

    # Save figure to a temporary file for embedding
    tmp_fig <- tempfile(fileext = ".png")
    ggplot2::ggsave(tmp_fig, plot = content$fig)
    doc <- officer::body_add_img(
      doc,
      src     = tmp_fig,
      alt_text = content$alt_text,
      width   = 6,
      height  = 4
    )
  }

  out_path <- "reports/Final_Technical_Report_2026.docx"
  print(doc, target = out_path)
  invisible(out_path)
}
