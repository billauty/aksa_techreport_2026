library(officer)
library(flextable)
library(ggplot2)

# Assemble the final accessible Word report.
# test_content_objects: list (or list of lists) returned by build_test_content();
#   each element must have $tables (named list of flextable objects) and
#   $figures (named list of lists with $plot and $alt_text).
# intro_docx: path to the rendered intro Word document from Quarto
# Saves the final report to reports/Final_Technical_Report_2026.docx and
# returns the output path invisibly.
create_accessible_yearbook <- function(test_content_objects, intro_docx) {
  doc <- officer::read_docx(intro_docx)

  # Normalize: allow a single content object or a list of objects
  if (!is.null(test_content_objects[["tables"]])) {
    test_content_objects <- list(test_content_objects)
  }

  for (content in test_content_objects) {
    # Embed all tables
    for (tbl in content$tables) {
      doc <- flextable::body_add_flextable(doc, tbl)
    }

    # Embed all figures
    for (fig in content$figures) {
      tmp_fig <- tempfile(fileext = ".png")
      ggplot2::ggsave(tmp_fig, plot = fig$plot)
      doc <- officer::body_add_img(
        doc,
        src      = tmp_fig,
        alt_text = fig$alt_text,
        width    = 6,
        height   = 4
      )
    }
  }
  
  out_path <- "reports/Final_Technical_Report_2026.docx"
  print(doc, target = out_path)
  return(out_path)
}