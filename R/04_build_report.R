library(officer)
library(flextable)
library(ggplot2)

# Create a blank intro Word document and return its file path.
# Used as the intro_docx argument to create_accessible_yearbook() when no
# pre-rendered Quarto intro document is available.
make_empty_intro_docx <- function() {
  path <- "reports/intro_template.docx"
  doc  <- officer::read_docx()
  print(doc, target = path)
  path
}

# Aggregate the individually built table_* and fig_* targets into the
# test_content_objects format expected by create_accessible_yearbook().
#
# Arguments:
#   table_01_ctt_summary - flextable: CTT score summary (Table 1)
#   table_02_item_stats  - flextable: CTT item statistics (Table 2)
#   table_06_irt_summary - flextable: IRT model summary (Table 6)
#   table_08_irt_params  - flextable: IRT item parameter estimates (Table 8)
#   fig_03_csem          - ggplot: Conditional SEM curve (Figure 3)
#
# Returns a list with $tables (named list of flextable objects) and
# $figures (named list of lists, each with $plot and $alt_text).
assemble_report_content <- function(table_01_ctt_summary,
                                    table_02_item_stats,
                                    table_06_irt_summary,
                                    table_08_irt_params,
                                    fig_03_csem) {
  list(
    tables = list(
      table_01_ctt_summary = table_01_ctt_summary,
      table_02_item_stats  = table_02_item_stats,
      table_06_irt_summary = table_06_irt_summary,
      table_08_irt_params  = table_08_irt_params
    ),
    figures = list(
      fig_03_csem = list(
        plot     = fig_03_csem,
        alt_text = "Conditional Standard Error of Measurement (CSEM) plotted across the theta (latent trait) range."
      )
    )
  )
}

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