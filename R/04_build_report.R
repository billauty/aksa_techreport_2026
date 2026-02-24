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
# Arguments (one per table/figure target, all per-test):
#   table_01 .. table_17  - flextable objects (Tables 1-17; Tables 12-14 are
#                           the $table element from make_table_12_dif_lord etc.)
#   fig_01  .. fig_06     - ggplot objects (Figures 1-6)
#   figs_07_10            - named list of ggplot objects (Figures 7-10)
#   fig_11                - ggplot or NULL (Figure 11; NULL for tests without
#                           anchor items)
#
# Returns a list with $tables (named list of flextable objects) and
# $figures (named list of lists, each with $plot and $alt_text).
assemble_report_content <- function(table_01, table_02, table_03, table_04, table_05,
                                    table_06, table_07, table_08, table_09, table_10,
                                    table_11, table_12, table_13, table_14,
                                    table_15, table_16, table_17,
                                    fig_01, fig_02, fig_03,
                                    fig_04, fig_05, fig_06,
                                    figs_07_10,
                                    fig_11) {
  # Build the figures list, skipping any NULL entries (e.g. Figure 11 for
  # Writing tests that have no anchor items).
  named_figs <- list(
    fig_01 = list(plot = fig_01,
                  alt_text = "Item Infit and Outfit mean-square statistics for each item."),
    fig_02 = list(plot = fig_02,
                  alt_text = "Wright Map showing student ability and item difficulty on the same logit scale."),
    fig_03 = list(plot = fig_03,
                  alt_text = "Conditional Standard Error of Measurement (CSEM) plotted across the theta range."),
    fig_04 = list(plot = fig_04,
                  alt_text = "DIF magnitude and direction by Gender (Lord's Delta)."),
    fig_05 = list(plot = fig_05,
                  alt_text = "DIF magnitude and direction by IEP status (Lord's Delta)."),
    fig_06 = list(plot = fig_06,
                  alt_text = "DIF magnitude and direction by LEP status (Lord's Delta).")
  )

  # Add Figures 7-10 from the list (each element is a ggplot or NULL)
  lci_labels <- c(
    "Score distribution by Expressive Communication category.",
    "Score distribution by Receptive Language category.",
    "Score distribution by Reading category.",
    "Score distribution by Mathematics category."
  )
  if (is.list(figs_07_10) && length(figs_07_10) > 0) {
    for (i in seq_along(figs_07_10)) {
      key <- paste0("fig_", sprintf("%02d", 6L + i))
      label <- if (i <= length(lci_labels)) lci_labels[[i]] else
                  paste0("Learner Characteristics figure ", i, ".")
      named_figs[[key]] <- list(plot = figs_07_10[[i]], alt_text = label)
    }
  }

  # Figure 11 — only include when not NULL
  if (!is.null(fig_11)) {
    named_figs[["fig_11"]] <- list(
      plot     = fig_11,
      alt_text = "Anchor item drift: robust-Z statistics with flagged items highlighted."
    )
  }

  # Remove any entries whose $plot is NULL
  named_figs <- Filter(function(x) !is.null(x$plot), named_figs)

  list(
    tables = list(
      table_01 = table_01,
      table_02 = table_02,
      table_03 = table_03,
      table_04 = table_04,
      table_05 = table_05,
      table_06 = table_06,
      table_07 = table_07,
      table_08 = table_08,
      table_09 = table_09,
      table_10 = table_10,
      table_11 = table_11,
      table_12 = table_12,
      table_13 = table_13,
      table_14 = table_14,
      table_15 = table_15,
      table_16 = table_16,
      table_17 = table_17
    ),
    figures = named_figs
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
      # Create external image object with alt text
      ext_img <- officer::external_img(
        src = tmp_fig,
        width = 6,
        height = 4,
        alt = fig$alt_text
      )

      # Wrap in an fpar and add to document
      doc <- officer::body_add_fpar(doc, officer::fpar(ext_img))
    }
  }
  
  out_path <- "reports/Final_Technical_Report_2026.docx"
  print(doc, target = out_path)
  return(out_path)
}