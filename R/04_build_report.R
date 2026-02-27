library(officer)
library(flextable)
library(ggplot2)

# Helper to add a blank line
add_blank_line <- function(doc) {
  officer::body_add_par(doc, "", style = "Normal")
}

# Helper to cleanly insert a single figure from the content list
add_report_figure <- function(doc, fig_obj) {
  if (is.null(fig_obj)) return(doc) # Skip safely if NULL (like missing drift plots)

  tmp_fig <- tempfile(fileext = ".png")
  suppressMessages(
    ggplot2::ggsave(tmp_fig, plot = fig_obj$plot, width = 6.5, height = 4.5, dpi = 300, bg = "white")
  )

  ext_img <- officer::external_img(src = tmp_fig, width = 6.5, height = 4.5, alt = fig_obj$alt_text)
  doc <- officer::body_add_fpar(doc, officer::fpar(ext_img))
  return(doc)
}

# Helper to apply density styling and pagination rules to tables
apply_table_style <- function(ft, is_table_5 = FALSE) {
  if (is.null(ft)) return(NULL)
  if (!inherits(ft, "flextable")) return(ft)
  
  # Ensure the table doesn't split awkwardly across pages 
  ft <- flextable::set_table_properties(ft, layout = "autofit")
  ft <- flextable::paginate(ft, init = TRUE, hdr_ftr = TRUE)
  
  if (!is_table_5) {
    # Reduce row height/padding on all tables EXCEPT Table 5
    ft <- flextable::padding(ft, padding = 1, part = "all")
    ft <- flextable::line_spacing(ft, space = 1, part = "all")
  }
  
  return(ft)
}

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
assemble_report_content <- function(test_id, table_01, table_02, table_03, table_04, table_05,
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
                  alt_text = "DIF magnitude and direction by Gender (Mantel-Haenszel Delta)."),
    fig_05 = list(plot = fig_05,
                  alt_text = "DIF magnitude and direction by Economic Disadvantage (Mantel-Haenszel Delta)."),
    fig_06 = list(plot = fig_06,
                  alt_text = "DIF magnitude and direction by Ethnicity (Mantel-Haenszel Delta).")
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
    test_id = test_id,
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

  if (!is.null(test_content_objects[["tables"]])) {
    test_content_objects <- list(test_content_objects)
  }

  for (content in test_content_objects) {
    
    # Pre-process styling for all tables in this test's content
    for (tbl_name in names(content$tables)) {
      is_t5 <- (tbl_name == "table_05")
      content$tables[[tbl_name]] <- apply_table_style(content$tables[[tbl_name]], is_t5)
    }

    # =========================================================================
    # 1. OVERALL TEST SUMMARY & DEMOGRAPHICS
    # =========================================================================
    doc <- officer::body_add_par(doc, paste("Test Report:", content$test_id), style = "heading 1")
    
    doc <- flextable::body_add_flextable(doc, content$tables$table_01)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_02)
    doc <- officer::body_add_break(doc)
    
    # =========================================================================
    # 2. SCALE SCORE DESCRIPTIVE STATISTICS
    # =========================================================================
    doc <- officer::body_add_par(doc, "Scale Score Descriptives", style = "heading 2")
    
    doc <- flextable::body_add_flextable(doc, content$tables$table_03)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_04)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_15)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_16)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_17)
    
    # Explicit page break before Section 3 / Table 5
    doc <- officer::body_add_break(doc)
    
    # =========================================================================
    # 3. RELIABILITY & STANDARD ERROR
    # =========================================================================
    doc <- officer::body_add_par(doc, "Reliability and Standard Error", style = "heading 2")
    
    doc <- flextable::body_add_flextable(doc, content$tables$table_05)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_06)
    doc <- add_report_figure(doc, content$figures$fig_03)
    doc <- officer::body_add_break(doc)
    
    # =========================================================================
    # 4. ITEM PARAMETERS & TEST ALIGNMENT (IRT/CTT)
    # =========================================================================
    doc <- officer::body_add_par(doc, "Item Statistics and Model Fit", style = "heading 2")
    
    doc <- add_report_figure(doc, content$figures$fig_02)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_07)
    doc <- add_blank_line(doc)
    doc <- add_report_figure(doc, content$figures$fig_01)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_08)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_09)
    doc <- officer::body_add_break(doc)
    
    # =========================================================================
    # 5. DIFFERENTIAL ITEM FUNCTIONING (DIF)
    # =========================================================================
    doc <- officer::body_add_par(doc, "Differential Item Functioning (DIF)", style = "heading 2")
    
    doc <- officer::body_add_par(doc, "DIF by Gender", style = "heading 3")
    doc <- flextable::body_add_flextable(doc, content$tables$table_12)
    doc <- add_report_figure(doc, content$figures$fig_04)
    doc <- officer::body_add_break(doc)
    
    doc <- officer::body_add_par(doc, "DIF by Economic Disadvantage", style = "heading 3")
    doc <- flextable::body_add_flextable(doc, content$tables$table_13)
    doc <- add_report_figure(doc, content$figures$fig_05)
    doc <- officer::body_add_break(doc)
    
    doc <- officer::body_add_par(doc, "DIF by Ethnicity", style = "heading 3")
    doc <- flextable::body_add_flextable(doc, content$tables$table_14)
    doc <- add_report_figure(doc, content$figures$fig_06)
    doc <- officer::body_add_break(doc)

    # =========================================================================
    # 6. EQUATING & ANCHOR ITEMS
    # =========================================================================
    doc <- officer::body_add_par(doc, "Equating and Anchor Drift", style = "heading 2")
    
    doc <- flextable::body_add_flextable(doc, content$tables$table_10)
    doc <- add_blank_line(doc)
    doc <- flextable::body_add_flextable(doc, content$tables$table_11)
    doc <- add_report_figure(doc, content$figures$fig_11)
    doc <- officer::body_add_break(doc)
    
    # =========================================================================
    # 7. LEARNER CHARACTERISTICS (LCI) DISTRIBUTIONS
    # =========================================================================
    doc <- officer::body_add_par(doc, "Learner Characteristics Inventory (LCI)", style = "heading 2")
    
    doc <- add_report_figure(doc, content$figures$fig_07)
    doc <- add_report_figure(doc, content$figures$fig_08)
    doc <- officer::body_add_break(doc)
    doc <- add_report_figure(doc, content$figures$fig_09)
    doc <- add_report_figure(doc, content$figures$fig_10)
    
    # --- FOOTER LOGIC --------------------------------------------------------
    footer_block <- officer::block_list(
      officer::fpar(officer::ftext(paste("Test:", content$test_id)))
    )
    
    doc <- officer::body_end_block_section(
      doc, 
      value = officer::block_section(
        officer::prop_section(
          type = "nextPage",
          footer_default = footer_block
        )
      )
    )
  }
  
  out_path <- "reports/Final_Technical_Report_2026.docx"
  print(doc, target = out_path)
  return(out_path)
}