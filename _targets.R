library(targets)
library(tarchetypes)
library(tidyverse)
library(mirt)
library(CTT)
library(flextable)
library(officer)
library(e1071)

# Source all R scripts in the R/ directory
lapply(list.files("R", full.names = TRUE), source)

list(
  # ── Data ingestion ──────────────────────────────────────────────────────────
  tar_target(data_list,      load_all_data()),
  tar_target(demo_data,      load_demo_data()),
  tar_target(lci_data,       load_lci_data()),
  tar_target(all_drift_data, load_drift_data()),
  tar_target(test_ids,       names(data_list$responses)),

  # ── Per-test targets (dynamic branching over all test IDs) ──────────────────
  tar_target(raw_resp,   data_list$responses[[test_ids]], pattern = map(test_ids)),
  tar_target(item_keys,  data_list$keys[[test_ids]],     pattern = map(test_ids)),
  tar_target(mirt_model, data_list$calib[[test_ids]],    pattern = map(test_ids)),
  tar_target(grade, as.integer(sub(".*_(\\d+)$", "\\1", test_ids)), pattern = map(test_ids)),
  tar_target(scored_resp, score_test_responses(raw_resp, item_keys),
             pattern = map(raw_resp, item_keys)),

  # KR-20 reliability (scalar, used by Tables 15-16) and cut-score row
  tar_target(kr20,
             compute_kr20(scored_resp),
             pattern = map(scored_resp)),
  tar_target(cut_scores_row,
             get_cut_score_row(data_list$cut_scores, test_ids),
             pattern = map(test_ids),
             iteration = "list"),
  tar_target(cut_score_val,
             extract_proficiency_cut(cut_scores_row),
             pattern = map(cut_scores_row)),
  tar_target(napd_cuts,
             extract_napd_cuts(cut_scores_row),
             pattern = map(cut_scores_row)),

  # ── CTT tables ──────────────────────────────────────────────────────────────
  tar_target(table_01_ctt_summary,
             make_table_01_score_summary(scored_resp),
             pattern = map(scored_resp)),
  tar_target(table_02_item_stats,
             make_table_02_item_stats(scored_resp),
             pattern = map(scored_resp)),
  tar_target(table_03_reliability,
             make_table_03_reliability(scored_resp),
             pattern = map(scored_resp)),
  tar_target(table_04_score_freq,
             make_table_04_score_frequencies(scored_resp),
             pattern = map(scored_resp)),
  tar_target(table_05_distractor,
             make_table_05_distractor_analysis(
               raw_resp[, intersect(names(raw_resp), names(item_keys)), drop = FALSE],
               unlist(item_keys)),
             pattern = map(raw_resp, item_keys)),

  # ── IRT tables and figures ──────────────────────────────────────────────────
  tar_target(table_06_irt_summary,
             make_table_06_irt_summary(mirt_model, grade = grade, n_persons = nrow(scored_resp)),
             pattern = map(mirt_model, scored_resp, grade)),
  tar_target(table_07_raw_to_theta,
             make_table_07_raw_to_theta(mirt_model, test_id = test_ids),
             pattern = map(mirt_model, test_ids)),
  tar_target(table_08_irt_params,
             make_table_08_irt_params(mirt_model),
             pattern = map(mirt_model)),
  tar_target(table_09_item_fit,
             make_table_09_item_fit(mirt_model),
             pattern = map(mirt_model)),
  
  # ── Original Figures ─────────────────────────────────────────────────────────
  tar_target(fig_01_item_fit,
             make_figure_01_item_fit(mirt_model),
             pattern = map(mirt_model)),
  tar_target(fig_02_wright_map,
             make_figure_02_wright_map(mirt_model, scored_resp, napd_cuts),
             pattern = map(mirt_model, scored_resp, napd_cuts)),
  tar_target(fig_03_csem,
             make_figure_03_csem(mirt_model),
             pattern = map(mirt_model)),
  
  # ── Dimensionality and Subgroup Tables ──────────────────────────────────────
  tar_target(table_10_q3_residuals,
             make_table_10_q3_residuals(mirt_model),
             pattern = map(mirt_model)),
  tar_target(table_11_subgroup_reliability,
             make_table_11_subgroup_reliability(scored_resp, demo_data, test_id = test_ids),
             pattern = map(scored_resp, test_ids)),
  
  # ── DIF — Mantel-Haenszel (3 demographic groups) ──────────────────────────────
  # Each returns list(table = <flextable>, data = <data.frame>)
  tar_target(dif_gender,
             make_table_12_dif_lord(scored_resp, demo_data, group_col = "SEX", table_num = 12L),
             pattern = map(scored_resp),
             iteration = "list"),
  tar_target(dif_econ,
             make_table_13_dif_lord(scored_resp, demo_data, group_col = "Disadvantaged"),
             pattern = map(scored_resp),
             iteration = "list"),
  tar_target(dif_ethnic,
             make_table_14_dif_lord(scored_resp, demo_data, group_col = "Ethnic"),
             pattern = map(scored_resp),
             iteration = "list"),
  
  # Extract flextable component from each DIF result
  tar_target(table_12_dif_gender, dif_gender$table, pattern = map(dif_gender)),
  tar_target(table_13_dif_econ,   dif_econ$table,   pattern = map(dif_econ)),
  tar_target(table_14_dif_ethnic, dif_ethnic$table, pattern = map(dif_ethnic)),
  
  # Generate Figure 4, 5, 6 (DIF Plots) using the data component
  tar_target(fig_04_dif_gender,
             make_figure_04_dif_plot(dif_gender$data, group_name = "Gender"),
             pattern = map(dif_gender)),
  tar_target(fig_05_dif_econ,
             make_figure_05_dif_plot(dif_econ$data, group_name = "Economic Disadvantage"),
             pattern = map(dif_econ)),
  tar_target(fig_06_dif_ethnic,
             make_figure_06_dif_plot(dif_ethnic$data, group_name = "Ethnicity"),
             pattern = map(dif_ethnic)),
  
  # ── Classification accuracy / consistency ───────────────────────────────────
  tar_target(table_15_class_accuracy,
             make_table_15_class_accuracy(mirt_model, scored_resp, napd_cuts),
             pattern = map(mirt_model, scored_resp, napd_cuts)),
  tar_target(table_16_decision_consistency,
             make_table_16_decision_consistency(mirt_model, scored_resp, napd_cuts),
             pattern = map(mirt_model, scored_resp, napd_cuts)),
  tar_target(table_17_napd_accuracy,
             make_table_17_napd_accuracy(mirt_model, scored_resp, napd_cuts),
             pattern = map(mirt_model, scored_resp, napd_cuts)),
  
  # Learner Characteristics figures (one list per test; one ggplot per trait)
  tar_target(figs_07_10_lci, {
    traits <- intersect(
      c("Expressive Communication", "Receptive Language", "Reading", "Mathematics"),
      names(lci_data)
    )
    lapply(traits, function(t)
      make_figures_07_10_learner_characteristics(scored_resp, lci_data, t))
  }, pattern = map(scored_resp), iteration = "list"),
  
  # Anchor-item drift figure (NULL for tests without anchor items)
  tar_target(fig_11_anchor_drift, {
    drift_df <- all_drift_data[[test_ids]]
    make_figure_11_anchor_drift(drift_df, test_id = test_ids)
  }, pattern = map(test_ids), iteration = "list"),
  
  # ── Report Generation ──────────────────────────────────────────────────────
  # Combine all the outputs for one test into a single list to pass to the report
  tar_target(
    report_content,
    list(
      test_id             = test_ids,
      cut_score_val       = cut_score_val,
      napd_cuts           = napd_cuts,
      kr20                = kr20,
      table_01            = table_01_ctt_summary,
      table_02            = table_02_item_stats,
      table_03            = table_03_reliability,
      table_04            = table_04_score_freq,
      table_05            = table_05_distractor,
      table_06            = table_06_irt_summary,
      table_07            = table_07_raw_to_theta,
      table_08            = table_08_irt_params,
      table_09            = table_09_item_fit,
      table_10            = table_10_q3_residuals,
      table_11            = table_11_subgroup_reliability,
      table_12            = table_12_dif_gender,     # IMPORTANT: Must be the extracted table!
      table_13            = table_13_dif_econ,       # IMPORTANT: Must be the extracted table!
      table_14            = table_14_dif_ethnic,     # IMPORTANT: Must be the extracted table!
      table_15            = table_15_class_accuracy,
      table_16            = table_16_decision_consistency,
      table_17            = table_17_napd_accuracy,
      figure_01           = fig_01_item_fit,
      figure_02           = fig_02_wright_map,
      figure_03           = fig_03_csem,
      figure_04           = fig_04_dif_gender,
      figure_05           = fig_05_dif_econ,
      figure_06           = fig_06_dif_ethnic,
      figs_07_10          = figs_07_10_lci,
      figure_11           = fig_11_anchor_drift
    ),
    pattern = map(
      test_ids, cut_score_val, napd_cuts, kr20,
      table_01_ctt_summary, table_02_item_stats, table_03_reliability, table_04_score_freq, table_05_distractor,
      table_06_irt_summary, table_07_raw_to_theta, table_08_irt_params, table_09_item_fit, table_10_q3_residuals,
      table_11_subgroup_reliability, 
      table_12_dif_gender, table_13_dif_econ, table_14_dif_ethnic,  # Extracted tables
      table_15_class_accuracy, table_16_decision_consistency, table_17_napd_accuracy,
      fig_01_item_fit, fig_02_wright_map, fig_03_csem, 
      fig_04_dif_gender, fig_05_dif_econ, fig_06_dif_ethnic,
      figs_07_10_lci, fig_11_anchor_drift
    ),
    iteration = "list"
  ),
  
  # ── Render word document ────────────────────────────────────────────────────
  tar_target(
    doc_file,
    {
      if (!dir.exists("reports")) dir.create("reports")
      
      # Assemble the report content objects for all 26 mapped tests
      all_test_contents <- lapply(report_content, function(rc) {
        assemble_report_content(
          test_id    = rc$test_id,
          table_01   = rc$table_01,
          table_02   = rc$table_02,
          table_03   = rc$table_03,
          table_04   = rc$table_04,
          table_05   = rc$table_05,
          table_06   = rc$table_06,
          table_07   = rc$table_07,
          table_08   = rc$table_08,
          table_09   = rc$table_09,
          table_10   = rc$table_10,
          table_11   = rc$table_11,
          table_12   = rc$table_12,
          table_13   = rc$table_13,
          table_14   = rc$table_14,
          table_15   = rc$table_15,
          table_16   = rc$table_16,
          table_17   = rc$table_17,
          fig_01     = rc$figure_01,
          fig_02     = rc$figure_02,
          fig_03     = rc$figure_03,
          fig_04     = rc$figure_04,
          fig_05     = rc$figure_05,
          fig_06     = rc$figure_06,
          figs_07_10 = rc$figs_07_10,
          fig_11     = rc$figure_11
        )
      })
      
      # Create placeholder intro if needed
      intro_path <- "reports/intro_template.docx"
      if (!file.exists(intro_path)) {
        make_empty_intro_docx()
      }
      
      # Build the massive combined Word Document with all 26 tests!
      out_path <- create_accessible_yearbook(
        test_content_objects = all_test_contents,
        intro_docx           = intro_path
      )
      
      out_path
    },
    format = "file" # Compiles all 26 tests into 1 document!
  )
)