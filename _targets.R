library(targets)
library(tarchetypes)
library(tidyverse)
library(mirt)
library(CTT)
library(flextable)
library(officer)
library(e1071)
library(difR)

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

  # ── Dimensionality / Q3 ─────────────────────────────────────────────────────
  tar_target(table_10_q3_residuals,
             make_table_10_q3_residuals(mirt_model),
             pattern = map(mirt_model)),

  # ── Subgroup reliability (IRT) ───────────────────────────────────────────────
  tar_target(table_11_irt_reliability,
             make_table_11_subgroup_reliability(scored_resp, demo_data, test_id = test_ids),
             pattern = map(scored_resp, test_ids)),

  # ── DIF — Lord's Delta (3 demographic groups) ───────────────────────────────
  # Each returns list(table = <flextable>, data = <data.frame>)
  tar_target(dif_gender,
             make_table_12_dif_lord(scored_resp, demo_data, group_col = "SEX"),
             pattern = map(scored_resp),
             iteration = "list"),
  tar_target(dif_iep,
             make_table_13_dif_lord(scored_resp, demo_data, group_col = "IEP"),
             pattern = map(scored_resp),
             iteration = "list"),
  tar_target(dif_lep,
             make_table_14_dif_lord(scored_resp, demo_data, group_col = "LEP"),
             pattern = map(scored_resp),
             iteration = "list"),

  # Extract flextable component from each DIF result
  tar_target(table_12_dif_gender, dif_gender$table, pattern = map(dif_gender)),
  tar_target(table_13_dif_iep,    dif_iep$table,    pattern = map(dif_iep)),
  tar_target(table_14_dif_lep,    dif_lep$table,    pattern = map(dif_lep)),

  # ── Classification accuracy / consistency ───────────────────────────────────
  tar_target(table_15_class_accuracy,
             make_table_15_class_accuracy(kr20, cut_score_val),
             pattern = map(kr20, cut_score_val)),
  tar_target(table_16_decision_consistency,
             make_table_16_decision_consistency(kr20, cut_score_val),
             pattern = map(kr20, cut_score_val)),
  tar_target(table_17_napd_accuracy,
             make_table_17_napd_accuracy(cut_scores_row, kr20),
             pattern = map(cut_scores_row, kr20)),

  # ── Figures ─────────────────────────────────────────────────────────────────
  tar_target(fig_01_item_fit,
             make_figure_01_item_fit(mirt_model),
             pattern = map(mirt_model)),
  tar_target(fig_02_wright_map,
             make_figure_02_wright_map(mirt_model, scored_resp, napd_cuts),
             pattern = map(mirt_model, scored_resp, napd_cuts)),
  tar_target(fig_03_csem,
             make_figure_03_csem(mirt_model),
             pattern = map(mirt_model)),

  # DIF figures — use the $data element from each DIF target
  tar_target(fig_04_dif_gender,
             make_figure_04_dif_plot(dif_gender$data, group_name = "Gender"),
             pattern = map(dif_gender)),
  tar_target(fig_05_dif_iep,
             make_figure_05_dif_plot(dif_iep$data, group_name = "IEP"),
             pattern = map(dif_iep)),
  tar_target(fig_06_dif_lep,
             make_figure_06_dif_plot(dif_lep$data, group_name = "LEP"),
             pattern = map(dif_lep)),

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

  # ── Per-test report content ─────────────────────────────────────────────────
  tar_target(report_content, assemble_report_content(
    test_id = test_ids,
    table_01_ctt_summary,
    table_02_item_stats,
    table_03_reliability,
    table_04_score_freq,
    table_05_distractor,
    table_06_irt_summary,
    table_07_raw_to_theta,
    table_08_irt_params,
    table_09_item_fit,
    table_10_q3_residuals,
    table_11_irt_reliability,
    table_12_dif_gender,    # Fixed name
    table_13_dif_iep,
    table_14_dif_lep,
    table_15_class_accuracy,       # Fixed name
    table_16_decision_consistency, # Fixed name
    table_17_napd_accuracy,        # Fixed name
    fig_01_item_fit,
    fig_02_wright_map,
    fig_03_csem,
    fig_04_dif_gender,      # Fixed name
    fig_05_dif_iep,
    fig_06_dif_lep,
    figs_07_10_lci,         # Fixed name
    fig_11_anchor_drift
  ),
  pattern = map(
    test_ids,
    table_01_ctt_summary, table_02_item_stats, table_03_reliability, table_04_score_freq, table_05_distractor,
    table_06_irt_summary, table_07_raw_to_theta, table_08_irt_params, table_09_item_fit, table_10_q3_residuals,
    table_11_irt_reliability, table_12_dif_gender, table_13_dif_iep, table_14_dif_lep, table_15_class_accuracy,
    table_16_decision_consistency, table_17_napd_accuracy, fig_01_item_fit, fig_02_wright_map, fig_03_csem,
    fig_04_dif_gender, fig_05_dif_iep, fig_06_dif_lep, figs_07_10_lci, fig_11_anchor_drift
  ),
  iteration = "list"),

  # ── Final report assembly ───────────────────────────────────────────────────
  tar_target(intro_docx,   make_empty_intro_docx(), format = "file"),
  tar_target(final_report, create_accessible_yearbook(report_content, intro_docx), format = "file")
)
