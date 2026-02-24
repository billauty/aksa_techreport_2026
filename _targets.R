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
  # Data ingestion
  tar_target(data_list, load_all_data()),
  tar_target(test_ids,  names(data_list$responses)),

  # Per-test targets (dynamic branching over all test IDs)
  tar_target(raw_resp,   data_list$responses[[test_ids]], pattern = map(test_ids)),
  tar_target(item_keys,  data_list$keys[[test_ids]],     pattern = map(test_ids)),
  tar_target(mirt_model, data_list$calib[[test_ids]],    pattern = map(test_ids)),
  tar_target(grade, as.integer(sub(".*_(\\d+)$", "\\1", test_ids)), pattern = map(test_ids)),
  tar_target(scored_resp, score_test_responses(raw_resp, item_keys),
             pattern = map(raw_resp, item_keys)),

  # CTT tables
  tar_target(table_01_ctt_summary, make_table_01_score_summary(scored_resp),
             pattern = map(scored_resp)),
  tar_target(table_02_item_stats,  make_table_02_item_stats(scored_resp),
             pattern = map(scored_resp)),

  # IRT tables and figures
  tar_target(table_06_irt_summary,
             make_table_06_irt_summary(mirt_model, grade = grade, n_persons = nrow(scored_resp)),
             pattern = map(mirt_model, scored_resp, grade)),
  tar_target(table_08_irt_params,  make_table_08_irt_params(mirt_model),
             pattern = map(mirt_model)),
  tar_target(fig_03_csem,          make_figure_03_csem(mirt_model),
             pattern = map(mirt_model)),

  # Per-test report content (one list per branch)
  tar_target(report_content, assemble_report_content(
    table_01_ctt_summary,
    table_02_item_stats,
    table_06_irt_summary,
    table_08_irt_params,
    fig_03_csem
  ), pattern = map(table_01_ctt_summary, table_02_item_stats,
                   table_06_irt_summary, table_08_irt_params, fig_03_csem),
     iteration = "list"),

  # Final report assembly — report_content aggregates all 26 branch results
  tar_target(intro_docx,   make_empty_intro_docx(), format = "file"),
  tar_target(final_report, create_accessible_yearbook(report_content, intro_docx), format = "file")
)
