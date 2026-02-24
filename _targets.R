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
  tar_target(data_list,  load_all_data()),
  tar_target(test_id,    "MA_07"),
  tar_target(raw_resp,   data_list$responses[[test_id]]),
  tar_target(item_keys,  data_list$keys[[test_id]]),
  tar_target(mirt_model, data_list$calib[[test_id]]),
  tar_target(scored_resp, score_test_responses(raw_resp, item_keys)),

  # CTT tables
  tar_target(table_01_ctt_summary, make_table_01_score_summary(scored_resp)),
  tar_target(table_02_item_stats,  make_table_02_item_stats(scored_resp)),

  # IRT tables and figures
  tar_target(table_06_irt_summary, make_table_06_irt_summary(mirt_model, grade = 7, n_persons = nrow(scored_resp))),
  tar_target(table_08_irt_params,  make_table_08_irt_params(mirt_model)),
  tar_target(fig_03_csem,          make_figure_03_csem(mirt_model)),

  # Report assembly
  tar_target(intro_docx,    make_empty_intro_docx(), format = "file"),
  tar_target(report_content, assemble_report_content(
    table_01_ctt_summary,
    table_02_item_stats,
    table_06_irt_summary,
    table_08_irt_params,
    fig_03_csem
  )),
  tar_target(final_report,  create_accessible_yearbook(report_content, intro_docx), format = "file")
)
