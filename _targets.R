library(targets)
library(tarchetypes)
library(tidyverse)
library(flextable)
library(officer)
library(ggplot2)

# Source all R scripts in the R/ directory
lapply(list.files("R", full.names = TRUE), source)

list(
  # Data ingestion
  tar_target(raw_data, ingest_data()),
  tar_target(scored_data, score_responses(raw_data)),

  # Psychometric models
  tar_target(model_results, fit_models(scored_data)),

  # Content generation (tables and figures)
  tar_target(test_content_objects, build_test_content(model_results)),

  # Quarto rendering for the intro
  tar_quarto(intro_docx, path = "quarto/yearbook_intro.qmd"),

  # officer assembly of the final Word document
  tar_target(
    final_report,
    create_accessible_yearbook(test_content_objects, intro_docx)
  )
)
