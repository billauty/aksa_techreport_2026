# Load the primary data files and return them as a named list.
# Parses the SAS answer key file into a list of named vectors per test.
#
# Arguments:
#   data_dir  - path to the top-level data directory (default: "data")
#
# Returns a list with elements: calib, responses, keys, cut_scores.
load_all_data <- function(data_dir = "data") {
  
  # 1. Ingest and transform the SAS keys into a named list of vectors
  # Groups by Subject and grade (e.g., "MA_03") 
  raw_keys <- haven::read_sas(file.path(data_dir, "raw/alt_response_key.sas7bdat"))
  
  keys_list <- split(raw_keys, paste(raw_keys$Subject, raw_keys$grade, sep = "_")) |> 
    lapply(function(df) {
      # Extract the 'Key' column and name the elements using the 'Task' column
      setNames(as.character(df$Key), as.character(df$Task))
    })
  
  list(
    calib      = readRDS(file.path(data_dir, "processed/calib25_final.rds")),
    responses  = readRDS(file.path(data_dir, "raw/responses.rds")),
    keys       = keys_list,
    cut_scores = readRDS(file.path(data_dir, "raw/AKSA_NAPD_Cut_Scores.rds"))
  )
}

# Score raw item responses against an answer key.
#
# Arguments:
#   resp_df     - data frame with columns SSID, complete, and item columns
#                 (e.g., A1, A2, …).
#   key_vector  - named character vector of correct answers (names = item names).
#
# Returns a data frame with SSID, complete, and numeric 0/1 scored item columns
# for every item in key_vector that is present in resp_df.
# score_test_responses <- function(resp_df, key_vector) {
#   present_items <- names(key_vector)[names(key_vector) %in% names(resp_df)]
# 
#   for (item in present_items) {
#     resp_df[[item]] <- as.integer(resp_df[[item]] == key_vector[[item]])
#   }
# 
#   resp_df[, c("SSID", "complete", present_items)]
# }
# Define a safe scorer (copy/paste and run)
score_test_responses <- function(resp_df, key_vector) {
  library(dplyr)
  library(tibble)
  
  # Candidate item columns (exclude SSID and complete)
  item_cols <- setdiff(names(resp_df), c("SSID", "SSID.", "complete", "Complete"))
  if (length(item_cols) == 0) stop("No item columns found in resp_df.")
  
  # If key_vector is unnamed, name it using item order
  if (is.null(names(key_vector)) || any(names(key_vector) == "")) {
    if (length(key_vector) != length(item_cols)) {
      stop("Unnamed key_vector length does not match number of item columns.")
    }
    names(key_vector) <- item_cols
  } else {
    # Make sure keys align with available item cols, keep order of item_cols
    if (!all(names(key_vector) %in% item_cols)) {
      warning("Some key names are not present in resp_df; will use intersection.")
    }
    key_vector <- key_vector[item_cols[item_cols %in% names(key_vector)]]
    names(key_vector) <- intersect(item_cols, names(key_vector))
  }
  
  # Lowercase everything for matching; preserve NAs
  resp_items <- resp_df %>%
    mutate(across(all_of(item_cols), ~ ifelse(is.na(.), NA_character_, tolower(as.character(.)))))
  
  # Score: 1 if match, 0 if not, NA preserved
  for (itm in names(key_vector)) {
    correct <- tolower(as.character(key_vector[[itm]]))
    if (! itm %in% names(resp_items)) next
    resp_items[[itm]] <- ifelse(is.na(resp_items[[itm]]), NA_real_,
                                ifelse(resp_items[[itm]] == correct, 1L, 0L))
  }
  
  # Return SSID, complete, plus scored item columns (in original item order)
  scored <- resp_df %>%
    select(SSID, complete) %>%
    bind_cols(resp_items %>% select(all_of(names(key_vector))))
  
  as_tibble(scored)
}

# # Run it for the test you're working on:
# raw <- targets::tar_read(raw_resp)
# keys <- targets::tar_read(item_keys)
# 
# scored_test <- score_test_responses_safe(raw, keys)
# 
# # Quick check
# str(scored_test)
# head(scored_test, 3)
# 
# # If ok, generate Table 2 interactively:
# table2 <- make_table_02_item_stats(scored_test)
# table2  # this will show the flextable in the Viewer

# Load demographic data (SSID, SEX, IEP, LEP, etc.) used for DIF and
# subgroup reliability analyses.
#
# Arguments:
#   data_dir  - path to the top-level data directory (default: "data")
#
# Returns a data.frame.  If the file does not yet exist, returns an empty
# data.frame with the expected column structure so the pipeline can still
# be assembled.
load_demo_data <- function(data_dir = "data") {
  path <- file.path(data_dir, "raw/demographics.rds")
  if (file.exists(path)) {
    readRDS(path)
  } else {
    data.frame(SSID = character(0), SEX = character(0),
               IEP  = character(0), LEP = character(0),
               stringsAsFactors = FALSE)
  }
}

# Load Learner Characteristics Inventory (LCI) data used for Figures 7-10.
#
# Arguments:
#   data_dir  - path to the top-level data directory (default: "data")
#
# Returns a data.frame.  If the file does not yet exist, returns an empty
# data.frame with just an SSID column so the pipeline can still be assembled.
load_lci_data <- function(data_dir = "data") {
  rds_path <- file.path(data_dir, "raw/Learner_Characteristics_Inventory_24_25.rds")
  if (file.exists(rds_path)) return(readRDS(rds_path))
  data.frame(SSID = character(0), stringsAsFactors = FALSE)
}

# Load anchor-item drift data used for Figure 11.
# Expected format: a named list where each element is a data.frame with
# columns item_id (character), robust_z (numeric), and unstable (logical).
# The list is keyed by test_id (e.g., "RD_04").
#
# Arguments:
#   data_dir  - path to the top-level data directory (default: "data")
#
# Returns a named list.  Returns an empty list if the file does not exist.
load_drift_data <- function(data_dir = "data") {
  path <- file.path(data_dir, "raw/anchor_drift.rds")
  if (file.exists(path)) readRDS(path) else list()
}

# Compute KR-20 (Kuder-Richardson Formula 20) reliability as a scalar value.
#
# Arguments:
#   scored_data  - data.frame/tibble of item responses (may include SSID /
#                  complete columns; only numeric columns are used).
#
# Returns a single numeric value (NA_real_ when fewer than 2 items).
compute_kr20 <- function(scored_data) {
  scored_data <- scored_data[, sapply(scored_data, is.numeric), drop = FALSE]
  k <- ncol(scored_data)
  if (k < 2) return(NA_real_)
  total <- rowSums(scored_data, na.rm = TRUE)
  p     <- colMeans(scored_data, na.rm = TRUE)
  q     <- 1 - p
  (k / (k - 1)) * (1 - sum(p * q) / var(total, na.rm = TRUE))
}

# Extract the single-row cut-score record for a given test_id from the
# AKSA_NAPD_Cut_Scores data.
#
# Arguments:
#   cut_scores  - data.frame loaded from AKSA_NAPD_Cut_Scores.rds.
#   test_id     - character; the test identifier (e.g., "RD_04").
#
# Returns a single-row data.frame.  If no matching row is found, returns
# a minimal placeholder row so the downstream table functions still work.
get_cut_score_row <- function(cut_scores, test_id) {
  if (is.null(cut_scores) || nrow(cut_scores) == 0) {
    return(data.frame(test_id = test_id, cut_proficient = NA_real_,
                      stringsAsFactors = FALSE))
  }
  id_col <- if ("test_id" %in% names(cut_scores)) "test_id" else names(cut_scores)[1]
  row <- cut_scores[cut_scores[[id_col]] == test_id, , drop = FALSE]
  if (nrow(row) == 0) {
    warning(paste("No cut score found for test_id:", test_id))
    return(data.frame(test_id = test_id, cut_proficient = NA_real_,
                      stringsAsFactors = FALSE))
  }
  row[1, , drop = FALSE]
}

# Extract the single-number proficiency cut score from a cut-score row.
# Used when a scalar is needed (e.g., for Table 15 and Table 16).
#
# Arguments:
#   cut_scores_row  - single-row data.frame from get_cut_score_row().
#
# Returns a numeric scalar (NA_real_ if the column is absent).
extract_proficiency_cut <- function(cut_scores_row) {
  candidates <- c("cut_proficient", "Proficient_min", "proficient_cut",
                  "cut_score", "CutScore")
  col <- intersect(candidates, names(cut_scores_row))
  if (length(col) == 0) return(NA_real_)
  as.numeric(cut_scores_row[[col[1]]][1])
}
