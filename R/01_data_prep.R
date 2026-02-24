# Load the four primary data files and return them as a named list.
#
# Arguments:
#   data_dir  - path to the top-level data directory (default: "data")
#
# Returns a list with elements: calib, responses, keys, cut_scores.
load_all_data <- function(data_dir = "data") {
  list(
    calib      = readRDS(file.path(data_dir, "processed/calib25_final.rds")),
    responses  = readRDS(file.path(data_dir, "raw/responses.rds")),
    keys       = readRDS(file.path(data_dir, "raw/keyVectors.rds")),
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
