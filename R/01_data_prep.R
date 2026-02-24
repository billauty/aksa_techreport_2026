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
score_test_responses <- function(resp_df, key_vector) {
  present_items <- names(key_vector)[names(key_vector) %in% names(resp_df)]

  for (item in present_items) {
    resp_df[[item]] <- as.integer(resp_df[[item]] == key_vector[[item]])
  }

  resp_df[, c("SSID", "complete", present_items)]
}
