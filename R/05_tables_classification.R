library(flextable)

# ------------------------------------------------------------
# Table 7 — Raw Score to Theta (IRT Lookup Table)
# Maps every possible raw score (0..n_items) to its IRT theta
# estimate (EAP) and the associated standard error of
# measurement (SEM) derived from the test information function.
#
# Arguments:
#   mirt_model - a SingleGroupClass object fitted with mirt()
#   test_id    - character label used in the table caption
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_07_raw_to_theta <- function(mirt_model, test_id = "") {
  n_items <- mirt::extract.mirt(mirt_model, "nitems")

  # Build a pattern matrix with one row per possible raw score.
  # Each row has exactly `raw` ones followed by zeros so that
  # mirt::expected.test() / fscores() can map it to theta.
  raw_scores  <- 0:n_items
  item_names  <- mirt::extract.mirt(mirt_model, "itemnames")

  theta_vals <- numeric(length(raw_scores))
  sem_vals   <- numeric(length(raw_scores))

  for (i in seq_along(raw_scores)) {
    r <- raw_scores[i]
    # Construct a response pattern: r correct followed by (n_items - r) incorrect
    pattern <- matrix(c(rep(1L, r), rep(0L, n_items - r)),
                      nrow = 1,
                      dimnames = list(NULL, item_names))
    fs <- mirt::fscores(mirt_model,
                        response.pattern = pattern,
                        full.scores.SE   = TRUE,
                        verbose          = FALSE)
    theta_vals[i] <- as.numeric(fs[1, "F1"])
    sem_vals[i]   <- as.numeric(fs[1, "SE_F1"])
  }

  lookup_df <- data.frame(
    `Raw Score`      = raw_scores,
    `Theta (Logit)`  = round(theta_vals, 2),
    `SEM`            = round(sem_vals,   2),
    check.names      = FALSE
  )

  caption_text <- if (nchar(test_id) > 0) {
    paste0("Table 7: Raw Score to Theta Conversion (", test_id, ")")
  } else {
    "Table 7: Raw Score to Theta Conversion"
  }

  ft <- flextable(lookup_df) |>
    set_header_labels(
      `Raw Score`     = "Raw Score",
      `Theta (Logit)` = "Theta (Logit)",
      `SEM`           = "SEM"
    ) |>
    colformat_int(j = "Raw Score") |>
    colformat_double(j = c("Theta (Logit)", "SEM"), digits = 2) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = caption_text) |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 15 — Classification Accuracy (2-Category Placeholder)
# Reports a simplified Proficient / Not Proficient accuracy
# summary. Values are placeholder estimates (0.85) until the
# Beta-Binomial / Livingston-Lewis parameters are finalised.
#
# Arguments:
#   reliability - numeric; test reliability coefficient (e.g. KR-20)
#   cut_score   - numeric; the single cut score (raw) for Proficient
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_15_class_accuracy <- function(reliability, cut_score) {
  acc_df <- data.frame(
    Classification = c("Proficient", "Not Proficient", "Overall"),
    Accuracy       = c(0.85, 0.85, 0.85),
    stringsAsFactors = FALSE
  )

  ft <- flextable(acc_df) |>
    set_header_labels(
      Classification = "Classification",
      Accuracy       = "Accuracy (Proportion)"
    ) |>
    colformat_double(j = "Accuracy", digits = 2) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(j = "Classification", align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    add_footer_lines(
      paste0("Note. Placeholder values (reliability = ",
             round(reliability, 2),
             ", cut score = ", cut_score,
             "). To be replaced with Livingston-Lewis estimates.")
    ) |>
    set_caption(caption = "Table 15: Classification Accuracy (Proficient / Not Proficient)") |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 16 — Decision Consistency (2×2 Contingency Matrix)
# Displays a 2×2 layout of True/False Positives/Negatives,
# along with the proportion of consistent classifications and
# Cohen's Kappa. Values are placeholders pending true
# Livingston-Lewis computation.
#
# Arguments:
#   reliability - numeric; test reliability coefficient
#   cut_score   - numeric; the single cut score (raw) for Proficient
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_16_decision_consistency <- function(reliability, cut_score) {
  consist_df <- data.frame(
    `Test 1 Status`         = c("Test 1: Proficient",
                                "Test 1: Not Proficient",
                                "Column Total"),
    `Test 2: Proficient`    = c(NA_real_, NA_real_, NA_real_),
    `Test 2: Not Proficient`= c(NA_real_, NA_real_, NA_real_),
    `Row Total`             = c(NA_real_, NA_real_, NA_real_),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ft_matrix <- flextable(consist_df) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(j = "Test 1 Status", align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    bold(i = 3, part = "body") |>
    add_footer_lines(
      paste0("Note. Placeholder values (reliability = ",
             round(reliability, 2),
             ", cut score = ", cut_score,
             "). To be replaced with Livingston-Lewis estimates.")
    ) |>
    set_caption(caption = "Table 16: Decision Consistency (2\u00d72 Contingency Matrix)") |>
    autofit()

  ft_matrix
}

# ------------------------------------------------------------
# Table 17 — NAPD Classification Accuracy (4 Levels)
# Detailed accuracy statistics for the four AKSA performance
# levels: Novice, Apprentice, Proficient, Distinguished.
# Columns: Level, TP, FP, TN, FN, Sensitivity, Specificity,
#          p (proportion in level), c (consistency), Kappa.
# Values are placeholders (NA) pending finalised
# Livingston-Lewis computation.
#
# Arguments:
#   cut_scores_row - a single-row tibble/data.frame from
#                    AKSA_NAPD_Cut_Scores.rds, containing at
#                    minimum columns for the Min/Max of N, A, P, D
#   reliability    - numeric; test reliability coefficient
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_17_napd_accuracy <- function(cut_scores_row, reliability) {
  levels_vec <- c("Novice", "Apprentice", "Proficient", "Distinguished")

  napd_df <- data.frame(
    Level       = levels_vec,
    TP          = rep(NA_real_, 4),
    FP          = rep(NA_real_, 4),
    TN          = rep(NA_real_, 4),
    FN          = rep(NA_real_, 4),
    Sensitivity = rep(NA_real_, 4),
    Specificity = rep(NA_real_, 4),
    p           = rep(NA_real_, 4),
    c           = rep(NA_real_, 4),
    Kappa       = rep(NA_real_, 4),
    stringsAsFactors = FALSE
  )

  ft <- flextable(napd_df) |>
    set_header_labels(
      Level       = "Level",
      TP          = "TP",
      FP          = "FP",
      TN          = "TN",
      FN          = "FN",
      Sensitivity = "Sensitivity",
      Specificity = "Specificity",
      p           = "p",
      c           = "c",
      Kappa       = "Kappa"
    ) |>
    colformat_double(
      j      = c("TP", "FP", "TN", "FN",
                 "Sensitivity", "Specificity", "p", "c", "Kappa"),
      digits = 2,
      na_str = "—"
    ) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(j = "Level", align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    add_footer_lines(
      paste0("Note. TP = True Positive; FP = False Positive; ",
             "TN = True Negative; FN = False Negative; ",
             "p = proportion classified into level; ",
             "c = proportion of consistent classifications. ",
             "Reliability = ", round(reliability, 2), ". ",
             "Placeholder values (\u2014) to be replaced with ",
             "Livingston-Lewis estimates.")
    ) |>
    set_caption(caption = "Table 17: NAPD Classification Accuracy") |>
    autofit()

  ft
}
