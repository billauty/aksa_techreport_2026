library(flextable)
library(dplyr)
library(officer)

# Internal helper: sample skewness (consistent with e1071 type 1)
.skew <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^(3 / 2)
}

# Internal helper: excess kurtosis (consistent with e1071 type 1)
.kurt <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x)
  (sum((x - m)^4) / n) / (sum((x - m)^2) / n)^2 - 3
}

# ------------------------------------------------------------
# Table 3 — Score Descriptive Statistics
# Reports N, Mean, SD, Median, Min, Max, Skewness, and Kurtosis
# of scale scores for each test.
#
# Arguments:
#   scored_data  - data.frame/tibble with at minimum two columns:
#                    test_id     - character; test identifier used to
#                                  group rows (e.g., "RD_04", "MA_05")
#                    scale_score - numeric; examinee scale score
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_03_score_descriptives <- function(scored_data) {
  stopifnot(all(c("test_id", "scale_score") %in% names(scored_data)))

  desc_df <- scored_data |>
    dplyr::group_by(test_id) |>
    dplyr::summarise(
      N        = sum(!is.na(scale_score)),
      Mean     = mean(scale_score, na.rm = TRUE),
      SD       = sd(scale_score, na.rm = TRUE),
      Median   = median(scale_score, na.rm = TRUE),
      Min      = min(scale_score, na.rm = TRUE),
      Max      = max(scale_score, na.rm = TRUE),
      Skewness = .skew(scale_score),
      Kurtosis = .kurt(scale_score),
      .groups  = "drop"
    )

  big_b <- fp_border(color = "black", width = 1.5)
  std_b <- fp_border(color = "black", width = 1)

  ft <- flextable(desc_df) |>
    set_header_labels(
      test_id  = "Test",
      N        = "N",
      Mean     = "Mean",
      SD       = "SD",
      Median   = "Median",
      Min      = "Min",
      Max      = "Max",
      Skewness = "Skewness",
      Kurtosis = "Kurtosis"
    ) |>
    colformat_int(j = "N") |>
    colformat_double(
      j = c("Mean", "SD", "Median", "Min", "Max", "Skewness", "Kurtosis"),
      digits = 2
    ) |>
    border_remove() |>
    hline_top(part = "header", border = big_b) |>
    hline_bottom(part = "header", border = std_b) |>
    hline_bottom(part = "body", border = big_b) |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = "test_id", align = "left", part = "all") |>
    set_caption(caption = "Table 3: Score Descriptive Statistics",
                fp_p = fp_par(text.align = "left")) |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 4 — Score Descriptive Statistics by Subgroup
# Reports N, Mean, and SD of scale scores broken out by each
# demographic subgroup.
#
# Arguments:
#   scored_data  - data.frame/tibble in long format with columns:
#                    scale_score    - numeric; examinee scale score
#                    subgroup_var   - character; name of the demographic
#                                    variable (e.g., "Gender",
#                                    "IEP Status", "ELL Status")
#                    subgroup_level - character; category within the
#                                    variable (e.g., "Male", "Female",
#                                    "Yes", "No")
#
#   To construct this input from wide-format demographic data, pivot
#   the flag columns into long format with tidyr::pivot_longer() before
#   calling this function.
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_04_subgroup_descriptives <- function(scored_data) {
  stopifnot(
    all(c("scale_score", "subgroup_var", "subgroup_level") %in% names(scored_data))
  )

  desc_df <- scored_data |>
    dplyr::group_by(subgroup_var, subgroup_level) |>
    dplyr::summarise(
      N    = sum(!is.na(scale_score)),
      Mean = mean(scale_score, na.rm = TRUE),
      SD   = sd(scale_score, na.rm = TRUE),
      .groups = "drop"
    )

  big_b <- fp_border(color = "black", width = 1.5)
  std_b <- fp_border(color = "black", width = 1)

  ft <- flextable(desc_df) |>
    set_header_labels(
      subgroup_var   = "Subgroup",
      subgroup_level = "Level",
      N              = "N",
      Mean           = "Mean",
      SD             = "SD"
    ) |>
    colformat_int(j = "N") |>
    colformat_double(j = c("Mean", "SD"), digits = 2) |>
    border_remove() |>
    hline_top(part = "header", border = big_b) |>
    hline_bottom(part = "header", border = std_b) |>
    hline_bottom(part = "body", border = big_b) |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = "subgroup_var", align = "left", part = "all") |>
    set_caption(caption = "Table 4: Score Descriptive Statistics by Subgroup",
                fp_p = fp_par(text.align = "left")) |>
    autofit()

  ft
}
