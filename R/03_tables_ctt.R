library(flextable)
library(officer)

# Internal helper: sample skewness (consistent with e1071 type 1)
.skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^(3 / 2)
}

# Internal helper: excess kurtosis (consistent with e1071 type 1)
.kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x)
  (sum((x - m)^4) / n) / (sum((x - m)^2) / n)^2 - 3
}

# ------------------------------------------------------------
# Table 1 — CTT Score Summary
# Reports N, Mean, SD, Min, and Max of total raw scores.
#
# Arguments:
#   scored_data  - data.frame/tibble where rows are students and columns
#                  are binary item scores (0/1).
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_01_score_summary <- function(scored_data) {
  scored_data <- scored_data[, sapply(scored_data, is.numeric), drop = FALSE]
  total <- rowSums(scored_data, na.rm = TRUE)

  summary_df <- data.frame(
    N    = length(total),
    Mean = mean(total, na.rm = TRUE),
    SD   = sd(total, na.rm = TRUE),
    Min  = min(total, na.rm = TRUE),
    Max  = max(total, na.rm = TRUE)
  )

  big_b <- fp_border(color = "black", width = 1.5)
  std_b <- fp_border(color = "black", width = 1)

  ft <- flextable(summary_df) |>
    set_header_labels(N = "N", Mean = "Mean", SD = "SD",
                      Min = "Min", Max = "Max") |>
    colformat_double(j = c("Mean", "SD"), digits = 2) |>
    colformat_int(j = c("N", "Min", "Max")) |>
    border_remove() |>
    hline_top(part = "header", border = big_b) |>
    hline_bottom(part = "header", border = std_b) |>
    hline_bottom(part = "body", border = big_b) |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all") |>
    set_caption(caption = "Table 1: CTT Score Summary",
                fp_p = fp_par(text.align = "left")) |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 2 — CTT Item Statistics
# Reports per-item N, Mean (p-value), SD, Median, Min, Max,
# Range, Skewness, and Kurtosis.
#
# Arguments:
#   scored_data  - data.frame/tibble of binary item scores (0/1).
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_02_item_stats <- function(scored_data) {
  scored_data <- scored_data[, sapply(scored_data, is.numeric), drop = FALSE]
  item_stats <- lapply(names(scored_data), function(item) {
    x <- scored_data[[item]]
    x_complete <- x[!is.na(x)]
    data.frame(
      Item     = item,
      N        = length(x_complete),
      Mean     = mean(x_complete),
      SD       = sd(x_complete),
      Median   = median(x_complete),
      Min      = min(x_complete),
      Max      = max(x_complete),
      Range    = max(x_complete) - min(x_complete),
      Skewness = .skewness(x_complete),
      Kurtosis = .kurtosis(x_complete),
      stringsAsFactors = FALSE
    )
  })

  stats_df <- do.call(rbind, item_stats)

  big_b <- fp_border(color = "black", width = 1.5)
  std_b <- fp_border(color = "black", width = 1)

  ft <- flextable(stats_df) |>
    set_header_labels(
      Item     = "Item",
      N        = "N",
      Mean     = "Mean (p)",
      SD       = "SD",
      Median   = "Median",
      Min      = "Min",
      Max      = "Max",
      Range    = "Range",
      Skewness = "Skewness",
      Kurtosis = "Kurtosis"
    ) |>
    colformat_double(
      j = c("Mean", "SD", "Median", "Min", "Max", "Range", "Skewness", "Kurtosis"),
      digits = 2
    ) |>
    colformat_int(j = "N") |>
    border_remove() |>
    hline_top(part = "header", border = big_b) |>
    hline_bottom(part = "header", border = std_b) |>
    hline_bottom(part = "body", border = big_b) |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = "Item", align = "left", part = "all") |>
    set_caption(caption = "Table 2: CTT Item Statistics",
                fp_p = fp_par(text.align = "left")) |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 3 — KR-20 Reliability
# Reports the Kuder-Richardson Formula 20 (KR-20) reliability
# coefficient for a binary-scored test.
#
# Formula: KR-20 = (k / (k - 1)) * (1 - sum(p * q) / var(total))
#
# Arguments:
#   scored_data  - data.frame/tibble of binary item scores (0/1).
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_03_reliability <- function(scored_data) {
  k <- ncol(scored_data)
  if (k < 2) {
    warning("KR-20 is undefined for fewer than 2 items; returning NA.")
    rel_df <- data.frame(k = k, KR20 = NA_real_)
  } else {
    total <- rowSums(scored_data, na.rm = TRUE)
    p     <- colMeans(scored_data, na.rm = TRUE)
    q     <- 1 - p
    kr20  <- (k / (k - 1)) * (1 - sum(p * q) / var(total))
    rel_df <- data.frame(k = k, KR20 = kr20)
  }

  ft <- flextable(rel_df) |>
    set_header_labels(k = "Number of Items", KR20 = "KR-20") |>
    colformat_double(j = "KR20", digits = 2) |>
    colformat_int(j = "k") |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = "Table 3: KR-20 Reliability") |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 4 — Score Frequency Distribution
# Reports Score, N, Percent, and Cumulative Percent for total
# raw scores.
#
# Arguments:
#   scored_data  - data.frame/tibble of binary item scores (0/1).
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_04_score_frequencies <- function(scored_data) {
  total  <- rowSums(scored_data, na.rm = TRUE)
  n_total <- length(total)

  freq_df <- as.data.frame(table(Score = total), stringsAsFactors = FALSE)
  freq_df$Score   <- as.integer(freq_df$Score)
  freq_df$N       <- as.integer(freq_df$Freq)
  freq_df$Freq    <- NULL
  freq_df$Percent <- freq_df$N / n_total * 100
  freq_df$CumPct  <- cumsum(freq_df$Percent)

  ft <- flextable(freq_df) |>
    set_header_labels(
      Score  = "Score",
      N      = "N",
      Percent = "Percent (%)",
      CumPct  = "Cumulative %"
    ) |>
    colformat_int(j = c("Score", "N")) |>
    colformat_double(j = c("Percent", "CumPct"), digits = 2) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = "Table 4: Score Frequency Distribution") |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 5 — Distractor Analysis
# For each item and each response option, reports n, resP, pBis,
# discrim, lower, mid66, and upper. The correct response is
# flagged with an asterisk (*).
#
# Arguments:
#   raw_responses  - data.frame of chosen options (e.g., A, B, C, D),
#                    one row per student, one column per item.
#   answer_key     - named character vector (names = item names,
#                    values = correct option) OR a one-row data.frame
#                    with item names as column names.
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_05_distractor_analysis <- function(raw_responses, answer_key) {
  # Normalise answer_key to a named character vector
  if (is.data.frame(answer_key)) {
    key_vec <- unlist(answer_key[1, ], use.names = TRUE)
  } else if (is.character(answer_key) || is.factor(answer_key)) {
    key_vec <- as.character(answer_key)
    if (is.null(names(key_vec))) {
      stop("answer_key must be a named vector with item names, or a data.frame.")
    }
  } else {
    stop("answer_key must be a named character vector or a data.frame with one row per item.")
  }

  # Build a scored (0/1) matrix for pBis and discrimination indices
  scored <- as.data.frame(
    mapply(function(resp, correct) as.integer(resp == correct),
           raw_responses, key_vec[names(raw_responses)],
           SIMPLIFY = FALSE)
  )
  total_score <- rowSums(scored, na.rm = TRUE)

  # Determine group boundaries (lower 27 %, upper 27 %)
  n_total  <- nrow(raw_responses)
  # type = 1 (inverse empirical CDF) matches the standard psychometric convention
  # for defining the lower and upper 27% groups used in item discrimination indices.
  cut_low  <- quantile(total_score, probs = 0.27, type = 1)
  cut_high <- quantile(total_score, probs = 0.73, type = 1)
  grp_lower <- total_score <= cut_low
  grp_upper <- total_score >= cut_high
  grp_mid   <- !grp_lower & !grp_upper

  rows <- lapply(names(raw_responses), function(item) {
    responses <- raw_responses[[item]]
    correct   <- key_vec[[item]]
    options   <- sort(unique(na.omit(responses)))

    lapply(options, function(opt) {
      chose <- !is.na(responses) & responses == opt

      n_opt   <- sum(chose)
      resP    <- n_opt / n_total

      # Point-biserial correlation between choosing this option and total score
      x <- as.numeric(chose)
      if (sd(x) == 0 || sd(total_score) == 0) {
        pbis <- NA_real_
      } else {
        pbis <- cor(x, total_score, use = "complete.obs")
      }

      lower_prop <- if (sum(grp_lower) > 0) mean(chose[grp_lower]) else NA_real_
      upper_prop <- if (sum(grp_upper) > 0) mean(chose[grp_upper]) else NA_real_
      mid_prop   <- if (sum(grp_mid)   > 0) mean(chose[grp_mid])   else NA_real_
      discrim    <- if (!is.na(upper_prop) && !is.na(lower_prop)) upper_prop - lower_prop else NA_real_

      # Flag correct answer
      option_label <- if (opt == correct) paste0(opt, "*") else opt

      data.frame(
        Item    = item,
        Option  = option_label,
        n       = n_opt,
        resP    = resP,
        pBis    = pbis,
        discrim = discrim,
        lower   = lower_prop,
        mid66   = mid_prop,
        upper   = upper_prop,
        stringsAsFactors = FALSE
      )
    })
  })

  distractor_df <- do.call(rbind, do.call(c, rows))

  ft <- flextable(distractor_df) |>
    set_header_labels(
      Item    = "Item",
      Option  = "Option",
      n       = "n",
      resP    = "Prop.",
      pBis    = "pBis",
      discrim = "Discrim.",
      lower   = "Lower 27%",
      mid66   = "Mid 66%",
      upper   = "Upper 27%"
    ) |>
    colformat_int(j = "n") |>
    colformat_double(j = c("resP", "pBis", "discrim", "lower", "mid66", "upper"),
                     digits = 2) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(j = c("Item", "Option"), align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = "Table 5: Distractor Analysis (* = correct response)") |>
    autofit()

  ft
}
