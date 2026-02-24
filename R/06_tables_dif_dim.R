library(mirt)
library(dplyr)
library(tidyr)
library(flextable)
library(ggplot2)
library(CTT)

# ------------------------------------------------------------
# Table 10 — Summary of Yen's Q3 Residual Correlations
# Reports Maximum Q3, Mean Q3, and Proportion of |Q3| > 0.20
# as a dimensionality assessment for a fitted mirt model.
#
# Arguments:
#   mirt_model  - fitted mirt model object
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_10_q3_residuals <- function(mirt_model) {
  # suppress = 1 retains all pairs in the returned matrix (nothing is hidden)
  q3_mat <- mirt::residuals(mirt_model, type = "Q3", suppress = 1)

  # Extract lower triangle (excluding diagonal) to avoid duplicate pairs
  lower_vals <- q3_mat[lower.tri(q3_mat, diag = FALSE)]
  lower_vals <- lower_vals[!is.na(lower_vals)]

  max_q3  <- max(abs(lower_vals))
  mean_q3 <- mean(lower_vals)
  prop_gt <- mean(abs(lower_vals) > 0.20)

  summary_df <- data.frame(
    Statistic = c("Maximum |Q3|", "Mean Q3", "Proportion |Q3| > 0.20"),
    Value     = c(max_q3, mean_q3, prop_gt),
    stringsAsFactors = FALSE
  )

  ft <- flextable(summary_df) |>
    set_header_labels(Statistic = "Statistic", Value = "Value") |>
    colformat_double(j = "Value", digits = 3) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(j = "Statistic", align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = "Table 10: Summary of Yen's Q3 Residual Correlations") |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 11 — Subgroup Reliability (KR-20)
# Reports Kuder-Richardson Formula 20 reliability coefficients
# for "All Students" and major demographic subgroups.
#
# Arguments:
#   scored_data  - data.frame with an SSID column and binary item
#                  columns (e.g., A1 … F5).
#   demo_data    - data.frame with columns SSID, SEX, IEP, LEP, etc.
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_11_subgroup_reliability <- function(scored_data, demo_data) {
  joined <- dplyr::inner_join(scored_data, demo_data, by = "SSID")

  # Identify item columns (all numeric columns except SSID)
  item_cols <- setdiff(
    names(scored_data)[sapply(scored_data, is.numeric)],
    "SSID"
  )

  # Helper: KR-20 via CTT::reliability() on just the item columns
  .kr20 <- function(df) {
    items <- df[, item_cols, drop = FALSE]
    items <- items[complete.cases(items), , drop = FALSE]
    if (nrow(items) < 2) return(NA_real_)
    # CTT::reliability()$alpha equals KR-20 when item scores are binary (0/1)
    CTT::reliability(items)$alpha
  }

  rows <- list(
    data.frame(Subgroup = "All Students",
               N = nrow(joined),
               Reliability = .kr20(joined),
               stringsAsFactors = FALSE)
  )

  # Define subgroups as named logical filters
  subgroups <- list(
    "Male (SEX = M)"   = if ("SEX" %in% names(joined)) joined$SEX == "M"  else NULL,
    "Female (SEX = F)" = if ("SEX" %in% names(joined)) joined$SEX == "F"  else NULL,
    "IEP (IEP = Y)"    = if ("IEP" %in% names(joined)) joined$IEP == "Y"  else NULL,
    "LEP (LEP = Y)"    = if ("LEP" %in% names(joined)) joined$LEP == "Y"  else NULL
  )

  for (label in names(subgroups)) {
    mask <- subgroups[[label]]
    if (is.null(mask)) next
    sub_df <- joined[!is.na(mask) & mask, , drop = FALSE]
    n_sub  <- nrow(sub_df)
    if (n_sub <= 30) next
    rows <- c(rows, list(
      data.frame(Subgroup = label,
                 N = n_sub,
                 Reliability = .kr20(sub_df),
                 stringsAsFactors = FALSE)
    ))
  }

  rel_df <- do.call(rbind, rows)

  ft <- flextable(rel_df) |>
    set_header_labels(
      Subgroup    = "Subgroup",
      N           = "N",
      Reliability = "Reliability (KR-20)"
    ) |>
    colformat_int(j = "N") |>
    colformat_double(j = "Reliability", digits = 3) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(j = "Subgroup", align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = "Table 11: Subgroup Reliability (KR-20)") |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 12 — DIF Analysis: Lord's Delta
# Reports item-level Lord's Delta values, ETS classification
# (A / B / C), and a flag for items with large DIF (Class C).
#
# NOTE: Full Lord's Delta requires fitting separate IRT models
# per group (reference and focal). This scaffold generates
# placeholder Delta values and applies ETS categorisation,
# so the downstream pipeline and report template can be
# developed and validated prior to full IRT calibration.
#
# ETS Classification:
#   A  |Delta| < 1.0            Negligible DIF
#   B  1.0 <= |Delta| < 1.5     Slight / Moderate DIF
#   C  |Delta| >= 1.5           Moderate / Large DIF
#
# Arguments:
#   scored_data  - data.frame with SSID column and binary item columns.
#   demo_data    - data.frame with SSID and a column named `group_col`.
#   group_col    - character; name of the grouping variable (e.g. "SEX").
#   seed         - integer seed for the placeholder RNG (default: 42).
#
# Returns a named list:
#   $table  - flextable
#   $data   - underlying data.frame (for use in make_figure_04_dif_plot)
# ------------------------------------------------------------
make_table_12_dif_lord <- function(scored_data, demo_data, group_col, seed = 42) {
  joined <- dplyr::inner_join(scored_data, demo_data, by = "SSID")
  # Drop rows with missing group information
  joined <- joined[!is.na(joined[[group_col]]), , drop = FALSE]

  item_cols <- setdiff(
    names(scored_data)[sapply(scored_data, is.numeric)],
    "SSID"
  )

  set.seed(seed)
  delta_vals <- stats::runif(length(item_cols), min = -1.5, max = 1.5)

  ets_class <- dplyr::case_when(
    abs(delta_vals) >= 1.5 ~ "C",
    abs(delta_vals) >= 1.0 ~ "B",
    TRUE                   ~ "A"
  )

  dif_df <- data.frame(
    Item      = item_cols,
    DeltaLord = delta_vals,
    ETSClass  = ets_class,
    Flag      = ets_class == "C",
    stringsAsFactors = FALSE
  )

  ft <- flextable(dif_df) |>
    set_header_labels(
      Item      = "Item",
      DeltaLord = "\u0394Lord",
      ETSClass  = "ETS Class",
      Flag      = "Flag"
    ) |>
    colformat_double(j = "DeltaLord", digits = 3) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(j = "Item", align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = paste0("Table 12: DIF Analysis \u2014 Lord\u2019s Delta by ", group_col)) |>
    autofit()

  list(table = ft, data = dif_df)
}

# ------------------------------------------------------------
# Figure 4 — DIF Magnitude and Direction Plot
# Displays Lord's Delta values for each item, colour-coded by
# ETS Class (A = gray, B = gold, C = red).
#
# Arguments:
#   dif_data    - data.frame returned in make_table_12_dif_lord()$data.
#                 Expected columns: Item, DeltaLord, ETSClass, Flag.
#   group_name  - character; label for the focal group (used in title).
#
# Returns a ggplot.
# ------------------------------------------------------------
make_figure_04_dif_plot <- function(dif_data, group_name) {
  # Preserve original item order on the y-axis
  dif_data$Item <- factor(dif_data$Item, levels = rev(dif_data$Item))

  ggplot(dif_data, aes(x = DeltaLord, y = Item, colour = ETSClass)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(size = 3) +
    scale_colour_manual(
      name   = "ETS Class",
      values = c("A" = "gray60", "B" = "#E6A817", "C" = "#C0392B"),
      labels = c(
        "A" = "A \u2014 Negligible (|\u0394| < 1.0)",
        "B" = "B \u2014 Slight/Moderate (1.0 \u2264 |\u0394| < 1.5)",
        "C" = "C \u2014 Moderate/Large (|\u0394| \u2265 1.5)"
      )
    ) +
    labs(
      x     = "\u0394Lord (Lord\u2019s Delta)",
      y     = "Item",
      title = paste0("Figure 4: DIF Magnitude and Direction \u2014 ", group_name),
      caption = "Dashed line at \u0394 = 0. Items coloured by ETS DIF classification."
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
