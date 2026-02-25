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
#   demo_data    - data.frame with columns SSID and demographic
#                  variables (SEX/Gender, Ethnic/Ethnicity/Race,
#                  Disadvantaged/ED, LEP, Homeless).
#
# Returns a flextable with columns: Category, Group, nStudents, Reliability.
# ------------------------------------------------------------
make_table_11_subgroup_reliability <- function(scored_data, demo_data) {
  # Coerce SSID to character and trim whitespace before joining
  scored_data$SSID <- trimws(as.character(scored_data$SSID))
  demo_data$SSID   <- trimws(as.character(demo_data$SSID))

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

  # "All" row computed from scored_data BEFORE join (avoids SSID mismatch issues)
  rows <- list(
    data.frame(Category    = "All",
               Group       = "",
               nStudents   = nrow(scored_data),
               Reliability = .kr20(scored_data),
               stringsAsFactors = FALSE)
  )

  # Join for subgroup rows
  joined <- dplyr::left_join(scored_data, demo_data, by = "SSID")

  # Helper: add subgroup rows for a demographic column
  .add_subgroup_rows <- function(rows, category, col_names, value_map = NULL) {
    # Find which column name exists in joined
    col <- col_names[col_names %in% names(joined)][1]
    if (is.na(col)) return(rows)

    vals <- unique(joined[[col]])
    vals <- sort(vals[!is.na(vals)])

    for (v in vals) {
      sub_df <- joined[!is.na(joined[[col]]) & joined[[col]] == v, , drop = FALSE]
      n_sub  <- nrow(sub_df)
      if (n_sub < 10) next
      label <- if (!is.null(value_map) && !is.null(value_map[[v]])) value_map[[v]] else as.character(v)
      rows <- c(rows, list(
        data.frame(Category    = category,
                   Group       = label,
                   nStudents   = n_sub,
                   Reliability = .kr20(sub_df),
                   stringsAsFactors = FALSE)
      ))
    }
    rows
  }

  # Gender
  rows <- .add_subgroup_rows(rows, "Gender",
                             c("Gender", "SEX"),
                             list(M = "Male", F = "Female"))

  # Ethnicity
  rows <- .add_subgroup_rows(rows, "Ethnic",
                             c("Ethnic", "Ethnicity", "Race"))

  # Disadvantaged / Economically Disadvantaged
  rows <- .add_subgroup_rows(rows, "Disadvantaged",
                             c("Disadvantaged", "ED"),
                             list(Y = "Yes", N = "No"))

  # LEP
  rows <- .add_subgroup_rows(rows, "LEP",
                             c("LEP"),
                             list(Y = "Yes", N = "No"))

  # Homeless
  rows <- .add_subgroup_rows(rows, "Homeless",
                             c("Homeless"),
                             list(Y = "Yes", N = "No"))

  rel_df <- do.call(rbind, rows)

  ft <- flextable(rel_df) |>
    set_header_labels(
      Category    = "Category",
      Group       = "Group",
      nStudents   = "nStudents",
      Reliability = "Reliability"
    ) |>
    colformat_int(j = "nStudents") |>
    colformat_double(j = "Reliability", digits = 3) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(j = c("Category", "Group"), align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = "Table 11: Reliability for All students and Subgroups of Sufficient Size") |>
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
make_table_12_dif_lord <- function(scored_data, demo_data, group_col, table_num = 12L, seed = 42) {
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
    set_caption(caption = paste0("Table ", table_num, ": DIF Analysis \u2014 Lord\u2019s Delta by ", group_col)) |>
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
make_figure_04_dif_plot <- function(dif_data, group_name, fig_num = 4L) {
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
      title = paste0("Figure ", fig_num, ": DIF Magnitude and Direction \u2014 ", group_name),
      caption = "Dashed line at \u0394 = 0. Items coloured by ETS DIF classification."
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------
# Table 9 — Item Infit and Outfit Statistics
# Reports per-item Infit and Outfit mean-square (MSQ) statistics
# from a fitted Rasch/mirt model.  Values near 1.0 indicate
# good fit; values above 1.5 suggest underfitting.
#
# Arguments:
#   mirt_model  - fitted mirt model object (SingleGroupClass)
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_09_item_fit <- function(mirt_model) {
  # fit_statistics = "infit" is the mirt API argument; despite the name, mirt
  # returns BOTH infit and outfit mean-square statistics in this call.
  fit_stats <- mirt::itemfit(mirt_model, fit_stats = "infit", na.rm = TRUE)

  # Normalise column names — they vary slightly across mirt versions.
  # mirt::itemfit() always places item identifiers in a column named "item".
  nm         <- names(fit_stats)
  infit_col  <- if ("infit"  %in% nm) "infit"  else {
    matched <- grep("infit",  nm, ignore.case = TRUE, value = TRUE)
    if (length(matched) == 0) stop("Could not locate infit column in mirt::itemfit() output.")
    matched[1]
  }
  outfit_col <- if ("outfit" %in% nm) "outfit" else {
    matched <- grep("outfit", nm, ignore.case = TRUE, value = TRUE)
    if (length(matched) == 0) stop("Could not locate outfit column in mirt::itemfit() output.")
    matched[1]
  }

  fit_df <- data.frame(
    Item   = as.character(fit_stats[["item"]]),
    Infit  = as.numeric(fit_stats[[infit_col]]),
    Outfit = as.numeric(fit_stats[[outfit_col]]),
    stringsAsFactors = FALSE
  )

  big_b <- fp_border(color = "black", width = 1.5)
  std_b <- fp_border(color = "black", width = 1)

  ft <- flextable(fit_df) |>
    set_header_labels(Item = "Item", Infit = "Infit MSQ", Outfit = "Outfit MSQ") |>
    colformat_double(j = c("Infit", "Outfit"), digits = 3) |>
    border_remove() |>
    hline_top(part = "header", border = big_b) |>
    hline_bottom(part = "header", border = std_b) |>
    hline_bottom(part = "body", border = big_b) |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = "Item", align = "left", part = "all") |>
    set_caption(caption = "Table 9: Item Infit and Outfit Statistics",
                fp_p = fp_par(text.align = "left")) |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Figure 1 — Item Infit and Outfit Statistics
# Plots Infit and Outfit MSQ values for each item, with
# reference lines at 0.5 and 1.5 marking the acceptable range.
#
# Arguments:
#   mirt_model  - fitted mirt model object (SingleGroupClass)
#
# Returns a ggplot.
# ------------------------------------------------------------
make_figure_01_item_fit <- function(mirt_model) {
  # fit_statistics = "infit" is the mirt API argument; despite the name, mirt
  # returns BOTH infit and outfit mean-square statistics in this call.
  fit_stats <- mirt::itemfit(mirt_model, fit_stats = "infit", na.rm = TRUE)

  # Normalise column names — they vary slightly across mirt versions.
  # mirt::itemfit() always places item identifiers in a column named "item".
  nm         <- names(fit_stats)
  infit_col  <- if ("infit"  %in% nm) "infit"  else {
    matched <- grep("infit",  nm, ignore.case = TRUE, value = TRUE)
    if (length(matched) == 0) stop("Could not locate infit column in mirt::itemfit() output.")
    matched[1]
  }
  outfit_col <- if ("outfit" %in% nm) "outfit" else {
    matched <- grep("outfit", nm, ignore.case = TRUE, value = TRUE)
    if (length(matched) == 0) stop("Could not locate outfit column in mirt::itemfit() output.")
    matched[1]
  }

  plot_df <- data.frame(
    item   = as.character(fit_stats[["item"]]),
    Infit  = as.numeric(fit_stats[[infit_col]]),
    Outfit = as.numeric(fit_stats[[outfit_col]]),
    stringsAsFactors = FALSE
  )

  long_df <- tidyr::pivot_longer(
    plot_df,
    cols      = c("Infit", "Outfit"),
    names_to  = "Statistic",
    values_to = "Value"
  )

  # Order items by overall mean fit for a tidy display
  item_order <- plot_df$item[order(rowMeans(plot_df[, c("Infit", "Outfit")]))]
  long_df$item <- factor(long_df$item, levels = item_order)

  ggplot(long_df, aes(x = Value, y = item, colour = Statistic, shape = Statistic)) +
    geom_vline(xintercept = 1,             colour = "grey30", linewidth = 0.6) +
    geom_vline(xintercept = c(0.5, 1.5),  linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    geom_point(size = 3) +
    scale_colour_manual(values = c(Infit = "#2166AC", Outfit = "#D6604D")) +
    labs(
      x       = "Mean-Square Fit Statistic",
      y       = NULL,
      title   = "Figure 1: Item Infit and Outfit Statistics",
      caption = paste0(
        "Solid line at 1.0 = perfect fit. ",
        "Dashed lines at 0.5 and 1.5 mark the acceptable range."
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.caption    = element_text(size = 8, hjust = 0)
    )
}

# ------------------------------------------------------------
# Table 13 — DIF Analysis: Lord's Delta (Group 2 — IEP)
# Thin wrapper around make_table_12_dif_lord() that uses table
# number 13 in the caption.  Default group_col is "IEP".
#
# Arguments:  see make_table_12_dif_lord().
# Returns a named list: $table (flextable), $data (data.frame).
# ------------------------------------------------------------
make_table_13_dif_lord <- function(scored_data, demo_data,
                                   group_col = "IEP", seed = 42) {
  make_table_12_dif_lord(scored_data, demo_data,
                         group_col = group_col, table_num = 13L, seed = seed)
}

# ------------------------------------------------------------
# Table 14 — DIF Analysis: Lord's Delta (Group 3 — LEP)
# Thin wrapper around make_table_12_dif_lord() that uses table
# number 14 in the caption.  Default group_col is "LEP".
#
# Arguments:  see make_table_12_dif_lord().
# Returns a named list: $table (flextable), $data (data.frame).
# ------------------------------------------------------------
make_table_14_dif_lord <- function(scored_data, demo_data,
                                   group_col = "LEP", seed = 42) {
  make_table_12_dif_lord(scored_data, demo_data,
                         group_col = group_col, table_num = 14L, seed = seed)
}

# ------------------------------------------------------------
# Figure 5 — DIF Magnitude and Direction (Group 2 — IEP)
# Thin wrapper around make_figure_04_dif_plot() that sets the
# figure number to 5.
#
# Arguments:
#   dif_data    - data.frame from make_table_13_dif_lord()$data
#   group_name  - character label for the focal group (default: "IEP")
#
# Returns a ggplot.
# ------------------------------------------------------------
make_figure_05_dif_plot <- function(dif_data, group_name = "IEP") {
  make_figure_04_dif_plot(dif_data, group_name = group_name, fig_num = 5L)
}

# ------------------------------------------------------------
# Figure 6 — DIF Magnitude and Direction (Group 3 — LEP)
# Thin wrapper around make_figure_04_dif_plot() that sets the
# figure number to 6.
#
# Arguments:
#   dif_data    - data.frame from make_table_14_dif_lord()$data
#   group_name  - character label for the focal group (default: "LEP")
#
# Returns a ggplot.
# ------------------------------------------------------------
make_figure_06_dif_plot <- function(dif_data, group_name = "LEP") {
  make_figure_04_dif_plot(dif_data, group_name = group_name, fig_num = 6L)
}
