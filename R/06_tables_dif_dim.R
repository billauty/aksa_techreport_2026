library(mirt)
library(dplyr)
library(tidyr)
library(flextable)
library(ggplot2)
library(CTT)
library(difR)

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
# ------------------------------------------------------------
make_table_11_subgroup_reliability <- function(scored_data, demo_data, test_id = NULL) {
  
  # Ensure scored_data is a plain data frame
  scored_data <- as.data.frame(scored_data)
  demo_data <- as.data.frame(demo_data)
  
  # Strip any lingering haven labels from items just to be safe
  for (col in names(scored_data)) {
    attr(scored_data[[col]], "class")  <- NULL
    attr(scored_data[[col]], "labels") <- NULL
  }
  
  # Coerce SSID to character and trim whitespace before joining
  scored_data$SSID <- trimws(as.character(scored_data$SSID))
  demo_data$SSID   <- trimws(as.character(demo_data$SSID))
  
  # Identify item columns (all numeric columns except SSID and complete)
  item_cols <- setdiff(
    names(scored_data)[sapply(scored_data, is.numeric)],
    c("SSID", "complete")
  )
  
  # Helper: KR-20 via CTT::reliability() on just the item columns
  .kr20 <- function(df) {
    items <- df[, item_cols, drop = FALSE]
    items <- items[complete.cases(items), , drop = FALSE]
    if (nrow(items) < 2) return(NA_real_)
    CTT::reliability(items)$alpha
  }
  
  rows <- list(
    data.frame(Category    = "All",
               Group       = "",
               nStudents   = nrow(scored_data),
               Reliability = .kr20(scored_data),
               stringsAsFactors = FALSE)
  )
  
  # Join all demographic columns directly
  joined <- dplyr::left_join(scored_data, demo_data, by = "SSID")
  
  # Helper: Add rows for a specific column. No complex mapping needed because
  # strings are already "Male", "Female", "Yes", "No", etc.
  .add_subgroup_rows <- function(rows, category, col_name) {
    # Check if the column exists in the joined data
    if (!col_name %in% names(joined)) return(rows)
    
    # Grab the string values, drop NAs/blanks
    grp_vec <- trimws(as.character(joined[[col_name]]))
    vals <- unique(grp_vec)
    vals <- sort(vals[!is.na(vals) & vals != ""])
    
    for (v in vals) {
      sub_df <- joined[!is.na(grp_vec) & grp_vec == v, , drop = FALSE]
      n_sub  <- nrow(sub_df)
      if (n_sub < 10) next  # Exclude subgroups smaller than 10
      
      rows <- c(rows, list(
        data.frame(Category    = category,
                   Group       = v,        # Use the string directly!
                   nStudents   = n_sub,
                   Reliability = .kr20(sub_df),
                   stringsAsFactors = FALSE)
      ))
    }
    rows
  }
  
  # Call the helper exactly using the names we generated in R/01_data_prep.R
  rows <- .add_subgroup_rows(rows, "Gender", "Gender")
  rows <- .add_subgroup_rows(rows, "Ethnicity", "Ethnic")
  rows <- .add_subgroup_rows(rows, "Disadvantaged", "Disadvantaged")
  rows <- .add_subgroup_rows(rows, "LEP", "LEP")
  rows <- .add_subgroup_rows(rows, "IEP", "IEP")
  rows <- .add_subgroup_rows(rows, "Homeless", "Homeless")
  
  rel_df <- do.call(rbind, rows)
  
  caption_text <- "Table 11: Reliability for All students and Subgroups of Sufficient Size"
  if (!is.null(test_id)) {
    caption_text <- paste0(caption_text, " (", test_id, ")")
  }
  
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
    set_caption(caption = caption_text) |>
    autofit()
  
  ft
}

# ------------------------------------------------------------
# Table 12, 13, 14 — DIF Analysis: Mantel-Haenszel
# ------------------------------------------------------------
make_table_12_dif_lord <- function(scored_data, demo_data, group_col, table_num = 12L) {
  
  # Ensure plain data frames and clean out labels
  scored_data <- as.data.frame(scored_data)
  demo_data <- as.data.frame(demo_data)
  for (col in names(scored_data)) {
    attr(scored_data[[col]], "class") <- NULL
    attr(scored_data[[col]], "labels") <- NULL
  }
  
  # Join score and demographic data
  scored_data$SSID <- trimws(as.character(scored_data$SSID))
  demo_data$SSID   <- trimws(as.character(demo_data$SSID))
  
  joined <- dplyr::inner_join(scored_data, demo_data, by = "SSID")
  
  # Drop rows missing the target demographic group
  joined <- joined[!is.na(joined[[group_col]]) & joined[[group_col]] != "", , drop = FALSE]
  
  item_cols <- setdiff(
    names(scored_data)[sapply(scored_data, is.numeric)],
    c("SSID", "complete")
  )
  
  num_items <- length(item_cols)
  
  # Set reference and focal groups
  group_counts <- sort(table(joined[[group_col]]), decreasing = TRUE)
  
  # If there is no variation in the group (e.g., all students are Male), DIF cannot run
  if (length(group_counts) < 2 || nrow(joined) < 30) {
    dif_df <- data.frame(Item = item_cols, DeltaLord = rep(0, num_items), p_value = rep(1, num_items), ETSClass = rep("A", num_items), Flag = rep(FALSE, num_items), stringsAsFactors = FALSE)
    ft <- flextable::flextable(dif_df[, c("Item", "DeltaLord", "ETSClass", "Flag")]) |> 
      flextable::set_header_labels(DeltaLord = "MH \u0394") |> 
      flextable::set_caption(paste0("Table ", table_num, ": Mantel-Haenszel DIF Analysis by ", group_col, " (Insufficient Subgroup Data)")) |> 
      flextable::autofit()
    return(list(table = ft, data = dif_df))
  }
  
  ref_group <- names(group_counts)[1]
  foc_group <- names(group_counts)[2]
  
  # Limit joined data just to the two largest groups for binary MH comparison
  joined <- joined[joined[[group_col]] %in% c(ref_group, foc_group), , drop = FALSE]
  
  # Prepare data for difR::difMH
  dif_data <- joined[, item_cols, drop = FALSE]
  dif_data[] <- lapply(dif_data, as.numeric) # ensure pure numeric
  
  # Identify items where everyone got it right (1) or everyone got it wrong (0)
  item_variances <- sapply(dif_data, var, na.rm = TRUE)
  valid_items <- item_cols[item_variances > 0]
  
  # Initialize output vectors with 0s and 1s
  delta_vals <- rep(0, num_items)
  names(delta_vals) <- item_cols
  
  p_vals <- rep(1, num_items)
  names(p_vals) <- item_cols
  
  # Only run DIF if there are valid items to test
  if (length(valid_items) > 0 && requireNamespace("difR", quietly = TRUE)) {
    
    # Subset Data to just valid items + Append the Group Column for difR
    dif_subset <- dif_data[, valid_items, drop = FALSE]
    dif_subset$Group_Var <- ifelse(joined[[group_col]] == ref_group, 0, 1)
    
    mh_results <- tryCatch({
      difR::difMH(Data = dif_subset, group = "Group_Var", focal.name = 1, purify = FALSE)
    }, error = function(e) {
      warning(paste("difR failed:", e$message))
      NULL
    })
    
    if (!is.null(mh_results) && !is.null(mh_results$alphaMH)) {
      # Safely extract and map the results back to the valid items
      for (i in seq_along(valid_items)) {
        itm <- valid_items[i]
        
        # Calculate Delta Lord from MH Alpha: -2.35 * ln(alphaMH)
        alpha_val <- as.numeric(mh_results$alphaMH[i])
        
        # Protect against extreme cases where alpha is exactly 0 or infinite
        if (!is.na(alpha_val) && alpha_val > 0 && is.finite(alpha_val)) {
          delta_vals[itm] <- -2.35 * log(alpha_val)
        } else {
          delta_vals[itm] <- 0 
        }
        
        p_vals[itm] <- as.numeric(mh_results$p.value[i])
      }
    }
  }
  
  # Safety check: replace NA/NaN values with 0/1 BEFORE case_when
  delta_vals[is.na(delta_vals)] <- 0
  p_vals[is.na(p_vals)] <- 1
  
  # Apply ETS Classification Rules for MH D-DIF:
  ets_class <- dplyr::case_when(
    abs(delta_vals) >= 1.5 & p_vals < 0.05 ~ "C",
    abs(delta_vals) >= 1.0 & p_vals < 0.05 ~ "B",
    TRUE ~ "A"
  )
  
  dif_df <- data.frame(
    Item      = item_cols,
    DeltaLord = as.numeric(delta_vals),
    p_value   = as.numeric(p_vals),
    ETSClass  = ets_class,
    Flag      = ets_class == "C",
    stringsAsFactors = FALSE
  )
  
  # Create Table
  ft <- flextable::flextable(dif_df[, c("Item", "DeltaLord", "ETSClass", "Flag")]) |>
    flextable::set_header_labels(
      Item      = "Item",
      DeltaLord = "MH \u0394",
      ETSClass  = "ETS Class",
      Flag      = "Flag"
    ) |>
    flextable::colformat_double(j = "DeltaLord", digits = 3) |>
    flextable::theme_vanilla() |>
    flextable::align(align = "right", part = "body") |>
    flextable::align(j = "Item", align = "left", part = "body") |>
    flextable::align(align = "center", part = "header") |>
    flextable::set_caption(caption = paste0("Table ", table_num, ": Mantel-Haenszel DIF Analysis by ", group_col, " (Ref: ", ref_group, ")")) |>
    flextable::autofit()
  
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
      x     = "MH \u0394 (Mantel-Haenszel)",
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
# Table 13 — DIF Analysis: Mantel-Haenszel (Group 2 — Disadvantaged)
# ------------------------------------------------------------
make_table_13_dif_lord <- function(scored_data, demo_data, group_col = "Disadvantaged") {
  make_table_12_dif_lord(scored_data, demo_data, group_col = group_col, table_num = 13L)
}

# ------------------------------------------------------------
# Table 14 — DIF Analysis: Mantel-Haenszel (Group 3 — Ethnicity)
# ------------------------------------------------------------
make_table_14_dif_lord <- function(scored_data, demo_data, group_col = "Ethnic") {
  make_table_12_dif_lord(scored_data, demo_data, group_col = group_col, table_num = 14L)
}

# ------------------------------------------------------------
# Figure 5 — DIF Magnitude and Direction (Group 2 — Disadvantaged)
# ------------------------------------------------------------
make_figure_05_dif_plot <- function(dif_data, group_name = "Economic Disadvantage") {
  make_figure_04_dif_plot(dif_data, group_name = group_name, fig_num = 5L)
}

# ------------------------------------------------------------
# Figure 6 — DIF Magnitude and Direction (Group 3 — Ethnicity)
# ------------------------------------------------------------
make_figure_06_dif_plot <- function(dif_data, group_name = "Ethnicity") {
  make_figure_04_dif_plot(dif_data, group_name = group_name, fig_num = 6L)
}