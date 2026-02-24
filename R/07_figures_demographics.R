library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# ------------------------------------------------------------
# Figures 7–10 — Learner Characteristics Boxplots
# Displays total raw score distributions across categories of a
# single Learner Characteristics Inventory (LCI) trait.
#
# Arguments:
#   scored_data  - data.frame with an SSID column and binary item
#                  columns (0/1); total raw score is computed here.
#   lci_data     - data.frame loaded from
#                  Learner_Characteristics_Inventory_24_25.xlsx;
#                  must contain an SSID column (may need zero-padding)
#                  and a column whose name matches `trait`.
#   trait        - character; name of the LCI trait column to plot on
#                  the x-axis (e.g., "Expressive Communication").
#
# Returns a ggplot.
# ------------------------------------------------------------
make_figures_07_10_learner_characteristics <- function(scored_data, lci_data, trait) {
  # Width used to zero-pad SSID values in LCI data to match scored_data format
  SSID_PADDING_WIDTH <- 10L

  # Identify binary item columns (all numeric columns except SSID)
  item_cols <- setdiff(
    names(scored_data)[sapply(scored_data, is.numeric)],
    "SSID"
  )

  # Compute total raw score per student
  scored_data <- scored_data |>
    dplyr::mutate(
      TotalRawScore = rowSums(dplyr::pick(dplyr::all_of(item_cols)),
                              na.rm = TRUE)
    )

  # Zero-pad SSID in LCI data so it matches scored_data format
  lci_data <- lci_data |>
    dplyr::mutate(
      SSID = stringr::str_pad(as.character(SSID), SSID_PADDING_WIDTH, pad = "0")
    )

  # Join on SSID, keeping only rows present in both data sets
  joined <- dplyr::inner_join(
    scored_data[, c("SSID", "TotalRawScore")],
    lci_data[, c("SSID", trait)],
    by = "SSID"
  )

  # Drop rows with NA in the selected trait
  joined <- joined[!is.na(joined[[trait]]), , drop = FALSE]

  # Build N-count labels and join back to avoid deprecated dplyr::recode()
  n_counts <- joined |>
    dplyr::count(.data[[trait]]) |>
    dplyr::mutate(
      trait_label = stringr::str_wrap(
        paste0(.data[[trait]], "\n(n = ", n, ")"),
        width = 20
      )
    )

  joined <- dplyr::left_join(joined, n_counts[, c(trait, "trait_label")], by = trait)

  ggplot(joined, aes(x = trait_label, y = TotalRawScore, fill = trait_label)) +
    geom_boxplot(colour = "grey30", outlier.shape = 21,
                 outlier.size = 1.5, alpha = 0.75) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      x       = stringr::str_wrap(trait, width = 40),
      y       = "Total Raw Score",
      title   = paste0("Score Distribution by ", trait),
      caption = paste0("Boxes show median, IQR, and 1.5\u00d7IQR whiskers. ",
                       "N-counts in x-axis labels.")
    ) +
    theme_minimal() +
    theme(
      legend.position  = "none",
      axis.text.x      = element_text(size = 9),
      plot.caption     = element_text(size = 8, hjust = 0)
    )
}

# ------------------------------------------------------------
# Figure 11 — Anchor Item Drift Plot
# Displays robust-Z drift statistics for anchor items, coloured
# by stability flag, with significance-threshold reference lines.
#
# Arguments:
#   drift_df  - tibble / data.frame with columns:
#                 item_id   - character item identifier
#                 robust_z  - numeric robust-Z drift statistic
#                 unstable  - logical; TRUE when item is flagged
#               Pass NULL (or an empty data.frame) for Writing tests
#               that have no anchor items — the function returns NULL.
#   test_id   - character test identifier used in the plot title
#               (e.g., "RD_04").
#
# Returns a ggplot, or NULL when drift_df is NULL / empty.
# ------------------------------------------------------------
make_figure_11_anchor_drift <- function(drift_df, test_id) {
  # Return NULL early for Writing tests (no anchor items)
  if (is.null(drift_df) || nrow(drift_df) == 0) {
    return(NULL)
  }

  # Order items alphabetically on the y-axis
  drift_df <- drift_df |>
    dplyr::mutate(item_id = factor(item_id, levels = sort(unique(item_id))))

  ggplot(drift_df, aes(x = robust_z, y = item_id, colour = unstable)) +
    geom_vline(xintercept = c(-1.96, 1.96),
               linetype = "dashed", colour = "grey50", linewidth = 0.6) +
    geom_vline(xintercept = 0,
               linetype = "solid", colour = "grey70", linewidth = 0.4) +
    geom_point(size = 3) +
    scale_colour_manual(
      name   = "Item Status",
      values = c("FALSE" = "grey30", "TRUE" = "#C0392B"),
      labels = c("FALSE" = "Stable", "TRUE" = "Unstable (flagged)")
    ) +
    labs(
      x       = "Robust Z",
      y       = "Item ID",
      title   = paste0("Figure 11: Anchor Item Drift \u2014 ", test_id),
      caption = paste0("Dashed lines at \u00b11.96. Points in red exceed the drift threshold.")
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.caption    = element_text(size = 8, hjust = 0)
    )
}
