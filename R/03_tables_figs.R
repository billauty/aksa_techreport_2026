library(flextable)
library(ggplot2)
library(dplyr)
library(tidyr)
library(officer)

# ------------------------------------------------------------
# 1. Native Word Table for Item Statistics
# Translates your old `table_01_item_stats.R` and `render_table_pdf.R`
# ------------------------------------------------------------
make_item_stats_table <- function(item_stats_df) {
  
  # Ensure expected columns exist
  stopifnot(all(c("item", "n", "mean", "sd", "se") %in% names(item_stats_df)))
  
  # Create a highly styled Word table that doesn't need an image wrapper
  ft <- flextable(item_stats_df) |>
    set_header_labels(
      item = "Item ID",
      n = "N Responses",
      mean = "P-Value (Mean)",
      sd = "Std. Dev.",
      se = "Std. Error"
    ) |>
    colformat_double(j = c("mean", "sd", "se"), digits = 2) |>
    colformat_int(j = "n") |>
    theme_vanilla() |>  # This replaces kableExtra's "booktabs" look
    align(align = "center", part = "all") |>
    align(j = "item", align = "left", part = "all") |>
    set_caption(caption = "Item Statistics (Classical Test Theory)") |>
    autofit()
  
  # Optional: Replicate kableExtra's "striped" effect
  ft <- bg(ft, i = ~ row_number() %% 2 == 0, bg = "#F5F5F5", part = "body")
  
  return(ft)
}

# ------------------------------------------------------------
# 2. Reusable ggplot for Item Fit (From `figure_02_item_fit.R`)
# Kept exactly the same, as ggplot is naturally supported.
# ------------------------------------------------------------
make_item_fit_plot <- function(item_fit_tbl) {
  
  stopifnot(all(c("item", "Infit", "Outfit") %in% names(item_fit_tbl)))
  
  p <- item_fit_tbl |>
    tidyr::pivot_longer(
      cols = c(Infit, Outfit),
      names_to = "Statistic",
      values_to = "Value"
    ) |>
    ggplot(aes(x = reorder(item, Value), y = Value, colour = Statistic, group = Statistic)) +
    geom_point(size = 2) +
    geom_line() +
    geom_hline(yintercept = 1, colour = "black") +
    geom_hline(yintercept = c(0.5, 1.5), linetype = "dashed", colour = "grey50") +
    coord_flip() +
    theme_minimal() +
    labs(
      x = NULL,
      y = "Fit Statistic",
      title = "Item Infit and Outfit Statistics",
      caption = "Values between 0.5 and 1.5 are typically considered productive for measurement."
    )
  
  return(p)
}

# ------------------------------------------------------------
# 3. Content Builder (Orchestrator for the Pipeline)
# ------------------------------------------------------------
build_test_content <- function(appendix_obj) {
  # This function gets mapped over all tests in _targets.R
  
  # 1. Build the Table
  item_table <- make_item_stats_table(appendix_obj$ctt$item_stats)
  
  # 2. Build the Figure
  fit_plot <- make_item_fit_plot(appendix_obj$irt$fit_stats)
  
  # Return a bundle ready for `officer`
  list(
    test_id = appendix_obj$meta$test,
    test_name = paste(appendix_obj$meta$subject, "Grade", appendix_obj$meta$grade),
    item_stats_table = item_table,
    item_fit_plot = fit_plot,
    
    # Pre-generate dynamic alt-text for accessibility
    fit_plot_alt = paste(
      "Dot and line plot showing Infit and Outfit statistics for", 
      nrow(appendix_obj$irt$fit_stats), "items on the", 
      appendix_obj$meta$test, "test. The acceptable range between 0.5 and 1.5 is marked."
    )
  )
}