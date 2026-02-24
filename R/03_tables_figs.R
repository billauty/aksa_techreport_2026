library(flextable)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(officer)
library(mirt)

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

# Build all tables and figures for one test from model results.
#
# Arguments:
#   model_results  - list returned by fit_models(); expected to contain at
#                    minimum a fitted mirt model object, CTT item statistics,
#                    scored response matrices, and examinee metadata.
#
# Returns a named list with two elements:
#   $tables  - named list of flextable objects (tbl_01 … tbl_17)
#   $figures - named list of lists, each with $plot (ggplot) and $alt_text
#              (character); fig_01 … fig_11
build_test_content <- function(model_results) {

  # ── TABLES ────────────────────────────────────────────────────────────────

  # Table 1 — Test Administration Summary
  # Inputs : scored_data columns: grade, content_area, form_id
  # Package: flextable (summary from dplyr::count())
  # TODO: implement tbl_01
  tbl_01 <- flextable::flextable(data.frame())

  # Table 2 — Examinee Demographics
  # Inputs : scored_data demographic flag columns
  # Package: flextable (formatted from dplyr::summarise())
  # TODO: implement tbl_02
  tbl_02 <- flextable::flextable(data.frame())

  # Table 3 — Score Descriptive Statistics
  # Inputs : scored_data$scale_score grouped by test_id
  # Package: flextable; descriptives via psych::describe()
  # TODO: implement tbl_03
  tbl_03 <- flextable::flextable(data.frame())

  # Table 4 — Score Descriptive Statistics by Subgroup
  # Inputs : scored_data$scale_score x demographic flags
  # Package: flextable (from dplyr::group_by() + summarise())
  # TODO: implement tbl_04
  tbl_04 <- flextable::flextable(data.frame())

  # Table 5 — Internal Consistency Reliability
  # Inputs : item-level scored response matrix
  # Package: CTT::reliability() for alpha and SEM; flextable
  # TODO: implement tbl_05
  tbl_05 <- flextable::flextable(data.frame())

  # Table 6 — Conditional Standard Error of Measurement
  # Inputs : item-level response matrix
  # Package: CTT::cSEM() for binomial SEM; flextable
  # TODO: implement tbl_06
  tbl_06 <- flextable::flextable(data.frame())

  # Table 7 — Item Difficulty Statistics (CTT)
  # Inputs : item-level binary response matrix
  # Package: CTT::itemAnalysis() -> flextable
  # TODO: implement tbl_07
  tbl_07 <- flextable::flextable(data.frame())

  # Table 8 — Item Discrimination Statistics (CTT)
  # Inputs : item-level binary response matrix
  # Package: CTT::itemAnalysis() -> flextable
  # TODO: implement tbl_08
  tbl_08 <- flextable::flextable(data.frame())

  # Table 9 — Distractor Analysis
  # Inputs : raw (unscored) nominal response matrix from raw_data
  # Package: CTT::distractorAnalysis() -> flextable
  # TODO: implement tbl_09
  tbl_09 <- flextable::flextable(data.frame())

  # Table 10 — IRT Item Parameter Estimates
  # Inputs : model_results mirt model object
  # Package: mirt::coef() -> flextable
  # TODO: implement tbl_10
  tbl_10 <- flextable::flextable(data.frame())

  # Table 11 — IRT Model-Data Fit Statistics
  # Inputs : model_results mirt model object
  # Package: mirt::itemfit() -> flextable
  # TODO: implement tbl_11
  tbl_11 <- flextable::flextable(data.frame())

  # Table 12 — Differential Item Functioning (DIF) Summary
  # Inputs : item-level responses + focal/reference group indicator
  # Package: difR::difMH() or mirt::DIF() -> flextable
  # TODO: implement tbl_12
  tbl_12 <- flextable::flextable(data.frame())

  # Table 13 — Raw-to-Scale Score Conversion Table
  # Inputs : equating/scaling parameters from model_results; score range metadata
  # Package: mirt::fscores() or linear equating; flextable
  # TODO: implement tbl_13
  tbl_13 <- flextable::flextable(data.frame())

  # Table 14 — Performance Level Cut Scores
  # Inputs : external cut-score specifications + scale parameters from model_results
  # Package: flextable
  # TODO: implement tbl_14
  tbl_14 <- flextable::flextable(data.frame())

  # Table 15 — Classification Accuracy and Consistency
  # Inputs : IRT ability estimates and cut scores from model_results
  # Package: mirt::classify() or custom computation; flextable
  # TODO: implement tbl_15
  tbl_15 <- flextable::flextable(data.frame())

  # Table 16 — Content-Domain (Subscore) Statistics
  # Inputs : scored_data with item-to-strand mapping; item-level responses
  # Package: CTT::reliability() per strand subset; flextable
  # TODO: implement tbl_16
  tbl_16 <- flextable::flextable(data.frame())

  # Table 17 — Item Pool Summary
  # Inputs : item metadata (strand, Depth of Knowledge level) joined to scored_data
  # Package: flextable (from dplyr::count(strand, dok_level))
  # TODO: implement tbl_17
  tbl_17 <- flextable::flextable(data.frame())

  # ── FIGURES ───────────────────────────────────────────────────────────────

  # Figure 1 — Scale Score Distribution Histogram
  # Inputs : scored_data$scale_score grouped by test_id
  # Package: ggplot2 (geom_histogram() + stat_function())
  # TODO: implement fig_01
  fig_01 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Histogram of scale scores with normal-curve overlay."
  )

  # Figure 2 — Score Distribution by Performance Level
  # Inputs : scored_data$performance_level
  # Package: ggplot2 (geom_bar(position = "fill"))
  # TODO: implement fig_02
  fig_02 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Bar chart showing percentage of examinees at each performance level."
  )

  # Figure 3 — Item Difficulty Distribution
  # Inputs : p-values from CTT::itemAnalysis()
  # Package: ggplot2
  # TODO: implement fig_03
  fig_03 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Histogram of item p-values with reference lines at 0.2 and 0.8."
  )

  # Figure 4 — Item Discrimination Distribution
  # Inputs : point-biserial correlations from CTT::itemAnalysis()
  # Package: ggplot2
  # TODO: implement fig_04
  fig_04 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Histogram of item point-biserial correlations with reference line at 0.2."
  )

  # Figure 5 — Item Characteristic Curves (ICC)
  # Inputs : fitted mirt model from model_results
  # Package: mirt::probtrace() -> ggplot2
  # TODO: implement fig_05
  fig_05 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Item characteristic curves showing probability of correct response across the theta range."
  )

  # Figure 6 — Test Information Function (TIF)
  # Inputs : fitted mirt model from model_results
  # Package: mirt::testinfo() -> ggplot2
  # TODO: implement fig_06
  fig_06 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Test information function and conditional SEM across the theta range."
  )

  # Figure 7 — Person-Item Map (Wright Map)
  # Inputs : mirt::fscores() ability estimates and item b parameters
  # Package: WrightMap::wrightMap()
  # TODO: implement fig_07
  fig_07 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Wright Map showing side-by-side distributions of person ability and item difficulty."
  )

  # Figure 8 — DIF Item Scatterplot
  # Inputs : group-specific p-values or b parameters + DIF flag from tbl_12
  # Package: ggplot2 (geom_point() + ggrepel::geom_label_repel())
  # TODO: implement fig_08
  fig_08 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Scatterplot of item difficulty by group with DIF-flagged items annotated."
  )

  # Figure 9 — Scree Plot / Parallel Analysis
  # Inputs : item-level scored response matrix
  # Package: psych::fa.parallel() or ggplot2 with base::eigen()
  # TODO: implement fig_09
  fig_09 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Scree plot with parallel-analysis reference line for dimensionality assessment."
  )

  # Figure 10 — Expected Score Curve
  # Inputs : fitted mirt model from model_results
  # Package: mirt::expected.test() -> ggplot2
  # TODO: implement fig_10
  fig_10 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Expected total test score as a function of theta."
  )

  # Figure 11 — Subgroup Score Distribution Comparison
  # Inputs : scored_data$scale_score and demographic flag columns
  # Package: ggplot2 (geom_density() + geom_vline())
  # TODO: implement fig_11
  fig_11 <- list(
    plot     = ggplot2::ggplot(),
    alt_text = "Overlapping kernel-density plots of scale scores by demographic subgroup."
  )

  # ── RETURN ────────────────────────────────────────────────────────────────

  list(
    tables = list(
      tbl_01 = tbl_01, tbl_02 = tbl_02, tbl_03 = tbl_03,
      tbl_04 = tbl_04, tbl_05 = tbl_05, tbl_06 = tbl_06,
      tbl_07 = tbl_07, tbl_08 = tbl_08, tbl_09 = tbl_09,
      tbl_10 = tbl_10, tbl_11 = tbl_11, tbl_12 = tbl_12,
      tbl_13 = tbl_13, tbl_14 = tbl_14, tbl_15 = tbl_15,
      tbl_16 = tbl_16, tbl_17 = tbl_17
    ),
    figures = list(
      fig_01 = fig_01, fig_02 = fig_02, fig_03 = fig_03,
      fig_04 = fig_04, fig_05 = fig_05, fig_06 = fig_06,
      fig_07 = fig_07, fig_08 = fig_08, fig_09 = fig_09,
      fig_10 = fig_10, fig_11 = fig_11
    )
  )
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

# ------------------------------------------------------------
# Table 6 — IRT Model Summary
# Reports test-level IRT summary statistics: grade, N, number of
# items, model type, and fit statistics.
#
# Arguments:
#   mirt_model  - fitted mirt model object
#   grade       - integer grade level
#   n_persons   - integer number of examinees
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_06_irt_summary <- function(mirt_model, grade, n_persons) {
  n_items    <- extract.mirt(mirt_model, "nitems")
  model_type <- paste(unique(extract.mirt(mirt_model, "itemtype")), collapse = ", ")
  
  # Extract fit statistics directly instead of calculating empirical reliability
  log_lik <- extract.mirt(mirt_model, "logLik")
  aic <- extract.mirt(mirt_model, "AIC")
  bic <- extract.mirt(mirt_model, "BIC")
  
  summary_df <- data.frame(
    Grade           = grade,
    N               = n_persons,
    Items           = n_items,
    Model           = model_type,
    LogLikelihood   = log_lik,
    AIC             = aic,
    BIC             = bic,
    stringsAsFactors = FALSE
  )
  
  big_b <- fp_border(color = "black", width = 1.5)
  std_b <- fp_border(color = "black", width = 1)

  ft <- flextable(summary_df) |>
    set_header_labels(
      Grade         = "Grade",
      N             = "N",
      Items         = "No. Items",
      Model         = "IRT Model",
      LogLikelihood = "Log-Likelihood",
      AIC           = "AIC",
      BIC           = "BIC"
    ) |>
    colformat_int(j = c("Grade", "N", "Items")) |>
    colformat_double(j = c("LogLikelihood", "AIC", "BIC"), digits = 2) |>
    border_remove() |>
    hline_top(part = "header", border = big_b) |>
    hline_bottom(part = "header", border = std_b) |>
    hline_bottom(part = "body", border = big_b) |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = "Model", align = "left", part = "all") |>
    set_caption(caption = "Table 6: IRT Model Summary",
                fp_p = fp_par(text.align = "left")) |>
    autofit()

  return(ft)
}

# ------------------------------------------------------------
# Table 8 — IRT Item Parameter Estimates
# Reports per-item IRT parameters (a, b, g) from the calibrated
# mirt model.
#
# Arguments:
#   mirt_model  - fitted mirt model object
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_08_irt_params <- function(mirt_model) {
  params <- coef(mirt_model, IRTpars = TRUE, simplify = TRUE)$items
  params_df <- as.data.frame(params)
  params_df$Item <- rownames(params_df)
  rownames(params_df) <- NULL

  # Keep only columns that exist in this model
  keep_cols <- c("Item", intersect(c("a", "b", "g", "u"), names(params_df)))
  params_df <- params_df[, keep_cols, drop = FALSE]

  col_labels <- c(Item = "Item", a = "Discrimination (a)",
                  b = "Difficulty (b)", g = "Guessing (g)", u = "Upper Asymptote (u)")
  col_labels <- col_labels[names(col_labels) %in% names(params_df)]

  big_b <- fp_border(color = "black", width = 1.5)
  std_b <- fp_border(color = "black", width = 1)

  ft <- flextable(params_df) |>
    set_header_labels(.list = as.list(col_labels)) |>
    colformat_double(
      j = intersect(c("a", "b", "g", "u"), names(params_df)),
      digits = 3
    ) |>
    border_remove() |>
    hline_top(part = "header", border = big_b) |>
    hline_bottom(part = "header", border = std_b) |>
    hline_bottom(part = "body", border = big_b) |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = "Item", align = "left", part = "all") |>
    set_caption(caption = "Table 8: IRT Item Parameter Estimates",
                fp_p = fp_par(text.align = "left")) |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Figure 3 — Conditional Standard Error of Measurement (CSEM)
# Plots the CSEM as a function of the latent trait (theta).
#
# Arguments:
#   mirt_model  - fitted mirt model object
#
# Returns a ggplot.
# ------------------------------------------------------------
make_figure_03_csem <- function(mirt_model) {
  theta_seq  <- seq(-4, 4, by = 0.1)
  info_vals  <- testinfo(mirt_model, Theta = matrix(theta_seq))
  csem_vals  <- 1 / sqrt(info_vals)

  plot_df <- data.frame(theta = theta_seq, CSEM = csem_vals)

  ggplot(plot_df, aes(x = theta, y = CSEM)) +
    geom_line(linewidth = 1) +
    labs(
      x     = expression(theta ~ "(Latent Trait)"),
      y     = "Conditional SEM",
      title = "Figure 3: Conditional Standard Error of Measurement"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Figure 2 — Wright Map (Person-Item Map)
# Displays student ability estimates and item difficulty estimates
# on the same logit scale.  Item difficulties are shown as labelled
# points; student abilities are shown as a histogram.
#
# Arguments:
#   mirt_model   - fitted mirt model object (SingleGroupClass)
#   scored_resp  - data.frame of scored item responses as returned by
#                  score_test_responses(); used to obtain person ability
#                  estimates via mirt::fscores().
#
# Returns a ggplot.
# ------------------------------------------------------------
make_figure_02_wright_map <- function(mirt_model, scored_resp) {
  # ── Item difficulties ──────────────────────────────────────────────────────
  params    <- mirt::coef(mirt_model, IRTpars = TRUE, simplify = TRUE)$items
  item_b    <- params[, "b"]
  items_df  <- data.frame(
    item_id    = names(item_b),
    difficulty = as.numeric(item_b),
    stringsAsFactors = FALSE
  )

  # ── Person ability estimates ───────────────────────────────────────────────
  item_cols   <- setdiff(names(scored_resp)[sapply(scored_resp, is.numeric)], "SSID")
  resp_matrix <- as.matrix(scored_resp[, item_cols, drop = FALSE])
  theta_est   <- as.numeric(
    mirt::fscores(mirt_model, response.pattern = resp_matrix, verbose = FALSE)[, "F1"]
  )
  persons_df  <- data.frame(theta = theta_est)

  # ── Plot ───────────────────────────────────────────────────────────────────
  ggplot() +
    # Person ability histogram (semi-transparent, top half)
    geom_histogram(
      data    = persons_df,
      mapping = aes(x = theta, y = after_stat(count)),
      binwidth = 0.25,
      fill    = "steelblue",
      alpha   = 0.50,
      colour  = "white"
    ) +
    # Item difficulty points on a rug at the bottom
    # Shape 124 is the ASCII vertical-bar character (|), used as a tick mark.
    geom_point(
      data    = items_df,
      mapping = aes(x = difficulty, y = 0),
      shape   = 124,
      size    = 6,
      colour  = "tomato"
    ) +
    ggrepel::geom_text_repel(
      data    = items_df,
      mapping = aes(x = difficulty, y = 0, label = item_id),
      size    = 3,
      colour  = "tomato",
      nudge_y = 2,
      segment.size = 0.3
    ) +
    labs(
      x       = "Logit Scale (\u03b8 / Item Difficulty)",
      y       = "Number of Students",
      title   = "Figure 2: Wright Map — Student Ability and Item Difficulty",
      caption = paste0(
        "Blue histogram = student ability estimates. ",
        "Red ticks/labels = item difficulty (b) parameters."
      )
    ) +
    theme_minimal() +
    theme(plot.caption = element_text(size = 8, hjust = 0))
}