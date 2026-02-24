library(flextable)
library(ggplot2)
library(tidyr)

# Fit thresholds used in Table 9 highlighting and Figure 1 annotations
.FIT_LOWER <- 0.5
.FIT_UPPER <- 1.5

# ------------------------------------------------------------
# Table 6 — IRT Model Summary
# Reports Grade, Number of Items, Number of Persons,
# Log-Likelihood, AIC, and BIC from a fitted mirt model.
#
# Arguments:
#   mirt_model  - fitted mirt model object
#   grade       - grade label (integer or character)
#   n_persons   - number of examinees used to fit the model
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_06_irt_summary <- function(mirt_model, grade, n_persons) {
  n_items <- mirt::extract.mirt(mirt_model, "nitems")
  loglik  <- mirt::extract.mirt(mirt_model, "logLik")
  aic     <- mirt::extract.mirt(mirt_model, "AIC")
  bic     <- mirt::extract.mirt(mirt_model, "BIC")

  summary_df <- data.frame(
    Grade     = grade,
    N_Items   = as.integer(n_items),
    N_Persons = as.integer(n_persons),
    LogLik    = loglik,
    AIC       = aic,
    BIC       = bic
  )

  ft <- flextable(summary_df) |>
    set_header_labels(
      Grade     = "Grade",
      N_Items   = "Number of Items",
      N_Persons = "Number of Persons",
      LogLik    = "Log-Likelihood",
      AIC       = "AIC",
      BIC       = "BIC"
    ) |>
    colformat_double(j = c("LogLik", "AIC", "BIC"), digits = 2) |>
    colformat_int(j = c("N_Items", "N_Persons")) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = "Table 6: IRT Model Summary") |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 8 — IRT Item Parameter Estimates
# Reports Item ID, Discrimination (a), Difficulty (b), and
# Standard Error of b (SE_b) from a fitted mirt model.
#
# Arguments:
#   mirt_model  - fitted mirt model object
#
# Returns a flextable.
# ------------------------------------------------------------
make_table_08_irt_params <- function(mirt_model) {
  coef_list  <- mirt::coef(mirt_model, IRTpars = TRUE, printSE = TRUE)
  item_names <- setdiff(names(coef_list), "GroupPars")

  params_df <- do.call(rbind, lapply(item_names, function(item) {
    m <- coef_list[[item]]
    data.frame(
      Item = item,
      a    = m["par", "a1"],
      b    = m["par", "b"],
      SE_b = m["SE",  "b"],
      stringsAsFactors = FALSE
    )
  }))

  ft <- flextable(params_df) |>
    set_header_labels(
      Item = "Item ID",
      a    = "Discrimination (a)",
      b    = "Difficulty (b)",
      SE_b = "SE (b)"
    ) |>
    colformat_double(j = c("a", "b", "SE_b"), digits = 3) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(j = "Item", align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = "Table 8: IRT Item Parameter Estimates") |>
    autofit()

  ft
}

# ------------------------------------------------------------
# Table 9 — Item Fit Statistics
# Reports Infit and Outfit MNSQ for each item. Cells with
# values outside the range [0.5, 1.5] are highlighted in red.
#
# Arguments:
#   mirt_model  - fitted mirt model object
#
# Returns a named list:
#   $data  - data.frame from mirt::itemfit()
#   $table - flextable of item fit statistics
# ------------------------------------------------------------
make_table_09_item_fit <- function(mirt_model) {
  # mirt::itemfit() with fit_stats = "infit" returns both Infit and Outfit
  # columns (they are computed together as complementary Rasch-style indices).
  fit_df <- mirt::itemfit(mirt_model, fit_stats = "infit", na.rm = TRUE)

  ft <- flextable(fit_df) |>
    set_header_labels(
      item   = "Item",
      Infit  = "Infit",
      Outfit = "Outfit"
    ) |>
    colformat_double(j = c("Infit", "Outfit"), digits = 2) |>
    theme_vanilla() |>
    color(
      i = ~ Infit > .FIT_UPPER | Infit < .FIT_LOWER,
      j = "Infit",
      color = "red",
      part = "body"
    ) |>
    color(
      i = ~ Outfit > .FIT_UPPER | Outfit < .FIT_LOWER,
      j = "Outfit",
      color = "red",
      part = "body"
    ) |>
    align(align = "right", part = "body") |>
    align(j = "item", align = "left", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(
      caption = paste0(
        "Table 9: Item Fit Statistics (Infit and Outfit MNSQ; values outside [",
        .FIT_LOWER, ", ", .FIT_UPPER, "] in red)"
      )
    ) |>
    autofit()

  list(data = fit_df, table = ft)
}

# ------------------------------------------------------------
# Figure 1 — Item Infit and Outfit Statistics
# Dot-and-line plot of Infit and Outfit MNSQ per item with
# reference lines at 1.0 (solid) and 0.5 / 1.5 (dashed).
#
# Arguments:
#   fit_data  - data.frame (the $data element from
#               make_table_09_item_fit())
#
# Returns a ggplot.
# ------------------------------------------------------------
make_figure_01_item_fit <- function(fit_data) {
  long_df <- tidyr::pivot_longer(
    fit_data,
    cols      = c("Infit", "Outfit"),
    names_to  = "Statistic",
    values_to = "Value"
  )

  ggplot(long_df, aes(x = item, y = Value, colour = Statistic, group = Statistic)) +
    geom_point(size = 2) +
    geom_line() +
    geom_hline(yintercept = 1.0, linetype = "solid",  colour = "black") +
    geom_hline(yintercept = c(.FIT_LOWER, .FIT_UPPER), linetype = "dashed", colour = "grey50") +
    coord_flip() +
    theme_minimal() +
    labs(
      x       = "Item",
      y       = "Fit Statistic",
      colour  = "Statistic",
      title   = "Figure 1: Item Infit and Outfit Statistics",
      caption = paste0("Dashed lines at ", .FIT_LOWER, " and ", .FIT_UPPER,
                       " indicate the acceptable fit range.")
    )
}

# ------------------------------------------------------------
# Figure 3 — Conditional Standard Error of Measurement (CSEM)
# Plots CSEM (1 / sqrt(test information)) across the theta scale.
#
# Arguments:
#   mirt_model  - fitted mirt model object
#
# Returns a ggplot.
# ------------------------------------------------------------
make_figure_03_csem <- function(mirt_model) {
  theta_seq <- seq(-4, 4, length.out = 100)
  Theta     <- matrix(theta_seq, ncol = 1)
  info      <- mirt::testinfo(mirt_model, Theta)
  csem      <- 1 / sqrt(info)

  csem_df <- data.frame(Theta = theta_seq, CSEM = csem)

  ggplot(csem_df, aes(x = Theta, y = CSEM)) +
    geom_line() +
    theme_minimal() +
    labs(
      x     = "Theta",
      y     = "Conditional SEM",
      title = "Figure 3: Conditional Standard Error of Measurement"
    )
}
