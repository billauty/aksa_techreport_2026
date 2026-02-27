library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsignif)
library(stringr)

# ------------------------------------------------------------
# Figures 7–10 — Learner Characteristics Boxplots
# Displays Scale Score distributions across categories of 4
# Learner Characteristics Inventory (LCI) traits, annotated
# with pairwise Wilcoxon test p-values.
# ------------------------------------------------------------
make_figures_07_10_learner_characteristics <- function(scored_data, lci_data, raw_data, test_id) {
  
  # 1. Resolve scale-score column and grade from test_id
  subject <- sub("_.*$", "", test_id)
  
  score_var <- paste0(subject, "_Scale_Score")
  
  # If the specific score variable isn't in the data, default to using the raw scores
  use_raw_fallback <- !(score_var %in% names(raw_data))
  
  # 2. Extract secure scores
  if (!use_raw_fallback) {
    secure_scores <- raw_data |>
      dplyr::transmute(
        SSID  = as.character(SSID),
        score = as.numeric(.data[[score_var]])
      )
  } else {
    # Fallback to computing total raw score if Scale Score isn't available
    item_cols <- setdiff(names(scored_data)[sapply(scored_data, is.numeric)], "SSID")
    secure_scores <- scored_data |>
      dplyr::mutate(
        SSID = as.character(SSID),
        score = rowSums(dplyr::pick(dplyr::all_of(item_cols)), na.rm = TRUE)
      ) |>
      dplyr::select(SSID, score)
  }
  
  # 3. Clean LCI Data and pad SSIDs to match
  names(lci_data) <- gsub(" ", ".", names(lci_data))
  
  req_cols <- c("Expressive.Communication", "Receptive.Language", "Reading", "Mathematics")
  if (!all(req_cols %in% names(lci_data))) {
    return(list(NULL, NULL, NULL, NULL))
  }
  
  lci_data <- lci_data |>
    dplyr::mutate(SSID = stringr::str_pad(as.character(SSID), 10L, pad = "0")) |>
    dplyr::select(SSID, dplyr::all_of(req_cols))
  
  # 4. Build long dataset 
  lc_long <- lci_data |>
    tidyr::pivot_longer(
      cols = -SSID,
      names_to  = "LearnerCharacteristic",
      values_to = "value"
    ) |>
    dplyr::inner_join(secure_scores, by = "SSID") |>
    dplyr::filter(
      !is.na(value),
      !is.na(score)
    ) |>
    dplyr::mutate(
      LearnerCharacteristic = factor(LearnerCharacteristic),
      value = factor(value)
    )
  
  # 5. Plotting function with 2025 statistical logic
  make_boxplot <- function(dat, trait_name) {
    
    # Drop extreme 1% tails
    dat <- dat |>
      dplyr::group_by(value) |>
      dplyr::filter(
        score >= quantile(score, 0.01, na.rm = TRUE),
        score <= quantile(score, 0.99, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
    
    # Pairwise Wilcoxon Test
    pw <- pairwise.wilcox.test(dat$score, dat$value, p.adjust.method = "none")
    
    pw_tbl <- as.data.frame(as.table(pw$p.value)) |>
      dplyr::filter(!is.na(Freq)) |>
      dplyr::rename(g1 = Var1, g2 = Var2, p = Freq)
    
    # Filter significant
    alpha <- 0.05
    pw_sig <- pw_tbl |> dplyr::filter(p < alpha)
    
    y_max  <- max(dat$score, na.rm = TRUE)
    y_span <- diff(range(dat$score, na.rm = TRUE))
    
    y_lower <- floor(quantile(dat$score, 0.02, na.rm = TRUE))
    y_upper <- ceiling(
      if (nrow(pw_sig) > 0)
        max(y_max + seq_len(nrow(pw_sig)) * (0.05 * y_span))
      else y_max
    )
    
    # Add N-counts to x-axis labels
    n_counts <- dat |> dplyr::count(value)
    dat <- dat |> dplyr::left_join(n_counts, by = "value") |>
      dplyr::mutate(value_label = paste0(value, "\n(n = ", n, ")"))
    
    # Update significance table with the new labels
    if (nrow(pw_sig) > 0) {
      pw_sig$g1_label <- n_counts$n[match(pw_sig$g1, n_counts$value)]
      pw_sig$g2_label <- n_counts$n[match(pw_sig$g2, n_counts$value)]
      pw_sig$g1_full <- paste0(pw_sig$g1, "\n(n = ", pw_sig$g1_label, ")")
      pw_sig$g2_full <- paste0(pw_sig$g2, "\n(n = ", pw_sig$g2_label, ")")
    }
    
    p <- ggplot(dat, aes(x = value_label, y = score)) +
      geom_boxplot(
        outlier.shape = NA,
        varwidth = TRUE,
        linewidth = 0.4,
        fill = "lightblue",
        alpha = 0.6
      ) +
      labs(
        title = paste("Score Distribution by", gsub("\\.", " ", trait_name)),
        x = "Category",
        y = if(use_raw_fallback) "Total Raw Score" else "Scale Score"
      ) +
      scale_y_continuous(
        limits = c(y_lower, y_upper),
        expand = c(0, 0)
      ) +
      theme_minimal(base_size = 10) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
    
    # Add bracket annotations for significant differences
    if (nrow(pw_sig) > 0) {
      pw_sig <- pw_sig |>
        dplyr::arrange(g1_full, g2_full) |>
        dplyr::mutate(
          xmin = g1_full,
          xmax = g2_full,
          annotations = paste0("p = ", formatC(p, digits = 3, format = "f")),
          y_position = y_max + seq_len(dplyr::n()) * (0.04 * y_span)
        )
      
      p <- p +
        ggsignif::geom_signif(
          data = pw_sig,
          aes(xmin = xmin, xmax = xmax,
              annotations = annotations,
              y_position = y_position),
          manual = TRUE,
          textsize = 3.5,
          vjust = 0.3,
          tip_length = 0.01
        )
    } else {
      p <- p +
        annotate(
          "text",
          x = mean(seq_along(unique(dat$value_label))),
          y = y_upper - 0.05 * y_span,
          label = "No statistically significant pairwise differences (α = 0.05)",
          size = 3.5,
          fontface = "italic"
        )
    }
    
    return(p)
  }
  
  # 6. Build and return the 4 plots directly as an unnamed list
  # R/04_build_report.R expects exactly 4 list elements mapped 1:1 to indices 1:4
  traits <- c("Expressive.Communication", "Receptive.Language", "Reading", "Mathematics")
  plots <- lapply(traits, function(t) {
    sub_dat <- lc_long |> dplyr::filter(LearnerCharacteristic == t)
    if(nrow(sub_dat) == 0) return(NULL)
    make_boxplot(sub_dat, t)
  })
  
  return(plots)
}