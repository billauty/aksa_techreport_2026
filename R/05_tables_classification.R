library(flextable)

# ------------------------------------------------------------
# Table 7 — Raw Score to Theta (IRT Lookup Table)
# ------------------------------------------------------------
make_table_07_raw_to_theta <- function(mirt_model, test_id = "") {
  n_items <- mirt::extract.mirt(mirt_model, "nitems")
  raw_scores  <- 0:n_items
  item_names  <- mirt::extract.mirt(mirt_model, "itemnames")
  
  theta_vals <- numeric(length(raw_scores))
  sem_vals   <- numeric(length(raw_scores))
  
  for (i in seq_along(raw_scores)) {
    r <- raw_scores[i]
    pattern <- matrix(c(rep(1L, r), rep(0L, n_items - r)),
                      nrow = 1, dimnames = list(NULL, item_names))
    fs <- mirt::fscores(mirt_model, response.pattern = pattern, full.scores.SE = TRUE, verbose = FALSE)
    theta_vals[i] <- as.numeric(fs[1, "F1"])
    sem_vals[i]   <- as.numeric(fs[1, "SE_F1"])
  }
  
  lookup_df <- data.frame(
    `Raw Score`      = raw_scores,
    `Theta (Logit)`  = round(theta_vals, 2),
    `SEM`            = round(sem_vals,   2),
    check.names      = FALSE
  )
  
  caption_text <- if (nchar(test_id) > 0) paste0("Table 7: Raw Score to Theta Conversion (", test_id, ")") else "Table 7: Raw Score to Theta Conversion"
  
  ft <- flextable(lookup_df) |>
    set_header_labels(`Raw Score` = "Raw Score", `Theta (Logit)` = "Theta (Logit)", `SEM` = "SEM") |>
    colformat_int(j = "Raw Score") |>
    colformat_double(j = c("Theta (Logit)", "SEM"), digits = 2) |>
    theme_vanilla() |>
    align(align = "right", part = "body") |>
    align(align = "center", part = "header") |>
    set_caption(caption = caption_text) |>
    autofit()
  ft
}

# ------------------------------------------------------------
# Internal Helper: Rudner (2001, 2005) Classification Indices
# ------------------------------------------------------------
.calc_rudner_indices <- function(mirt_model, scored_resp, cut_scores) {
  valid_cuts <- sort(na.omit(as.numeric(cut_scores)))
  if (length(valid_cuts) == 0) return(NULL)
  
  item_cols <- setdiff(names(scored_resp)[sapply(scored_resp, is.numeric)], c("SSID", "complete"))
  resp_mat  <- as.matrix(scored_resp[, item_cols, drop = FALSE])
  fs <- mirt::fscores(mirt_model, response.pattern = resp_mat, full.scores.SE = TRUE, verbose = FALSE)
  
  thetas <- as.numeric(fs[, "F1"])
  ses    <- as.numeric(fs[, "SE_F1"])
  
  bounds <- c(-Inf, valid_cuts, Inf)
  k <- length(bounds) - 1
  
  # 1. P_mat: N x K matrix of probabilities that each student's true theta is in level k
  P_mat <- matrix(0, nrow = length(thetas), ncol = k)
  for (i in 1:k) {
    P_mat[, i] <- pnorm(bounds[i+1], mean = thetas, sd = ses) - pnorm(bounds[i], mean = thetas, sd = ses)
  }
  
  # 2. Observed category for each student
  # right = FALSE ensures [lower, upper) grouping. Scoring exact cut = next level.
  obs_cat <- as.integer(cut(thetas, breaks = bounds, right = FALSE, include.lowest = TRUE))
  
  # 3. Classification Accuracy Matrix (Observed x True)
  CA_mat <- matrix(0, nrow = k, ncol = k)
  for (i in 1:k) {
    idx <- which(obs_cat == i)
    if (length(idx) > 0) CA_mat[i, ] <- colSums(P_mat[idx, , drop = FALSE])
  }
  N <- length(thetas)
  CA_mat <- CA_mat / N
  
  # 4. Decision Consistency Matrix (Test 1 x Test 2)
  DC_mat <- t(P_mat) %*% P_mat / N
  
  list(CA_mat = CA_mat, DC_mat = DC_mat, k = k)
}

# ------------------------------------------------------------
# Table 15 — Classification Accuracy (2-Category)
# ------------------------------------------------------------
make_table_15_class_accuracy <- function(mirt_model, scored_resp, napd_cuts) {
  # Extract ONLY the Proficient cut score (the second value in the 3-cut sequence)
  prof_cut <- sort(as.numeric(na.omit(napd_cuts)))[2]
  
  res <- .calc_rudner_indices(mirt_model, scored_resp, prof_cut)
  if (is.null(res)) return(flextable::flextable(data.frame(Note="No cut scores available")))
  
  CA <- res$CA_mat
  df <- data.frame(
    Observed = c("Observed: Not Proficient", "Observed: Proficient", "Column Total"),
    True_Not = c(CA[1,1], CA[2,1], sum(CA[,1])),
    True_Pro = c(CA[1,2], CA[2,2], sum(CA[,2])),
    Total    = c(sum(CA[1,]), sum(CA[2,]), 1.000)
  )
  
  accuracy <- CA[1,1] + CA[2,2]
  
  ft <- flextable::flextable(df) |>
    flextable::set_header_labels(Observed = "Observed Status", True_Not = "True: Not Proficient", True_Pro = "True: Proficient", Total = "Row Total") |>
    flextable::colformat_double(j = 2:4, digits = 3) |>
    flextable::theme_vanilla() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::set_caption("Table 15: Classification Accuracy Matrix") |>
    flextable::add_footer_lines(paste0("Note. Overall Accuracy = ", sprintf("%.3f", accuracy), ". Estimates computed using the IRT-based expected true score method (Rudner, 2001).")) |>
    flextable::autofit() |>
    flextable::fit_to_width(max_width = 6.5) # Prevent spilling over Word margins
  
  ft
}

# ------------------------------------------------------------
# Table 16 — Decision Consistency (2-Category)
# ------------------------------------------------------------
make_table_16_decision_consistency <- function(mirt_model, scored_resp, napd_cuts) {
  # Extract ONLY the Proficient cut score
  prof_cut <- sort(as.numeric(na.omit(napd_cuts)))[2]
  
  res <- .calc_rudner_indices(mirt_model, scored_resp, prof_cut)
  if (is.null(res)) return(flextable::flextable(data.frame(Note="No cut scores available")))
  
  DC <- res$DC_mat
  df <- data.frame(
    Test1    = c("Test 1: Not Proficient", "Test 1: Proficient", "Column Total"),
    Test2_NP = c(DC[1,1], DC[2,1], sum(DC[,1])),
    Test2_P  = c(DC[1,2], DC[2,2], sum(DC[,2])),
    Total    = c(sum(DC[1,]), sum(DC[2,]), 1.000)
  )
  
  consistency <- DC[1,1] + DC[2,2]
  p_marginal  <- sum(DC[2, ]) 
  pe          <- (p_marginal^2) + ((1 - p_marginal)^2)
  kappa       <- (consistency - pe) / (1 - pe)
  
  ft <- flextable::flextable(df) |>
    flextable::set_header_labels(Test1 = "Test 1 Status", Test2_NP = "Test 2: Not Proficient", Test2_P = "Test 2: Proficient", Total = "Row Total") |>
    flextable::colformat_double(j = 2:4, digits = 3) |>
    flextable::theme_vanilla() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::set_caption("Table 16: Decision Consistency (2x2 Contingency Matrix)") |>
    flextable::add_footer_lines(paste0("Note. Overall Consistency = ", sprintf("%.3f", consistency), "; Cohen's Kappa = ", sprintf("%.3f", kappa), ". Estimates computed using the IRT-based expected true score method (Rudner, 2001).")) |>
    flextable::autofit() |>
    flextable::fit_to_width(max_width = 6.5) # Prevent spilling over Word margins
  
  ft
}

# ------------------------------------------------------------
# Table 17 — NAPD Classification Accuracy
# ------------------------------------------------------------
make_table_17_napd_accuracy <- function(mirt_model, scored_resp, napd_cuts) {
  res <- .calc_rudner_indices(mirt_model, scored_resp, napd_cuts)
  if (is.null(res)) return(flextable::flextable(data.frame(Note="No NAPD cut scores available")))
  
  CA <- res$CA_mat
  DC <- res$DC_mat
  k <- res$k
  
  # Default names based on typical 4-level structure
  level_names <- c("Novice", "Apprentice", "Proficient", "Distinguished")[1:k]
  if (length(level_names) < k) level_names <- paste("Level", 1:k)
  
  df <- data.frame(
    Level = level_names,
    TP = numeric(k), FP = numeric(k), TN = numeric(k), FN = numeric(k),
    Sensitivity = numeric(k), Specificity = numeric(k),
    p = numeric(k), c = numeric(k), Kappa = numeric(k)
  )
  
  for (i in 1:k) {
    TP <- CA[i, i]
    FP <- sum(CA[i, -i])
    FN <- sum(CA[-i, i])
    TN <- sum(CA[-i, -i])
    
    df$TP[i] <- TP
    df$FP[i] <- FP
    df$TN[i] <- TN
    df$FN[i] <- FN
    df$Sensitivity[i] <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
    df$Specificity[i] <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
    
    p_i <- sum(CA[i, ])
    c_i <- DC[i, i] + sum(DC[-i, -i])
    pe_i <- (p_i^2) + ((1 - p_i)^2)
    
    df$p[i] <- p_i
    df$c[i] <- c_i
    df$Kappa[i] <- ifelse((1 - pe_i) > 0, (c_i - pe_i) / (1 - pe_i), NA)
  }
  
  ft <- flextable::flextable(df) |>
    flextable::colformat_double(j = 2:ncol(df), digits = 3) |>
    flextable::theme_vanilla() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = "Level", align = "left", part = "body") |>
    flextable::set_caption("Table 17: NAPD Classification Accuracy") |>
    flextable::add_footer_lines("Note. TP = True Positive; FP = False Positive; TN = True Negative; FN = False Negative;") |>
    flextable::add_footer_lines("p = proportion classified into level; c = proportion of consistent classifications.") |>
    flextable::add_footer_lines("Estimates computed using the IRT-based expected true score method (Rudner, 2001).") |>
    flextable::autofit() |>
    flextable::fit_to_width(max_width = 6.5) # Squash to page width!
  
  ft
}