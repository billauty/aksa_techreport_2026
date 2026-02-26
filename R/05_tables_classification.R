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
  
  ft <- flextable::flextable(lookup_df) |>
    flextable::set_header_labels(`Raw Score` = "Raw Score", `Theta (Logit)` = "Theta (Logit)", `SEM` = "SEM") |>
    flextable::colformat_int(j = "Raw Score") |>
    flextable::colformat_double(j = c("Theta (Logit)", "SEM"), digits = 2) |>
    flextable::theme_vanilla() |>
    flextable::align(align = "right", part = "body") |>
    flextable::align(align = "center", part = "header") |>
    flextable::set_caption(caption = caption_text) |>
    flextable::autofit()
  ft
}

# ------------------------------------------------------------
# Internal Helper: Rudner (2001) Expected True Score Indices
# ------------------------------------------------------------
.calc_rudner_indices <- function(mirt_model, scored_resp, thet_cuts) {
  # The thet_cuts coming from _targets.R are already mapped to the Theta metric!
  valid_cuts <- sort(as.numeric(na.omit(thet_cuts)))
  if (length(valid_cuts) == 0) return(NULL)
  
  item_cols <- setdiff(names(scored_resp)[sapply(scored_resp, is.numeric)], c("SSID", "complete"))
  resp_mat  <- as.matrix(scored_resp[, item_cols, drop = FALSE])
  fs <- mirt::fscores(mirt_model, response.pattern = resp_mat, full.scores.SE = TRUE, verbose = FALSE)
  
  thetas <- as.numeric(fs[, "F1"])
  ses    <- as.numeric(fs[, "SE_F1"])
  
  bounds <- c(-Inf, valid_cuts, Inf)
  k <- length(bounds) - 1
  
  # P_mat: N x K matrix of probabilities that each student's true theta is in level k
  P_mat <- t(sapply(seq_along(thetas), function(i) {
    pnorm(bounds[-1], mean = thetas[i], sd = ses[i]) - pnorm(bounds[-(k+1)], mean = thetas[i], sd = ses[i])
  }))
  
  # Observed category for each student based on expected test score equivalent theta
  obs_cat <- as.integer(cut(thetas, breaks = bounds, right = FALSE, include.lowest = TRUE))
  
  # Classification Accuracy Matrix (Observed x True)
  CA_mat <- matrix(0, nrow = k, ncol = k)
  for (i in 1:k) {
    idx <- which(obs_cat == i)
    if (length(idx) > 0) CA_mat[i, ] <- colSums(P_mat[idx, , drop = FALSE])
  }
  N <- length(thetas)
  CA_mat <- CA_mat / N
  
  # Decision Consistency Matrix (Test 1 x Test 2)
  DC_mat <- (t(P_mat) %*% P_mat) / N
  
  list(CA_mat = CA_mat, DC_mat = DC_mat, k = k)
}

# ------------------------------------------------------------
# Table 15 — Proficiency Classification Accuracy
# ------------------------------------------------------------
make_table_15_class_accuracy <- function(mirt_model, scored_resp, napd_cuts) {
  cuts_num <- as.numeric(na.omit(as.numeric(napd_cuts)))
  if(length(cuts_num) < 2) return(flextable::flextable(data.frame(Note="Missing cuts")))
  prof_cut <- sort(cuts_num)[2] 
  
  res <- .calc_rudner_indices(mirt_model, scored_resp, prof_cut)
  if (is.null(res)) return(flextable::flextable(data.frame(Note="No cut scores available")))
  
  CA <- res$CA_mat
  accuracy <- CA[1,1] + CA[2,2]
  
  df <- data.frame(
    Classification = "Proficient / Not Proficient",
    Accuracy       = accuracy,
    stringsAsFactors = FALSE
  )
  
  ft <- flextable::flextable(df) |>
    flextable::set_header_labels(Classification = "Classification", Accuracy = "Accuracy") |>
    flextable::colformat_double(j = "Accuracy", digits = 4) |>
    flextable::theme_vanilla() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::set_caption("Table 15: Proficiency Classification Accuracy (Expected True Score)") |>
    flextable::autofit()
  
  ft
}

# ------------------------------------------------------------
# Table 16 — Proficiency Decision Consistency
# ------------------------------------------------------------
make_table_16_decision_consistency <- function(mirt_model, scored_resp, napd_cuts) {
  cuts_num <- as.numeric(na.omit(as.numeric(napd_cuts)))
  if(length(cuts_num) < 2) return(flextable::flextable(data.frame(Note="Missing cuts")))
  prof_cut <- sort(cuts_num)[2] 
  
  res <- .calc_rudner_indices(mirt_model, scored_resp, prof_cut)
  if (is.null(res)) return(flextable::flextable(data.frame(Note="No cut scores available")))
  
  DC <- res$DC_mat
  
  # Format perfectly to match the image you provided
  df <- data.frame(
    RowLabel = c("True", "False", "Total"),
    Positive = c(DC[2,2], DC[2,1], sum(DC[2,])),
    Negative = c(DC[1,2], DC[1,1], sum(DC[1,])),
    Total    = c(DC[2,2]+DC[1,2], DC[2,1]+DC[1,1], 1.000)
  )
  
  consistency <- DC[1,1] + DC[2,2]
  p_marginal  <- sum(DC[2, ]) 
  pe          <- (p_marginal^2) + ((1 - p_marginal)^2)
  kappa       <- (consistency - pe) / (1 - pe)
  
  ft <- flextable::flextable(df) |>
    flextable::set_header_labels(RowLabel = "", Positive = "Positive", Negative = "Negative", Total = "Total") |>
    flextable::add_header_row(values = c("", "Contingency Matrix", "Contingency Matrix", "Contingency Matrix")) |>
    flextable::merge_h(part = "header") |>
    flextable::colformat_double(j = 2:4, digits = 4) |>
    flextable::theme_vanilla() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::set_caption("Table 16: Proficiency Decision Consistency (Expected True Score)") |>
    flextable::add_footer_lines(paste0("Proportion of Consistent Classifications = ", sprintf("%.4f", consistency))) |>
    flextable::add_footer_lines(paste0("Cohen's Kappa = ", sprintf("%.4f", kappa))) |>
    flextable::bold(part = "footer") |>
    flextable::autofit()
  
  ft
}

# ------------------------------------------------------------
# Table 17 — NAPD Classification Accuracy
# ------------------------------------------------------------
make_table_17_napd_accuracy <- function(mirt_model, scored_resp, napd_cuts) {
  cuts_num <- as.numeric(na.omit(as.numeric(napd_cuts)))
  
  res <- .calc_rudner_indices(mirt_model, scored_resp, cuts_num)
  if (is.null(res)) return(flextable::flextable(data.frame(Note="No NAPD cut scores available")))
  
  CA <- res$CA_mat
  DC <- res$DC_mat
  k <- res$k
  
  level_names <- c("Novice", "Apprentice", "Proficient", "Distinguished")[1:k]
  if (length(level_names) < k) level_names <- paste("Level", 1:k)
  
  df <- data.frame(
    Performance_Level = level_names,
    TP = numeric(k), FP = numeric(k), TN = numeric(k), FN = numeric(k),
    Sensitivity = numeric(k), Specificity = numeric(k),
    p_c = numeric(k), Kappa = numeric(k)
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
    
    df$p_c[i] <- c_i
    df$Kappa[i] <- ifelse((1 - pe_i) > 0, (c_i - pe_i) / (1 - pe_i), NA)
  }
  
  ft <- flextable::flextable(df) |>
    flextable::set_header_labels(Performance_Level = "Performance_Level") |>
    flextable::colformat_double(j = 2:ncol(df), digits = 3) |>
    flextable::theme_vanilla() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::set_caption("Table 17: NAPD Classification Accuracy (Expected True Score)") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::autofit() |>
    flextable::fit_to_width(max_width = 6.5) # Protects from tiny font syndrome
  
  ft
}
