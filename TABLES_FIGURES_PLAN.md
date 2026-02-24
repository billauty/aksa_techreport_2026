# Tables & Figures Plan — AKSA 2026 Psychometric Technical Report

This document tracks the 17 tables and 11 figures to be implemented in
`R/03_tables_figs.R` as part of the `build_test_content()` pipeline.
Each entry describes the required data inputs and the recommended R
package or approach.

---

## Tables (17)

### Table 1 — Test Administration Summary
**Description:** Sample sizes (N) by grade, content area, and test form.  
**Inputs:** `scored_data` — one row per examinee; columns `grade`,
`content_area`, `form_id`.  
**Package:** `flextable` (summary table from `dplyr::count()`).

---

### Table 2 — Examinee Demographics
**Description:** Counts and percentages by race/ethnicity, gender,
IEP status, and ELL status.  
**Inputs:** `scored_data` — demographic flag columns.  
**Package:** `flextable` (formatted from `dplyr::summarise()`).

---

### Table 3 — Score Descriptive Statistics
**Description:** Mean, SD, median, min, max, skewness, and kurtosis of
scale scores per test.  
**Inputs:** `scored_data` — column `scale_score` grouped by `test_id`.  
**Package:** `flextable`; descriptives via base R or `psych::describe()`.

---

### Table 4 — Score Descriptive Statistics by Subgroup
**Description:** Mean and SD of scale scores broken out by each
demographic subgroup.  
**Inputs:** `scored_data` — `scale_score` × demographic flags.  
**Package:** `flextable` (pivoted from `dplyr::group_by()` + `summarise()`).

---

### Table 5 — Internal Consistency Reliability
**Description:** Cronbach's alpha and SEM for each test form.  
**Inputs:** Item-level scored response matrix from `scored_data`.  
**Package:** `CTT::reliability()` for alpha and SEM; `flextable`.

---

### Table 6 — Conditional Standard Error of Measurement
**Description:** SEM at each raw-score point across the score scale.  
**Inputs:** Item-level response matrix.  
**Package:** `CTT::cSEM()` for binomial SEM; `flextable`.

---

### Table 7 — Item Difficulty Statistics (CTT)
**Description:** Proportion-correct (*p*-value) and item mean for every
scored item.  
**Inputs:** Item-level binary response matrix.  
**Package:** `CTT::itemAnalysis()` → `flextable`.

---

### Table 8 — Item Discrimination Statistics (CTT)
**Description:** Point-biserial correlation (*r*-pbis) and corrected
item-total correlation for every item.  
**Inputs:** Item-level binary response matrix.  
**Package:** `CTT::itemAnalysis()` → `flextable`.

---

### Table 9 — Distractor Analysis
**Description:** Response frequency and percentage for each option
(A/B/C/D) per item, including point-biserial per option.  
**Inputs:** Raw (unscored) response matrix with nominal option codes from
`raw_data`.  
**Package:** `CTT::distractorAnalysis()` → `flextable`.

---

### Table 10 — IRT Item Parameter Estimates
**Description:** Estimated difficulty (*b*), discrimination (*a*), and
guessing (*c*) parameters (3PL) or Rasch *b* for each item.  
**Inputs:** `model_results` — `mirt` model object.  
**Package:** `mirt::coef()` → `flextable`.

---

### Table 11 — IRT Model-Data Fit Statistics
**Description:** Item-level fit indices (S-X² statistic, *p*-value,
RMSEA) from the fitted IRT model.  
**Inputs:** `model_results` — `mirt` model object.  
**Package:** `mirt::itemfit()` → `flextable`.

---

### Table 12 — Differential Item Functioning (DIF) Summary
**Description:** Mantel-Haenszel Δ and logistic-regression DIF
statistics per item, with flagging codes (A/B/C).  
**Inputs:** Item-level scored responses + focal/reference group indicator
from `scored_data`.  
**Package:** `difR::difMH()` or `mirt::DIF()` → `flextable`.

---

### Table 13 — Raw-to-Scale Score Conversion Table
**Description:** Full lookup table mapping every possible raw score to
its corresponding scale score and performance level.  
**Inputs:** Equating/scaling parameters from `model_results`; score
range metadata.  
**Package:** Computed via `mirt::fscores()` or linear equating; formatted
with `flextable`.

---

### Table 14 — Performance Level Cut Scores
**Description:** Cut scores (raw and scaled) for each performance level
with their standard errors.  
**Inputs:** Cut-score specifications (external) + scale parameters from
`model_results`.  
**Package:** `flextable`.

---

### Table 15 — Classification Accuracy and Consistency
**Description:** Percent correct classification, kappa, and hit rate for
each performance level boundary.  
**Inputs:** `model_results` — IRT ability estimates and cut scores.  
**Package:** `mirt::classify()` or custom computation; `flextable`.

---

### Table 16 — Content-Domain (Subscore) Statistics
**Description:** N items, mean, SD, and alpha for each content strand or
reporting category.  
**Inputs:** `scored_data` with item-to-strand mapping; item-level
responses.  
**Package:** `CTT::reliability()` per strand subset; `flextable`.

---

### Table 17 — Item Pool Summary
**Description:** Count of operational items by content strand and
cognitive-complexity level.  
**Inputs:** Item metadata file (content strand, Depth of Knowledge level)
joined to `scored_data`.  
**Package:** `flextable` (from `dplyr::count(strand, dok_level)`).

---

## Figures (11)

### Figure 1 — Scale Score Distribution Histogram
**Description:** Histogram of scale scores with a normal-curve overlay
for each test.  
**Inputs:** `scored_data$scale_score` grouped by `test_id`.  
**Package:** `ggplot2` (`geom_histogram()` + `stat_function()`).

---

### Figure 2 — Score Distribution by Performance Level
**Description:** Stacked or grouped bar chart showing the percentage of
examinees at each performance level.  
**Inputs:** `scored_data` — `performance_level` column.  
**Package:** `ggplot2` (`geom_bar(position = "fill")`).

---

### Figure 3 — Item Difficulty Distribution
**Description:** Histogram of *p*-values for all operational items,
with reference lines at 0.2 and 0.8.  
**Inputs:** Item *p*-values from `CTT::itemAnalysis()`.  
**Package:** `ggplot2`.

---

### Figure 4 — Item Discrimination Distribution
**Description:** Histogram of point-biserial correlations for all
operational items, with a reference line at 0.2.  
**Inputs:** Item discrimination values from `CTT::itemAnalysis()`.  
**Package:** `ggplot2`.

---

### Figure 5 — Item Characteristic Curves (ICC)
**Description:** Predicted probability-of-correct-response curves across
the theta range for a selected set of items.  
**Inputs:** `model_results` — fitted `mirt` model.  
**Package:** `mirt::plot(type = "trace")` or `ggplot2` via
`mirt::probtrace()`.

---

### Figure 6 — Test Information Function (TIF)
**Description:** Total test information curve and conditional SEM across
the theta range, with cut-score markers.  
**Inputs:** `model_results` — fitted `mirt` model.  
**Package:** `mirt::plot(type = "infoSE")` or `ggplot2` via
`mirt::testinfo()`.

---

### Figure 7 — Person-Item Map (Wright Map)
**Description:** Side-by-side display of person ability and item
difficulty distributions on the theta scale.  
**Inputs:** `model_results` — IRT ability estimates (`mirt::fscores()`)
and item *b* parameters.  
**Package:** `WrightMap::wrightMap()`.

---

### Figure 8 — DIF Item Scatterplot
**Description:** Scatterplot of item difficulty for focal vs. reference
group, with DIF-flagged items annotated.  
**Inputs:** Group-specific *p*-values or *b* parameters from
`model_results` + DIF flag from Table 12.  
**Package:** `ggplot2` (`geom_point()` + `ggrepel::geom_label_repel()`).

---

### Figure 9 — Scree Plot / Parallel Analysis
**Description:** Eigenvalue scree plot with parallel-analysis reference
line to support dimensionality assessment.  
**Inputs:** Item-level scored response matrix.  
**Package:** `psych::fa.parallel()` or `ggplot2` with eigenvalues from
`base::eigen()`.

---

### Figure 10 — Expected Score Curve
**Description:** Expected total test score (summed score) as a function
of theta from the IRT model.  
**Inputs:** `model_results` — fitted `mirt` model.  
**Package:** `mirt::plot(type = "score")` or `ggplot2` via
`mirt::expected.test()`.

---

### Figure 11 — Subgroup Score Distribution Comparison
**Description:** Overlapping kernel-density plots of scale scores for
each demographic subgroup, with mean markers.  
**Inputs:** `scored_data` — `scale_score` and demographic flag columns.  
**Package:** `ggplot2` (`geom_density()` + `geom_vline()`).

---

## Implementation Notes

- All tables should be built with `flextable` and styled for accessible
  Word output via `officer`.
- All figures should be `ggplot2` objects (even if a specialist package
  like `WrightMap` or `mirt` is used for the computation) so that
  `ggplot2::ggsave()` in `04_build_report.R` works uniformly.
- Each figure needs a descriptive `alt_text` string for Section 508
  accessibility.
- The `build_test_content()` function in `R/03_tables_figs.R` should
  return a named list with elements `tables` (a named list of
  `flextable` objects) and `figures` (a named list of lists, each with
  `plot` and `alt_text`).
