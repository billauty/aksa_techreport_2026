# Tables & Figures Plan — AKSA 2026 Psychometric Technical Report

## Tables

### Table 1: CTT Score Summary

This table reports the number of tests scores, the mean total raw score, the standard deviation, the minimum and maximum observed scores.

---

### Table 2: CTT Item Statistics

This table shows Classical Test Theory (CTT) statistics for each test item:

- **n:** Number of students who answered the item
- **mean:** Proportion of students who answered correctly (item difficulty, 0–1 scale)
- **sd:** Standard deviation of responses
- **median:** Middle value (0.0, 0.5, or 1.0 for binary items)
- **min/max/range:** Always 0, 1, 1 for dichotomous items
- **skew:** Distribution symmetry (negative = more correct responses; positive = fewer correct)
- **kurtosis:** All negative values (~-2) indicate flat, uniform distributions typical of binary items

---

### Table 3: Reliability

KR-20 (Kuder-Richardson Formula 20) is a measure of internal consistency reliability for tests with dichotomous items (right/wrong, 0/1 scoring). The reliability value is reported.

---

### Table 4: Raw Score Frequencies

Observed test scores, based on the sum of correct responses, are reported along with the percent of all scores and the cumulative percent.

---

### Table 5: Distractor Analysis

This table lists item responses by response option, with the correct response indicated by an asterisk (*). For each response, the following statistics are reported:

- **n:** number of students selecting the response
- **resP:** proportion of students selecting the response
- **pBis:** point-biserial correlation between the response and the total score with the item removed. This value is expected to be highest for the correct response.
- **discrim:** discrimination index, defined as the difference between upper and lower score groups. As with pBis, higher values are expected for the correct response.
- **lower, mid66, upper:** proportions of students in the lowest, middle, and highest third score groups selecting the response.

---

### Table 6: IRT Model Summary

This table presents the Item Response Theory (IRT) model fit statistics for the grade 10 test:

- **grade:** Grade level of the test
- **n_items:** Number of items on the test
- **n_persons:** Number of students who took the test
- **logLik:** Log-likelihood value indicating model fit to the data
- **AIC:** Akaike Information Criterion — lower values indicate better model fit
- **BIC:** Bayesian Information Criterion — lower values indicate better model fit, with stronger penalty for model complexity

These statistics provide goodness-of-fit measures for the IRT model and can be used to compare different models or assess model adequacy.

---

### Table 7: Raw Score to Theta Conversion

The Rasch model produces a single ability estimate (theta) for each raw score. Because the raw score is a sufficient statistic, this table may be used to convert raw scores to theta values. Theta is reported in logit units and centered at zero. An adequate range of theta values indicates good measurement coverage. Theta values are linearly transformed to the reporting scale used by the state. The standard error of measurement is reported for each raw score.

---

### Table 8: IRT Item Parameters

This table presents Item Response Theory (IRT) parameters for each test item using a Rasch model:

- **item_id:** Item identifier (A1–F5)
- **a:** Discrimination parameter — fixed at 1 for all items in the Rasch model
- **b:** Difficulty parameter — the ability level at which a student has a 50% probability of answering correctly (measured on the logit scale)
- **SE_b:** Standard error of the difficulty parameter estimate, indicating precision of the estimate

---

### Table 9: Item Infit and Outfit Statistics

This table reports Infit and Outfit mean-square statistics for each item. Values near 1.0 indicate good fit to the Rasch model. Mean-square values below 1.0 indicate overfit, while values above 1.0 indicate underfit. Values above 2.0 suggest distortion of the measurement model. Infit is more sensitive to responses near a student's ability level, while Outfit is more sensitive to unexpected responses far from a student's estimated ability.

---

## Rasch Model Unidimensionality

### Table 10: Summary of Yen's Q3 Residual Correlations

This table summarizes the distribution of Yen's Q3 residual correlations to assess local independence:

- **Statistic:** Type of summary statistic reported
- **Value:** Corresponding value for each statistic
- **Maximum Q3:** Largest residual correlation observed
- **Mean Q3:** Average residual correlation across all item pairs
- **Proportion |Q3| > .20:** Proportion of absolute residual correlations exceeding 0.20

---

### Table 11: Reliability

Reliability is reported as a function of the variance of theta and the squared standard error of measurement. Reliability estimates are provided for all students and for subgroups with sufficient sample sizes. Values near or above 0.70 are generally considered acceptable.

---

## Differential Item Functioning (DIF)

### Tables 12–14

These tables present results of differential item functioning (DIF) analysis examining whether items function differently for demographic subgroups:

- **Item:** Item identifier (A1–F5)
- **ΔLord:** Lord's chi-square statistic measuring the magnitude of DIF between groups (larger absolute values indicate greater DIF)
- **ETS Class:** Educational Testing Service classification of DIF magnitude (A = negligible, B = slight to moderate, C = moderate to large)
- **Flag:** Whether the item is flagged for meaningful DIF (TRUE = statistically significant DIF detected, FALSE = no significant DIF)

---

## Classification Accuracy and Consistency

### Table 15: Proficiency Classification Accuracy (Livingston-Lewis)

This table reports the accuracy of proficiency classifications based on the Livingston-Lewis method:

- **Classification:** The proficiency classification categories (e.g., Proficient / Not Proficient)
- **Accuracy:** The proportion of students correctly classified into each category, accounting for measurement error

Classification accuracy represents the probability that students are correctly assigned to their true proficiency level given the reliability of the test. Higher accuracy values indicate more dependable classification decisions. This statistic helps evaluate whether the test provides sufficiently precise scores for making proficiency determinations.

---

### Table 16: Proficiency Decision Consistency (Livingston-Lewis)

This table presents decision consistency statistics using the Livingston-Lewis method:

- **Contingency Matrix:** A 2×2 table showing the proportions of students classified into each category on hypothetical repeated administrations
  - True Positive: Proportion classified as proficient on both administrations
  - False Positive: Proportion classified as proficient on first but not second administration
  - False Negative: Proportion classified as not proficient on first but proficient on second administration
  - True Negative: Proportion classified as not proficient on both administrations
  - Total: Row and column totals representing overall classification proportions
- **Proportion of Consistent Classifications:** Overall proportion of students who would receive the same classification on repeated testing
- **Cohen's Kappa:** Agreement statistic correcting for chance agreement (values range from 0 to 1, with higher values indicating better consistency)

Decision consistency evaluates the reliability of proficiency classifications. High consistency indicates students would likely receive the same classification if tested again with a parallel form. Cohen's Kappa provides a more stringent measure by accounting for agreement that would occur by chance alone.

---

### Table 17: NAPD Classification Accuracy (Livingston-Lewis)

This table provides detailed classification accuracy statistics for the four performance levels (NAPD) using the Livingston-Lewis (Multi-Category Extension) method:

**Livingston-Lewis Calculation:**

- The method assumes each student has a "true score" that differs from their observed score due to measurement error.
- Uses the beta-binomial distribution (hence the betafunctions package) to model the relationship between observed and true scores.
- Calculates the probability distribution of true scores for each observed score.
- Determines classification accuracy by comparing observed classifications to estimated true classifications.

**Output Statistics:** For each performance level, calculates:

- **TP (True Positive):** Proportion truly at this level correctly classified here
- **FP (False Positive):** Proportion not at this level incorrectly classified here
- **TN (True Negative):** Proportion not at this level correctly classified elsewhere
- **FN (False Negative):** Proportion truly at this level incorrectly classified elsewhere
- **Sensitivity:** Same as TP rate
- **Specificity:** Same as TN rate
- **p:** Observed proportion classified at this level
- **c:** Estimated true proportion at this level (corrected for measurement error)
- **Kappa:** Agreement beyond chance for this level

Note: If a test had an insufficient distribution of scores to estimate all four performance levels, NAPD statistics are not reported.

**Practical Interpretation:**

- High Sensitivity: Test correctly identifies students who truly belong at this level
- High Specificity: Test correctly excludes students who don't belong at this level
- Kappa: Reliability of classification at this level beyond chance

---

## Figures

### Figure 1: Item Infit and Outfit Statistics

This figure displays the Infit and Outfit statistics reported in the item fit table.

---

### Figure 2: Comparison of Student Ability and Item Difficulty: Wright Map

This figure displays student ability estimates and item difficulty estimates on the same logit scale. Item difficulties are shown as points, and student abilities are displayed as a histogram. Optimal measurement occurs when student abilities overlap the range of item difficulties.

---

### Figure 3: Conditional Standard Error of Measurement (CSEM)

This figure shows the conditional standard error of measurement across a wide range of ability values. Lower CSEM values indicate greater measurement precision, particularly in the range where most students score. The Observed Theta Range indicates the student abilities calculated this year.

---

## Differential Item Functioning (DIF)

### Figures 4–6

This figure displays the magnitude and direction of differential item functioning (DIF) by gender for each test item:

- **Y-axis:** Item identifiers (A1–F5)
- **X-axis:** ΔLord (ETS) statistic values, ranging from approximately -1.0 to +0.5
- **Dots:** Individual item DIF values (gray = negligible DIF, orange/yellow = flagged for meaningful DIF)
- **Vertical dashed line:** Reference line at 0.0 indicating no DIF

---

## Learner Characteristics

### Figures 7–10: Learner Characteristics

Learner Characteristics figures examine the relationship between test scores and teacher-reported characteristics from the Learner Characteristic Inventory. The following characteristics are reported when available:

- Expressive Communication
- Receptive Language
- Reading
- Mathematics

Each figure displays score distributions by category using boxplots. The interquartile range, median, and relative sample sizes are shown. Categories are ordered by increasing expected ability. Horizontal lines above the plots display Wilcoxon test p-values for pairwise comparisons between categories.

---

### Figure 11: Anchor Item Drift

This figure displays potential anchor items administered in consecutive years and the change in item difficulty between administrations. The x-axis shows the robust Z-score of the difference in item difficulty, and the y-axis lists item identifiers. Items exceeding the drift threshold (p < .01) are flagged and were excluded from the equating process. Note that Writing tests did not include anchor items this year and were equated using Editing & Mechanics items.
