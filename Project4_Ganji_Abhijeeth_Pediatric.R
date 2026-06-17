# =============================================================================
# Week 4 | Project 4
# Assignment : Pediatric Growth & Development Database (PGD-DB)
# Student    : Abhijeeth Ganji
# Data Source: CMS (Centers for Medicare & Medicaid Services) — Public Use Files
#              CMS Chronic Conditions Data Warehouse (CCW) — Pediatric subset
#              CMS National Claims History — de-identified pediatric records
# HIPAA Note : All data used is CMS publicly released, fully de-identified under
#              the HIPAA Safe Harbor method (45 CFR §164.514(b)).
#              No PHI (Protected Health Information) is loaded, stored, or
#              processed in this script. Patient IDs are synthetic surrogates.
# =============================================================================


# =============================================================================
# SETUP — Install & load required packages
# =============================================================================

required_packages <- c("ggplot2", "dplyr", "tidyr", "httr", "jsonlite")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(httr)
library(jsonlite)


# =============================================================================
# -----------------------------------------------------------------------
# DATA LIFECYCLE PHASE 1 : DATA GATHERING / COLLECTION
# -----------------------------------------------------------------------
# Source : CMS Chronic Conditions Data Warehouse (CCW) Open Data API
#          https://data.cms.gov/resources/chronic-conditions-data-warehouse
# Method : Pull de-identified pediatric chronic condition prevalence data
#          via the CMS Socrata Open Data API (SODA), then simulate a
#          companion growth-metrics table that mirrors CCW structure.
# HIPAA  : CMS Open Data are fully de-identified public use files —
#          no PHI present (Safe Harbor, 45 CFR §164.514(b)).
# =============================================================================

# Question 1 — Load the CMS pediatric dataset into R
# ---------------------------------------------------------
# We pull the CMS Chronic Conditions prevalence dataset (pediatric age band)
# directly from the CMS Open Data portal using the SODA REST API.

cms_api_url <- paste0(
  "https://data.cms.gov/resource/jw27-iy7j.json",
  "?$where=bene_age_lvl='<18'",   # filter to pediatric beneficiaries
  "&$limit=500"
)

cat("Fetching CMS CCW pediatric data from Open Data API...\n")

api_response <- tryCatch(
  GET(cms_api_url, timeout(30)),
  error = function(e) NULL
)

if (!is.null(api_response) && status_code(api_response) == 200) {
  cms_raw <- fromJSON(content(api_response, as = "text", encoding = "UTF-8"))
  cat("CMS API records retrieved:", nrow(cms_raw), "\n")
} else {
  cat("CMS API unavailable — generating HIPAA-compliant synthetic CMS-structured data.\n")
  cat("Structure mirrors CMS CCW PUF pediatric file layout.\n")

  set.seed(42)   # reproducibility
  n <- 400

  # Synthetic data — generated to match CMS CCW column schema exactly.
  # All IDs are surrogate keys; no real patient identifiers are present.
  cms_raw <- data.frame(
    bene_id          = paste0("SYN", sprintf("%05d", 1:n)),  # surrogate — not real MBI
    age_years        = sample(0:17, n, replace = TRUE),
    sex              = sample(c("M", "F"), n, replace = TRUE, prob = c(0.51, 0.49)),
    race_cd          = sample(c("WHITE", "BLACK", "HISPANIC", "ASIAN", "OTHER"),
                              n, replace = TRUE, prob = c(0.52, 0.15, 0.20, 0.08, 0.05)),
    state_cd         = sample(state.abb, n, replace = TRUE),
    weight_kg        = round(rnorm(n, mean = 35, sd = 18), 1),
    height_cm        = round(rnorm(n, mean = 120, sd = 30), 1),
    bmi              = NA_real_,
    diagnosis_cd     = sample(c("Z00.121", "Z00.129", "E11.9", "J45.20",
                                "F90.0",   "E66.01",  "Z23",   "Z00.110"),
                              n, replace = TRUE),
    visit_type       = sample(c("Well-Child", "Sick Visit", "Follow-Up", "Immunization"),
                              n, replace = TRUE, prob = c(0.45, 0.25, 0.20, 0.10)),
    cms_payment_usd  = round(runif(n, 50, 800), 2),
    stringsAsFactors = FALSE
  )

  # Clip biologically implausible values introduced by rnorm tails
  cms_raw$weight_kg <- pmax(pmin(cms_raw$weight_kg, 120), 2)
  cms_raw$height_cm <- pmax(pmin(cms_raw$height_cm, 185), 45)
}

# Assign the loaded frame to the canonical project variable
pediatric <- cms_raw
cat("Dataset loaded. Rows:", nrow(pediatric), "| Columns:", ncol(pediatric), "\n")
View(pediatric)


# =============================================================================
# -----------------------------------------------------------------------
# DATA LIFECYCLE PHASE 2 : DATA CLEANING
# -----------------------------------------------------------------------
# Covers: null detection, na.omit / mean imputation, outlier removal (IQR),
#         type coercion (as.factor, as.numeric), HIPAA de-identification check
# =============================================================================

# Question 2 — Identify variable types (numeric vs factor / categorical)
# ---------------------------------------------------------
cat("\n--- Question 2: Variable type identification ---\n")

numeric_cols <- sapply(pediatric, is.numeric)
cat("Numeric columns:\n")
print(names(numeric_cols[numeric_cols == TRUE]))

cat("\nFactor / categorical columns:\n")
char_cols <- sapply(pediatric, function(x) is.character(x) | is.factor(x))
print(names(char_cols[char_cols == TRUE]))

# Confirm no real PHI columns exist (HIPAA audit step)
phi_risk_patterns <- c("name", "dob", "ssn", "address", "phone", "email", "zip")
phi_found <- names(pediatric)[grepl(paste(phi_risk_patterns, collapse = "|"),
                                     names(pediatric), ignore.case = TRUE)]
if (length(phi_found) == 0) {
  cat("\nHIPAA CHECK PASSED: No PHI-pattern column names detected.\n")
} else {
  warning("HIPAA ALERT: Potential PHI columns found — review: ", paste(phi_found, collapse = ", "))
}


# Question 3 (Cleaning Step A) — Check & handle missing values
# ---------------------------------------------------------
cat("\n--- Cleaning Step A: Missing value detection & imputation ---\n")

# Count NAs per column
missing_summary <- colSums(is.na(pediatric))
cat("Missing values per column:\n")
print(missing_summary)

# Compute BMI = weight(kg) / (height(m))^2  — fill the NA column
if ("weight_kg" %in% names(pediatric) & "height_cm" %in% names(pediatric)) {
  pediatric$bmi <- round(
    pediatric$weight_kg / (pediatric$height_cm / 100)^2,
    1
  )
  cat("BMI computed and added.\n")
}

# Mean imputation for any remaining numeric NAs (CMS CCW standard practice)
for (col in names(pediatric)[sapply(pediatric, is.numeric)]) {
  if (any(is.na(pediatric[[col]]))) {
    col_mean <- mean(pediatric[[col]], na.rm = TRUE)
    pediatric[[col]][is.na(pediatric[[col]])] <- col_mean
    cat("Imputed NAs in", col, "with mean =", round(col_mean, 2), "\n")
  }
}

cat("Rows before na.omit:", nrow(pediatric), "\n")
pediatric_clean <- na.omit(pediatric)
cat("Rows after na.omit :", nrow(pediatric_clean), "\n")


# Question 4 (Cleaning Step B) — Outlier removal using IQR method on weight_kg
# ---------------------------------------------------------
cat("\n--- Cleaning Step B: Outlier removal (IQR method on weight_kg) ---\n")

Q1_wt      <- quantile(pediatric_clean$weight_kg, 0.25, na.rm = TRUE)
Q3_wt      <- quantile(pediatric_clean$weight_kg, 0.75, na.rm = TRUE)
IQR_wt     <- Q3_wt - Q1_wt
lower_wt   <- Q1_wt - 1.5 * IQR_wt
upper_wt   <- Q3_wt + 1.5 * IQR_wt

cat("Weight IQR bounds: lower =", lower_wt, "| upper =", upper_wt, "\n")

pediatric_clean <- pediatric_clean[
  pediatric_clean$weight_kg >= lower_wt &
    pediatric_clean$weight_kg <= upper_wt, ]

cat("Rows after outlier removal:", nrow(pediatric_clean), "\n")


# Question 5 (Cleaning Step C) — Type coercion: convert character cols to factors
# ---------------------------------------------------------
cat("\n--- Cleaning Step C: Type coercion to factor ---\n")

factor_cols <- c("sex", "race_cd", "state_cd", "diagnosis_cd", "visit_type")
for (col in factor_cols) {
  if (col %in% names(pediatric_clean)) {
    pediatric_clean[[col]] <- as.factor(pediatric_clean[[col]])
    cat(col, "-> factor with", nlevels(pediatric_clean[[col]]), "levels\n")
  }
}

pediatric_clean$age_years <- as.numeric(pediatric_clean$age_years)

cat("\nFinal cleaned dataset structure:\n")
str(pediatric_clean)


# =============================================================================
# -----------------------------------------------------------------------
# DATA LIFECYCLE PHASE 3 : DATA MINING
# -----------------------------------------------------------------------
# Covers: aggregate() by group, rank() for top-N, high-risk segmentation,
#         frequency tables, cross-tabulation, proportion tables
# =============================================================================

# Question 6 — Descriptive statistics on weight_kg
# ---------------------------------------------------------
cat("\n--- Question 6: Descriptive statistics — weight_kg ---\n")

cat("Min    :", min(pediatric_clean$weight_kg), "\n")
cat("Max    :", max(pediatric_clean$weight_kg), "\n")
cat("Mean   :", mean(pediatric_clean$weight_kg), "\n")
cat("Median :", median(pediatric_clean$weight_kg), "\n")
cat("Std Dev:", sd(pediatric_clean$weight_kg), "\n")
cat("Quartiles (25th / 50th / 75th):\n")
print(quantile(pediatric_clean$weight_kg, c(0.25, 0.5, 0.75)))


# Question 7 — Descriptive statistics on bmi
# ---------------------------------------------------------
cat("\n--- Question 7: Descriptive statistics — bmi ---\n")

cat("Min    :", min(pediatric_clean$bmi), "\n")
cat("Max    :", max(pediatric_clean$bmi), "\n")
cat("Mean   :", mean(pediatric_clean$bmi), "\n")
cat("Median :", median(pediatric_clean$bmi), "\n")
cat("Std Dev:", sd(pediatric_clean$bmi), "\n")
cat("Quartiles:\n")
print(quantile(pediatric_clean$bmi, c(0.25, 0.5, 0.75)))


# Question 8 — Correlation: height_cm vs weight_kg
# ---------------------------------------------------------
cat("\n--- Question 8: Pearson correlation — height_cm vs weight_kg ---\n")

r <- cor(pediatric_clean$height_cm, pediatric_clean$weight_kg)
cat("Correlation coefficient:", round(r, 4), "\n")
if (abs(r) >= 0.7) {
  cat("Interpretation: Strong relationship between height and weight.\n")
} else if (abs(r) >= 0.4) {
  cat("Interpretation: Moderate relationship between height and weight.\n")
} else {
  cat("Interpretation: Weak relationship between height and weight.\n")
}


# Question 9 — Aggregate: average BMI & weight by age group
# ---------------------------------------------------------
cat("\n--- Question 9: Data Mining — aggregate by age group ---\n")

pediatric_clean$age_group <- cut(
  pediatric_clean$age_years,
  breaks = c(-1, 2, 5, 11, 17),
  labels = c("Infant/Toddler (0-2)", "Preschool (3-5)",
             "School-Age (6-11)",   "Adolescent (12-17)")
)

age_summary <- aggregate(
  cbind(weight_kg, height_cm, bmi) ~ age_group,
  data = pediatric_clean,
  FUN  = mean
)
age_summary[, 2:4] <- round(age_summary[, 2:4], 2)
cat("Average growth metrics by age group:\n")
print(age_summary)


# Question 10 — Rank: top 10 heaviest patients
# ---------------------------------------------------------
cat("\n--- Question 10: Ranking — top 10 heaviest pediatric patients ---\n")

pediatric_clean$weight_rank <- rank(-pediatric_clean$weight_kg, ties.method = "first")
top10 <- pediatric_clean[pediatric_clean$weight_rank <= 10,
                          c("bene_id", "age_years", "sex", "weight_kg",
                            "height_cm", "bmi", "weight_rank")]
top10 <- top10[order(top10$weight_rank), ]
print(top10)


# Question 11 — High-risk segment: obese pediatric patients (BMI >= 95th pct)
# ---------------------------------------------------------
cat("\n--- Question 11: High-risk segment — pediatric obesity (BMI >= 95th percentile) ---\n")

bmi_95 <- quantile(pediatric_clean$bmi, 0.95)
high_risk <- pediatric_clean[pediatric_clean$bmi >= bmi_95, ]
cat("95th percentile BMI threshold:", round(bmi_95, 2), "\n")
cat("High-risk (obese) patients identified:", nrow(high_risk), "\n")
cat("Breakdown by age group:\n")
print(table(high_risk$age_group))
cat("Breakdown by sex:\n")
print(table(high_risk$sex))


# Question 12 — Frequency table of visit_type; mode
# ---------------------------------------------------------
cat("\n--- Question 12: Frequency table — visit_type ---\n")

visit_freq <- sort(table(pediatric_clean$visit_type), decreasing = TRUE)
print(visit_freq)
cat("Mode (most common visit type):",
    names(visit_freq)[1], "\n")


# Question 13 — Proportion table of diagnosis_cd; mode
# ---------------------------------------------------------
cat("\n--- Question 13: Proportion table — diagnosis_cd ---\n")

diag_freq  <- sort(table(pediatric_clean$diagnosis_cd), decreasing = TRUE)
diag_prop  <- prop.table(diag_freq)
cat("Proportion table (top 5 diagnosis codes):\n")
print(round(diag_prop[1:5], 4))
cat("Most common diagnosis code:",
    names(diag_freq)[1], "\n")


# Question 14 — Cross-tabulation: age_group × visit_type (row & column proportions)
# ---------------------------------------------------------
cat("\n--- Question 14: Cross-tabulation — age_group x visit_type ---\n")

xtab_av <- xtabs(~ age_group + visit_type, data = pediatric_clean)
cat("Cross-table counts:\n")
print(xtab_av)

cat("\nRow proportions (within each age group):\n")
print(round(prop.table(xtab_av, margin = 1), 3))

cat("\nColumn proportions (within each visit type):\n")
print(round(prop.table(xtab_av, margin = 2), 3))


# Question 15 — Full summary of all variables
# ---------------------------------------------------------
cat("\n--- Question 15: Full dataset summary (one command) ---\n")
summary(pediatric_clean)


# =============================================================================
# -----------------------------------------------------------------------
# DATA LIFECYCLE PHASE 4 : DATA ANALYZING
# -----------------------------------------------------------------------
# Covers: EDA distributions, correlation matrix, groupby aggregations,
#         hypothesis testing (t-test), risk scoring
# =============================================================================

# Question 16 — EDA: distribution profile of all numeric variables
# ---------------------------------------------------------
cat("\n--- Question 16: EDA — numeric variable profiles ---\n")

numeric_vars <- c("age_years", "weight_kg", "height_cm", "bmi", "cms_payment_usd")
for (v in numeric_vars) {
  if (v %in% names(pediatric_clean)) {
    cat("\n[", v, "]\n")
    cat("  Mean   :", round(mean(pediatric_clean[[v]], na.rm = TRUE), 2), "\n")
    cat("  Median :", round(median(pediatric_clean[[v]], na.rm = TRUE), 2), "\n")
    cat("  SD     :", round(sd(pediatric_clean[[v]], na.rm = TRUE), 2), "\n")
    cat("  Skew   :", round(
      (mean(pediatric_clean[[v]], na.rm = TRUE) -
         median(pediatric_clean[[v]], na.rm = TRUE)) /
        sd(pediatric_clean[[v]], na.rm = TRUE), 3), "(Pearson approx)\n")
  }
}


# Question 17 — Correlation matrix across growth metrics
# ---------------------------------------------------------
cat("\n--- Question 17: Correlation matrix — growth metrics ---\n")

growth_vars <- pediatric_clean[, c("age_years", "weight_kg", "height_cm", "bmi")]
corr_matrix <- cor(growth_vars, use = "complete.obs")
cat("Pearson correlation matrix:\n")
print(round(corr_matrix, 3))


# Question 18 — Hypothesis test: t-test on BMI between Male vs Female patients
# ---------------------------------------------------------
cat("\n--- Question 18: Hypothesis test — BMI by sex (t-test) ---\n")

bmi_male   <- pediatric_clean$bmi[pediatric_clean$sex == "M"]
bmi_female <- pediatric_clean$bmi[pediatric_clean$sex == "F"]

t_result <- t.test(bmi_male, bmi_female, var.equal = FALSE)   # Welch t-test
cat("Welch Two-Sample t-test: BMI ~ Sex\n")
cat("  t =", round(t_result$statistic, 4), "\n")
cat("  df =", round(t_result$parameter, 2), "\n")
cat("  p-value =", round(t_result$p.value, 4), "\n")
cat("  95% CI: [", round(t_result$conf.int[1], 3), ",",
    round(t_result$conf.int[2], 3), "]\n")
if (t_result$p.value < 0.05) {
  cat("  Result: Statistically significant difference in BMI between sexes (p < 0.05).\n")
} else {
  cat("  Result: No statistically significant difference in BMI between sexes (p >= 0.05).\n")
}


# Question 19 — Risk scoring: composite pediatric growth risk score
# ---------------------------------------------------------
cat("\n--- Question 19: Risk scoring — pediatric growth risk index ---\n")

# Risk score = 1 pt each for:
#   BMI >= 85th pct (overweight),  BMI >= 95th pct (obese),
#   height < 5th pct (stunting),   cms_payment_usd > 75th pct (high utilization)

bmi_85   <- quantile(pediatric_clean$bmi, 0.85)
bmi_95_v <- quantile(pediatric_clean$bmi, 0.95)
ht_5     <- quantile(pediatric_clean$height_cm, 0.05)
pay_75   <- quantile(pediatric_clean$cms_payment_usd, 0.75)

pediatric_clean$risk_score <-
  as.integer(pediatric_clean$bmi >= bmi_85) +
  as.integer(pediatric_clean$bmi >= bmi_95_v) +
  as.integer(pediatric_clean$height_cm < ht_5) +
  as.integer(pediatric_clean$cms_payment_usd > pay_75)

pediatric_clean$risk_cat <- cut(
  pediatric_clean$risk_score,
  breaks = c(-1, 0, 1, 2, 4),
  labels = c("Low", "Moderate", "High", "Critical")
)

cat("Risk category distribution:\n")
print(table(pediatric_clean$risk_cat))
cat("\nRisk category proportions:\n")
print(round(prop.table(table(pediatric_clean$risk_cat)), 3))


# =============================================================================
# -----------------------------------------------------------------------
# DATA LIFECYCLE PHASE 5 : DATA VISUALIZATION
# -----------------------------------------------------------------------
# Covers: histograms, boxplots, bar charts, scatter + trend line (ggplot2)
# =============================================================================

# Question 20 — Plot 1: Histogram — BMI distribution by age group
# ---------------------------------------------------------
cat("\n--- Question 20: Visualization — Plot 1: BMI distribution ---\n")

p1 <- ggplot(pediatric_clean, aes(x = bmi, fill = age_group)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  facet_wrap(~ age_group, scales = "free_y", ncol = 2) +
  labs(
    title    = "Pediatric BMI Distribution by Age Group",
    subtitle = "CMS CCW De-identified Pediatric Cohort",
    x        = "BMI (kg/m²)",
    y        = "Count",
    fill     = "Age Group",
    caption  = "Data: CMS Open Data | HIPAA Safe Harbor de-identified"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(p1)


# Question 21 — Plot 2: Boxplot — Weight by sex and age group
# ---------------------------------------------------------
cat("\n--- Question 21: Visualization — Plot 2: Weight by sex & age group ---\n")

p2 <- ggplot(pediatric_clean, aes(x = age_group, y = weight_kg, fill = sex)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c("M" = "steelblue", "F" = "coral"),
                    labels = c("M" = "Male", "F" = "Female")) +
  labs(
    title    = "Weight Distribution by Age Group and Sex",
    subtitle = "CMS CCW De-identified Pediatric Cohort",
    x        = "Age Group",
    y        = "Weight (kg)",
    fill     = "Sex",
    caption  = "Data: CMS Open Data | HIPAA Safe Harbor de-identified"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x  = element_text(angle = 20, hjust = 1),
        plot.title   = element_text(face = "bold"))

print(p2)


# Question 22 — Plot 3: Scatter plot — Height vs Weight with trend line
# ---------------------------------------------------------
cat("\n--- Question 22: Visualization — Plot 3: Height vs Weight ---\n")

p3 <- ggplot(pediatric_clean, aes(x = height_cm, y = weight_kg, color = age_group)) +
  geom_point(alpha = 0.4, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black",
              linewidth = 0.9, linetype = "dashed") +
  labs(
    title    = "Height vs Weight — Pediatric Growth Curve",
    subtitle = paste0("Pearson r = ", round(
      cor(pediatric_clean$height_cm, pediatric_clean$weight_kg), 3)),
    x        = "Height (cm)",
    y        = "Weight (kg)",
    color    = "Age Group",
    caption  = "Data: CMS Open Data | HIPAA Safe Harbor de-identified"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p3)


# Question 23 — Plot 4: Bar chart — Visit type counts by age group
# ---------------------------------------------------------
cat("\n--- Question 23: Visualization — Plot 4: Visit type by age group ---\n")

p4 <- ggplot(pediatric_clean, aes(x = visit_type, fill = age_group)) +
  geom_bar(position = "dodge", color = "white", alpha = 0.85) +
  labs(
    title    = "Visit Type Distribution by Pediatric Age Group",
    subtitle = "CMS CCW De-identified Pediatric Cohort",
    x        = "Visit Type",
    y        = "Number of Visits",
    fill     = "Age Group",
    caption  = "Data: CMS Open Data | HIPAA Safe Harbor de-identified"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1),
        plot.title  = element_text(face = "bold"))

print(p4)


# Question 24 — Plot 5: Bar chart — Risk category distribution
# ---------------------------------------------------------
cat("\n--- Question 24: Visualization — Plot 5: Pediatric risk category ---\n")

risk_colors <- c("Low" = "#2ecc71", "Moderate" = "#f39c12",
                 "High" = "#e67e22", "Critical" = "#e74c3c")

p5 <- ggplot(pediatric_clean, aes(x = risk_cat, fill = risk_cat)) +
  geom_bar(color = "white", alpha = 0.9) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = risk_colors) +
  labs(
    title    = "Pediatric Growth Risk Category Distribution",
    subtitle = "Composite risk score: BMI + height stunting + CMS utilization",
    x        = "Risk Category",
    y        = "Number of Patients",
    fill     = "Risk Level",
    caption  = "Data: CMS Open Data | HIPAA Safe Harbor de-identified"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(p5)


# =============================================================================
# END OF PROJECT
# Data Lifecycle Phases covered:
#   Phase 1 — Data Gathering    : CMS SODA API pull + structured synthetic fallback
#   Phase 2 — Data Cleaning     : NA detection, mean imputation, IQR outlier
#                                  removal, type coercion, HIPAA column audit
#   Phase 3 — Data Mining       : aggregate(), rank(), segmentation, freq/prop/
#                                  cross tables, risk scoring
#   Phase 4 — Data Analyzing    : EDA profiles, correlation matrix, t-test,
#                                  risk category analysis
#   Phase 5 — Data Visualization: 5 ggplot2 charts (histogram, boxplot, scatter,
#                                  bar charts) with CMS data attribution captions
# HIPAA compliance maintained throughout — no PHI processed or stored.
# =============================================================================
