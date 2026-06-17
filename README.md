# PGD-DB: Pediatric Growth & Development Database

**[Click here to view the full interactive HTML report](https://abhijeethganji99.github.io/PGD-DB/)**

**Author:** Abhijeeth Ganji  
**Language:** R  
**Data source:** CMS (Centers for Medicare & Medicaid Services) Open Data — Chronic Conditions Data Warehouse (CCW) Pediatric Public Use File  

---

## Project overview

PGD-DB is a reproducible R analysis pipeline that explores pediatric growth and development patterns using de-identified CMS claims data. The project walks through all five phases of the data lifecycle — gathering, cleaning, mining, analyzing, and visualizing — applied to a pediatric cohort (ages 0–17) drawn from the CMS Chronic Conditions Data Warehouse (CCW).

Key outputs include:

- Descriptive statistics on height, weight, and BMI across age groups
- IQR-based outlier removal and mean imputation for missing values
- Aggregate growth profiles by age group (infant/toddler, preschool, school-age, adolescent)
- A composite pediatric growth risk score (obesity + stunting + high CMS utilization)
- Five ggplot2 visualizations: BMI histogram, weight boxplot, height-vs-weight scatter, visit-type bar chart, and risk category bar chart
- Welch t-test comparing BMI between male and female patients

---

## Repository structure

```
pediatric-growth-db/
├── Project4_Ganji_Abhijeeth_Pediatric.R   # Main analysis script
├── README.md                               # This file
├── LICENSE                                 # MIT License
└── outputs/                                # Generated plots (after running the script)
    ├── plot_bmi_distribution.png
    ├── plot_weight_by_sex.png
    ├── plot_height_vs_weight.png
    ├── plot_visit_type.png
    └── plot_risk_category.png
```

---

## How to run

### Option A — RStudio (local)

1. Clone or download this repository.
2. Open `Project4_Ganji_Abhijeeth_Pediatric.R` in RStudio.
3. Run the entire script (`Ctrl+Shift+Enter` on Windows / `Cmd+Shift+Enter` on Mac).  
   All required packages are auto-installed on first run.

### Option B — Google Colab (R runtime)

1. Go to [colab.research.google.com](https://colab.research.google.com) and create a new notebook.
2. Switch runtime: **Runtime → Change runtime type → R → Save**.
3. In the first cell, install packages:
   ```r
   install.packages(c("ggplot2", "dplyr", "tidyr", "httr", "jsonlite"),
                    repos = "https://cloud.r-project.org", quiet = TRUE)
   ```
4. Upload the `.R` file using the Files panel (left sidebar), then in the next cell:
   ```r
   source("/content/Project4_Ganji_Abhijeeth_Pediatric.R")
   ```

### Option C — From GitHub (Colab or RStudio)

```r
source("https://raw.githubusercontent.com/YourUsername/pediatric-growth-db/main/Project4_Ganji_Abhijeeth_Pediatric.R")
```

---

## Data source

| Field | Detail |
|---|---|
| Provider | Centers for Medicare & Medicaid Services (CMS) |
| Dataset | Chronic Conditions Data Warehouse (CCW) — Pediatric Public Use File |
| Access | CMS Open Data portal — [data.cms.gov](https://data.cms.gov) |
| API endpoint | `https://data.cms.gov/resource/jw27-iy7j.json` |
| De-identification method | HIPAA Safe Harbor (45 CFR §164.514(b)) |
| Fallback | If the API is unavailable, the script generates a synthetic dataset that mirrors the exact CMS CCW column schema |

---

## HIPAA compliance

This project is designed and verified to comply with the Health Insurance Portability and Accountability Act (HIPAA) Privacy Rule. The following controls are in place:

### De-identification standard

All data used in this project is de-identified under the **HIPAA Safe Harbor method** as defined in 45 CFR §164.514(b). CMS has removed or generalized all 18 PHI identifiers before publishing the data. No re-identification is attempted at any point in this project.

### The 18 HIPAA Safe Harbor identifiers — none present in this dataset

| # | Identifier | Status in this project |
|---|---|---|
| 1 | Names | Not present — surrogate IDs only (`SYN00001`) |
| 2 | Geographic subdivisions smaller than state | Not present — state code only |
| 3 | Dates (except year) | Not present — no admission or birth dates |
| 4 | Phone numbers | Not present |
| 5 | Fax numbers | Not present |
| 6 | Email addresses | Not present |
| 7 | Social Security numbers | Not present |
| 8 | Medical record numbers | Not present |
| 9 | Health plan beneficiary numbers | Not present — synthetic surrogate only |
| 10 | Account numbers | Not present |
| 11 | Certificate/license numbers | Not present |
| 12 | Vehicle identifiers | Not present |
| 13 | Device identifiers | Not present |
| 14 | Web URLs | Not present |
| 15 | IP addresses | Not present |
| 16 | Biometric identifiers | Not present |
| 17 | Full-face photographs | Not present |
| 18 | Any other unique identifying number | Not present |

### Runtime HIPAA audit

The script includes an automated column-name audit at runtime. It scans all loaded column names against a list of PHI-risk patterns (`name`, `dob`, `ssn`, `address`, `phone`, `email`, `zip`) and prints a `HIPAA CHECK PASSED` confirmation or raises a warning if any suspicious column names are detected.

```
HIPAA CHECK PASSED: No PHI-pattern column names detected.
```

### Data handling rules

- No data is written to disk except aggregated summary outputs and plots — no raw records are saved.
- No network transmission of individual records occurs.
- Synthetic fallback data uses algorithmically generated surrogate IDs with no link to any real individual.
- This project does not constitute a Covered Entity or Business Associate relationship under HIPAA, as it uses only publicly released, pre-de-identified CMS Open Data.

### Relevant regulation

> "Health information that does not identify an individual and with respect to which there is no reasonable basis to believe that the information can be used to identify an individual is not individually identifiable health information."  
> — 45 CFR §164.514(a)

---

## Dependencies

| Package | Version tested | Purpose |
|---|---|---|
| `ggplot2` | ≥ 3.4.0 | Data visualization (all 5 plots) |
| `dplyr` | ≥ 1.1.0 | Data manipulation |
| `tidyr` | ≥ 1.3.0 | Data reshaping |
| `httr` | ≥ 1.4.0 | CMS API HTTP requests |
| `jsonlite` | ≥ 1.8.0 | JSON parsing of API responses |

All packages are available on CRAN and auto-installed by the script if not already present.

---

## Data lifecycle phases covered

| Phase | Questions | Key functions |
|---|---|---|
| 1 — Gathering | Q1 | `GET()`, `fromJSON()`, synthetic fallback |
| 2 — Cleaning | Q2–Q5 | `colSums(is.na())`, `na.omit()`, IQR outlier filter, `as.factor()` |
| 3 — Mining | Q6–Q15 | `aggregate()`, `rank()`, `table()`, `prop.table()`, `xtabs()`, `summary()` |
| 4 — Analyzing | Q16–Q19 | EDA profiles, `cor()`, `t.test()`, risk scoring |
| 5 — Visualization | Q20–Q24 | 5 `ggplot2` charts |

---

## License

This project is released under the MIT License. See `LICENSE` for details.  
CMS data is public domain as a U.S. government work under 17 U.S.C. §105.

---

## Disclaimer

This project is for **academic and educational purposes only**. It does not constitute medical advice, clinical decision support, or a covered healthcare operation under HIPAA. The synthetic fallback data does not represent any real patients.
