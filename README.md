# Home Insurance Premium Modelling with R

This repository contains an actuarial modelling project that investigates the relationship between socio-economic area indicators and home insurance premium affordability in Australia. The project applies multiple regression and spline-based modelling techniques in R to analyze premium trends, evaluate model performance, and perform graduation tests.

## Project Overview

Climate change has significantly increased the frequency and severity of natural disasters, leading to rising home insurance premiums across Australia. This project explores how insurance affordability varies across different areas and proposes potential policy ideas to improve affordability.

The study focuses on modelling average premium payments using `Area.ID` (a proxy for SEIFA socio-economic ranking) and compares three actuarial smoothing approaches:

- Polynomial Regression
- Natural Cubic Spline
- Smoothing Spline

The repository includes:

- A full actuarial report (`.docx`)
- Complete R implementation (`.R`)
- Data cleaning, exploratory analysis, modelling, and graduation testing workflows

---

## Repository Structure

```bash
├── 48348015HuynhLuongReport.docx   # Full actuarial report
├── 48348015HuynhLuongReport.R      # R source code
└── README.md                       # Project documentation
```

---

## Objectives

The project aims to:

- Analyze home insurance affordability patterns
- Model average premium costs based on socio-economic area ranking
- Compare spline and regression techniques
- Evaluate predictive performance using MSE
- Conduct graduation tests for actuarial model validation
- Discuss potential insurance policy innovations

---

## Methodology

### 1. Data Cleaning & Exploratory Data Analysis

The preprocessing stage includes:

- Renaming variables
- Handling invalid and missing values
- Correcting negative premium values
- Summary statistics
- Scatterplots and boxplots

Techniques used:

- `ggplot2`
- `gridExtra`

---

### 2. Model Development

#### Polynomial Regression

Polynomial models with degrees 1–8 were tested using Mean Squared Error (MSE) comparison.

Final selected model:
- Degree 4 polynomial regression

---

#### Natural Cubic Spline

- Chebyshev spacing was used to determine spline knots
- Multiple knot combinations were tested
- Validation MSE was used for model selection

Libraries:
- `splines`

---

#### Smoothing Spline

- Tuned using `spar` parameter search
- Validation-based optimization
- Achieved the lowest out-of-sample test MSE

---

## Model Performance

| Model | Test MSE |
|---|---|
| Polynomial Regression | 1,903,392 |
| Natural Cubic Spline | 1,791,044 |
| Smoothing Spline | 1,675,495 |

The smoothing spline achieved the best predictive performance on the test dataset.

---

## Graduation Tests

The project performs several classical actuarial graduation tests:

- Chi-squared Test of Fit
- Standardized Deviations Test
- Signs Test
- Cumulative Deviations Test
- Grouping of Signs Test
- Serial Correlation Test

These tests evaluate:
- Goodness-of-fit
- Randomness of deviations
- Overfitting and robustness
- Generalization capability

---

## Key Findings

- Premium unaffordability exists in both disadvantaged and advantaged areas
- Outliers indicate severe regional exposure to climate-related risks
- Smoothing splines provide the best predictive accuracy but may suffer from overfitting
- Additional explanatory variables may improve model robustness

---

## Proposed Insurance Policy Ideas

The report also discusses innovative affordability strategies such as:

- Selective disaster coverage
- Community-service-based premium reduction systems
- Government-supported reinsurance pools

These ideas aim to reduce insurance costs while improving social resilience and disaster recovery support.

---

## Technologies Used

### Language
- R

### Libraries
- `ggplot2`
- `gridExtra`
- `splines`

---

## How to Run

1. Open the R script in RStudio

```r
source("48348015HuynhLuongReport.R")
```

2. Ensure the dataset path inside the script is updated correctly:

```r
read.csv("your_dataset_path.csv")
```

3. Install required packages if necessary:

```r
install.packages(c("ggplot2", "gridExtra", "splines"))
```

---

## Potential Improvements

Future work may include:

- Additional predictive variables
- Non-parametric approaches (e.g., KNN)
- Robust outlier detection methods
- Cross-validation improvements
- Bias-variance tradeoff evaluation metrics

---

## Author

**Trung Luong Huynh**  
Actuarial Modelling Project  
Macquarie University

---

## References

The project references literature and reports from:

- Australian Actuaries Institute
- Australian Bureau of Statistics
- Academic spline modelling research
- Robust statistics and actuarial graduation literature

See the full report for complete citations.
