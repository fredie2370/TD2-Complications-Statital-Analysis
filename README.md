# Type 2 Diabetes Complications: Statistical Analysis Pipeline

## Overview

This repository contains the statistical analysis pipeline used to investigate the association between clinical variables and the development of complications in patients with Type 2 Diabetes (T2D). The project focuses on identifying statistically significant predictors of diabetic complications through reproducible data analysis implemented in **R**.

The code provided here supports exploratory data analysis, hypothesis testing, and statistical modeling applied to clinical registry data.

## Objectives

The main objectives of this analysis are:

* To characterize clinical variables associated with Type 2 Diabetes complications.
* To evaluate statistical associations between demographic, clinical, and metabolic factors.
* To generate descriptive statistics, visualizations, and inferential models supporting epidemiological interpretation.

## Repository Structure

```
TD2-Complications-Statistical-Analysis
│
├── data/                   # Input datasets (not always included due to privacy restrictions)
├── scripts/                # R scripts for statistical analysis
├── figures/                # Generated plots and figures
├── tables/                 # Output tables used in reports
├── results/                # Processed data and analysis outputs
└── README.md               # Project documentation
```

## Methods

The statistical workflow implemented in this repository includes:

1. **Data preprocessing**

   * Data cleaning
   * Variable formatting
   * Handling of missing values

2. **Exploratory Data Analysis**

   * Descriptive statistics
   * Distribution analysis
   * Visualization of clinical variables

3. **Inferential Statistics**

   * Hypothesis testing
   * Group comparisons
   * Association analysis

4. **Statistical Modeling**

   * Regression-based approaches
   * Risk factor evaluation
   * Identification of predictors for diabetic complications

All analyses are implemented in **R**, enabling full reproducibility.

## Software Requirements

* R (≥ 4.0 recommended)

Commonly used packages may include:

```
tidyverse
ggplot2
dplyr
readr
stats
```

Additional dependencies are specified within the scripts.

## Reproducibility

To reproduce the analysis:

1. Clone the repository

```
git clone https://github.com/fredie2370/TD2-Complications-Statital-Analysis.git
```

2. Open the project in **RStudio**.

3. Run the scripts in the `scripts/` directory following the analytical workflow.

Note: Clinical datasets may not be publicly distributed due to privacy restrictions.

## Data Availability

The dataset used in this study contains clinical patient information and may be subject to institutional or ethical restrictions. Access to the data may require authorization from the corresponding data custodian.

## Limitations

This repository focuses on statistical analysis and does not include a graphical interface for data entry or registry management.

## Note

You may change the file name/path and number of total patients and TD2 patients in **config.R**before you start.


## Author

Manuel Gutierrez Rodríguez

## License

This program is free to use for academic and research purposes under the de MIT License.

