# Campylobacter spp. Incidence and COVID-19 Intervention Analysis 🦠📊

Welcome to the analysis repository! Here, you’ll find an R script designed to explore the incidence of _Campylobacter spp._ over time and how different COVID-19 interventions (like lockdowns and the Eat Out to Help Out scheme) may have affected these trends. Below is a friendly guide on what the script does and how to use it.

## 1. campy_analysis.R - Campylobacter spp. Incidence and COVID-19 Interventions 🦠
This script processes _Campylobacter spp._ surveillance data and adds important variables such as weekly incidence, COVID-19 intervention indicators, and spatial data. It then creates counterfactual predictions to assess what the incidence would have looked like without interventions.

### Key Steps:
1. **COVID-19 Intervention Phases**:
   - Creates variables indicating the different phases of COVID-19 interventions (e.g., lockdowns, Eat Out to Help Out scheme).
   - Marks periods of intervention with specific indicators for each phase.

2. **Fourier Transformations for Seasonal Trends**:
   - Adds Fourier terms to the dataset to account for weekly seasonal patterns in the incidence of Campylobacter spp.

3. **Fit Counterfactual Model**:
   - Fits a negative binomial regression model to pre-COVID data to predict what the incidence of Campylobacter spp. would have been without the COVID-19 interventions.

4. **Model Predictions and Visualisation**:
   - Predicts counterfactual values for the entire dataset and visualises the observed vs. counterfactual incidence using `ggplot2`.
   - Highlights the periods of COVID-19 interventions (lockdowns and other schemes) with vertical dashed lines for easy comparison.

### Final Output:
The output is a visualisation of the observed and counterfactual incidence of _Campylobacter spp._ that will help you understand the potential impact of COVID-19 interventions.

## 📦 Prerequisites
### R Packages Required:
To run the script, you’ll need the following R packages. They are all easily installable:

- **sp** – For handling spatial data.
- **ggplot2** – For data visualisation.
- **tmap** – For thematic mapping.
- **basemapR** – For base map generation.
- **arm** – For robust regression analysis.
- **BAMMtools** – For biodiversity analysis.
- **spdep** – For spatial dependence analysis.
- **car** – Companion to applied regression.
- **broom** – For tidying model outputs.
- **tidyverse** – For data manipulation.
- **readxl** – For reading Excel files.
- **MASS** – For statistical modelling.
- **scales** – For scaling visualisations.

### Install Packages:
You can install the required packages using the following code:

```r
install.packages(c("sp", "ggplot2", "tmap", "basemapR", "arm", "BAMMtools", 
                   "spdep", "car", "broom", "tidyverse", "readxl", "MASS", "scales"))
```
## 🚀 How to Use

Follow these steps to run the script:

### 1. Install the required R packages:
Run the installation command above in your R environment
## 2. Update File Paths:
In `campy_analysis.R`, update the file paths for your data (CSV file for case data).

## 3. Run the Script:
Start by running the `campy_analysis.R` script in your R environment.

## 4. Visualisation:
The script will generate plots comparing the actual incidence of *Campylobacter spp.* with the counterfactual predictions, highlighting the COVID-19 interventions.

### Ready for Analysis:
Once the script runs, you’ll have a dataset that includes both the actual and counterfactual *Campylobacter spp.* incidence, along with clear visual representations of how interventions might have affected the disease trend over time.

## Anonymised Aggregated Data File 📊
An anonymised, aggregated data file is included in this repository. It contains the weekly case counts, region, and population data, which have been processed and anonymised to ensure privacy. This file allows you to skip ahead to the part of the analysis after the data aggregation is complete, making it easier to move forward with analysis without needing to repeat the data preprocessing steps.

**Data Structure:**
- **Week**: Continuous weekly index (ensuring no weeks are skipped).
- **Region**: Identifiers for the different regions (anonymised for privacy).
- **Population**: Population count for region
- **Total Case Count**: Aggregated cases for each week and region.

You can load this file directly into the script to continue with the analysis from the aggregation stage.

📝 **How to Use the Data File:**
- After downloading the anonymised data file, simply update the file path in the script to point to this file.
- You can now run the part of the script focusing on visualisation or model fitting as needed.

## 🤝 Contributions:
Feel free to contribute! If you find any issues or have suggestions for improvements, feel free to submit a pull request. Together, we can make this analysis even better! ✨
