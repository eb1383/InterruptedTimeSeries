# Campylobacter spp. Incidence and COVID-19 Intervention Analysis 🦠📊

Welcome to the analysis repository! Here, you’ll find an R script designed to explore the incidence of _Campylobacter spp._ over time and how different COVID-19 interventions (like lockdowns and the Eat Out to Help Out scheme) may have affected these trends. Below is a friendly guide on what the script does and how to use it.

## 1. campy_analysis.R - Campylobacter spp. Incidence and COVID-19 Interventions 🦠
This script processes _Campylobacter spp._ surveillance data and adds important variables such as weekly incidence, COVID-19 intervention indicators, and spatial data. It then creates counterfactual predictions to assess what the incidence would have looked like without interventions.

### Key Steps:
1. **Prepare the Case Data**:
   - Loads _Campylobacter spp._ case data from a CSV file.
   - Filters data to keep the period from January 1st, 2014 to December 31st, 2021.
   - Adds a week number column and computes Fourier terms for seasonal variations (weekly cycles).
   
2. **Load and Merge Regional Data**:
   - Loads region data to map the cases to specific Lower Super Output Areas (LSOAs).
   - Merges region data with case data to incorporate regional variations in the analysis.

3. **Population Data**:
   - Merges population data to allow the analysis to be adjusted by population size (important for normalising case counts).

4. **COVID-19 Intervention Phases**:
   - Creates variables indicating the different phases of COVID-19 interventions (e.g., lockdowns, Eat Out to Help Out scheme).
   - Marks periods of intervention with specific indicators for each phase.

5. **Fourier Transformations for Seasonal Trends**:
   - Adds Fourier terms to the dataset to account for weekly seasonal patterns in the incidence of Campylobacter spp.

6. **Fit Counterfactual Model**:
   - Fits a negative binomial regression model to pre-COVID data to predict what the incidence of Campylobacter spp. would have been without the COVID-19 interventions.

7. **Model Predictions and Visualisation**:
   - Predicts counterfactual values for the entire dataset and visualises the observed vs. counterfactual incidence using `ggplot2`.
   - Highlights the periods of COVID-19 interventions (lockdowns and other schemes) with vertical dashed lines for easy comparison.

### How it works:
- **Load Files**: The script loads all required datasets, including surveillance data, region data, and population data.
- **Process and Aggregate Data**: Aggregates data by week, computes Fourier terms for seasonality, and merges it with regional and population data.
- **Fit and Predict**: The script fits a model to predict counterfactual values of incidence and plots both the observed and predicted values over time, marking the COVID-19 interventions.

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
In `campy_analysis.R`, update the file paths for your data (CSV files for case data, region data, and population data).

## 3. Run the Script:
Start by running the `campy_analysis.R` script in your R environment.

## 4. Visualisation:
The script will generate plots comparing the actual incidence of *Campylobacter spp.* with the counterfactual predictions, highlighting the COVID-19 interventions.

### Ready for Analysis:
Once the script runs, you’ll have a dataset that includes both the actual and counterfactual *Campylobacter spp.* incidence, along with clear visual representations of how interventions might have affected the disease trend over time.


## 🤝 Contributions:
Feel free to contribute! If you find any issues or have suggestions for improvements, feel free to submit a pull request. Together, we can make this analysis even better! ✨
