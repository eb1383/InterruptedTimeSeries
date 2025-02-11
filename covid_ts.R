# Load required libraries
library(sp)         # Spatial data handling
library(ggplot2)    # Data visualisation
library(tmap)       # Thematic mapping
library(basemapR)   # Base map generation
library(arm)        # Analysis of regression models
library(BAMMtools)  # Analysis of biodiversity data
library(spdep)      # Spatial dependence
library(car)        # Companion to applied regression
library(broom)      # Tidy model outputs
library(tidyverse)  # Data manipulation and visualisation
library(readxl)     # Excel file reading
library(MASS)       # Functions for robust statistics
library(scales)     # Functions for scaling visualizations

### LOAD AND PREPARE DATA ####
# Define the source directory and the data file name
source <- ""
file <- paste0("")
# Load the dataset into a data frame
df <- read.csv(paste(source, file, sep = "/"))

### ADD WEEK ####
# Define the start and end dates
start_date <- as.Date("2014-01-01")
end_date <- as.Date("2021-12-31")
# Generate a sequence of dates between start and end
seqdate <- seq(start_date, end_date, by = "days")
# Filter the data frame to match the date range
df <- df %>%
  filter(date >= start_date & date <= end_date)
# Create a 'week' column representing the week number of the year
df$week <- lubridate::week(df$date)
# Replace week 53 with week 52 (since some years have a 53rd week)
df$week <- ifelse(df$week == 53, 52, df$week)
# Create a continuous week column across years
df$week <- with(df, (year(date) - min(year(date))) * 52 + week - min(week) + 1)

### FILTER DATES ####
# Filter the data frame to only include weeks 53 and beyond
df <- df %>%
  filter(week >= 53)
# Adjust the 'week' column to make it continuous starting from 1
df$week <- df$week - 52
# Store the modified data frame as db for further use
db <- df

### ADD REGION ####
# Define the source directory for region data
source <- ""
# Load region data (e.g., lsoa to region mapping)
file <- paste0("") 
reg <- read.csv(paste(source, file, sep = "/"))
# Join the region data with the main dataset
db <- left_join(db, reg, by = "lsoa", relationship = "many-to-many")
# Summarise data by region and week
db <- db %>%
  group_by(region, week) %>%
  summarise(
    total_count = sum(count)
  )

### MISSING VALUES ####
# Create a list of unique regions
regions <- unique(db$region)
# Create a complete dataset with all possible combinations of week and region
complete_df <- expand.grid(week = 1:364, region = regions)
# Merge the complete dataset with the actual data, filling in missing values
final_df <- merge(complete_df, db, all.x = TRUE)
# Replace any missing total_count values with 0
final_df$total_count[is.na(final_df$total_count)] <- 0
# Update db to reflect the complete dataset with missing values filled
db <- final_df

### ADD POPULATION DATA ####
# Define the source and file for population data
source <- ""
file <- paste0("reg_pop.csv")
# Load the population data
pop <- read.csv(paste(source, file, sep = "/"))
# Merge the population data with the main dataset
db <- merge(db, pop, by = "region")
# Update df to reflect the merged data
df <- db

### FOURIER TERMS ####
# Add Fourier terms to model seasonality (sine and cosine terms for weekly cycles)
df$week_52_sin <- sin(2 * pi * df$week / 52)
df$week_52_cos <- cos(2 * pi * df$week / 52)
df$week_26_sin <- sin(2 * pi * df$week / 26)
df$week_26_cos <- cos(2 * pi * df$week / 26)

### ADD COVID INDICATORS ####
# Add indicators for different phases of COVID-19 intervention (lockdowns, etc.)
df <- df %>%
  mutate(covid1 = ifelse(week >= 272, 1, 0), 
         covid2 = ifelse(week >= 305, 1, 0), 
         covid3 = ifelse(week >= 313, 1, 0),
         eat = ifelse(week >= 291, 1, 0),
         post = ifelse(covid1 == 0, 0, week - 272),
         post1 = ifelse(week >= 341, week - 341, 0))

### TIME SERIES ####
# Subset data for different phases of the time series based on COVID indicators
ts_pre <- df %>% filter(covid1 == 0)
ts_post_1 <- df %>% filter(covid1 == 1)
ts_post_2 <- df %>% filter(covid2 == 1)
ts_post_3 <- df %>% filter(covid3 == 1)

### COUNTERFACTUAL ####
# Fit a model to the pre-COVID data using negative binomial regression
m2.fourier <- glm.nb(total_count ~ week + week_52_cos + week_52_sin + week_26_cos + 
                       week_26_sin + region + offset(log(population)), data = ts_pre)
# Summarise the model
summary(m2.fourier)

### PREDICTED VALUES ####
# Use the model to predict the counterfactual (expected) values for all weeks
predicted_values = predict(m2.fourier, newdata=df, type="response")
# Prepare data for plotting
plot_data <- data.frame(week = df$week,
                        actual_values = df$total_count,
                        predicted_values = predicted_values)
# Summarise the data by week
plot_data <- plot_data %>%
  group_by(week) %>%
  summarise(
    actual_values = sum(actual_values),
    predicted_values1 = sum(predicted_values) 
  )
# Function to generate month labels for x-axis
generate_month_labels <- function(start_date, num_weeks) {
  dates <- seq.Date(from = start_date, by = "week", length.out = num_weeks)
  years <- format(dates, "%Y")
  months <- format(dates, "%b")
  labels <- paste(months, years)
  month_changes <- which(diff(as.numeric(format(dates, "%m"))) != 0)
  month_starts <- c(1, month_changes + 1)
  month_labels <- labels[month_starts]
  return(list(week_starts = month_starts, labels = month_labels))
}
# Generate the month labels for the plot
start_date <- as.Date("2015-01-01")
num_weeks <- 364  # Adjust according to the dataset
month_info <- generate_month_labels(start_date, num_weeks)
# Subset labels and week starts to only every 4th element for clearer axis labeling
every_fourth_label <- month_info$labels[seq(1, length(month_info$labels), by = 3)]
every_fourth_week_start <- month_info$week_starts[seq(1, length(month_info$week_starts), by = 3)]
# Plot the data with the actual and predicted values
ggplot(plot_data, aes(x = week, y = actual_values)) +
  geom_point(size = 1, aes(color = "Actual")) + 
  geom_line(aes(y = predicted_values1, color = "Counterfactual"), linewidth = 1) +
  scale_color_manual(values = c("black", "salmon")) +
  labs(title = expression(paste("Observed and counterfactual ", italic("Campylobacter spp."), " incidence 2015-2021, accounting for COVID-19 interventions")),
       y = "Notifiable counts", x = "Month-Year", color = "") +
  theme_bw() +
  scale_x_continuous(breaks = every_fourth_week_start, labels = every_fourth_label, expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(breaks = pretty_breaks(n = 7)) +
  geom_vline(xintercept = 273, color = "#dd879a", linetype = "dashed") +
  geom_vline(xintercept = 293, color = "#9dcead", linetype = "dashed") +
  geom_vline(xintercept = 306, color = "#91d5f7", linetype = "dashed") +
  geom_vline(xintercept = 315, color = "#c5c3c3", linetype = "dashed") +
  annotate("text", x = 272, y = 1350, label = "Lockdown 1", vjust = 1.5, color = "#dd879a", angle = 90, size = 4, fontface = "bold") +
  annotate("text", x = 292, y = 1350, label = "Eat Out to Help Out", vjust = 1.5, color = "#9dcead", angle = 90, size = 4, fontface = "bold") +
  annotate("text", x = 305, y = 1350, label = "Lockdown 2", vjust = 1.5, color = "#91d5f7", angle = 90, size = 4, fontface = "bold") +
  annotate("text", x = 314, y = 1350, label = "Lockdown 3", vjust = 1.5, color = "#c5c3c3", angle = 90, size = 4, fontface = "bold") +
  annotate("rect", xmin = 273, xmax = 288, ymin = -Inf, ymax = Inf, fill = "#dd879a", alpha = 0.2) +
  annotate("rect", xmin = 293, xmax = 298, ymin = -Inf, ymax = Inf, fill = "#9dcead", alpha = 0.2) +
  annotate("rect", xmin = 306, xmax = 309, ymin = -Inf, ymax = Inf, fill = "#91d5f7", alpha = 0.2) +
  annotate("rect", xmin = 315, xmax = 323, ymin = -Inf, ymax = Inf, fill = "#c5c3c3", alpha = 0.2)

### INTERRUPTED TIME SERIES ####
# Define the null model (no predictors)
# The null model includes only the time-related variables and an offset for population
null_model <- glm.nb(total_count ~ week + week_52_sin + week_52_cos + week_26_cos + week_26_sin + offset(log(population)), data = df)
# Define the full model (with all predictors)
# The full model includes time, region, covid phases, post-lockdown effects, and interactions between region and these factors
full_model <- glm.nb(total_count ~ week + region + covid1 + covid2 + covid3 + post + post1 + eat + week_52_sin + week_52_cos + week_26_cos + week_26_sin +
                       region*covid1 + region*covid2 + region*covid3 + region*post + region*post1 + region*eat + offset(log(population)), data = df)
# Perform forward selection based on Bayesian Information Criterion (BIC)
# The stepAIC function will select the most parsimonious model with the lowest BIC
forward_selection <- stepAIC(null_model, scope = list(lower = null_model, upper = full_model), 
                             direction = "forward", k = log(nrow(db)))
# Print the summary of the selected forward model
summary(forward_selection)

### PREDICTED VALUES ####
# Generate the predicted values using the selected model
predicted_values = predict(forward_selection, newdata=df, type="response")
# Create a data frame for plotting actual vs. predicted values, grouped by week
plot_data1 <- data.frame(week = df$week,
                        actual_values = df$total_count,
                        predicted_values = predicted_values)
# Summarise the data by week, aggregating the total actual and predicted counts
plot_data1 <- plot_data1 %>%
  group_by(week) %>%
  summarise(
    actual_values = sum(actual_values),
    predicted_values = sum(predicted_values) 
  )
# Add the predicted values from an undefined data frame 
plot_data1$predicted_values1 <- plot_data$predicted_values1

# Create the plot with pretty breaks on the y-axis
# This plot compares actual vs. predicted values with annotations for interruptions (lockdowns and events)
ggplot(plot_data1, aes(x = week, y = actual_values)) +
  geom_point(size = 1, aes(color = "Actual values")) + # Add points for actual values
  geom_line(aes(y = predicted_values1, color = "Counterfactual values"), linewidth = 1) + # Add line for counterfactual values
  geom_line(aes(y = predicted_values, color = "Forecasted values"), linewidth = 1) + # Add line for forecasted values
  scale_color_manual(values = c("black", "salmon", "#83c7e8")) +
  labs(
    y = "Notifiable counts", x = "Month-Year",
    color = "") +
  theme_bw() +
  scale_x_continuous(breaks = every_fourth_week_start, labels = every_fourth_label, expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 10),  # Increase y-axis text size
    axis.title.x = element_text(size = 13, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 13, face = "bold"),  # Bold y-axis title
    legend.text = element_text(size = 13),  # Increase legend text size
    legend.title = element_text(size = 12),  # Set legend title size
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5)  # Centered plot title
  ) +
  # Use pretty breaks for the y-axis
  scale_y_continuous(breaks = pretty_breaks(n = 7)) +
  # Lines marking the interruptions (lockdowns, events)
  geom_vline(xintercept = 273, color = "#dd879a", linetype = "dashed") +
  geom_vline(xintercept = 293, color = "#9dcead", linetype = "dashed") +
  geom_vline(xintercept = 306, color = "#91d5f7", linetype = "dashed") +
  geom_vline(xintercept = 315, color = "#c5c3c3", linetype = "dashed") +
  # Add labels for each interruption point
  annotate("text", x = 272, y = 1850, label = "Lockdown 1", vjust = 1.5, 
           color = "#dd879a", angle = 90, size = 5, fontface = "bold") +
  annotate("text", x = 292, y = 1850, label = "EOTHO", vjust = 1.5, 
           color = "#9dcead", angle = 90, size = 5, fontface = "bold") +
  annotate("text", x = 305, y = 1850, label = "Lockdown 2", vjust = 1.5, 
           color = "#91d5f7", angle = 90, size = 5, fontface = "bold") +
  annotate("text", x = 314, y = 1850, label = "Lockdown 3", vjust = 1.5, 
           color = "#c5c3c3", angle = 90, size = 5, fontface = "bold") +
  # Shaded areas indicating the period of each interruption with different colors
  annotate("rect", xmin = 273, xmax = 288, ymin = -Inf, ymax = Inf, fill = "#dd879a", alpha = 0.2) +
  annotate("rect", xmin = 293, xmax = 298, ymin = -Inf, ymax = Inf, fill = "#9dcead", alpha = 0.2) +
  annotate("rect", xmin = 306, xmax = 309, ymin = -Inf, ymax = Inf, fill = "#91d5f7", alpha = 0.2) +
  annotate("rect", xmin = 315, xmax = 323, ymin = -Inf, ymax = Inf, fill = "#c5c3c3", alpha = 0.2)

# Extract coefficients from the selected model and calculate their exponentiated values (relative risk/odds ratio)
model_coef <- summary(forward_selection)$coefficients
coef <- round(exp(model_coef[, "Estimate"]), 3)
lower_ci <- round(exp(model_coef[, "Estimate"] - 1.96 * model_coef[, "Std. Error"]), 3)
upper_ci <- round(exp(model_coef[, "Estimate"] + 1.96 * model_coef[, "Std. Error"]), 3)
# Create a data frame to store coefficient estimates and their 95% confidence intervals
ci_df <- data.frame(
  Coefficient = names(coef),
  result = paste0(format(coef, big.mark = ",", scientific = FALSE), "(", 
                  format(lower_ci, big.mark = ",", scientific = FALSE), ",", 
                  format(upper_ci, big.mark = ",", scientific = FALSE), ")")
)





