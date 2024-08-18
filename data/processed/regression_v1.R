#===================
# PAPER 2 REGRESSIONS
#===================
# REVISED: Hoa Nguyen

# Clear workspace
rm(list = ls()); graphics.off(); cat("\14")

# Load necessary libraries
library(plm)
library(stargazer)
library(readxl)
library(ggplot2)
library(broom)
library(dplyr)
library(rstudioapi)

# Set working directory to the folder containing the data and script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#==========
# READ DATA
#==========

df_1 <- read.csv("score.csv", header = TRUE, stringsAsFactors = FALSE)
summary(df_1)

df_2 = read.csv("company_sector.csv", header=TRUE, stringsAsFactors = FALSE)
summary(df_2)

# Merge datasets based on company_name
df <- merge(df_1, df_2, by = "company_name", all.x = TRUE)

# View summary of the merged dataset
summary(df)

# Print summary tables
print(table(df$evidence_query))
print(table(df$new_region))
print(table(df$evidence_year))

#############################
# ANALYSIS AT COMPANY LEVEL #
#############################

# Function to run regression and plot results
run_analysis <- function(subset_data, query) {
  # Convert relevant columns to factors
  subset_data$new_region <- factor(subset_data$new_region, levels = c("global", unique(subset_data$new_region)[unique(subset_data$new_region) != "global"]))
  subset_data$company_name <- factor(subset_data$company_name, levels = c("AES Corporation", unique(subset_data$company_name)[unique(subset_data$company_name) != "AES Corporation"]))
  subset_data$evidence_year <- factor(subset_data$evidence_year, levels = c("2014", unique(subset_data$evidence_year)[unique(subset_data$evidence_year) != "2014"]))
  
  # Fit the regression model
  model <- lm(evidence_score ~ new_region + company_name + evidence_year, data = subset_data)
  
  # Extract the model coefficients and confidence intervals
  tidy_model <- tidy(model, conf.int = TRUE)
  
  # Filter and plot for regional and yearly effects with p-value < 0.05
  tidy_model_filtered <- tidy_model %>% filter(grepl("new_region|evidence_year", term) & p.value < 0.05)
  p1 <- ggplot(tidy_model_filtered, aes(x = estimate, y = reorder(term, estimate))) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = paste("Regional and Yearly Effects on Evidence Score for", query),
         x = "Estimate",
         y = "Term")
  print(p1)
  
  # Filter and plot for company effects with p-value < 0.05
  tidy_model_filtered_comp <- tidy_model %>% filter(grepl("company_name", term) & p.value < 0.05)
  p2 <- ggplot(tidy_model_filtered_comp, aes(x = estimate, y = reorder(term, estimate))) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = paste("Significant Company Effects on Evidence Score for", query),
         x = "Estimate",
         y = "Company")
  print(p2)
}

# Run analysis for each evidence_query category
unique_queries <- unique(df$evidence_query)
lapply(unique_queries, function(query) {
  subset_data <- df[df$evidence_query == query, ]
  run_analysis(subset_data, query)
})



#############################
# ANALYSIS AT SECTOR LEVEL #
#############################


# Function to run regression and plot results
run_analysis <- function(subset_data, sector) {
  # Convert relevant columns to factors
  subset_data$new_region <- factor(subset_data$new_region, levels = c("global", unique(subset_data$new_region)[unique(subset_data$new_region) != "global"]))
  subset_data$sector <- factor(subset_data$sector, levels = c("Electric Utilities", unique(subset_data$sector)[unique(subset_data$sector) != "Electric Utilities"]))
  subset_data$evidence_year <- factor(subset_data$evidence_year, levels = c("2014", unique(subset_data$evidence_year)[unique(subset_data$evidence_year) != "2014"]))
  
  # Fit the regression model
  model <- lm(evidence_score ~ new_region + sector + evidence_year, data = subset_data)
  
  # Extract the model coefficients and confidence intervals
  tidy_model <- tidy(model, conf.int = TRUE)
  
  # Filter and plot for regional and yearly effects with p-value < 0.05
  tidy_model_filtered <- tidy_model %>% filter(grepl("new_region|evidence_year", term) & p.value < 0.05)
  p1 <- ggplot(tidy_model_filtered, aes(x = estimate, y = reorder(term, estimate))) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = paste("Regional and Yearly Effects on Evidence Score for:", query),
         x = "Estimate",
         y = "Term")
  print(p1)
  
  # Filter and plot for sector effects with p-value < 0.05
  tidy_model_filtered_comp <- tidy_model %>% filter(grepl("sector", term) & p.value < 0.05)
  p2 <- ggplot(tidy_model_filtered_comp, aes(x = estimate, y = reorder(term, estimate))) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = paste("Significant sector Effects on Evidence Score for:", query),
         x = "Estimate",
         y = "Sector")
  print(p2)
}

# Run analysis for each sector
unique_sectors <- unique(df$sector)
lapply(unique_sectors, function(sector) {
  subset_data <- df[df$sector == sector, ]
  run_analysis(subset_data, sector)
})

