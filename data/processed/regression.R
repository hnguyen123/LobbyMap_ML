#===================
#PAPER 2 REGRESSIONS
#===================

# REVISED: Hoa Nguyen

#install.packages("rstudioapi")

rm(list=ls()); graphics.off(); cat("\14"); 

#Set working directory as the same folder that contains data and R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(plm)
#install.packages("stargazer")
library(stargazer)
library(readxl) #package to import excel file
# Load necessary libraries
library(ggplot2)
library(broom)
library(dplyr)
#install.packages("Hmisc")
library(Hmisc)


#==========
#READ DATA
#==========


df_1 <- read.csv("score.csv", header = TRUE, stringsAsFactors = FALSE)
summary(df_1)

df_2 = read.csv("company_sector.csv", header=TRUE, stringsAsFactors = FALSE)
summary(df_2)

# Merge datasets based on company_name
df <- merge(df_1, df_2, by = "company_name", all.x = TRUE)

# View summary of the merged dataset
summary(df)

# View summary of the merged dataset
summary(df)

tab_query <- table(df$evidence_query)
print(tab_query)

tab_region <- table(df$new_region)
print(tab_region)

tab_year <- table(df$evidence_year)
print(tab_year)

tab_company <- table(df$company_name)
print(tab_company)

### sector ###
# Create the table of sector counts
tab_sector <- table(df$sector)

# Convert the table to a dataframe
tab_sector_df <- as.data.frame(tab_sector)

# Rename the columns for clarity
colnames(tab_sector_df) <- c("Sector", "Count")

# Sort the dataframe by the counts in descending order
tab_sector_df <- tab_sector_df[order(-tab_sector_df$Count), ]

# Print the sorted table
print(tab_sector_df)


# Count the total number of unique companies
num_unique_companies <- length(tab_company)
print(num_unique_companies)

# Subset data for category 'A'
subset_data <- df[df$evidence_query == 'Energy Transition & Zero Carbon Technologies', ]
print(subset_data)


# Convert new_region to factor if it is not
subset_data$new_region <- as.factor(subset_data$new_region)
subset_data$new_region <- relevel(subset_data$new_region, ref = "global")

subset_data$company_name <- as.factor(subset_data$company_name)
subset_data$company_name <- relevel(subset_data$company_name, ref = "AES Corporation")


subset_data$evidence_year <- as.factor(subset_data$evidence_year)
subset_data$evidence_year <- relevel(subset_data$evidence_year, ref = "2014")

subset_data$sector <- as.factor(subset_data$sector)
subset_data$sector <- relevel(subset_data$sector, ref = "Diversified Mining")

# Check the structure of the data
str(subset_data)

# Linear regression model
model1 <- lm(evidence_score ~ new_region + company_name + evidence_year, data = subset_data)
model2 <- lm(evidence_score ~ new_region + sector + evidence_year, data = subset_data)

# Summary of the model
summary(model1)
summary(model2)

# Extract the model coefficients and confidence intervals
tidy_model_1 <- tidy(model1, conf.int = TRUE)
tidy_model_2 <- tidy(model2, conf.int = TRUE)
# 

# Filter for regional and yearly effects with p-value < 0.05
tidy_model_filtered_1 <- tidy_model_1 %>%
  filter(grepl("new_region|evidence_year", term) & p.value < 0.05)

# Create a coefficient plot for regional and yearly effects
ggplot(tidy_model_filtered_1, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Regional and Yearly Effects on Evidence Score (controlled for company)",
       x = "Estimate",
       y = "Term")

# Filter for company effects with p-value < 0.05
tidy_model_filtered_comp <- tidy_model_1 %>%
  filter(grepl("company_name", term) & p.value < 0.05)

# Create a coefficient plot for significant company effects with p < 0.05
ggplot(tidy_model_filtered_comp, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Significant Company Effects on Evidence Score",
       x = "Estimate",
       y = "Company")


##### Analysis for sector

# Filter for regional and yearly effects with p-value < 0.05
tidy_model_filtered_2 <- tidy_model_2 %>%
  filter(grepl("new_region|evidence_year", term) & p.value < 0.05)

# Create a coefficient plot for regional and yearly effects
ggplot(tidy_model_filtered_2, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Regional and Yearly Effects on Evidence Score (controlled for sector)",
       x = "Estimate",
       y = "Term")

# Filter for company effects with p-value < 0.05
tidy_model_filtered_sector <- tidy_model_2 %>%
  filter(grepl("sector", term) & p.value < 0.05)

# Create a coefficient plot for significant company effects with p < 0.05
ggplot(tidy_model_filtered_sector, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Significant Sector Effects on Evidence Score",
       x = "Estimate",
       y = "Sector")

