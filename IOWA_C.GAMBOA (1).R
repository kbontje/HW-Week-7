# District Iowa Education Analysis

# (1) Import libraries and read files
library(tidyverse)
library(readr)
library (dplyr)
library(stringr)
library(ggplot2)

Iowa <- read_csv("Class_7/Iowa_School_District_Expenditures_by_Fiscal_Year-EASY.csv")
View(Iowa)

# (2) EDA

# Review column names and structure
colnames(Iowa)

# Rename all columns to snake_case and verify changes
Iowa <- Iowa %>%
  rename_with(~ str_replace_all(., "\\s+", "_") %>% 
                str_to_lower())
colnames(Iowa)

#Check if columns are identical to drop them by counting unique values
Iowa %>%
  count(enrollment_category) %>%
  print()

Iowa %>%
  count(enrollment_category_number) %>%
  print()

Iowa %>%
  count(dist) %>%
  print()

Iowa %>%
  count(de_district) %>%
  print()

# Drop the 'column_name' and'enrollment_category' column
Iowa <- Iowa %>%
  select(-'column_name',-'enrollment_category', 'de_district')
colnames(Iowa)

#Let's first inspect the data structure
str(Iowa)
summary(Iowa)
head(Iowa)

# Get min and max values for 'amount'
min_amount <- min(Iowa$amount, na.rm = TRUE)
max_amount <- max(Iowa$amount, na.rm = TRUE)

#Since we've negative values in the Expenditures Per Pupil and Amount we will check if we convert them to '0'
#2.1 Count the negative values

negative_count <- Iowa %>%
  summarise(
    total_values = n(),
    negative_expenditures = sum(expenditures_per_pupil < 0, na.rm = TRUE),
    proportion_negative_expenditures = (negative_expenditures / total_values) * 100,
    negative_amount = sum(amount < 0, na.rm = TRUE),
    proportion_negative_amount = (negative_amount / total_values) * 100
  )

print(negative_count)

summary(Iowa$expenditures_per_pupil)
summary(Iowa$amount)

#2.2 Visualization to analyze if we should convert negatives to zero or just remove rows

str(Iowa$expenditures_per_pupil)

# Plot expenditures_per_pupil distribution
ggplot(Iowa, aes(x = expenditures_per_pupil)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  xlim(-50, 1000) +
  ggtitle("Distribution of Expenditures Per Pupil (Adjusted)") +
  xlab("Expenditures Per Pupil") +
  ylab("Count")

# Plot amount distribution
ggplot(Iowa, aes(x = amount)) +
  geom_histogram(binwidth = 5000, fill = "green", color = "black") +
  xlim(0, 1e+05) + 
  ggtitle("Distribution of Amount") + 
  xlab("Amount") + 
  ylab("Count") 

#2.3 Count zeros in expenditures_per_pupil and amount
zero_count <- Iowa %>%
  summarise(
    zero_expenditures = sum(expenditures_per_pupil == 0, na.rm = TRUE),
    proportion_zero_expenditures = (zero_expenditures / n()) * 100,
    zero_amount = sum(amount == 0, na.rm = TRUE),
    proportion_zero_amount = (zero_amount / n()) * 100
  )

print(zero_count)

#2.4 Count NAs in expenditures_per_pupil and amount
na_count <- Iowa %>%
  summarise(
    na_expenditures = sum(is.na(expenditures_per_pupil)),
    proportion_na_expenditures = (na_expenditures / n()) * 100,
    na_amount = sum(is.na(amount)),
    proportion_na_amount = (na_amount / n()) * 100
  )

print(na_count)

### Observations:
# 1. Distribution of Amount and Expenditures per Pupil:
# - The histogram for amount shows that the majority of values are close to zero.
# - The summary statistics for expenditures_per_pupil and amount confirm that:
#    - The median for both columns is already 0.
#    - The mean is very low compared to the maximum values, indicating a highly skewed distribution.
# 2. High Proportion of Zeros:
# - 79.3% of expenditures_per_pupil values are already zero.
# - 78.7% of amount values are zero.
#
# Thus, transforming the data and replacing all negative and NA values with zero, will further increase 
# the dominance of zero values, making it harder to extract meaningful insights from the data.
# Also, quartiles (1st, median, and 3rd) are already zero, so this transformation won't affect them but 
# will likely eliminate the variability in the data.
###

#Considering this, I'm eliminating only the rows where both expenditures and amount are negative
# Remove rows where both expenditures_per_pupil and amount are negative
Iowa_filtered <- Iowa %>%
  filter(amount >= 0 & expenditures_per_pupil >= 0)

# Summary of the final dataset
summary(Iowa_filtered)

#Now, I want to create a dictionary ofor AEAs, so they've meaning and we can have a better view
# Define the dictionary for AEA mappings
agency_mapping <- c(
  "1" = "Keystone",
  "2" = "AEA 267",
  "3" = "Lakeland",
  "4" = "Northwest",
  "5" = "Arrowhead",
  "6" = "AEA 267",
  "7" = "AEA 267",
  "8" = "Prairie Lakes",
  "9" = "Mississippi Bend",
  "10" = "Grant Wood",
  "11" = "Heartland",
  "12" = "Northwest",
  "13" = "Green Hills",
  "14" = "Green Hills",
  "15" = "Great Prairie",
  "16" = "Great Prairie"
)

# Map the `AEA` column to new agency names
Iowa_filtered <- Iowa_filtered %>%
  mutate(agency_name = agency_mapping[as.character(aea)])

# Drop AEA and DE_district columns, as Dist, District name and DE have same info
Iowa_cleaned <- Iowa_filtered %>%
  select(-aea, -de_district)

# Check the first few rows to confirm changes
head(Iowa_cleaned)

# Confirm that the columns have been removed
colnames(Iowa_cleaned)

# (3) Grouby

# 3.1 Groupby agency
agency_summary <- Iowa_cleaned %>%
group_by(agency_name) %>%
  summarise(
    total_amount = sum(amount),
    mean_amount = mean(amount),
    total_expenditures = sum(expenditures_per_pupil),
    mean_expenditures = mean(expenditures_per_pupil)
  )

print(agency_summary)

# Compare
Iowa_non_zero <- Iowa_cleaned %>%
  filter(amount > 0 & expenditures_per_pupil > 0)

summary(Iowa_non_zero)

###Conclusion and next steps##
# 1.Agency highlights:Heartland has the highest total and average spending, indicating 
# larger or more resource-intensive districts. Arrowhead and Green Hills show lower totals and averages.
# 2. Non-Zero insights:Median expenditures per pupil (23.1) and amount (169,231) 
# suggest most districts operate with lower budgets, while outliers inflate the means.
# 3. Enrollment correlation: Most districts fall into mid-size enrollment categories (mean = 3). 
# as a potential next step, I would analyze if larger districts receive proportional funding.
# 4. Impact of zeros:Removing zeros clarified resource allocation but highlighted disparities in 
# spending, requiring targeted analysis of underfunded districts.
### Next Steps:
# 1. Continue grouping by fund to explore spending patterns, aiming to answer for spending efficiency and allocation trends
# 2. Investigate outliers and their context to refine insights.
###

# (4) Hypothesis and visualizatios

# H1: Spending efficiency

fund_summary <- Iowa_cleaned %>%
  group_by(fund) %>%
  summarise(
    total_amount = sum(amount, na.rm = TRUE),
    mean_amount = mean(amount, na.rm = TRUE),
    total_expenditures = sum(expenditures_per_pupil, na.rm = TRUE),
    mean_expenditures = mean(expenditures_per_pupil, na.rm = TRUE)
  )

print(fund_summary)

fund_agency_summary <- Iowa_cleaned %>%
  group_by(fund, agency_name) %>%
  summarise(
    total_amount = sum(amount, na.rm = TRUE),
    mean_amount = mean(amount, na.rm = TRUE),
    total_expenditures = sum(expenditures_per_pupil, na.rm = TRUE),
    mean_expenditures = mean(expenditures_per_pupil, na.rm = TRUE)
  )

print(fund_agency_summary)

# Visualization total spending by fund
ggplot(fund_summary, aes(x = fund, y = total_amount, fill = fund)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Spending by Fund") +
  xlab("Fund") +
  ylab("Total Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Preeliminary conclusion
# 1. Spending efficiency analysis: General Fund accounts for the majority of spending, 
# indicating its pivotal role in allocation and the need to analyze smaller funds for hidden inefficiencies 
# or missed opportunities. Additionally, Funds like Entrepreneurial Reorganization and Emergency Disaster have 
# significantly low total and mean expenditures, signaling potential inefficiencies or underutilization.
# 2. High-Spending funds and agencies:General Fund and Sales Tax Fund have the highest total spending. Agencies like 
# Heartland and Grant Wood lead in mean expenditures, potentially reflecting higher allocations or more effective spending strategies.
# 3. Allocation trends: Consistency in higher expenditures is observed in specific funds and agencies. Sales Tax Fund, 
# despite being one of the highest total spenders, has relatively modest mean expenditures.
###
  
# H2: Allocation trends:
fiscal_year_agency_summary <- Iowa_cleaned %>%
  group_by(fiscalyear, agency_name) %>%
  summarise(
    total_amount = sum(amount, na.rm = TRUE),
    mean_amount = mean(amount, na.rm = TRUE),
    total_expenditures = sum(expenditures_per_pupil, na.rm = TRUE),
    mean_expenditures = mean(expenditures_per_pupil, na.rm = TRUE)
  )

print(fiscal_year_agency_summary)

# Visualization yearly trends by agency
ggplot(fiscal_year_agency_summary, aes(x = fiscalyear, y = total_amount, color = agency_name, group = agency_name)) +
  geom_line() +
  ggtitle("Yearly Spending Trends by Agency") +
  xlab("Fiscal Year") +
  ylab("Total Amount") +
  theme_minimal()

###Preeliminary conclusion
# 1. Consistent agency growth: Agencies like Heartland and Green Hills show steady and significant 
# growth in total amount allocation across fiscal years, indicating prioritized or increasing funding trends.
# 2. Stable agencies: Agencies such as Arrowhead and Great Prairie exhibit more stable funding 
# levels over the years, suggesting consistent but non-priority allocations.
# 3. Outliers: Heartland leads in total allocation by a large margin, emphasizing its dominance or 
# higher financial needs compared to other agencies.
# 4. Yearly allocation fluctuations: Some agencies, such as Grant Wood and Keystone, exhibit periodic jumps in funding, 
# which could indicate special projects or cyclical funding requirements. COntraty, Agencies like AEA 267 and 
# Mississippi Bend have comparatively lower total funding levels, possibly reflecting their geographic, demographic, or operational constraints.
###

###Overall comments and next steps###
# 1. Heartland consistently leads in mean and total funding, followed by Grant Wood and Mississippi Bend.
# 2. Expenditures per pupil steadily increased from 2017 to 2022, with Mississippi Bend and Green Hills 
# leading in per-student spending.
# 3. Funding stability is evident across agencies, with minimal year-over-year variation in total allocations.
# 4. Lower-funded agencies like AEA 267 and Northwest align with lower expenditures per pupil, 
# reflecting consistent resource allocation strategies.
### Next steps###
# Next Steps:
# 1. Analyze expenditures vs. amount at fund/agency levels.
# 2. Investigate project-based funding spikes.
# 3. Add demographic data for context on funding disparities.
# 4. Forecast future allocation trends using historical patterns.
# 5. Visualize fund-specific trends for deeper insights.

