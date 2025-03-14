# Load packages
library(ggplot2)
library(here)
library(dplyr)
library(scales)
library(lubridate)


# load data
LCAdata <- readRDS(here("data", "processed-data", "LCAdata.rds"))

# Univariate Analysis: Distribution Analysis

# Distribution of top 10 SOC_TITLE
# Calculate counts and filter top 10 SOC_TITLE
top10_soc <- LCAdata %>%
  count(SOC_TITLE, sort = TRUE) %>%
  top_n(10, n)
# Plot the top 10 SOC_TITLE
p1 <- ggplot(top10_soc, aes(x = reorder(SOC_TITLE, n), y = n)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Job Titles", x = "Job Title", y = "Count")
# Save the plot
ggsave(filename = "results/figures/top10_soc_title.png", plot = p1, width = 8, height = 6)


# Distribution of top 10 EMPLOYER_NAME
top10_emp_name <- LCAdata %>%
  count(EMPLOYER_NAME, sort = TRUE) %>%
  top_n(10, n)
# Plot the top 10 SOC_TITLE
p2 <- ggplot(top10_emp_name, aes(x = reorder(EMPLOYER_NAME, n), y = n)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Employers", x = "Employer Name", y = "Count")
# Save the plot
ggsave(filename = "results/figures/top10_emp_name.png", plot = p2, width = 8, height = 6)


# Distribution of top 10 EMPLOYER_STATE
top10_emp <- LCAdata %>%
  count(EMPLOYER_STATE, sort = TRUE) %>%
  top_n(10, n)
# Plot the top 10 EMPLOYER_STATE
p3 <- ggplot(top10_emp, aes(x = reorder(EMPLOYER_STATE, n), y = n)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Employer Locations", x = "State", y = "Count")
# Save the plot
ggsave(filename = "results/figures/top10_emp_state.png", plot = p3, width = 8, height = 6)


# Wage units were standardized by retaining only observations where WAGE_UNIT_OF_PAY and PW_UNIT_OF_PAY were specified as "Year", ensuring consistency in wage comparisons.
LCAwage <- LCAdata %>%
  filter(WAGE_UNIT_OF_PAY == "Year", PW_UNIT_OF_PAY == "Year")
# Save as rds
save_data_location <- here::here("data","processed-data","LCAwage.rds")
saveRDS(LCAwage, file = save_data_location)

# Distribution of PREVAILING_WAGE
p4<-ggplot(LCAwage, aes(x = PREVAILING_WAGE)) +
  geom_histogram(binwidth = 5000, fill = "pink", color = "black") +
  scale_x_continuous(labels = comma) +
  labs(title = "Distribution of Prevailing Wage", x = "Prevailing Wage", y = "Frequency")
ggsave(filename = "results/figures/distribution_PREV_WAGE.png", plot = p4, width = 8, height = 6)

# Create a summary table for PREVAILING_WAGE
wage_summary <- LCAwage %>%
  summarise(
    count = n(),
    mean_wage = mean(PREVAILING_WAGE, na.rm = TRUE),
    median_wage = median(PREVAILING_WAGE, na.rm = TRUE),
    sd_wage = sd(PREVAILING_WAGE, na.rm = TRUE),
    min_wage = min(PREVAILING_WAGE, na.rm = TRUE),
    max_wage = max(PREVAILING_WAGE, na.rm = TRUE)
  )
# Save the summary table as an RDS file
saveRDS(wage_summary, file = "results/tables/sum_prev_wage.rds")


# Bivariate Analysis
# To visualize how WAGE_RATE_OF_PAY_FROM varies across different EMPLOYER_STATE, SOC_TITLE, and EMPLOYER_NAME
# Create bar plots for the top 10 categories with the highest median wage within each variable
# Function to get top 10 categories based on highest median wage
get_top10_median <- function(LCAwage, WAGE_RATE_OF_PAY_FROM) {
  LCAwage %>%
    group_by(!!sym(WAGE_RATE_OF_PAY_FROM)) %>%
    summarise(median_wage = median(WAGE_RATE_OF_PAY_FROM, na.rm = TRUE)) %>%
    arrange(desc(median_wage)) %>%
    slice_head(n = 10) %>%
    pull(!!sym(WAGE_RATE_OF_PAY_FROM))
}

# Get top 10 for each category based on highest median wage
top10_states <- get_top10_median(LCAwage, "EMPLOYER_STATE")
top10_soc <- get_top10_median(LCAwage, "SOC_TITLE")
top10_employers <- get_top10_median(LCAwage, "EMPLOYER_NAME")

# Filter data for plotting
LCA_top10 <- LCAwage %>%
  filter(EMPLOYER_STATE %in% top10_states |
           SOC_TITLE %in% top10_soc |
           EMPLOYER_NAME %in% top10_employers)

# Summarize median wage for each category for better bar plot representation
soc_summary <- LCA_top10 %>%
  filter(SOC_TITLE %in% top10_soc) %>%
  group_by(SOC_TITLE) %>%
  summarise(median_wage = median(WAGE_RATE_OF_PAY_FROM, na.rm = TRUE))

emp_summary <- LCA_top10 %>%
  filter(EMPLOYER_NAME %in% top10_employers) %>%
  group_by(EMPLOYER_NAME) %>%
  summarise(median_wage = median(WAGE_RATE_OF_PAY_FROM, na.rm = TRUE))

state_summary <- LCA_top10 %>%
  filter(EMPLOYER_STATE %in% top10_states) %>%
  group_by(EMPLOYER_STATE) %>%
  summarise(median_wage = median(WAGE_RATE_OF_PAY_FROM, na.rm = TRUE))

# Bar plot for SOC_TITLE
p5 <- ggplot(soc_summary, aes(x = reorder(SOC_TITLE, median_wage), y = median_wage)) +
  geom_bar(stat = "identity", fill = "plum") +
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0, 800000), oob = scales::squish) +
  labs(title = "Top 10 Job Titles by Highest Median Wage",
       x = "Job Title",
       y = "Median Wage Rate of Pay")

ggsave("results/figures/wage_by_soc_title.png", plot = p5, width = 8, height = 6)

# Bar plot for EMPLOYER_NAME
p6 <- ggplot(emp_summary, aes(x = reorder(EMPLOYER_NAME, median_wage), y = median_wage)) +
  geom_bar(stat = "identity", fill = "plum") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 2500000), labels = comma, oob = scales::squish) +
  labs(title = "Top 10 Employers by Highest Median Wage",
       x = "Employer Name",
       y = "Median Wage Rate of Pay")

ggsave("results/figures/wage_by_employer.png", plot = p6, width = 8, height = 6)

# Bar plot for EMPLOYER_STATE
p7 <- ggplot(state_summary, aes(x = reorder(EMPLOYER_STATE, median_wage), y = median_wage)) +
  geom_bar(stat = "identity", fill = "plum") +
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0, 200000), oob = scales::squish) +
  labs(title = "Top 10 States by Highest Median Wage",
       x = "Employer State",
       y = "Median Wage Rate of Pay")

ggsave("results/figures/wage_by_state.png", plot = p7, width = 8, height = 6)


# Function to get bottom 10 categories based on lowest median wage
get_bottom10_median <- function(LCAwage, WAGE_RATE_OF_PAY_FROM) {
  LCAwage %>%
    group_by(!!sym(WAGE_RATE_OF_PAY_FROM)) %>%
    summarise(median_wage = median(WAGE_RATE_OF_PAY_FROM, na.rm = TRUE)) %>%
    arrange(median_wage) %>%  # Arrange in ascending order to get the lowest wages
    slice_head(n = 10) %>%
    pull(!!sym(WAGE_RATE_OF_PAY_FROM))
}

# Get bottom 10 for each category based on lowest median wage
bottom10_states <- get_bottom10_median(LCAwage, "EMPLOYER_STATE")
bottom10_soc <- get_bottom10_median(LCAwage, "SOC_TITLE")
bottom10_employers <- get_bottom10_median(LCAwage, "EMPLOYER_NAME")

# Filter data for plotting
LCA_bottom10 <- LCAwage %>%
  filter(EMPLOYER_STATE %in% bottom10_states |
           SOC_TITLE %in% bottom10_soc |
           EMPLOYER_NAME %in% bottom10_employers)

# Summarize median wage for each category for bar plot representation
soc_summary1 <- LCA_bottom10 %>%
  filter(SOC_TITLE %in% bottom10_soc) %>%
  group_by(SOC_TITLE) %>%
  summarise(median_wage = median(WAGE_RATE_OF_PAY_FROM, na.rm = TRUE))

emp_summary1 <- LCA_bottom10 %>%
  filter(EMPLOYER_NAME %in% bottom10_employers) %>%
  group_by(EMPLOYER_NAME) %>%
  summarise(median_wage = median(WAGE_RATE_OF_PAY_FROM, na.rm = TRUE))

state_summary1 <- LCA_bottom10 %>%
  filter(EMPLOYER_STATE %in% bottom10_states) %>%
  group_by(EMPLOYER_STATE) %>%
  summarise(median_wage = median(WAGE_RATE_OF_PAY_FROM, na.rm = TRUE))

# Bar plot for SOC_TITLE
p8 <- ggplot(soc_summary, aes(x = reorder(SOC_TITLE, median_wage), y = median_wage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0, 30000), oob = scales::squish) +
  labs(title = "Bottom 10 Job Titles by Lowest Median Wage",
       x = "Job Title",
       y = "Median Wage Rate of Pay")

ggsave("results/figures/bottom_wage_by_soc_title.png", plot = p8, width = 8, height = 6)

# Bar plot for EMPLOYER_NAME
p9 <- ggplot(emp_summary, aes(x = reorder(EMPLOYER_NAME, median_wage), y = median_wage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 30000), labels = comma, oob = scales::squish) +
  labs(title = "Bottom 10 Employers by Lowest Median Wage",
       x = "Employer Name",
       y = "Median Wage Rate of Pay")

ggsave("results/figures/bottom_wage_by_employer.png", plot = p9, width = 8, height = 6)

# Bar plot for EMPLOYER_STATE
p10 <- ggplot(state_summary, aes(x = reorder(EMPLOYER_STATE, median_wage), y = median_wage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0, 100000), oob = scales::squish) +
  labs(title = "Bottom 10 States by Lowest Median Wage",
       x = "Employer State",
       y = "Median Wage Rate of Pay")

ggsave("results/figures/bottom_wage_by_state.png", plot = p10, width = 8, height = 6)




# Analyze remote trend over time
# Convert RECEIVED_DATE to Date format and extract the year
LCAdata <- LCAdata %>%
  mutate(RECEIVED_DATE = as.Date(RECEIVED_DATE, format = "%Y-%m-%d"),
         year = year(RECEIVED_DATE))

# Create a remote flag: TRUE if EMPLOYER_STATE and WORKSITE_STATE differ
LCAdata <- LCAdata %>%
  mutate(remote = EMPLOYER_STATE != WORKSITE_STATE)

# Aggregate to get absolute counts of remote applications per year
remote_trends_abs <- LCAdata %>%
  group_by(year) %>%
  summarise(remote_count = sum(remote, na.rm = TRUE))

# Plot the absolute count of remote applications over the years
p_remote_abs <- ggplot(remote_trends_abs, aes(x = year, y = remote_count)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "lightcoral", size = 2) +
  scale_y_continuous(labels = comma) +
  labs(title = "Remote Work Trends Over Years",
       x = "Year",
       y = "Number of Remote Work Cases") +
  theme_minimal()

# Save the plot
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(filename = "results/figures/remote_trend_absolute.png", plot = p_remote_abs, width = 8, height = 6)

