# Choose WAGE_RATE_OF_PAY_FROM as the outcome of interest to study the wages of H-1B workers

# Load package
library(tidymodels)
library(dplyr)
library(lubridate)
library(embed)
library(here)
library(ggplot2)
library(scales)
library(rsample)
#install.packages("ranger")

# Load the data
LCAwage <- readRDS(here("data", "processed-data", "LCAwage.rds"))

# Filter for January 2024 using RECEIVED_DATE and convert predictors to factors
LCAwage_Jan2024 <- LCAwage %>% 
  filter(year(RECEIVED_DATE) == 2024, month(RECEIVED_DATE) == 1, , day(RECEIVED_DATE) == c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) %>%
  mutate(
    SOC_TITLE = as.factor(SOC_TITLE),
    EMPLOYER_STATE = as.factor(EMPLOYER_STATE),
    WORKSITE_STATE = as.factor(WORKSITE_STATE),
    EMPLOYER_NAME = as.factor(EMPLOYER_NAME)
  )

# Make histogram to visualize distribution
hist_wage<-ggplot(LCAwage_Jan2024, aes(x = WAGE_RATE_OF_PAY_FROM)) +
  geom_histogram(binwidth = 5000, fill = "pink", color = "black") +
  scale_x_continuous(labels = comma) +
  labs(title = "Distribution of Wage", x = "Wage", y = "Frequency")
ggsave(filename = "results/figures/distribution_WAGE_Jan2024.png", plot = hist_wage, width = 8, height = 6)

# Create a categorical variable for wage ranges
LCAwage_Jan2024 <- LCAwage_Jan2024 %>%
  mutate(WAGE_CAT = cut(
    WAGE_RATE_OF_PAY_FROM,
    breaks = c(0, 75000, 150000, 225000, Inf),
    labels = c("(0,75000)", "(75000,150000)", "(150000,225000)", "Above 225000"),
    right = FALSE
  ))
head(LCAwage_Jan2024)

save_data_location <- here::here("data","processed-data","LCAwage_Jan2024.rds")
saveRDS(LCAwage_Jan2024, file = save_data_location)

# Split the data into training (80%) and testing (20%) sets
set.seed(123)
data_split <- initial_split(LCAwage_Jan2024, prop = 0.8, strata = WAGE_CAT)
train_data <- training(data_split)
test_data  <- testing(data_split)

# Use SOC_TITLE as predictor
# Create a recipe to one-hot encode SOC_TITLE (only predictor)
class_recipe1 <- recipe(WAGE_CAT ~ SOC_TITLE, data = train_data) %>%
  step_dummy(SOC_TITLE, one_hot = TRUE)

# Specify a random forest model for classification using the ranger engine
rf_class_spec1 <- rand_forest() %>% 
  set_engine("ranger") %>%
  set_mode("classification")

# Create a workflow combining the recipe and the model specification
rf_class_workflow1 <- workflow() %>%
  add_recipe(class_recipe1) %>%
  add_model(rf_class_spec1)

# Fit the classification model on the training data
rf_class_fit1 <- rf_class_workflow1 %>% fit(data = train_data)

# Generate predictions on the test data and bind with test_data
test_predictions1 <- test_data %>% 
  bind_cols(
    predict(rf_class_fit1, test_data),
    predict(rf_class_fit1, test_data, type = "prob")
  )

# Evaluate accuracy on test data and label it
test_accuracy1 <- test_predictions1 %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Test")

# Generate predictions on the training data and bind with train_data
train_predictions1 <- train_data %>% 
  bind_cols(
    predict(rf_class_fit1, train_data),
    predict(rf_class_fit1, train_data, type = "prob")
  )

# Evaluate accuracy on training data and label it
train_accuracy1 <- train_predictions1 %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Train")

# Combine the metrics into one tibble
combined_accuracy1 <- bind_rows(train_accuracy1, test_accuracy1) %>%
  select(dataset, .metric, .estimator, .estimate)

combined_accuracy1

# Use EMPLOYER_STATE as predictor
# Create a recipe to one-hot encode EMPLOYER_STATE (only predictor)
class_recipe2 <- recipe(WAGE_CAT ~ EMPLOYER_STATE, data = train_data) %>%
  step_dummy(EMPLOYER_STATE, one_hot = TRUE)

# Specify a random forest model for classification using the ranger engine
rf_class_spec2 <- rand_forest() %>% 
  set_engine("ranger") %>%
  set_mode("classification")

# Create a workflow combining the recipe and the model specification
rf_class_workflow2 <- workflow() %>%
  add_recipe(class_recipe2) %>%
  add_model(rf_class_spec2)

# Fit the classification model on the training data
rf_class_fit2 <- rf_class_workflow2 %>% fit(data = train_data)

# Generate predictions on the test data and bind with test_data
test_predictions2 <- test_data %>% 
  bind_cols(
    predict(rf_class_fit2, test_data),
    predict(rf_class_fit2, test_data, type = "prob")
  )

# Evaluate accuracy on test data and label it
test_accuracy2 <- test_predictions2 %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Test")

# Generate predictions on the training data and bind with train_data
train_predictions2 <- train_data %>% 
  bind_cols(
    predict(rf_class_fit2, train_data),
    predict(rf_class_fit2, train_data, type = "prob")
  )

# Evaluate accuracy on training data and label it
train_accuracy2 <- train_predictions2 %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Train")

# Combine the metrics into one tibble
combined_accuracy2 <- bind_rows(train_accuracy2, test_accuracy2) %>%
  select(dataset, .metric, .estimator, .estimate)

combined_accuracy2


# Use WORKSITE_STATE as predictor
# Create a recipe to one-hot encode WORKSITE_STATE (only predictor)
class_recipe3 <- recipe(WAGE_CAT ~ WORKSITE_STATE, data = train_data) %>%
  step_dummy(WORKSITE_STATE, one_hot = TRUE)

# Specify a random forest model for classification using the ranger engine
rf_class_spec3 <- rand_forest() %>% 
  set_engine("ranger") %>%
  set_mode("classification")

# Create a workflow combining the recipe and the model specification
rf_class_workflow3 <- workflow() %>%
  add_recipe(class_recipe3) %>%
  add_model(rf_class_spec3)

# Fit the classification model on the training data
rf_class_fit3 <- rf_class_workflow3 %>% fit(data = train_data)

# Generate predictions on the test data and bind with test_data
test_predictions3 <- test_data %>% 
  bind_cols(
    predict(rf_class_fit3, test_data),
    predict(rf_class_fit3, test_data, type = "prob")
  )

# Evaluate accuracy on test data and label it
test_accuracy3 <- test_predictions3 %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Test")

# Generate predictions on the training data and bind with train_data
train_predictions3 <- train_data %>% 
  bind_cols(
    predict(rf_class_fit3, train_data),
    predict(rf_class_fit3, train_data, type = "prob")
  )

# Evaluate accuracy on training data and label it
train_accuracy3 <- train_predictions3 %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Train")

# Combine the metrics into one tibble
combined_accuracy3 <- bind_rows(train_accuracy3, test_accuracy3) %>%
  select(dataset, .metric, .estimator, .estimate)

combined_accuracy3


# Use EMPLOYER_NAME as predictor
# Create a recipe to one-hot encode EMPLOYER_NAME (only predictor)
class_recipe4 <- recipe(WAGE_CAT ~ EMPLOYER_NAME, data = train_data) %>%
  step_dummy(EMPLOYER_NAME, one_hot = TRUE)

# Specify a random forest model for classification using the ranger engine
rf_class_spec4 <- rand_forest() %>% 
  set_engine("ranger") %>%
  set_mode("classification")

# Create a workflow combining the recipe and the model specification
rf_class_workflow4 <- workflow() %>%
  add_recipe(class_recipe4) %>%
  add_model(rf_class_spec4)

# Fit the classification model on the training data
rf_class_fit4 <- rf_class_workflow4 %>% fit(data = train_data)

# Generate predictions on the test data and bind with test_data
test_predictions4 <- test_data %>% 
  bind_cols(
    predict(rf_class_fit4, test_data),
    predict(rf_class_fit4, test_data, type = "prob")
  )

# Evaluate accuracy on test data and label it
test_accuracy4 <- test_predictions4 %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Test")

# Generate predictions on the training data and bind with train_data
train_predictions4 <- train_data %>% 
  bind_cols(
    predict(rf_class_fit4, train_data),
    predict(rf_class_fit4, train_data, type = "prob")
  )

# Evaluate accuracy on training data and label it
train_accuracy4 <- train_predictions4 %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Train")

# Combine the metrics into one tibble
combined_accuracy4 <- bind_rows(train_accuracy4, test_accuracy4) %>%
  select(dataset, .metric, .estimator, .estimate)

combined_accuracy4

# save as rds
saveRDS(combined_accuracy1, file = "results/tables/combined_accuracy1.rds")
saveRDS(combined_accuracy2, file = "results/tables/combined_accuracy2.rds")
saveRDS(combined_accuracy3, file = "results/tables/combined_accuracy3.rds")
saveRDS(combined_accuracy4, file = "results/tables/combined_accuracy4.rds")

# Fit the model using all the four predictors
# Create a recipe that one-hot encodes all four predictors
class_recipe_all <- recipe(WAGE_CAT ~ SOC_TITLE + EMPLOYER_STATE + WORKSITE_STATE + EMPLOYER_NAME, data = train_data) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# Specify a random forest model for classification using the ranger engine
rf_class_spec_all <- rand_forest() %>% 
  set_engine("ranger") %>%
  set_mode("classification")

# Combine the recipe and model into a workflow
rf_class_workflow_all <- workflow() %>%
  add_recipe(class_recipe_all) %>%
  add_model(rf_class_spec_all)

# Fit the classification model on the training data
rf_class_fit_all <- rf_class_workflow_all %>% fit(data = train_data)

# Generate predictions on the training data and bind with the original data
train_predictions_all <- train_data %>% 
  bind_cols(
    predict(rf_class_fit_all, train_data),
    predict(rf_class_fit_all, train_data, type = "prob")
  )

# Generate predictions on the test data and bind with the original data
test_predictions_all <- test_data %>% 
  bind_cols(
    predict(rf_class_fit_all, test_data),
    predict(rf_class_fit_all, test_data, type = "prob")
  )

# Evaluate accuracy on training data and label it
train_accuracy_all <- train_predictions_all %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Train")

# Evaluate accuracy on test data and label it
test_accuracy_all <- test_predictions_all %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Test")

# Combine the metrics into one tibble for comparison
combined_accuracy_all <- bind_rows(train_accuracy_all, test_accuracy_all) %>%
  select(dataset, .metric, .estimator, .estimate)

# Print combined accuracy
combined_accuracy_all

# save as rds
saveRDS(combined_accuracy_all, file = "results/tables/combined_accuracy_all.rds")


# Fit a null model for comparison

# Create a recipe that only includes an intercept (i.e. no predictors)
null_recipe <- recipe(WAGE_CAT ~ 1, data = train_data)

# Specify a null model for classification using the parsnip engine
null_spec <- null_model(mode = "classification") %>%
  set_engine("parsnip")

# Combine the recipe and the null model into a workflow
null_workflow <- workflow() %>%
  add_recipe(null_recipe) %>%
  add_model(null_spec)

# Fit the null model on the training data
null_fit <- null_workflow %>% fit(data = train_data)

# Generate predictions on the test data and bind them with test_data
null_test_predictions <- test_data %>% 
  bind_cols(
    predict(null_fit, test_data),
    predict(null_fit, test_data, type = "prob")
  )

# Evaluate accuracy on the test data and label the dataset
null_test_accuracy <- null_test_predictions %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Test")

# Generate predictions on the training data and bind them with train_data
null_train_predictions <- train_data %>% 
  bind_cols(
    predict(null_fit, train_data),
    predict(null_fit, train_data, type = "prob")
  )

# Evaluate accuracy on the training data and label the dataset
null_train_accuracy <- null_train_predictions %>%
  accuracy(truth = WAGE_CAT, estimate = .pred_class) %>%
  mutate(dataset = "Train")

# Combine the null model metrics into one tibble for comparison
combined_null_accuracy <- bind_rows(null_train_accuracy, null_test_accuracy) %>%
  select(dataset, .metric, .estimator, .estimate)

# Print the combined null model accuracy
combined_null_accuracy

# Save the null model accuracy as an RDS file
saveRDS(combined_null_accuracy, file = "results/tables/combined_null_accuracy.rds")
