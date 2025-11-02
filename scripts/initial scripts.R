# Required packages & libraries ----
# Load libraries
library(tidyverse)  
library(janitor)    
library(lubridate)  
library(here)       
library(knitr)
library(ggplot2)
library(dplyr)

# Modeling libraries (for Project 2) ----
library(caret)
library(xgboost)
library(randomForest)
library(Metrics)

# 1. Load data with explicit parsing ----
tx <- read_csv(here("data", "Superstore.csv")) %>%
  clean_names() %>%
  select(customer_id, order_date, sales) %>%
  mutate(
    order_date = mdy(order_date),
    sales = as.numeric(sales)
  ) %>%
  filter(!is.na(order_date), sales > 0)

# 2. Create time based RFM features & targets ----
# Define cutoff date
cutoff_date <- as.Date("2017-07-01")

# Split data into train (before cutoff) and future (after cutoff)
tx_train <- tx %>% filter(order_date < cutoff_date)
tx_future <- tx %>% filter(order_date >= cutoff_date)

# Build RFM features from TRAIN period
rfm_features <- tx_train %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.numeric(cutoff_date - max(order_date)),
    frequency = n(),
    monetary = sum(sales),
    first_purchase = min(order_date),
    .groups = "drop"
  ) %>%
  mutate(
    tenure_days = as.numeric(cutoff_date - first_purchase),
    r_score = ntile(-recency, 5),
    f_score = ntile(frequency, 5),
    m_score = ntile(monetary, 5)
  )

# CLV target: total spend in future window
clv_target <- tx_future %>%
  group_by(customer_id) %>%
  summarise(future_spend = sum(sales), .groups = "drop")

# Churn target: 1 if NO purchase in future window
all_customers <- tibble(customer_id = unique(rfm_features$customer_id))
churn_target <- all_customers %>%
  mutate(is_churn = ifelse(customer_id %in% tx_future$customer_id, 0, 1))

# Merge into final modeling dataset
model_data <- rfm_features %>%
  left_join(clv_target, by = "customer_id") %>%
  mutate(future_spend = replace_na(future_spend, 0)) %>%
  left_join(churn_target, by = "customer_id")

# 3. Train the churn prediction model ----
# Convert is_churn to factor with "No" = 0, "Yes" = 1 (and "Yes" as the positive class)
model_data$is_churn <- factor(model_data$is_churn, levels = c(0, 1), labels = c("No", "Yes"))

# Use 1 - probability as your "retention score"
model_data$retention_score <- 1 - predict(churn_xgb, model_data, type = "prob")[, "Yes"]

# High retention_score = likely to stay
# Low retention_score = likely to churn

# Prepare modeling data
churn_df <- model_data %>%
  select(is_churn, recency, frequency, monetary, r_score, f_score, m_score, tenure_days) %>%
  na.omit()

# Split into train and test
set.seed(123)
train_idx <- createDataPartition(churn_df$is_churn, p = 0.8, list = FALSE)
churn_train <- churn_df[train_idx, ]
churn_test  <- churn_df[-train_idx, ]

# Train control
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Train XGBoost model
churn_xgb <- train(
  is_churn ~ .,
  data = churn_train,
  method = "xgbTree",
  trControl = ctrl,
  metric = "ROC"
)

# Predict probabilities for "Yes" (churn)
churn_pred_prob <- predict(churn_xgb, churn_test, type = "prob")[, "Yes"]

# Evaluate AUC
auc_val <- Metrics::auc(ifelse(churn_test$is_churn == "Yes", 1, 0), churn_pred_prob)
cat("Churn Model - AUC:", round(auc_val, 3), "\n")

# 4. Advanced Segmentation for Marketing Strategy.----
# Load RFM segments (from Project 1)
rfm_segments <- read_csv(here("output", "rfm_customer_segments.csv")) %>%
  select(customer_id, Segment) %>%          
  rename(segment = Segment)                

# Merge into model_data
model_data <- model_data %>%
  left_join(rfm_segments, by = "customer_id")

# 5. Add CLV and Retention Predictions ----
# Add CLV prediction (from earlier)
model_data$clv_pred <- predict(clv_xgb, model_data)

# Add retention score (1 - churn probability)
model_data$retention_score <- 1 - predict(churn_xgb, model_data, type = "prob")[, "Yes"]

# 6. Create Advanced Marketing Segments ----
# Define value tiers based on CLV prediction
model_data <- model_data %>%
  mutate(
    value_tier = case_when(
      clv_pred >= quantile(clv_pred, 0.75) ~ "High Value",
      clv_pred >= quantile(clv_pred, 0.25) ~ "Medium Value",
      TRUE ~ "Low Value"
    ),
    retention_tier = case_when(
      retention_score >= quantile(retention_score, 0.75) ~ "High Retention",
      retention_score >= quantile(retention_score, 0.25) ~ "Medium Retention",
      TRUE ~ "Low Retention"
    ),
    marketing_segment = paste(value_tier, retention_tier, sep = " + ")
  )

# 7. Generate Actionable Marketing Recommendations ----
# Summarize segments
segment_summary <- model_data %>%
  count(marketing_segment, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# View top segments
print(segment_summary)

# 8. Save full dataset with predictions ----
write_csv(model_data, here("output", "predictive_customer_segments.csv"))

# 9. Visualize Segments in R Markdown (or R Script) ----

# Pre-calculate counts per segment
library(ggplot2)
library(dplyr)

# Pre-calculate counts per segment
viz_data <- model_data %>%
  count(value_tier, retention_tier) %>%
  mutate(
    value_tier = factor(value_tier, levels = c("Low Value", "Medium Value", "High Value")),
    retention_tier = factor(retention_tier, levels = c("Low Retention", "Medium Retention", "High Retention"))
  )

# Add a unique pastel color for each of the 9 segments
viz_data <- viz_data %>%
  mutate(
    segment_color = case_when(
      value_tier == "Low Value" & retention_tier == "Low Retention" ~ "#FFB3BA",
      value_tier == "Medium Value" & retention_tier == "Low Retention" ~ "#FFDFBA",
      value_tier == "High Value" & retention_tier == "Low Retention" ~ "#BAE1FF",
      value_tier == "Low Value" & retention_tier == "Medium Retention" ~ "#D8BFD8",
      value_tier == "Medium Value" & retention_tier == "Medium Retention" ~ "#FFFFBA",
      value_tier == "High Value" & retention_tier == "Medium Retention" ~ "#B0E0E6",
      value_tier == "Low Value" & retention_tier == "High Retention" ~ "#E6E6FA",
      value_tier == "Medium Value" & retention_tier == "High Retention" ~ "#F0E68C",
      value_tier == "High Value" & retention_tier == "High Retention" ~ "#ADD8E6"
    )
  )

# Plot with flat pastel colors
p1 <- ggplot(viz_data, aes(x = value_tier, y = retention_tier)) +
  geom_tile(aes(fill = segment_color), color = "white") +
  scale_fill_identity() +
  labs(
    title = "Customer Segments: Predicted Value vs Retention",
    x = "Predicted Value Tier",
    y = "Retention Tier"
  ) +
  theme_minimal()

print(p1)

# 10. Export Top 100 High Value, Low Retention Customers ----
# Identify and export top 100 high-value, low-retention customers
high_value_low_retention <- model_data %>%
  filter(value_tier == "High Value", retention_tier == "Low Retention") %>%
  arrange(desc(clv_pred)) %>%
  head(100)

# Save to output folder
write_csv(high_value_low_retention, here("output", "high_value_low_retention_customers.csv"))

cat("Exported 100 high-value, low-retention customers to output folder.\n")

# 11. Calculate Expected ROI by Segment ----
# ROI assumptions
campaign_cost_per_customer <- 5
retention_uplift <- 0.10  # 10% increase in retention

# Calculate segment-level ROI
roi_summary <- model_data %>%
  group_by(marketing_segment) %>%
  summarise(
    customers = n(),
    avg_clv = mean(clv_pred),
    total_clv = sum(clv_pred),
    .groups = "drop"
  ) %>%
  mutate(
    retained_customers = customers * retention_uplift,
    incremental_revenue = retained_customers * avg_clv,
    campaign_cost = customers * campaign_cost_per_customer,
    expected_roi = (incremental_revenue - campaign_cost) / campaign_cost
  ) %>%
  arrange(desc(expected_roi))

# View top ROI segments
print(roi_summary %>% select(marketing_segment, customers, avg_clv, expected_roi))

# Save ROI summary
write_csv(roi_summary, here("output", "segment_roi_summary.csv"))

# 12. Add Product Category to model_data ----
library(dplyr)
library(readr)

# Get top product category by total spend per customer
top_category <- read_csv(here("data", "Superstore.csv")) %>%
  clean_names() %>%
  select(customer_id, category, sales) %>%
  group_by(customer_id, category) %>%
  summarise(total_spend = sum(sales), .groups = "drop") %>%
  arrange(desc(total_spend)) %>%
  group_by(customer_id) %>%
  filter(row_number() == 1) %>%   # Keep only the top category per customer
  ungroup() %>%
  select(customer_id, top_category = category)

# Merge into model_data
model_data <- model_data %>%
  left_join(top_category, by = "customer_id")

# 13. Create Enhanced Marketing Segments with Category ----
# Create combined segment: Value + Retention + Top Category
model_data <- model_data %>%
  mutate(
    category_segment = paste(value_tier, retention_tier, top_category, sep = " + ")
  )

# 14. Save Final Enriched Dataset ----
write_csv(model_data, here("output", "predictive_customer_segments_with_category.csv"))

cat("saved final dataset with category segments to output folder.\n")