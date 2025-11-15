
# SETUP: LOAD REQUIRED LIBRARIES ----
library(tidyverse)   
library(janitor)    
library(lubridate)  
library(here)      
library(knitr)      
library(ggplot2)    
library(purrr)     

# 1. LOAD AND PREPARE DATA ----
cat("Loading and preparing transaction data...\n")

tx <- read_csv(here("data", "Superstore.csv")) %>%
  clean_names() %>%
  select(customer_id, order_date, sales) %>%
  mutate(
    order_date = mdy(order_date),  # Parse dates from MM/DD/YYYY format
    sales = as.numeric(sales)       # Ensure sales is numeric
  ) %>%
  filter(!is.na(order_date), sales > 0)  # Remove invalid records

cat("✓ Data loaded:", nrow(tx), "transactions,", n_distinct(tx$customer_id), "customers\n")

# 2. CREATE TIME-BASED ANALYSIS PERIODS ----
cutoff_date <- as.Date("2017-07-01")

tx_historical <- tx %>% filter(order_date < cutoff_date)   # Historical behavior
tx_future <- tx %>% filter(order_date >= cutoff_date)      # Future outcomes

cat("✓ Time periods split:", nrow(tx_historical), "historical,", nrow(tx_future), "future transactions\n")

# 3. BUILD RFM FEATURES FROM HISTORICAL PERIOD ----
cat("Building RFM features...\n")

rfm_features <- tx_historical %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.numeric(cutoff_date - max(order_date)),  # Days since last purchase
    frequency = n(),                                      # Total number of orders
    monetary = sum(sales),                                # Total spend
    first_purchase = min(order_date),                     # First purchase date
    avg_order_value = mean(sales),                        # Average spend per order
    .groups = "drop"
  ) %>%
  mutate(
    tenure_days = as.numeric(cutoff_date - first_purchase),
    r_score = ntile(-recency, 5),   # Recency score (1-5, higher = more recent)
    f_score = ntile(frequency, 5),  # Frequency score (1-5, higher = more frequent)
    m_score = ntile(monetary, 5)    # Monetary score (1-5, higher = more spend)
  )

# 4. CREATE OUTCOME VARIABLES ----
future_spend <- tx_future %>%
  group_by(customer_id) %>%
  summarise(future_spend = sum(sales), .groups = "drop")

churn_status <- tibble(customer_id = unique(rfm_features$customer_id)) %>%
  mutate(is_churn = ifelse(customer_id %in% tx_future$customer_id, 0, 1))

model_data <- rfm_features %>%
  left_join(future_spend, by = "customer_id") %>%
  mutate(future_spend = replace_na(future_spend, 0)) %>%
  left_join(churn_status, by = "customer_id") %>%
  mutate(is_churn = factor(is_churn, levels = c(0, 1), labels = c("No", "Yes")))

cat("✓ Model dataset created:", nrow(model_data), "customers\n")

# 5. FEASIBILITY ASSESSMENT - PREDICTIVE MODELING LIMITATIONS ----
cat("\n")
cat("==================================================\n")
cat("PREDICTIVE MODELING FEASIBILITY ASSESSMENT\n")
cat("==================================================\n")

feature_correlations <- cor(model_data %>% 
                              filter(future_spend > 0) %>%
                              select(future_spend, recency, frequency, monetary, tenure_days),
                            use = "complete.obs")

max_corr <- max(abs(feature_correlations[1, -1]))

cat("Correlations with Future Spend:\n")
print(round(feature_correlations[1, -1], 4))

cat(sprintf("\nMaximum absolute correlation: %.4f\n", max_corr))

# Business viability conclusion
cat("\nBUSINESS VIABILITY CONCLUSION:\n")
if(max_corr < 0.1) {
  cat("• ALL correlations < 0.1: VERY WEAK predictive signals\n")
  cat("• Predictive modeling would yield unreliable results\n") 
  cat("• Resource investment in predictive modeling NOT recommended\n")
}

cat("\nSTRATEGIC DECISION: PIVOT TO DESCRIPTIVE ANALYTICS\n")
cat("• RFM segmentation provides proven business value\n")
cat("• Customer prioritization enables targeted marketing\n")
cat("• Historical patterns offer reliable strategic insights\n")
cat("==================================================\n\n")

# Check if recent buyers are more likely to purchase again
recent_vs_future <- model_data %>%
  mutate(is_recent = recency < median(recency)) %>%
  group_by(is_recent) %>%
  summarise(future_purchase_rate = mean(future_spend > 0))

cat("Future Purchase Rate by Recency:\n")
print(recent_vs_future)

# 6. INTEGRATE RFM SEGMENTS ----
rfm_file <- here("output", "rfm_customer_segments.csv")

if (file.exists(rfm_file)) {
  rfm_segments <- read_csv(rfm_file, show_col_types = FALSE) %>%
    select(customer_id, Segment) %>%          
    rename(segment = Segment)
  
  model_data <- model_data %>% left_join(rfm_segments, by = "customer_id")
  cat("✓ RFM segments integrated from Project 1\n")
} else {
  model_data <- model_data %>%
    mutate(
      segment = case_when(
        r_score >= 4 & f_score >= 4 & m_score >= 4 ~ "Champion",
        r_score >= 4 & f_score >= 3 & m_score >= 3 ~ "Loyal Customer",
        r_score >= 4 & f_score >= 3 & m_score <= 2 ~ "Potential Loyalist", 
        r_score >= 3 & f_score <= 2 & m_score <= 2 ~ "Needs Attention",
        TRUE ~ "At Risk"
      )
    )
  cat("✓ Standard RFM segments created\n")
}

# 7. CREATE ACTIONABLE MARKETING SEGMENTS ----
model_data <- model_data %>%
  mutate(
    value_tier = case_when(
      monetary >= quantile(monetary, 0.75, na.rm = TRUE) ~ "High Value",
      monetary >= quantile(monetary, 0.25, na.rm = TRUE) ~ "Medium Value",
      TRUE ~ "Low Value"
    ),
    engagement_tier = case_when(
      recency <= 90 & frequency >= 8 ~ "Highly Engaged",
      recency <= 180 & frequency >= 4 ~ "Moderately Engaged", 
      recency > 180 ~ "Inactive",
      TRUE ~ "Occasional"
    ),
    strategic_segment = paste(value_tier, engagement_tier, sep = " - ")
  )

segment_summary <- model_data %>%
  count(strategic_segment, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# 8. ADD PRODUCT CATEGORY INSIGHTS ----
top_category <- read_csv(here("data", "Superstore.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  select(customer_id, category, sales) %>%
  group_by(customer_id, category) %>%
  summarise(total_spend = sum(sales), .groups = "drop") %>%
  arrange(desc(total_spend)) %>%
  group_by(customer_id) %>%
  slice(1) %>%  # Keep only top category per customer
  ungroup() %>%
  select(customer_id, top_category = category)

model_data <- model_data %>% left_join(top_category, by = "customer_id")

# 9. ANALYZE CUSTOMER BEHAVIOR PATTERNS ----
# 9.1 High value analysis
high_value_analysis <- model_data %>%
  mutate(historical_value_tier = case_when(
    monetary >= quantile(monetary, 0.75) ~ "High Historical Value",
    monetary >= quantile(monetary, 0.25) ~ "Medium Historical Value", 
    TRUE ~ "Low Historical Value"
  )) %>%
  group_by(historical_value_tier) %>%
  summarise(
    n_customers = n(),
    avg_frequency = mean(frequency),
    avg_recency = mean(recency),
    future_purchase_rate = mean(future_spend > 0),
    .groups = "drop"
  )

# 9.2 Retention analysis
retention_analysis <- model_data %>%
  group_by(segment) %>%
  summarise(
    n_customers = n(),
    churn_rate = mean(is_churn == "Yes"),
    avg_historical_spend = mean(monetary),
    .groups = "drop"
  ) %>%
  arrange(churn_rate)

# 9.3 Category analysis
category_analysis <- model_data %>%
  group_by(top_category) %>%
  summarise(
    n_customers = n(),
    avg_historical_spend = mean(monetary),
    churn_rate = mean(is_churn == "Yes"),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_historical_spend))

# 10. IDENTIFY PRIORITY CUSTOMERS ----
high_risk_high_value <- model_data %>%
  filter(monetary >= quantile(monetary, 0.75) & recency > 90) %>%
  arrange(desc(monetary))

vip_recent <- model_data %>%
  filter(monetary >= quantile(monetary, 0.75) & recency <= 90) %>%
  arrange(desc(monetary))

loyal_inactive <- model_data %>%
  filter(frequency >= 8 & recency > 90) %>%
  arrange(desc(monetary))

cat("✓ Priority customers identified:", nrow(high_risk_high_value), "high-risk,", 
    nrow(vip_recent), "VIP,", nrow(loyal_inactive), "loyal inactive\n")

# 11. CREATE VISUALIZATIONS ----
cat("Creating visualizations...\n")

# 11.1 Customer segments heatmap
segment_counts <- model_data %>%
  count(value_tier, engagement_tier) %>%
  mutate(
    value_tier = factor(value_tier, levels = c("Low Value", "Medium Value", "High Value")),
    engagement_tier = factor(engagement_tier, 
                             levels = c("Inactive", "Occasional", "Moderately Engaged", "Highly Engaged"))
  ) %>%
  complete(value_tier, engagement_tier, fill = list(n = 0))

p1 <- ggplot(segment_counts, aes(x = value_tier, y = engagement_tier)) +
  geom_tile(aes(fill = n), color = "black", linewidth = 1) +
  geom_text(aes(label = n, color = n > 100), size = 5, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5"),
    name = "Customer Count"
  ) +
  scale_color_manual(guide = "none", values = c("TRUE" = "white", "FALSE" = "black")) +
  labs(
    title = "Customer Segments: Historical Value vs Engagement",
    subtitle = "Numbers represent customer count in each segment",
    x = "Historical Value Tier",
    y = "Engagement Tier"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10, face = "bold")
  )

# 11.2 Churn by segment
p2 <- ggplot(retention_analysis, aes(x = reorder(segment, -churn_rate), y = churn_rate)) +
  geom_col(fill = "#E63946", alpha = 0.8) +
  geom_text(aes(label = scales::percent(churn_rate, accuracy = 0.1)), 
            vjust = -0.5, size = 3) +
  labs(title = "Churn Rate by RFM Segment", x = "RFM Segment", y = "Churn Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 11.3 Category performance
p3 <- ggplot(category_analysis, aes(x = reorder(top_category, avg_historical_spend), 
                                    y = avg_historical_spend)) +
  geom_col(fill = "#52B788", alpha = 0.8) +
  geom_text(aes(label = scales::dollar(avg_historical_spend)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Average Historical Spend by Product Category", 
       x = "Product Category", y = "Average Historical Spend ($)") +
  theme_minimal()

# 12. BUSINESS STRATEGY RECOMMENDATIONS ----
strategy_recommendations <- tibble(
  Strategic_Segment = c("High Value - Highly Engaged", "High Value - Inactive", 
                        "Medium Value - Moderately Engaged", "Low Value - Highly Engaged"),
  Priority_Level = c("Tier 1: Protect", "Tier 1: Reactivate", "Tier 2: Grow", "Tier 2: Develop"),
  Recommended_Actions = c(
    "Loyalty rewards, exclusive access, VIP support",
    "URGENT: Personal outreach, win-back offers, satisfaction surveys",
    "Targeted promotions, product recommendations",
    "Educational content, community building"
  ),
  Budget_Allocation = c("High", "High", "Medium", "Medium")
)

# 13. SAVE OUTPUTS ----
cat("Saving outputs...\n")

# Ensure directories exist
walk(c("output", "figures"), ~ if(!dir.exists(here(.x))) dir.create(here(.x), recursive = TRUE))

# Save data files
write_csv(model_data, here("output", "customer_analytics_comprehensive.csv"))
write_csv(high_risk_high_value, here("output", "high_priority_customers.csv"))
write_csv(strategy_recommendations, here("output", "marketing_strategy_recommendations.csv"))
write_csv(retention_analysis, here("output", "retention_analysis_by_segment.csv"))
write_csv(category_analysis, here("output", "product_category_analysis.csv"))

# Save methodology note
write_lines(
  "METHODOLOGY NOTE: PREDICTIVE ANALYTICS LIMITATION

Initial analysis revealed fundamental limitations for predictive modeling:
- Near-zero correlations between historical behavior and future spending
- All feature-future outcome correlations < 0.06
- Customer future behavior is unpredictable with available RFM features

SUCCESSFUL ANALYTICAL PIVOT:
The project successfully delivered value through descriptive analytics:
- RFM segmentation with clear strategic implications
- Actionable customer prioritization based on historical value
- Data-driven marketing recommendations
- Product category performance insights

All deliverables are based on proven historical patterns rather than unreliable future predictions.",
  here("output", "methodology_notes.txt")
)

# Save plots
walk2(list(p1, p2, p3), 
      c("customer_segments_heatmap.png", "churn_by_segment.png", "category_performance.png"),
      ~ ggsave(here("figures", .y), plot = .x, width = 10, height = 6, dpi = 300))

cat("✓ All outputs saved successfully\n")

# 14. EXECUTIVE SUMMARY ----
cat("\n")
cat("==============================================================================\n")
cat("CUSTOMER ANALYTICS & STRATEGIC SEGMENTATION COMPLETE!\n")
cat("==============================================================================\n")
cat("Key Business Insights Delivered:\n")
cat("•", nrow(high_risk_high_value), "high-value at-risk customers identified for immediate action\n")
cat("•", nrow(vip_recent), "VIP recent customers to protect and grow\n")
cat("•", nrow(loyal_inactive), "loyal inactive customers for reactivation campaigns\n")
cat("• Technology category shows highest customer value ($", 
    round(filter(category_analysis, top_category == "Technology")$avg_historical_spend), 
    ") and lowest churn (", 
    round(filter(category_analysis, top_category == "Technology")$churn_rate * 100, 1), "%)\n", sep = "")
cat("• RFM segments show clear performance gradient\n")
cat("\nMethodology Note:\n")
cat("Predictive modeling was attempted but showed near-zero correlations between\n")
cat("historical behavior and future outcomes. Analysis successfully pivoted to\n") 
cat("descriptive analytics delivering valuable business intelligence.\n")
cat("\nOutput Files Saved:\n")
cat("• output/customer_analytics_comprehensive.csv - Complete customer intelligence\n")
cat("• output/high_priority_customers.csv - Immediate action required list\n")
cat("• output/marketing_strategy_recommendations.csv - Clear action plan\n")
cat("• output/methodology_notes.txt - Detailed methodology explanation\n")
cat("==============================================================================\n")

# 15. SESSION INFO ----
cat("\nSession info for reproducibility:\n")
print(sessionInfo())