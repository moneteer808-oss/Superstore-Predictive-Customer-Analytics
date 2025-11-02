# Superstore Predictive Customer Analytics

This project extends the **Superstore RFM Customer Segmentation** by introducing **predictive analytics** to forecast customer behavior.  
Using R, it predicts **Customer Lifetime Value (CLV)** and identifies customers **at risk of churn**, enabling targeted marketing and retention strategies.

## Key Features
- Predictive modeling using R (`caret`, `randomForest`, `xgboost`)
- CLV prediction based on RFM scores
- Churn likelihood analysis
- Feature importance visualization for model interpretability
- Reproducible RMarkdown workflow with clear documentation

## Outputs
- `Superstore_Predictive_Analytic.Rmd`: Full predictive analysis code  
- `customer_clv_predictions.csv`: Estimated lifetime value per customer  
- `customer_churn_predictions.csv`: Churn probability per customer  
- `Superstore_Predictive_Analytic.html`: Interactive HTML report  

## Data Source
Dataset used in this project:  
[Superstore Dataset (Kaggle)](https://www.kaggle.com/datasets/vivek468/superstore-dataset-final/data)

> **Disclaimer:**  
> The dataset is owned and originally published by a third party.  
> This project is created **for educational and portfolio purposes only** and does not claim ownership of the data.

## Live Report
View the interactive predictive analytics report here:  
[Superstore Predictive Customer Analytics Report](https://moneteer808-oss.github.io/Superstore-Predictive-Customer-Analytics/)

> **Previous Step:** [Superstore RFM Customer Segmentation (Project 1)](https://moneteer808-oss.github.io/Superstore-RFM-Customer-Segmentation/)  
> This segmentation project provides the foundation for the predictive model by classifying customers into actionable groups such as **Champions**, **Loyal Customers**, and **At Risk**.

---

### 📂 Related Repository
- [Superstore RFM Customer Segmentation (Project 1)](https://github.com/moneteer808-oss/Superstore-RFM-Customer-Segmentation)

---

### 🧭 Next Steps
- Coming soon...
- Expand model to include product category and region-level variables  
- Integrate CLV predictions into a marketing dashboard
- Deploy model pipeline via Shiny app or R plumber API

