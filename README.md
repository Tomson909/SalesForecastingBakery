# Sales Forecasting for a Bakery Branch

## Repository Link

[< https://github.com/Tomson909/SalesForecastingBakery >]

## Description

This project focuses on sales forecasting for a bakery branch, utilizing historical sales data spanning from July 1, 2013, to July 30, 2018, to inform inventory and staffing decisions. We aim to predict future sales for six specific product categories: Bread, Rolls, Croissants, Confectionery, Cakes, and Seasonal Bread. Our methodology integrates statistical and machine learning techniques, beginning with a baseline linear regression model to identify fundamental trends, and progressing to a sophisticated neural network designed to discern more nuanced patterns and enhance forecast precision. The initiative encompasses data preparation, crafting bar charts with confidence intervals for visualization, and fine-tuning models to assess their performance on test data from August 1, 2018, to July 30, 2019, using the Mean Absolute Percentage Error (MAPE) metric for each product category.

### Task Type

Regression

### Results Summary

-   **Best Model:** SSSeModell.ipynb
-   **Evaluation Metric:** MAPE
-   **Result by Category** (Identifier):
    -   **Bread** (1): 13.9 %
    -   **Rolls** (2): 21.6  %
    -   **Croissant** (3): 18.4 %
    -   **Confectionery** (4): 22.4 %
    -   **Cake** (5): 14.5 %
    -   **Seasonal Bread** (6): 0 % (no datapoints, see presentation)

## Documentation

1.  [**Data Import and Preparation**](1_DatasetCharacteristics/final_script.r)
3.  [**Dataset Characteristics (Barcharts)**](1_DatasetCharacteristics/final_script.r)
4.  [**Baseline Model**](1_DatasetCharacteristics/final_script.r)
5.  [**Model Definition and Evaluation**](3_Model/best_model.ipynb) our best model, which won first place in the Kaggle competition 
6.  [**Presentation**](4_Presentation/Quarto_Präsi.qmd) presentation as powerpoint in the folder as well

