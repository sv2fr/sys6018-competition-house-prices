# sys6018-competition-house-prices
Kaggle - House Prices: Advanced Regression Techniques

# Team Mates -
Charu Rawat (cr4zy) - Data  Cleaning, Exploration, and Parametric Modeling
Kanika Dawar (kd2hr) - Normalization and Non Parametric Modeling
Sri Vaishnavi V (sv2fr) -  Normalization and Non Parametric Modeling - GitHub Manager

Collective discussion on selection of appropriate statistical methods and variable selection

# Description of files

## train.csv

* This is the given training file in the Kaggle competition

## test.csv

* This is the given test file in the Kaggle competition

## c-11_HousePriceCompetition.R

* It contains the data loading, cleaning (filling legit NAs), and imputation of missing data
* It also contains exploratory data analysis and removal of outliers
* To eliminate bias, we normalized all the variables
* We feature engineered the data as well as performed one - hot encoding to reduce bias by multi collinearity and categorical variables
* The statistical methods were selected based on Assignment guidelines
* Parametric modeling method for Sale Price Prediction using the House Price Dataset
* Non-Parametric modeling (user defined KNN) for Sale Price Prediction using the House Price Dataset

## non-param_soln.csv

* It contains the Sale Price Predictions for all the IDs in test.csv (using all variables as regressors) under the non-parametric method

## param_soln.csv

* It contains the Sale Price Predictions for all the IDs in test.csv (using all variables as regressors) under the parametric method

## Folder knn_scratch_all

* It contains user-defined KNN which takes all probabilities less than largest Kth value
* It also contains sample training and test data files after processing and variable selection

## OUTPUT

* Under both Parametric and Non-Parametric approaches, we got the best Kaggle Public Score when we used all the variables for modeling Sale Price
