"
Kaggle competition - Predicting House Prices
"
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#current_dir = dirname(rstudioapi::getSourceEditorContext()$path) #path of current script
#setwd('F:/UVA/fall_term/sys_6018/Competitions/2_Housing')

# Load necessary packages
library(ggplot2)
library(dplyr)
library(mice)
library(caret)
library(onehot)
library(zoo)
library(broom)

# ----------------------------------------------------------------------------------------------------------------------------------


#Load Data
train <- read.csv('train.csv',stringsAsFactors = F)
test <- read.csv('test.csv',stringsAsFactors = F)
housedata <- bind_rows(train,test)

#Initial look at data
str(housedata)
summary(housedata)
colSums(is.na(housedata))

# ----------------------------------------------------------------------------------------------------------------------------------

# IMPUTING NAs/MISSING VALUES

#Replace with 0 where necessary
housedata$GarageYrBlt <- ifelse(is.na(housedata$GarageYrBlt) , 0 ,housedata$GarageYrBlt )
housedata$BsmtFinSF1 <- ifelse(is.na(housedata$BsmtFinSF1) , 0 ,housedata$BsmtFinSF1 )
housedata$BsmtUnfSF <- ifelse(is.na(housedata$BsmtUnfSF) , 0 ,housedata$BsmtUnfSF )
housedata$TotalBsmtSF <- ifelse(is.na(housedata$TotalBsmtSF) , 0 ,housedata$TotalBsmtSF )
housedata$BsmtFullBath <- ifelse(is.na(housedata$BsmtFullBath) , 0 ,housedata$BsmtFullBath )
housedata$BsmtFinSF2 <- ifelse(is.na(housedata$BsmtFinSF2) , 0 ,housedata$BsmtFinSF2 )
housedata$BsmtHalfBath <- ifelse(is.na(housedata$BsmtHalfBath) , 0 ,housedata$BsmtHalfBath )

housedata[which(housedata$Id == 1916), ]$MSZoning <- 'RM'
housedata[which(housedata$Id == 2251), ]$MSZoning <- 'RM'
housedata[which(housedata$Id == 2217 | housedata$Id == 2905), ]$MSZoning <- 'RL'

#Replace categorical NA variables with mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


housedata_nafill <- housedata
housedata_nafill$MSZoning <- factor(housedata$MSZoning)
housedata_nafill$Utilities[is.na(housedata_nafill$Utilities)] <- Mode(housedata$Utilities) 
housedata_nafill$Exterior1st[is.na(housedata_nafill$Exterior1st)] <- Mode(housedata$Exterior1st) 
housedata_nafill$Exterior2nd[is.na(housedata_nafill$Exterior2nd)] <- Mode(housedata$Exterior2nd) 
housedata_nafill$MasVnrType[is.na(housedata_nafill$MasVnrType)] <- Mode(housedata$MasVnrType) 
housedata_nafill$Electrical[is.na(housedata_nafill$Electrical)] <- Mode(housedata$Electrical) 
housedata_nafill$KitchenQual[is.na(housedata_nafill$KitchenQual)] <- Mode(housedata$KitchenQual)
housedata_nafill$Functional[is.na(housedata_nafill$Functional)] <- Mode(housedata$Functional)
housedata_nafill$SaleType[is.na(housedata_nafill$SaleType)] <- Mode(housedata$SaleType)

# Replace legit NAs with "None"
housedata_nafill[c('Alley','BsmtQual','BsmtCond','BsmtExposure',
                   'BsmtFinType1','BsmtFinType2','FireplaceQu','GarageType',
                   'GarageFinish', 'GarageQual','GarageCond','PoolQC',
                   'Fence','MiscFeature')] <- data.frame(apply(housedata_nafill[c('Alley','BsmtQual','BsmtCond','BsmtExposure',
                                                                                  'BsmtFinType1','BsmtFinType2','FireplaceQu','GarageType',
                                                                                  'GarageFinish', 'GarageQual','GarageCond','PoolQC',
                                                                                  'Fence','MiscFeature')], 2, function(x) ifelse(is.na(x),"None",x)))


# Replace numerical NA variables with median
housedata_nafill$MasVnrArea[is.na(housedata_nafill$MasVnrArea)] <- median(housedata$MasVnrArea,na.rm = T)
housedata_nafill$GarageCars[is.na(housedata_nafill$GarageCars)] <- median(housedata$GarageCars,na.rm = T)
housedata_nafill$GarageArea[is.na(housedata_nafill$GarageArea)] <- median(housedata$GarageArea,na.rm = T)

# Replace NA values in LotFrontage with mice imputing (predictive mean method)
#housedata_nafill$SalePrice <- NULL #so that it doesnt impute test SalePrice values ; we will add column later
mice_mod <- mice(housedata_nafill[,c("LotFrontage","LotArea","LotConfig","LotShape")],method ='pmm')
mice_out <- complete(mice_mod)
housedata_nafill$LotFrontage <- mice_out$LotFrontage

# Adding certain variables
housedata_nafill$DateSold <- as.Date(as.yearmon(paste(housedata_nafill$YrSold,housedata_nafill$MoSold), "%Y %m"))  #Convert Dates
housedata_nafill$IfRemodelled <- as.factor(ifelse(housedata_nafill$YearRemodAdd-housedata_nafill$YearBuilt > 0 , 1,0)) #Add column if remodelling exists 
housedata_nafill$GarageExist <- as.factor(ifelse(housedata_nafill$GarageYrBlt==0,0,1)) #Add column if garage exists
housedata_nafill$TotalBaths <- housedata_nafill$BsmtFullBath + housedata_nafill$BsmtHalfBath + housedata_nafill$FullBath + housedata_nafill$HalfBath #Total bath
housedata_nafill$PorchExist <- as.factor(ifelse(housedata_nafill$OpenPorchSF+housedata_nafill$EnclosedPorch+housedata_nafill$X3SsnPorch+housedata_nafill$ScreenPorch > 0 , 1,0)) 
housedata_nafill$PoolExist <- as.factor(ifelse(housedata_nafill$PoolArea > 0 , 1,0)) #Add column if pool exists

# One-hot encoding
dmy <- dummyVars(" ~ .", data = housedata_nafill,sep = "_",fullRank = T)
house_df <- data.frame(predict(dmy, newdata = housedata_nafill))
house_df$DateSold <- as.Date(house_df$DateSold)

# Split data back
train <- house_df[1:1460,]
test <- house_df[1461:2919,]

train_eda <- housedata_nafill[1:1460,] #without one-hot encodes for plotting charts

# ----------------------------------------------------------------------------------------------------------------------------------

# EDA and OUTLIER REMOVAL

# Distribution of Sale prices
ggplot(train, aes(x=SalePrice)) + geom_histogram(binwidth = 20000)
#The distribution doesn't look normal, there does seem to be outliers esp on the right tail
#we will treat these outliers by ignoring them from our analyses
#trim off % top and bottom to remove outliers
upperb <- quantile(train$SalePrice, 0.98)
lowerb <- quantile(train$SalePrice, 0.01)
train <- subset( train,SalePrice > lowerb & SalePrice < upperb)
ggplot(train, aes(x=SalePrice)) + geom_histogram(binwidth = 20000)
#length(train$SalePrice) #removed ~ 45 dp

#Plot of Ground Living Area and SalePrice
ggplot(train, aes(x=GrLivArea, y=SalePrice)) + geom_point()
# While these 2 variables mostly have a linear relationship, We can see that there are observations
# which seem like outliers - have a low sale price for more ground area, hence we will ignore these datapoints
# since they are not in keeping with the trend.
train <- subset(train,!(GrLivArea > 4000 & SalePrice < 300000))
#length(train$SalePrice) #removed 2 dp

# Avg Sale Price distribution across neighbourhoods
ggplot(train_eda, aes(x=factor(Neighborhood), y=SalePrice)) + stat_summary(fun.y="mean", geom="bar")
# We can see that for some neighborhoods, the avg sale price is higher.

#House being sold across time
ggplot(train_eda, aes(x=DateSold)) + geom_histogram(binwidth = 20)

#relation b/w SalePrice and GarageArea
ggplot(train_eda, aes(x=GarageArea, y=SalePrice)) + geom_point()

#overall qual and sale price
ggplot(train_eda, aes(x=factor(OverallQual), y=SalePrice)) + stat_summary(fun.y="mean", geom="bar")

#relation b/w SalePrice and TotroomsAbvGround
ggplot(train_eda, aes(x=TotRmsAbvGrd, y=SalePrice)) + geom_point()

#CONTINUE USING TRAIN (train) as your training data

# ----------------------------------------------------------------------------------------------------------------------------------

# PARAMETRIC MODEL- MULTIVAR REGRESSION
# Run multi regression on all variables then select only those variables with p-value < 0.05
# Train final model on those variables.
multivar_lm <- lm (SalePrice ~ ., data = train)
tm = tidy(multivar_lm)
terms_list <- tm$term[tm$p.value < 0.005] #show only those variables with p values less than 0.05
train.model <- select (train,c(terms_list[-1])) #-1 to remove intercept
train.model$SalePrice <- train$SalePrice

param_model <- lm (SalePrice ~ ., data = train.model)
summary(param_model)

# Predict on test set
prediction <- predict(param_model,newdata = test)
solution <- data.frame(ID = test$Id)
solution$SalePrice = prediction
length(prediction)

# Write solution to csv file
write.csv(solution,file = 'param_soln.csv',row.names=F)

# ----------------------------------------------------------------------------------------------------------------------------------

#KNN from scratch

# combining dataset for normalization
house_knn <-  rbind(train,test)

house_knn <- house_knn[,-c(1,262)]

#normalize function
normalize <- function(x,y) {
  min_y <-  min(house_knn[,y])
  max_y <- max(house_knn[,y])
  return ((house_knn[x,y] - min_y) / (max_y - min_y)) }

#normalizing values in df
for (x in 1:nrow(house_knn)) {
  for (y in 1:ncol(house_knn)) {
    if (y != 260) {
      house_knn[x,y] <- normalize (x,y)
    }
  }
}

#reordering dataset
train_knn <- house_knn [1:1413,]
train_knn <-  train_knn[c(260,1:259,261:265)]

test_knn <- house_knn[1414:2872,]
test_knn <-  test_knn[c(260,1:259,261:265)]

# return euclidean distance function
distance <- function(d1, d2){
  # takes two argument
  # example distance(df1, df2)
  # d1 is train dataframe
  # d2 is a test datapoint
  # returns a vector with all the distance of a test data point from each train data point
  train.row <- nrow(d1)
  dis <- sapply(apply((apply(data.frame(matrix(rep(d2,each=train.row),nrow=train.row)),2,as.numeric) - d1)^2,1,sum),sqrt)
  return(dis)
}


# KNN from scratch function
KNN_method <- function(test_data, train_data, k_value){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(x in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & sale_price empty  vector
    sale_price = c()
    
    eu_dist <- c(eu_dist, distance(train_data[,-1],test_data[x,-1]))
    
    #adding SalePrice
    sale_price <- c(sale_price, train_data[,1])
    #print(sale_price)
    
    eu <- data.frame(sale_price, eu_dist) #eu dataframe created with sales_price & eu_dist columns
    
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    #print(eu[k_value,2])
    eu <- eu[eu$eu_dist <= eu[k_value,2],]               #eu dataframe with top K neighbors (more if there is a tie)
    #print(eu)
    
    pred <- c(pred, mean(eu$sale_price))
    #print(pred)
    
  }
  return(pred) #return pred vector
}

prediction_KNN <- KNN_method(test_knn, train_knn, 7)

# Predict on test set
solution_KNN <- data.frame(ID = test$Id)
solution_KNN$SalePrice = prediction_KNN

# Write solution to csv file
write.csv(solution_KNN,file = 'non-param_soln.csv',row.names=F)
