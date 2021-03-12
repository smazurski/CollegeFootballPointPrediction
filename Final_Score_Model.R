library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(stats)
library(caret)
library(corrplot)

urlfile<-'https://raw.githubusercontent.com/smazurski/ShinyData/master/Score_Prediction_Data.csv'
Orig_Score_Data <- read_csv(urlfile)


# view dataset
glimpse(Orig_Score_Data)


# remove non-predictor variables
Orig_Score_Data <- Orig_Score_Data %>%
  select(-GameID, -Offense, -Defense)


# View data distributions
Orig_Score_Data %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal()


# View Corr plot
Orig_Score_Data %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()


#Remove Year
Orig_Score_Data <- Orig_Score_Data %>%
  select(-Year)


# View Corr plot
Orig_Score_Data %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()


# Convert Conference and is_Home to Factor 
Orig_Score_Data$is_home <- as.factor(Orig_Score_Data$is_home)
Orig_Score_Data$OffConf <- as.factor(Orig_Score_Data$OffConf)


# View continuous variables in plot
Orig_Score_Data %>%
  keep(is.numeric) %>%
  plot()


# View in Regression Model
Orig_Score_Model <- lm(Orig_Score_Data$Points~., data = Orig_Score_Data)
summary(Orig_Score_Model)


# R-squared is .5316, F-stat = 196
# Variable Importance
imp <- varImp(Orig_Score_Model, scale=FALSE)
imp


# Remove Insignificant Variables with high p-values

Orig_Score_Data <- Orig_Score_Data %>%
  select(-Year, -DefRecruitRank, -OffRanking, -OffRecruitRank, -OffRanking)


Orig_Score_Model <- lm(Orig_Score_Data$Points~., data = Orig_Score_Data)
summary(Orig_Score_Model)


# R-squared is .5315, F-stat = 233.5
## Pretty Decent, let's see which variables add the most value to the model
imp <- varImp(Orig_Score_Model, scale=FALSE)
imp


# Remove DFPI and QBR
Orig_Score_Data <- Orig_Score_Data %>%
  select(-DFPI, -QBR)


Orig_Score_Model <- lm(Orig_Score_Data$Points~., data = Orig_Score_Data)
summary(Orig_Score_Model)

# R-squared is .5307, F-stat = 258.6



#Let's test the predictions
Score_Data <- Orig_Score_Data


#Split the dataset
test_data_train <- data.frame(Score_Data[1:3400,])
test_data_test <- data.frame(Score_Data[3401:4365,])

# Create Model
lm_train <- lm(test_data_train$Points~., data = test_data_train)

# Make predictions
lm_pred <- predict(lm_train, test_data_test)


#Convert to dataframe and merge for residuals
lm_pred <- data.frame(lm_pred)
lm_pred$Points <- test_data_test$Points
lm_pred <- na.omit(lm_pred)


#RMSE .7428
sqrt(mean(lm_pred$lm_pred[1:965]-lm_pred$Points[1:965])^2)


lm_pred <- lm_pred %>%
  mutate(diff=lm_pred-Points)

mean(lm_pred$diff)
median(lm_pred$diff)
# The mean/median predictions distribution are pretty close to zero. Looks like we're slightly
# underestimating the scores

ggplot(lm_pred, aes(x=diff)) +
  geom_density(alpha=0.8, color = 'lightblue', fill = 'lightblue') +
  theme_classic()


sd(lm_pred$diff)
 # 1 SD = 10.11913 points


## Let's try a LASSO regression model

#lasso
library(glmnet)

# Split target variable from features
x <- model.matrix(Score_Data$Points~.,Score_Data)[,-1]
y <- Score_Data$Points

lambda <- 10^seq(10, -2, length = 100)

#split test and training datasets
train=sample(x[1:3400,])
test=(x[3401:4365,])
y.test=y[3401:4365]

# Create Model
lasso.mod=glmnet(x[1:3400,],y[1:3400], alpha=1, lambda = lambda)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[1:3400,],y[1:3400],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx = x[3401:4365,])

# RMSE: .7227...Slightly lower than linear regression model
sqrt(mean(lasso.pred-y.test)^2)


lasso_data <- data.frame(lasso.pred, y.test)

lasso_data <- lasso_data %>%
  mutate(difference=X1-y.test)

mean(lasso_data$difference)
median(lasso_data$difference)
hist(lasso_data$difference)
sd(lasso_data$difference) 
# slightly wider distribution than the multiple regression model,  # 1 SD = 10.12273 points


out=glmnet(x,y,alpha = 1,lambda = lambda)
lasso.coef=predict(out,type = "coefficients", s=bestlam)

lasso.coef

