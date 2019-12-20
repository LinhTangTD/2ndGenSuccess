library(prettyR)
library(readr)
library(dplyr)
library(factoextra)  ## Used for PCA visualizations
library(caret)  ## package for model comparisons
library(glmnet) ## package for fitting lasso models
library(mgcv)   ## package for fitting GAM models
library(pls)
library(tidyr) 

path = "/Users/MAC/Desktop/STA230/FinalProject"
Final = read_csv(paste(path, "cleaned_data.csv", sep = "/"))
predictors = read_csv(paste(path, "predictors.csv", sep = "/"))
result = read_csv(paste(path, "result.csv", sep = "/"))

#### New Data Set ####
# Removing the result coloumns and kept only the sucess index
ImmigrantSuccess<-select(Final,-c(1:15))
colnames(ImmigrantSuccess)<-make.names(names(ImmigrantSuccess))

#### Fitting into models####

#Set the number of repeats and folds on the dataset
fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10,verboseIter = TRUE)
#Set seeds so that all models starts the same for compairsion purpose in the future
set.seed(123)
#Linear Regression model 
fit.linear <- train(successIndex ~ ., data = ImmigrantSuccess, method = "lm", trControl = fit.control)
#LASSO model
X = model.matrix(data = ImmigrantSuccess[,-44], ~ -1 + .) ## Set up the matrix including dummy vars
y = ImmigrantSuccess$successIndex

lasso <- glmnet(x = X, y = y)
set.seed(123)
lams <- expand.grid(alpha = 1, lambda = lasso$lambda)
fit.lasso <- train(successIndex ~ ., data = ImmigrantSuccess, method = "glmnet", trControl = fit.control, tuneGrid = lams)

#GAM Model
set.seed(123)
fit.gam <- train(successIndex ~ ., data = ImmigrantSuccess, method = "gam", trControl = fit.control)

#PCR Model
set.seed(123)
fit.pcr <- train(successIndex ~ ., data = ImmigrantSuccess, method = "pcr", trControl = fit.control)

#Summary
rs<- resamples(list(LM = fit.linear, 
                    LASSO = fit.lasso,
                    GAM = fit.gam,
                    PCR = fit.pcr))
sumdata<-summary(rs)
coef(fit.lasso$finalModel, s = fit.lasso$bestTune$lambda)

## Graph for evaluation 
# Getting a copy of the data
RMSEframe<-data.frame(sumdata$statistics$RMSE)
#Removing the NA, which are all zero
RMSEframe<-RMSEframe[1:6]
#Have a column for the model names
RMSEframe$Model<-rownames(RMSEframe)
#change the row names to null
rownames(RMSEframe)<-NULL
#Reorganize the dataframe into something plotable 
RMSEframe <- gather(data = RMSEframe, key = Quatile, value = RMSE, `Min.`,`X1st.Qu.`,`Median`,`Mean`,`X3rd.Qu.`,`Max.`)

model_compare <- RMSEframe %>%
  ggplot(aes(x=Quatile, y=RMSE,color=Model,group = Model)) +
  geom_line() +
  geom_point(size=2) +
  ggtitle("RMSE Model Comparison") 

model_compare