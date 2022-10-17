
# R Libraries used
library(dplyr)
library(fBasics)
library(faraway)
library(ggplot2)
library(caret)
library(cvms)
library(tibble)
library("mice")
library(e1071)


###################### Pre-Processing and Data Insights ############################
# dimension of pima dataset
dim(pima)
# data of top 5 rows
head(pima)
# summary of pima dataset
summary(pima)
# assigning pima dataset to a data frame
pima_df= pima


#changing name to standardize to naming convention of features
names(pima_df)[names(pima_df) == 'diastolic'] <- 'pressure'
names(pima_df)[names(pima_df) == 'test'] <- 'outcome'
names(pima_df)[names(pima_df) == 'diabetes'] <- 'pedigree'


# creating outcome factor which says positive or negative according whether the person has diabetes or not respectively
pima_df$outcome_factor= factor(pima_df$outcome)
levels(pima_df$outcome_factor) = c("negative","positive")


# density plot of insulin with respect to outcome factor
ggplot(pima_df, aes(x=insulin, y=..density.., color=outcome_factor)) +
  geom_histogram(position='dodge',binwidth = 30, fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") 


# density plot of triceps with respect to outcome factor
ggplot(pima_df, aes(x=triceps, y=..density.., color=outcome_factor)) +
  geom_histogram(position='dodge',binwidth = 30, fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") 


# realised after desnity plots of insulin and triceps  that 0's can be NA but not for all
lapply(pima_df[1:8], function(x){ length(which(x==0))*100/length(x)})


# not every 0 is NA but 0's of of glucose,insulin,pressure,triceps,bmi is NA; pregnancy can be 0 so not assigning it with 0
pima_df$glucose[pima_df$glucose==0] <- NA
pima_df$insulin[pima_df$insulin==0] <- NA
pima_df$pressure[pima_df$pressure==0] <- NA
pima_df$triceps[pima_df$triceps==0] <- NA
pima_df$bmi[pima_df$bmi==0] <- NA
summary(pima_df)
summary(pima)


# imputed the NA in the data with "mice" using predictive mean matching
set.seed(1011) #setting the seed value
imp <- mice(pima_df, method = "pmm", m = 1) # Impute data
pima_imp_df <- complete(imp) # Store data
cat(" \n \n \t \t \t \t Summary of data before imputing with PMM \n \n")
summary(pima_df)
cat("\n \n \t \t \t \t Summary of data after imputing with PMM \n \n")
summary(pima_imp_df)
pima_imp_df_1=pima_imp_df


#Plotting the statistical outliers for triceps,insulin and pedigree after observing the summary of data before and after imputing
png(file="Outlier_Box_Plots.png")
  par(mfrow=c(1,3)) # arranges graphs in rows (1 row, 2 columns)
  boxplot(pima_imp_df_1$triceps, main = "Tricep Skin Fold Thickness", ylab="mm", col="red")
  boxplot(pima_imp_df_1$insulin, main = "2-Hour Serum Insulin", ylab="mu U/m", col="blue")
  boxplot(pima_imp_df_1$pedigree, main = "Diabetes Pedigree Function", ylab="value", col="green")
dev.off()


# removal of outliers after observing the boxplot
outlier_remover <- function(df, feature) {
  boxplot(df[, feature], plot=FALSE)$out
  outliers <- boxplot(df[,feature], plot=FALSE)$out
  x<-df[,feature] 
  df<- df[-which(df[,feature] %in% outliers),]
  return(df)
}
pima_imp_df_out=pima_imp_df
#removing the outlier for triceps,insulin and pedigree
for(feature in c(4,5,7)){
  pima_imp_df_out=outlier_remover(pima_imp_df_out,feature)
}
#observing the summary of data after outlier removal
summary(pima_imp_df_out)

########################## Data Analysis #################################

#Observation of Data
# Nutritional status based on BMI
pima_imp_df_out$Nutritional_Status <- cut(pima_imp_df_out$bmi, breaks=c(0, 18.5, 25, 30,35,40, Inf))
levels(pima_imp_df_out$Nutritional_Status) = c("Underweight","Normal","Pre-obesity","Obese class I","Obese class II","Obese class III")
pima_imp_df_out <- pima_imp_df_out %>% relocate(Nutritional_Status, .before = outcome)
# Plotting Nutritional status based on BMI
ggplot(pima_imp_df_out, aes(x=Nutritional_Status, color=outcome_factor)) +
  geom_bar(position='dodge', fill="white")


#Interpretation of Glucose level
# Interpretation of OGTT (Glucose) - using OGTT levels recommended by DIABETES UK (2019)
pima_imp_df_out$Glucose_Result <- cut(pima_imp_df_out$glucose, breaks=c(0, 140, 198, Inf))
levels(pima_imp_df_out$Glucose_Result) = c("Normal","Impaired Glucose Tolerance","Diabetic Level")
pima_imp_df_out <- pima_imp_df_out %>% relocate(Glucose_Result, .before = outcome)
# Plotting of Interpretation of OGTT (Glucose) 
ggplot(pima_imp_df_out, aes(x=Glucose_Result, color=outcome_factor)) +
  geom_bar(position='dodge', fill="white")+scale_x_discrete(drop=FALSE)



# Breaking the women into different age groups 0-20,20-30,30-40,40-60 and 60+
pima_imp_df_out_1=pima_imp_df_out
pima_imp_df_out_1$age_group<- cut(pima_imp_df_out$bmi, breaks=c(0,20, 30, 40, 60, Inf))
levels(pima_imp_df_out_1$age_group) = c("0-20","20-30","30-40","40-60","60+")
#relative percentage plot of people with and without diabetes with respect to age group 
myplot <- ggplot(pima_imp_df_out_1, aes(age_group, group = outcome),fill=age_group) + 
  geom_bar(aes(y = ..prop.., color = factor(..x..)),fill="white",    stat="count",position='dodge') + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Percentage") +
  xlab("Age group")+
  facet_grid(~outcome_factor)+
  theme(legend.position="none")
myplot


#correlation plot of features and outcome
corMat=cor(pima_imp_df_out[,c(1:8,11)])
corMat=data.frame(corMat)
ggcorrplot::ggcorrplot(corMat,lab=TRUE)
sum(pima_imp_df_out$outcome==0)
sum(pima_imp_df_out$outcome==1)

########################## Classification #################################

# Splitting the data into test and train
set.seed(1022 ) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 80% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(pima_imp_df_out), size = floor((0.8)*nrow(pima_imp_df_out)), replace = F)
train <- pima_imp_df_out[sample, ]
test  <- pima_imp_df_out[-sample, ]


#Training a model using SVM with radial basis function as kernel with a fixed cost
svmfit = svm(outcome_factor ~ ., data = train[c(1:8,12)], kernel = "radial", cost = 1)
print(svmfit)
# predicting values using the model with test data
predicted <- predict(svmfit, test[c(1:8)])
acc_svm <- 100*mean(predicted==test$outcome_factor)
actual <- test$outcome_factor
#creating the confusion matrix
cmL_svm = as_tibble(table( predicted,actual))
#plot of confusion matrix
plot_confusion_matrix(cmL_svm, 
                      target_col = "actual", 
                      prediction_col = "predicted",
                      counts_col = "n",
                      palette = "Greens",
                      add_sums = TRUE,sums_settings = sum_tile_settings(label = "Total") ,
                      place_x_axis_above = FALSE)



#Training a model using SVM with radial basis function as kernel using cross validation and hyperparameter tuning of cost

#parameters for cross validation 
train$outcome=as.factor(train$outcome)
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=10,         # do 10 repetitions of cv
                     summaryFunction=twoClassSummary,   # Use AUC to pick the best model
                     classProbs = TRUE)


#Train and Tune the SVM
train$outcome=as.factor(train$outcome)
svm.tune <- train(outcome_factor ~ .,
                  data = train[c(1:8,12)],
                  method = "svmRadial",   # Radial kernel
                  tuneLength = 10,                   # 10 values of the cost function+++
                  preProc = c("center","scale"),  # Center and scale data
                  metric="ROC",
                  trControl=ctrl)


predicted <- predict(svm.tune, test[c(1:8)])
levels(predicted)  = c("negative","positive")
acc_svm_cv <- 100*mean(predicted==test$outcome_factor)
#creating the confusion matrix
cmL_svm_cv = as_tibble(table( predicted,actual))
cmL_svm_cv
#plot of confusion matrix
plot_confusion_matrix(cmL_svm_cv, 
                      target_col = "actual", 
                      prediction_col = "predicted",
                      counts_col = "n",
                      palette = "Greens",
                      add_sums = TRUE,sums_settings = sum_tile_settings(label = "Total") ,
                      place_x_axis_above = FALSE)



