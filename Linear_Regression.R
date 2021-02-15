# Package loading
library(car)
library(DescTools)
library(corrplot)

getwd()

# Set Working Directory
# setwd("C:/Users/Rahul/Documents/IMR/Data_Linear_Regression/")
setwd("C:/Users/anusa/Downloads")
getwd() # Check/ Confirm if the directory has changed

# Read the data file
TrainRaw = read.csv("PropertyPrice_Train.csv", stringsAsFactors = TRUE)
PredictionRaw = read.csv("PropertyPrice_Prediction.csv", stringsAsFactors = TRUE)

View(TrainRaw)
View(PredictionRaw)

# Check total columns
ncol(TrainRaw)
ncol(PredictionRaw)


# Create Source Column in both Train and Test
TrainRaw$Source = "Train"
PredictionRaw$Source = "Test"

# Combine Train and Test
FullRaw = rbind(TrainRaw, PredictionRaw)

# View the data
View(FullRaw) # "Sale_Price" is our "dependent variable"

# Lets drop "Id" column from the data as it is not going to assist us in our model
FullRaw = subset(FullRaw, select = -Id)
# FullRaw$Id = NULL # OR this also works



# Validate the deletion
dim(FullRaw) # Should be 1 column less
colnames(FullRaw) # Should not have the "Id" column

# Check the summary of the file
summary(FullRaw)

# Check for NAs
colSums(is.na(FullRaw))


############################
# Missing value imputation (manually)
############################

# Variables having missing value
# Garage_Built_Year, Garage

# Garage_Built_Year
# Step 1: Find median
median(TrainRaw$Garage_Built_Year)
tempMedian = median(TrainRaw$Garage_Built_Year, na.rm = TRUE)
tempMedian

# Step 2: Find missing value rows
missingValueRows = is.na(FullRaw[, "Garage_Built_Year"]) 
sum(missingValueRows)

# Step 3: Impute (or Fill) missing values in data
FullRaw[missingValueRows, "Garage_Built_Year"] = tempMedian
colSums(is.na(FullRaw))
summary(FullRaw)

# Garage
# Step 1: Find Mode
Mode(TrainRaw$Garage, na.rm = TRUE)[1]
tempMode = Mode(TrainRaw$Garage, na.rm = TRUE)[1]
tempMode

# Step 2: Find missing value rows
missingValueRows = is.na(FullRaw[, "Garage"]) 
sum(missingValueRows)

# Step 3: Impute (or Fill) missing values in data
FullRaw[missingValueRows, "Garage"] = tempMode


# Check for NAs
colSums(is.na(FullRaw)) # Should have NO NAs except for Sale_Price

# ############################
# # Missing value imputation (automatic)
# ############################
# 
# 
# # Categorical: Impute with mode
# # Continuous: Impute with median
# 
# # # Categorical variable example
# # Mode(FullRaw$Garage)
# # Mode(FullRaw[FullRaw$Source == "Train", "Garage"])
# #
# # # Continuous variable example
# # median(FullRaw$Garage_Built_Year, na.rm = TRUE)
# # median(FullRaw[FullRaw$Source == "Train", "Garage_Built_Year"], na.rm = TRUE)
# 
# 
# # Loop to automatically impute missing values
# # Some checks/constraints for the loop:
# # 1. Drop a column if missing value is more than 50%
# # 2. Use median for continuous vars and mode for categorical vars
# # 3. Use "train" rows to find median/mode and use that to impute on the "full" data
# 
# colSums(is.na(FullRaw))
# 
# halfTrainingRows = 0.5*nrow(FullRaw[FullRaw$Source == "Train",])
# for(i in colnames(FullRaw))
# {
#   if((sum(is.na(FullRaw[FullRaw$Source == "Train",i])) < halfTrainingRows)) # 1st Check: NAs should be less than 50% of training rows
#   {
#     if((class(FullRaw[,i]) == "integer") | (class(FullRaw[,i]) == "numeric")) # 2nd Check: Identify continuous variables
#     {
#       # cat("Cont", column, "\n")
#       tempMedian = median(FullRaw[FullRaw$Source == "Train", i], na.rm = TRUE) # 3rd Check: Use train rows
#       # print(tempMedian)
#       missingValueRows = is.na(FullRaw[,i])
#       FullRaw[missingValueRows,i] = tempMedian
# 
#     }else
#     {
#       # cat("Categ", column, "\n")
#       tempMode = Mode(FullRaw[FullRaw$Source == "Train", i], na.rm = TRUE)[1] # 3rd Check: Use train rows
#       # print(tempMode)
#       missingValueRows = is.na(FullRaw[,i])
#       FullRaw[missingValueRows,i] = tempMode
#     }
#   }else
#   {
#       print(i)
#       FullRaw[,i] = NULL # Drop/remove the column
#   }
# }
# 
# summary(FullRaw)
# colSums(is.na(FullRaw))
# sum(is.na(FullRaw))
# dim(FullRaw)

############################
# Correlation check
############################

library(corrplot)

continuous_variable_check = function(x)
{
  return(is.numeric(x) | is.integer(x))
}

continuousVars = sapply(TrainRaw, continuous_variable_check)
continuousVars
corrDf = cor(TrainRaw[TrainRaw$Source == "Train", continuousVars])
View(corrDf)

windows()
corrplot(corrDf)


############################
# Dummy variable creation
############################

factorVars = sapply(FullRaw, is.factor)
factorVars
dummyDf = model.matrix(~ ., data = FullRaw[,factorVars])
View(dummyDf)

dim(dummyDf)
FullRaw2 = cbind(FullRaw[,!factorVars], dummyDf[,-1])

# Check the dimensions of FullRaw2
dim(FullRaw2)

# Check if all variables are now numeric/integer
str(FullRaw2) 


############################
# Sampling
############################

# Step 1: Divide Train into Train and Test
Train = subset(FullRaw2, subset = FullRaw2$Source == "Train", select = -Source)
PredictionDf = subset(FullRaw2, subset = FullRaw2$Source == "Test", select = -Source)


# Step 2: Divide Train further into Train and Test by random sampling
#set.seed(123) # This is used to reproduce the SAME composition of the sample EVERYTIME
set.seed(100)
RowNumbers = sample(x = 1:nrow(Train), size = 0.80*nrow(Train))
head(RowNumbers)
Test = Train[-RowNumbers, ] # Testset
Train = Train[RowNumbers, ] # Trainset

dim(Train)
dim(Test)


############################
# Multicollinearity check
############################

# Remove variables with VIF > 5

M1 = lm(Sale_Price ~ ., data = Train)
# M1 = lm(Dependent ~ x1 + x2 + x3 + x4, data = Train)

library(car)
sort(vif(M1), decreasing = TRUE)[1:3]



# Remove GarageAttchd
M2 = lm(Sale_Price ~ . - GarageAttchd, data = Train)
sort(vif(M2), decreasing = TRUE)[1:3]


# Remove Kitchen_QualityTA
M3 = lm(Sale_Price ~ . - GarageAttchd - Kitchen_QualityTA, data = Train)
sort(vif(M3), decreasing = TRUE)[1:3]

# Remove First_Floor_Area
M3 = lm(Sale_Price ~ . - GarageAttchd - Kitchen_QualityTA - First_Floor_Area, data = Train)
sort(vif(M3), decreasing = TRUE)[1:3]


# All variables are within the bound of VIF (VIF < 5). Now we can proceed towards model building and variable selection
summary(M3)

############################
# Model optimization (by selecting ONLY significant variables through step() function)
############################

# Use step() function to remove insignificant variables from the model iteratively
M4 = step(M3) # Step function works on the concept of reducing AIC. Lower the AIC, better the model

summary(M4) # Lets finalize this model


############################
# Model diagnostics
############################

# Few checks
# Homoskedasticity check
plot(M4$fitted.values, M4$residuals) 
# Should not show prominent non-constant variance (heteroskadastic) of errors against fitted values

# Normality of errors check
summary(M4$residuals) # To check the range. Will be used in histogram is next step
hist(M4$residuals, breaks = seq(-490000, 340000, 10000)) # Should be somewhat close to normal distribution



############################
# Model Evaluation
############################

# After doing all of this, the model has to be checked against the test data as well
# Lets predict on testset and then calculate a metric called MAPE to estimate the errors on testing data
M4_Pred = predict(M4, Test)
head(M4_Pred)
head(Test$Sale_Price)


############################
Actual = Test$Sale_Price
Prediction = M4_Pred

# RMSE (Root Mean Square Error)

sqrt(mean((Actual - Prediction)^2)) # 38705
# This means on an "average", the house price prediction would have +/- error of about 38705

# An RMSE of 13 might actually great, it completely depends on the range of the dependent variable. 
# For example, if your target variable was in the range [0,100000], than an RMSE of 13 is spectacular. 
# On the other hand, if your target is in the range [0,1], an RMSE of 0.5 is terrible.
# So, sometimes, its tough to point to your clients "EXACTLY" how good or bad your model predictions are!
# Thats where another metric called "MAPE" comes in. Its interpreted in percentage term.

# MAPE (Mean Absolute Percentage Error)
mean(abs((Actual - Prediction)/Actual))*100 # 16%
# This means on an "average", the house price prediction would have +/- error of 16%

# Generally, a MAPE under 10% is considered very good, and anything under 20% is reasonable.
# MAPE over 20% is usually not considered great.

############################

# Check MAPE and RMSE results using forecast package
# library(forecast)
# accuracy(M4_Pred, Test$Sale_Price)


############################
# Additional/ Practical aspects to be tried out 
############################

# 1. Take a different training and testing sample. Check MAPE score
# 2. Add more information (indep variables) to the model (this generally happens in client projects)
# 3. Try out other algorithms (We are yet to cover this point in this curriculum)

