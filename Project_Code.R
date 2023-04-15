####################################
##    Loan Prediction Project     ##
####################################

library(tidyverse)

## IMPORTING DATA AND DATA CLEANING ##
# Set your work directory #
getwd()

# Import data 
library(readr)
loan_prediction <- read_csv("loan_prediction_data.csv")

# Checks on the dataset 
head(loan_prediction, 5)

names(loan_prediction)

sapply(loan_prediction, class)

# Change class of categorical variables from 'character' to 'factor'
loan_prediction$Married <- as.factor(loan_prediction$Married)
loan_prediction$Gender <- as.factor(loan_prediction$Gender)
loan_prediction$Dependents <- as.factor(loan_prediction$Dependents)
loan_prediction$Education <- as.factor(loan_prediction$Education)
loan_prediction$Self_Employed <- as.factor(loan_prediction$Self_Employed)
loan_prediction$Property_Area <- as.factor(loan_prediction$Property_Area)
loan_prediction$Loan_Status <- as.factor(loan_prediction$Loan_Status)
loan_prediction$Credit_History <- as.factor(loan_prediction$Credit_History)

# Useful to have variable labels, so we know what each variable represents
install.packages("expss")
library(expss)
loan_prediction <- apply_labels(loan_prediction,
                                Loan_ID = "Unique Loan ID",
                                Gender = "Male/Female",
                                Married = "Marital status of applicant (Y/N)",
                                Dependents = "Number of dependents (0,1,2,3+)",
                                Education = "Education level of applicants (Graduate/Not Graduate)",
                                Self_Employed = "Whether applicant is self-employed (Y/N)",
                                ApplicantIncome = "Monthly income of applicant (USD)",
                                CoapplicantIncome = "Monthly income of co-applicant (USD) - 0 if no coapplicant",
                                LoanAmount = "Loan amount requested by applicant ('000 USD)",
                                Loan_Amount_Term = "Term of the loan in months",
                                Credit_History = "Whether credit history meets guidelines (0 - NO/1 - YES)",
                                Property_Area = "Property location (Rural/Semiurban/Urban)",
                                Loan_Status = "Loan approved (Y) or Rejected (N)")

# Viewing labels 
sapply(loan_prediction, var_lab) # all variables
var_lab(loan_prediction$Property_Area) # one variable

# Understanding Data #
summary(loan_prediction)

## EXPLORATORY DATA ANALYSIS ##

# Tables - Loan Approval Status for a few categories #

# 1) Education and Loan Status 
table1 <- table(loan_prediction$Education, loan_prediction$Loan_Status) # Frequency
prop.table(table1) # Proportions

# 2) Credit History and Loan Status 
table2 <- table(loan_prediction$Credit_History, loan_prediction$Loan_Status) # Frequency
prop.table(table2) # Proportions

# 3) Property Area and Loan Status 
table3 <- table(loan_prediction$Property_Area, loan_prediction$Loan_Status) # Frequency
prop.table(table3) # Proportions

# Other ways to visualize categorical variables #

# Barplots #
# Education levels 
ggplot(loan_prediction, aes(x=Education, fill=Loan_Status)) +
  geom_bar(position=position_dodge(width=0.9))+
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.3, position = position_dodge(width=0.9))+
  ggtitle("Loan Approved by Education Status") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_brewer(palette="Set1")

# Credit History 
ggplot(data=subset(loan_prediction, !is.na(Credit_History)), aes(x=Credit_History, fill=Loan_Status)) +
  geom_bar(position=position_dodge(width=0.9))+
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.3, position = position_dodge(width=0.9))+
  ggtitle("Loan Approved by Credit History") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_hue(l = 70, c = 30)

# Property Area 
ggplot(loan_prediction, aes(x=Property_Area, fill=Loan_Status)) +
  geom_bar(position=position_dodge(width=0.9))+
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.3, position = position_dodge(width=0.9))+
  ggtitle("Loan Approved by Property Area") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_hue()

## Barplots for continuous variables ##
# Income of Applicant and Loan Status #
loan_prediction$total_income <- loan_prediction$ApplicantIncome + loan_prediction$CoapplicantIncome

summary(loan_prediction$total_income)

# Divide total income into 3 groups
loan_prediction$income_group <- as.numeric(cut_number(loan_prediction$total_income,4))

ggplot(loan_prediction, aes(x = factor(income_group), fill = Loan_Status)) +
  geom_bar(position = position_dodge(width=0.9)) +
  geom_text(aes(label= ..count..), stat = "count", vjust=-0.3, color="black",
            position = position_dodge(width=0.9))+ 
  ggtitle("Loan Approved by Income Group") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_hue()

# Loan Amount and Loan Status #  
summary(loan_prediction$LoanAmount)

loan_prediction$amount_group <- as.numeric(cut_number(loan_prediction$LoanAmount,3))

ggplot(data=subset(loan_prediction, !is.na(LoanAmount)), aes(x = factor(amount_group), fill = Loan_Status)) +
  geom_bar(position = position_dodge(width=0.9)) +
  geom_text(aes(label= ..count..), stat = "count", vjust=-0.3, color="black",
            position = position_dodge(width=0.9))+ 
  ggtitle("Loan Approved by Loan Amount") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_brewer(palette="Set2")

# Think about other plots you can run - Marital Status, Gender etc 
# Would distribution plots for continuous, numerical variables - Loan Amount, Incomes help? 

## SEPARATING DATA INTO TEST/TRAIN SAMPLES #
set.seed(10) # So the samples are reproducible 

# Generally, a 70:30 fraction is used, but we're using 60:40 because the sample size is rather small
train <- sample_frac(loan_prediction, 0.60)
test <- anti_join(loan_prediction,train,by='Loan_ID')

## RUNNING DIFFERENT MODELS ##

# 1) Linear Probability Model #
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)

# Run a LPM - using different variables #

lpm_loan <- glm(train$Loan_Status ~ Gender + Dependents + Property_Area + Loan_Amount_Term + LoanAmount + Education,family = binomial(link = 'logit'),data = train)

# View regression table 
summary(lpm_loan)

# How well the LPM does in prediction 
prediction_lpm <- predict(lpm_loan, newdata = test, type = "response")

# Confusion Matrix 
table(test$Loan_Status, prediction_lpm > 0.5)

# 2) Decision Tree #
install.packages("rpart")
library(rpart)

tree <- rpart(Loan_Status ~ Gender + Dependents + Property_Area + Loan_Amount_Term + LoanAmount + Education, 
              method="class", data=train, parms=list(split="information"))

tree$cptable

# Pruning the Decision Tree
# Choose the spilt with the smallest xerror - this corresponds to cp=0.02564103
tree_pruned <- prune(tree,cp=0.02564103)

# How well the Decision Tree does in prediction 
tree_predict <- predict(tree_pruned, test, type="class")

# Confusion Matrix
tree_matrix <- table(test$Loan_Status, tree_predict,
                     dnn=c("Actual", "Predicted"))

tree_matrix

# 3) Random Forest #
install.packages("randomForest")
library(randomForest)

forest_train <- randomForest(Loan_Status ~ Gender + Dependents + Property_Area + Loan_Amount_Term + LoanAmount + Education, 
                             data=train,
                             na.action=na.roughfix,
                             importance=TRUE)

forest_train # The Random Forest ran 500 variations of the Decision Tree and returned the values that gave the least error

# How well the random forest does in prediction 
forest_test <- predict(forest_train, test)

# Confusion Matrix 
forest_matrix <- table(test$Loan_Status, forest_test,
                       dnn=c("Actual", "Predicted"))

forest_matrix
