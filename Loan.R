# Comprehensive Loan Information
# https://www.kaggle.com/datasets/nezukokamaado/auto-loan-dataset
# Dataset:

#=================================
# LIBRARIES
#=================================
# Packages & Libraries
install.packages("rio")
install.packages("mltools")
install.packages("CatEncoders")
install.packages("DAAG")
install.packages("factoextra")
install.packages("ggrepel")
install.packages("caret")
install.packages("class")
install.packages("gmodels")
library(gmodels)
library(class)
library(FactoMineR)
library(devtools)
library(dplyr) 
library(tidyverse)
library(ggplot2)
library(Amelia)
library(RColorBrewer)
library(plotly)
library(moments)
library(ggcorrplot)
library(corrplot)
library(GGally)
library(caret)
library(mltools)
library(data.table)
library(CatEncoders)
library(car)
library(DAAG)
library(factoextra)
library(ggrepel)
library(ggfortify)
library(pROC)
library(mlbench)
theme_set(theme_bw())

#=================================
# LOAD DATASET
#=================================
# Switch your working directory to where ever you have downloaded the file.
setwd('C:/Users/reyna/Documents/MSc in Data Analytics/Data Mining & Machine Learning 1/0. CA')

# Read in our csv and put it in a data.frame.
df <- read.csv('financial_loan.csv',  header=T, na.strings=c(""), stringsAsFactors = T)

#=================================
# EXPLORATORY DATA ANALYSIS
#=================================
head(df)
#read dimension of dataframe
dim(df)
#variable names of a data frame using the names function.
names(df)
#Explore Structure of the data frame columns
str(df)

#---------------------------------
# Checking unique values in each variable
#---------------------------------
#count unique values in each column
sapply(df, function(x) length(unique(x)))

#set "id" column as row names
df <- df %>% column_to_rownames(., var = 'id')

#Checking unique values in variables by lenght=1
sort(unique(df$application_type))
#Delete columns that does have the same value
df<-subset(df, select= -application_type)

#Convert in datetime variables that cointain a date
df$issue_date<-as.Date(df$issue_date, format = "%m-%d-%Y")
df$last_credit_pull_date<-as.Date(df$last_credit_pull_date, format = "%m-%d-%Y")
df$last_payment_date<-as.Date(df$last_payment_date, format = "%m-%d-%Y")
df$next_payment_date<-as.Date(df$next_payment_date, format = "%m-%d-%Y")

#---------------------------------
# Handling Missing Values
#---------------------------------
#Remove rows with NA's using rowSums()
colSums(is.na(df))
sum(is.na(df))
missmap(df, main = "Missing values vs Observed")

#Delete columns with high NA's values
df<-subset(df, select= -last_credit_pull_date)
df<-subset(df, select= -last_payment_date)
df<-subset(df, select= -next_payment_date)
sum(is.na(df))

#Remove rows with NA's using rowSums()
df<- df[rowSums(is.na(df)) == 0, ] 
sum(is.na(df))

#summarize dataset
summary(df)

#---------------------------------
# VISUALIZATIONS OF CARTEGORICAL VARIABLES
#---------------------------------
#Barplot according to addres_state
count_address<-df %>%
  group_by(address_state)%>%
  tally(sort=TRUE)
top_address<-count_address[1:15,]
address_top<-ggplot(top_address, aes(y=reorder(address_state, -n), x=n, fill=n)) + geom_bar(stat='identity') + geom_col(position = position_dodge()) + 
  labs(title = "Top of Address State", caption = " Figure X", y = "Address State")
ggplotly(address_top)

#Barplot according to emp_length
bar_lenght<-ggplot(data = df) + geom_bar(mapping = aes(y = emp_length, fill = emp_length)) + 
  labs(title = "Options of Time loan's payment ", caption = " Figure X", y = "Payment terms")
ggplotly(bar_lenght)

#Barplot according to emp_title
count_title<-df %>%
  group_by(emp_title)%>%
  tally(sort=TRUE)
top_title<-count_title[1:15,]
title_top<-ggplot(top_title, aes(y=reorder(emp_title, -n), x=n, fill=n)) + geom_bar(stat='identity') + geom_col(position = position_dodge()) + 
  labs(title = "The most popular job title supplied when applying for the loan ", caption = " Figure X", y = "Job Title Supplied")
ggplotly(title_top)

#Barplot according to grade
grade<-ggplot(data = df) + geom_bar(mapping = aes(y = grade, fill = grade)) + 
  labs(title = "Lending Club assigned loan grade ", caption = " Figure X", y = "Loan Grades")
ggplotly(grade)

#Barplot according to home
home<-ggplot(data = df) + geom_bar(mapping = aes(y = home_ownership, fill = home_ownership)) +
  labs(title = "Home Ownership Status ", caption = " Figure X", y = "Name of home ownership")
ggplotly(home)

#Barplot loan stauts
status<-ggplot(data = df) + geom_bar(mapping = aes(y = loan_status, fill = loan_status)) + 
  labs(title = "Status of Loan ", caption = " Figure X", y = "Type of Loan Status")
ggplotly(status)

#Barplot of purpose
purpose<-ggplot(data = df) + geom_bar(mapping = aes(y = purpose, fill = purpose)) + 
  labs(title = "Purpose of Loan ", caption = " Figure X", y = "Type of purpose")
ggplotly(purpose)

#Barplot of LC assigned loan subgrade
subgrade<-ggplot(data = df) + geom_bar(mapping = aes(y = sub_grade, fill = sub_grade)) +
  labs(title = "LC assigned load Subgrade ", caption = " Figure X", y = "Type of Load subgrade")
ggplotly(subgrade)

#Barplot of Term
term<-ggplot(data = df) + geom_bar(mapping = aes(y = term, fill = term)) + 
  labs(title = "Payment on the loan", caption = " Figure X", y = "The number of payments on the loan")
ggplotly(term)

#Barplot of verificacion_status
verification<-ggplot(data = df) + geom_bar(mapping = aes(y = verification_status, fill = verification_status)) + 
  labs(title = "Status of verification if the income was verified", caption = " Figure X", y = "Status of verification")
ggplotly(verification)

#---------------------------------
# VISUALIZATIONS OF NUMERIC VARIABLES
#---------------------------------
summary(df)
#Histogram of loan_amount: The listed amount of the loan applied for by the borrower
loan<-ggplot(df, aes(x=loan_amount, fill= verification_status)) + geom_histogram( position="identity") + scale_color_brewer() +
  labs(title = "The listed amount of the load applied for by the borrower", caption = " Figure X", x= "loan amount")
ggplotly(loan)
#Histogram of total_acc: The total number of credit lines currently in the borrower's credit file
acc<-ggplot(df, aes(x=total_acc, fill= verification_status)) + geom_histogram( position="identity") + scale_color_brewer() +
  labs(title = "The total number of credit lines currently in the borrower's credit file", caption = " Figure X", x= "Total number of credit lines")
ggplotly(acc)
#Histogram of total_payment: Payments received to date for total amount funded
payment<-ggplot(df, aes(x=total_payment, fill= verification_status)) + geom_histogram( position="identity") + scale_color_brewer() +
  labs(title = "Payments received to date for total amount funded", caption = " Figure X", x= "Payments received")
ggplotly(payment)

#---------------------------------
# VISUALIZATIONS BETWEEN VARIABLES
#---------------------------------
#create scatterplot of Loan-Amount vs Annual-Income
sct1<-ggplot(df, aes(x = loan_amount, y = annual_income)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Distribution of Loan-Amount vs Annual-Income")
ggplotly(sct1)
#create scatterplot of Loan-Amount vs dti
sct2<-ggplot(df, aes(x = loan_amount, y = dti)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Distribution of Loan-Amount vs dti")
ggplotly(sct2)
#create scatterplot of Loan-Amount vs installment
sct3<-ggplot(df, aes(x = loan_amount, y = installment)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Distribution of Loan-Amount vs Installment")
ggplotly(sct3)
#create scatterplot of Loan-Amount vs int_rate
sct4<-ggplot(df, aes(x = loan_amount, y = int_rate)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Distribution of Loan-Amount vs Int-Rate")
ggplotly(sct4)
#create scatterplot of Loan-Amount vs total_acc
sct5<-ggplot(df, aes(x = loan_amount, y = total_acc)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Distribution of Loan-Amount vs Total_acc")
ggplotly(sct5)
#create scatterplot of Loan-Amount vs total_payment
sct6<-ggplot(df, aes(x = loan_amount, y = total_payment)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Distribution of Loan-Amount vs Total_payment")
ggplotly(sct6)

#---------------------------------
# Handling Outliers
#---------------------------------
#create scatterplot of annual_income
income<-ggplot(df, aes(x = annual_income, y = annual_income)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Annual Income")
ggplotly(income)
sort(boxplot.stats(df$annual_income)$out)
#Remove annual_income > 145,596 
df<-df[df$annual_income <= 145596,]

#find Q1, Q3, and interquartile range for values in points column
Q1_1 <- quantile(df$annual_income, .25)
Q3_1<- quantile(df$annual_income, .75)
IQR_1 <- IQR(df$annual_income)

#subset data where points value is outside 1.5*IQR of Q1 and Q3
outliers1 <- subset(df, df$annual_income>(Q3_1 + 1.5*IQR_1) | df$annual_income >(Q3_1 + 1.5*IQR_1))
df<- subset(df, df$annual_income >(Q1_1 - 1.5*IQR_1) & df$annual_income <(Q3_1 + 1.5*IQR_1))
  
#create scatterplot of dti (Ratio of total monthly debt payments on the total debt obligations)
dti<-ggplot(df, aes(x = dti, y = dti)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Ratio of total monthly debt payments on the total obligations")
ggplotly(dti)

#find Q1, Q3, and interquartile range for values in points column
Q1_2 <- quantile(df$dti, .25)
Q3_2<- quantile(df$dti, .75)
IQR_2<- IQR(df$dti)

#subset data where points value is outside 1.5*IQR of Q1 and Q3
outliers2 <- subset(df, df$dti>(Q3_2 + 1.5*IQR_2)| df$dti >(Q3_2 + 1.5*IQR_2))
df<- subset(df, df$dti >(Q1_2 - 1.5*IQR_2) & df$dti <(Q3_2 + 1.5*IQR_2))

#create scatterplot of installment: Monthly payment owed
installment<-ggplot(df, aes(x = installment, y = installment)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Monthly payment owed by the borrower")
ggplotly(installment)

#find Q1, Q3, and interquartile range for values in points column
Q1_3 <- quantile(df$installment, .25)
Q3_3 <- quantile(df$installment, .75)
IQR_3 <- IQR(df$installment)

outliers3<- subset(df, df$installment <(Q1_3 - 1.5*IQR_3) | df$installment >(Q3_3 + 1.5*IQR_3))
df<- subset(df, df$installment >(Q1_3 - 1.5*IQR_3) & df$installment <(Q3_3 + 1.5*IQR_3))

#create scatterplot of int_rate: Interest Rate on the loan
rate<-ggplot(df, aes(x = int_rate, y = int_rate)) + geom_point(aes(color = factor(verification_status))) + ggtitle("Interest Rate on the loan")
ggplotly(rate)
sort(boxplot.stats(df$int_rate)$out)
#Remove int_rate > 0.2248
df<-df[df$int_rate <= 0.2248,]

#find Q1, Q3, and interquartile range for values in points column
Q1_4 <- quantile(df$int_rate, .25)
Q3_4 <- quantile(df$int_rate, .75)
IQR_4 <- IQR(df$int_rate)

outliers4<- subset(df, df$int_rate <(Q1_4 - 1.5*IQR_4) | df$int_rate >(Q3_4 + 1.5*IQR_4))
df<- subset(df, df$int_rate >(Q1_4 - 1.5*IQR_4) & df$int_rate <(Q3_4 + 1.5*IQR_4))


#create boxplot loan_amount
loan2<-ggplot(df, aes(x=verification_status, y=loan_amount, fill=verification_status )) + geom_boxplot() + guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Loan Amount")
ggplotly(loan2)
sort(boxplot.stats(df$loan_amount)$out)
#Remove loan_amount >= 29500
df<-df[df$loan_amount <= 29500,]

#find Q1, Q3, and interquartile range for values in points column
Q1_5 <- quantile(df$loan_amount, .25)
Q3_5 <- quantile(df$loan_amount, .75)
IQR_5 <- IQR(df$loan_amount)

outliers5<- subset(df, df$loan_amount <(Q1_5 - 1.5*IQR_5) | df$loan_amount >(Q3_5 + 1.5*IQR_5))
df<- subset(df, df$loan_amount >(Q1_5 - 1.5*IQR_5) & df$loan_amount <(Q3_5 + 1.5*IQR_5))

#create boxplot total_acc
acc2<-ggplot(df, aes(x=verification_status, y=total_acc, fill=verification_status )) + geom_boxplot() + guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of The total Number of credit lines")
ggplotly(acc2)
sort(boxplot.stats(df$total_acc)$out)
#Remove total_acc >= 51
df<-df[df$total_acc <= 51,]

#find Q1, Q3, and interquartile range for values in points column
Q1_6 <- quantile(df$total_acc, .25)
Q3_6 <- quantile(df$total_acc, .75)
IQR_6 <- IQR(df$total_acc)

outliers6<- subset(df, df$total_acc <(Q1_6 - 1.5*IQR_6) | df$total_acc >(Q3_6 + 1.5*IQR_6))
df<- subset(df, df$total_acc >(Q1_6 - 1.5*IQR_6) & df$total_acc <(Q3_6 + 1.5*IQR_6))

#create boxplot total_payment
payment2<-ggplot(df, aes(x=verification_status, y=total_payment, fill=verification_status )) + geom_boxplot() + guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Total Payment")
ggplotly(payment2)
sort(boxplot.stats(df$total_payment)$out)
#Remove total_payment >= 30358
df<-df[df$total_payment <= 30358,]

#find Q1, Q3, and interquartile range for values in points column
Q1_7 <- quantile(df$total_payment, .25)
Q3_7 <- quantile(df$total_payment, .75)
IQR_7 <- IQR(df$total_payment)

outliers7<- subset(df, df$total_payment <(Q1_7 - 1.5*IQR_7) | df$total_payment >(Q3_7 + 1.5*IQR_7))
df<- subset(df, df$total_payment >(Q1_7 - 1.5*IQR_7) & df$total_payment <(Q3_7 + 1.5*IQR_7))

#---------------------------------
# CORRELATION MATRIX
#---------------------------------
# correlation between only numerical variables
corr<-cor(df[,unlist(lapply(df, is.numeric))])
corr
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

#---------------------------------
# TRANSFORM DATA
#---------------------------------

df1<-df
df_num<-select_if(df1, is.numeric)
#descriptive measures
sapply(df_num, mean)
sapply(df_num, median)
sapply(df_num, sd)
moments::kurtosis(df_num)
moments::skewness(df_num)

#perform log transformation
df1$log_income<- log(df1$annual_income)
df1$log_installment<-log(df1$installment)

#create Q-Q plot
#Transformation annual_income
ggplot(df1, aes(sample=log_income)) +  stat_qq(size=2.5, color='red') + stat_qq_line() + ggtitle("Q-Q Plot Annual Income")
ggplot(df1, aes(log_income)) +geom_histogram(aes(x=log_income, y=after_stat(density)), bins=50) + stat_function(fun=dnorm, 
           args = list(mean=mean(df1$log_income), sd=sd(df1$log_income)), color="red") + ggtitle("Plot of Normal Distribution")

#Transformation installment
ggplot(df1, aes(sample=log_installment)) +  stat_qq(size=2.5, color='red') + stat_qq_line() + ggtitle("Q-Q Plot Monthly payment owed")
ggplot(df1, aes(log_installment)) +geom_histogram(aes(x=log_installment, y=after_stat(density)), bins=50) + stat_function(fun=dnorm, 
           args = list(mean=mean(df1$log_installment), sd=sd(df1$log_installment)), color="red") + ggtitle("Plot of Normal Distribution")

df_num<-select_if(df1, is.numeric)
sapply(df_num, mean)
sapply(df_num, median)
sapply(df_num, sd)
moments::kurtosis(df_num)
moments::skewness(df_num)

#---------------------------------
# PERFORM CATEGORICAL DATA BY DUMMY
#---------------------------------
dummy<-dummyVars(~ home_ownership +loan_status + term + verification_status, data = df1, fullRank = T)
table <- data.frame(predict(dummy, newdata = df1))
df2<- merge(df1, table, by = 0)
#set "Row.Names" column as row names
df2 <- df2 %>% column_to_rownames(., var = 'Row.names')

#---------------------------------
# PERFORM CATEGORICAL DATA BY LABEL ENCODING
#---------------------------------
labs = LabelEncoder.fit(df2$emp_length)
df2$emp_length = transform(labs, df2$emp_length)
head(df2)

#---------------------------------
# CORRELATION MATRIX
#---------------------------------
# correlation between only numerical variables
corr<-cor(df2[,unlist(lapply(df2, is.numeric))])
corr
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

#=================================
# MULTIPLE LINEAR REGRESSION
#=================================

#---------------------------------
# SPLITTING DATA INTO TRAINING AND TEST SETS BY HOLD-OUT
#---------------------------------
#create a random seed using ID student number = 23127635 
set.seed(23127635)
#Use 70% of dataset as training set and remaining 30% as testing set
splitting <- sample(c(TRUE, FALSE), nrow(df2), replace=TRUE, prob=c(0.7,0.3))
train  <- df2[splitting, ]
test   <- df2[!splitting, ]
head(train)

#---------------------------------
# MODEL 1
#---------------------------------
model1<- lm(loan_amount~ emp_length + annual_income + dti + installment + int_rate + total_acc + total_payment + 
              home_ownership.NONE + home_ownership.OTHER, + home_ownership.OWN + home_ownership.RENT + loan_status.Current +
              loan_status.Fully.Paid + term..60.months + verification_status.Source.Verified + verification_status.Verified, data = train)
summary(model1)
#---------------------------------
# MODEL 2
#---------------------------------
model2<- lm(loan_amount~ emp_length + annual_income + dti + installment + int_rate + total_acc + total_payment, 
            data = train)
summary(model2)

# Calculate the VIF
vif_values <- vif(model2)

# Create a data frame of the VIF values
vif_data <- data.frame(
  variable = names(vif_values),
  vif = vif_values
)

# Create a bar chart of VIF values
ggplot(vif_data, aes(x = variable, y = vif)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal()+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  labs(
    title = "Variance Inflation Factor (VIF)",
    x = "Predictor Variables",
    y = "VIF Values"
  )
vif(model2)

#Diagnostic plot Multiple Regression
par(mfrow = c(2,2))
plot(model2)

#---------------------------------
# MODEL 3
#---------------------------------
model3<- lm(loan_amount~ annual_income + dti + installment + int_rate + total_acc + total_payment, 
            data = train)
summary(model3)

# Calculate the VIF
vif_values <- vif(model3)

# Create a data frame of the VIF values
vif_data <- data.frame(
  variable = names(vif_values),
  vif = vif_values
)

# Create a bar chart of VIF values
ggplot(vif_data, aes(x = variable, y = vif)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal()+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  labs(
    title = "Variance Inflation Factor (VIF)",
    x = "Predictor Variables",
    y = "VIF Values"
  )
vif(model3)

#Diagnostic plot Multiple Regression
par(mfrow = c(2,2))
plot(model3)

#---------------------------------
# MODEL 4
#---------------------------------
model4<- lm(loan_amount~ installment + total_payment, data = train)
summary(model4)

# Calculate the VIF
vif_values <- vif(model4)

# Create a data frame of the VIF values
vif_data <- data.frame(
  variable = names(vif_values),
  vif = vif_values
)

# Create a bar chart of VIF values
ggplot(vif_data, aes(x = variable, y = vif)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal()+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  labs(
    title = "Variance Inflation Factor (VIF)",
    x = "Predictor Variables",
    y = "VIF Values"
  )
vif(model4)

#---------------------------------
# PREDICTION MODEL 4
#---------------------------------

predict(model4, newdata = test, interval = "confidence", level = 0.95)
#Diagnostic plot Multiple Regression
par(mfrow = c(2,2))
plot(model4)

# Other useful functions
anova(model3, model4)

#==================================
# K-NEAREST NEIGHBORS
#==================================
#---------------------------------
# MODEL 1
#---------------------------------

dfk<- df%>%select('verification_status','annual_income','dti','installment','int_rate', 'loan_amount','total_acc','total_payment')
summary(dfk)
#Normalize the variables
dfk[,-1]<- scale(dfk[,-1])
head(dfk)
levels(dfk$verification_status)
levels(dfk$verification_status)[levels(dfk$verification_status)=='Not Verified'] <- 'Not_Verified'
levels(dfk$verification_status)[levels(dfk$verification_status)=='Source Verified'] <- 'Source_Verified'
levels(dfk$verification_status)

#---------------------------------
# SPLITTING DATA INTO TRAINING AND TEST 
#---------------------------------
 
set.seed(9006)

#Use 70% of dataset as training set and remaining 30% as testing set
splitt2 <- createDataPartition(dfk$verification_status, p = 0.70, list = FALSE)
training <- dfk[splitt2, ]
testing <- dfk[-splitt2, ]
head(training)
head(testing)

#Select K-value
#create train control with 10 iterations, repeat the cross validation 3 times
control<- trainControl(method = "repeatedcv",
                       number =10,
                       repeats =3,
                       classProbs = TRUE,
      )

set.seed(9006)
knn_fit<- train(verification_status~.,
                data= training,
                method = 'knn',
                trControl = control,
                metric = "Accuracy")

#Model Performance
knn_fit
plot(knn_fit)

#---------------------------------
# PREDICTION MODEL 2
#---------------------------------

dfk2<- df%>%select('term','annual_income','dti','installment','int_rate', 'loan_amount','total_acc','total_payment')
summary(dfk2)

dfk2[,-1]<- scale(dfk2[,-1])
head(dfk2)
levels(dfk2$term)
levels(dfk2$term)[levels(dfk2$term)==' 36 months'] <- 'months_36'
levels(dfk2$term)[levels(dfk2$term)==' 60 months'] <- 'months_60'
levels(dfk2$term)

prop.table(table(dfk2$term))

#---------------------------------
# SPLITTING DATA INTO TRAINING AND TEST
#---------------------------------
set.seed(9006)

#Use 70% of dataset as training set and remaining 30% as testing set
splitt3 <- createDataPartition(dfk2$term, p = 0.70, list = FALSE)
training2 <- dfk2[splitt3, ]
testing2 <- dfk2[-splitt3, ]
head(training2)
head(testing2)

#Select K-value
#create train control with 10 iterations, repeat the cross validation 3 times
control2<- trainControl(method = "repeatedcv",
                       number =10,
                       repeats =3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary
)

set.seed(9006)
knn_fit2<- train(term~.,
                data= training2,
                method = 'knn',
                trControl = control2,
                metric = "ROC")

#keep this Initial k for testing with knn() function in next section
knn_k2<- knn_fit2$bestTune 
#Model Performance
knn_fit2
plot(knn_fit2)

#---------------------------------
# PREDICTION
#---------------------------------
#ROC curve Importance
plot(varImp(knn_fit2))

set.seed(7)
prediction<- predict(knn_fit2, newdata = testing2)

confusion_matrix<- confusionMatrix(prediction, testing2$term)
confusion_matrix

CrossTable(x =dfk2[-splitt3,1], y=prediction, prop.chisq = FALSE)
