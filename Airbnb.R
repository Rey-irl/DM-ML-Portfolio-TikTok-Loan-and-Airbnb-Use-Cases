# Airbnb Listings Real Estate Data .
# https://www.kaggle.com/datasets/deeplearner09/airbnb-listings
# Dataset: 'listings.csv

#=================================
# LIBRARIES
#=================================
# Packages & Libraries
install.packages("rio")
install.packages("mapview")
install.packages("rpart.plot")
install.packages("tree")
library(tree)
library(pROC)
library(rpart.plot)
library(rpart)
library(dplyr) 
library(tidyverse)
library(ggplot2)
library(Amelia)
library(RColorBrewer)
library(mapview)
library(plotly)

#=================================
# LOAD DATASET
#=================================
# Read in our csv and put it in a data.frame.
df <- read.csv('listings.csv',  header=T, na.strings=c(""), stringsAsFactors = T)

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
head(df)

#Checking unique values in variables by lenght=1
sort(unique(df$neighbourhood_group))
sort(unique(df$license))

#Delete columns that does have the same value
df<-subset(df, select= -neighbourhood_group)
df<-subset(df, select = -license)

#Convert in datetime "last_review"
df$last_review<-as.Date(df$last_review, format = "%Y-%m-%d")
head(df)
#---------------------------------
# Handling Missing Values
#---------------------------------
#Remove rows with NA's using rowSums()
colSums(is.na(df))
sum(is.na(df))
missmap(df, main = "Missing values vs Observed")

#Remove rows with NA's using rowSums()
df<- df[rowSums(is.na(df)) == 0, ] 
sum(is.na(df))

#summarize dataset
summary(df)

#---------------------------------
# VISUALIZATIONS OF NUMERIC VARIABLES
#---------------------------------
nei<-ggplot(df, aes(x=neighbourhood, fill= room_type)) + geom_histogram( position="identity") + scale_color_brewer() +
      labs(title = "Distribution of Neighbourhood", caption = " Figure X", x= "Neighbourhoods")
ggplotly(nei)
price<-ggplot(df, aes(x=price, fill= room_type)) + geom_histogram( position="identity") + scale_color_brewer() +
        labs(title = "Distribution of Price", caption = " Figure X", x= "Price by dollar")
ggplotly(price)
night<-ggplot(df, aes(x=minimum_nights, fill= room_type)) + geom_histogram( position="identity") + scale_color_brewer() +
        labs(title = "Distribution of Nights' number", caption = " Figure X", x= "Number of Nights")
ggplotly(night)
review<-ggplot(df, aes(x=number_of_reviews, fill= room_type)) + geom_histogram( position="identity") + scale_color_brewer() +
         labs(title = "Number of Reviews", caption = " Figure X", x= "Number of Reviews")
ggplotly(review)
month<-ggplot(df, aes(x=reviews_per_month, fill= room_type)) + geom_histogram( position="identity") + scale_color_brewer() +
        labs(title = "Number of Reviews per Month", caption = " Figure X", x= "Number of Monthly Reviews")
ggplotly(month)
cal<-ggplot(df, aes(x=calculated_host_listings_count, fill= room_type)) + geom_histogram( position="identity") + scale_color_brewer() +
       labs(title = "Count of Host Listings", caption = " Figure X", x= "Host Listings")
ggplotly(cal)
avai<-ggplot(df, aes(x=availability_365, fill= room_type)) + geom_histogram( position="identity") + scale_color_brewer() +
       labs(title = "Availability during the year", caption = " Figure X", x= "Days of availability")
ggplotly(avai)
lmt<-ggplot(df, aes(x=number_of_reviews_ltm, fill= room_type)) + geom_histogram( position="identity") + scale_color_brewer() +
      labs(title = "Number of Reviews", caption = " Figure X", x= "Number of Reviews (lmt)")
ggplotly(lmt)

#---------------------------------
# CORRELATION MATRIX
#---------------------------------
# correlation between only numerical variables
corr<-cor(df[,unlist(lapply(df, is.numeric))])
corr
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)


#=================================
# DECISION TREE
#=================================
df2<- df%>%select('room_type','price','minimum_nights','number_of_reviews','reviews_per_month', 
                  'calculated_host_listings_count','availability_365', 'number_of_reviews_ltm')
summary(df2)

#---------------------------------
# SPLITTING DATA INTO TRAINING AND TEST SETS
#---------------------------------
#Use 70% of dataset as training set and remaining 30% as testing set
splitting <- createDataPartition(df2$room_type, p = 0.70, list = FALSE)
training <- df2[splitting, ]
testing <- df2[-splitting, ]

head(training)
head(testing)
rm(splitting)
#---------------------------------
# TREE CLASIFICATION
#--------------------------------- 
set.seed(2312)
tree<- rpart(room_type~., method = "class", data = training, xval= 20)
tree
rpart.plot(tree, yesno = TRUE)
printcp(tree)

#A plot of xerror vs cp
plotcp(tree)

tree_class <- prune(tree, cp = tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"])rm(tree)
rpart.plot(tree_class, yesno = TRUE)

#---------------------------------
# PREDICTION
#--------------------------------- 
tree_prediction<- predict(tree_class, testing, type = "class")
plot(testing$room_type, tree_prediction,
     main = "Simple Classification: Predicted vs Actual",
     xlab = "Actual",
     ylab = "Predicted")
plot(tree_prediction)

#---------------------------------
# EVALUATION
#--------------------------------- 

confusion_matrix = confusionMatrix(data = tree_prediction,
                                   reference = testing$room_type)
confusion_matrix

tree_acc<- as.numeric(confusion_matrix$overall[1])
tree_acc
rm(tree_prediction)
rm(confusion_matrix)
