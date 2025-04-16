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
library(moments)
library(ggcorrplot)
library(corrplot)
library(GGally)
library(FactoMineR)
library(caret)
library(mltools)
library(data.table)
library(CatEncoders)
library(car)
library(DAAG)
library(factoextra)
library(ggrepel)
library(ggfortify)
theme_set(theme_bw())


#=================================
# LOAD DATASET
#=================================
# Switch your working directory to where ever you have downloaded the file.
setwd('C:/Users/reyna/Documents/MSc in Data Analytics/Data Mining & Machine Learning 1/0. CA')

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
# VISUALIZATIONS OF CARTEGORICAL VARIABLES
#---------------------------------
#Barplot according to Type of room
bar_room<-ggplot(data = df) + geom_bar(mapping = aes(y = room_type, fill = room_type)) + scale_fill_hue(c=20) + 
            labs(title = "Type of Room", caption = " Figure X", y = "Number of type of room")
ggplotly(bar_room)
#Barplot according to name
count_name<-df %>%
               group_by(name)%>%
               tally(sort=TRUE)
top_name<-count_name[1:15,]
bar_top<-ggplot(top_name, aes(x=n, y=name, fill=n)) + geom_bar(stat='identity') + geom_col(position = position_dodge()) + 
           labs(title = "The most popular Airbnb", caption = " Figure X", y = "Name of Airbnb")
ggplotly(bar_top)
#Barplot according to host_name
count_host<-df %>%
  group_by(host_name)%>%
  tally(sort=TRUE)
top_host<-count_host[1:15,]
bar_host<-ggplot(top_host, aes(x=n, y=host_name, fill=n)) + geom_bar(stat='identity') + geom_col(position = position_dodge()) + 
            labs(title = "The best Host", caption = " Figure X", y = "Name of Host")
ggplotly(bar_host)

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
# VISUALIZATIONS BETWEEN VARIABLES
#---------------------------------
#Location of Airbnb according thr Latitude and Longitude in the data
mapview(df, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
#create scatterplot of Price vs Minimum og nights
sct1<-ggplot(df, aes(x = price, y = minimum_nights)) + geom_point(aes(color = factor(room_type))) + ggtitle("Distribution of Price vs Minimum of Nights")
ggplotly(sct1)

#create scatterplot of Price vs Neighbourhood
sct2<-ggplot(df, aes(x = price, y = neighbourhood)) + geom_point(aes(color = factor(room_type))) + ggtitle("Distribution of Price vs Neighbourhood")
ggplotly(sct2)
#create scatterplot of Price vs Number of Reviews
sct3<-ggplot(df, aes(x = price , y = number_of_reviews)) + geom_point(aes(color = factor(room_type))) + ggtitle("Distribution of Price vs Number of Reviews")
ggplotly(sct3)
#create scatterplot of Reviews oer Month vs Availability
sct4<-ggplot(df, aes(x = reviews_per_month , y = availability_365)) + geom_point(aes(color = factor(room_type))) + ggtitle("Distribution of Reviews per Month vs Availability during the year ")
ggplotly(sct4)
#create scatterplot of Neighbourhood vs Number of Reviews (ltm)
sct5<-ggplot(df, aes(x = neighbourhood , y = number_of_reviews_ltm)) + geom_point(aes(color = factor(room_type))) + ggtitle("Distribution of Neighbourhood vs Number of Reviews (ltm)")
ggplotly(sct5)

#---------------------------------
# Handling Outliers
#---------------------------------
#create boxplot of neighbourhood by room_type
boxnei<-ggplot(df, aes(x=room_type, y=neighbourhood, fill=room_type)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Neighbourhood by Type of room")
ggplotly(boxnei)
boxplot.stats(df$neighbourhood)$out

#create boxplot of price by room_type
boxprice<-ggplot(df, aes(x=room_type, y=price, fill=room_type)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Price by Type of room")
ggplotly(boxprice)

sort(boxplot.stats(df$price)$out)
#Remove price > 3,350 
df<-df[df$price < 3350,]

#find Q1, Q3, and interquartile range for values in points column
Q1_1 <- quantile(df$price, .25)
Q3_1<- quantile(df$price, .75)
IQR_1 <- IQR(df$price)

#subset data where points value is outside 1.5*IQR of Q1 and Q3
outliers1<- subset(df, df$price <(Q1_1 - 1.5*IQR_1) | df$price >(Q3_1 + 1.5*IQR_1))
df<- subset(df, df$price >(Q1_1 - 1.5*IQR_1) & df$price <(Q3_1 + 1.5*IQR_1))

#create boxplot of Minimum nights by room_type
boxnight<-ggplot(df, aes(x="", y=minimum_nights)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Minimum Number of Nights by Type of room")
ggplotly(boxnight)
sort(boxplot.stats(df$minimun_nights)$out)
#Remove mimimun_nights > 120 
df<-df[df$minimum_nights < 120,]

#create boxplot of Number of Reviews by room_type
boxrev<-ggplot(df, aes(x=room_type, y=number_of_reviews, fill=room_type)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Number of Reviews by Type of room")
ggplotly(boxrev)
sort(boxplot.stats(df$number_of_reviews)$out)
#Remove number_of_reviews > 550 
df<-df[df$number_of_reviews < 550,]

#find Q1, Q3, and interquartile range for values in points column
Q1_2 <- quantile(df$number_of_reviews, .25)
Q3_2<- quantile(df$number_of_reviews, .75)
IQR_2 <- IQR(df$number_of_reviews)

#subset data where points value is outside 1.5*IQR of Q1 and Q3
outliers2<- subset(df, df$number_of_reviews <(Q1_2 - 1.5*IQR_2) | df$number_of_reviews >(Q3_2 + 1.5*IQR_2))
df<- subset(df, df$number_of_reviews >(Q1_2 - 1.5*IQR_2) & df$number_of_reviews <(Q3_2 + 1.5*IQR_2))

#create boxplot of Number of Reviews per month by room_type
boxmonth<-ggplot(df, aes(x=room_type, y=reviews_per_month, fill=room_type)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Number of Monthly reviews by Type of room")
ggplotly(boxmonth)
sort(boxplot.stats(df$reviews_per_month)$out)

#create boxplot of Number of Calculated host listings by room_type
boxcal<-ggplot(df, aes(x=room_type, y=calculated_host_listings_count, fill=room_type)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Calculated host Listings by Type of room")
ggplotly(boxcal)
sort(boxplot.stats(df$calculated_host_listings_count)$out)
#Remove calculated_host_listings_count >90   
df<-df[df$calculated_host_listings_count < 90,]

#find Q1, Q3, and interquartile range for values in points column
Q1_3 <- quantile(df$calculated_host_listings_count, .25)
Q3_3<- quantile(df$calculated_host_listings_count, .75)
IQR_3 <- IQR(df$calculated_host_listings_count)

#subset data where points value is outside 1.5*IQR of Q1 and Q3
outliers3<- subset(df, df$calculated_host_listings_count <(Q1_3 - 1.5*IQR_3) | df$calculated_host_listings_count >(Q3_3+ 1.5*IQR_3))
df<- subset(df, df$calculated_host_listings_count >(Q1_3 - 1.5*IQR_3) & df$calculated_host_listings_count <(Q3_3 + 1.5*IQR_3))

#create boxplot of Number of Availability during the year by room_type
boxyear<-ggplot(df, aes(x=room_type, y=availability_365, fill=room_type)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Availability by Type of room")
ggplotly(boxyear)
sort(boxplot.stats(df$availability_365)$out)

#create boxplot of Number of reviews lmt by room_type
boxltm<-ggplot(df, aes(x=room_type, y=number_of_reviews_ltm, fill=room_type)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Number of Reviews (ltm) by Type of room")
ggplotly(boxltm)
sort(boxplot.stats(df$ltm)$out)
#Remove number_of_reviews_ltm >130   
df<-df[df$number_of_reviews_ltm < 130,]

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
