# TikTok User Engagement Data: https://www.kaggle.com/datasets/yakhyojon/tiktok 
# Dataset: 'tiktok_dataset.csv'

#=================================
# LIBRARIES
#=================================
install.packages("rio")
install.packages("plotly")
install.packages("moments")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
#=================================
# LOAD DATASET
#=================================
# Read in our csv and put it in a data.frame.
df<- read.csv('tiktok_dataset.csv',  header=T, na.strings=c(""), stringsAsFactors = T)

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

#summarize dataset
summary(df)
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
#graphing categorical 
sta<-ggplot(data = df) + geom_bar(mapping = aes(y = claim_status, fill = verified_status)) + labs(title = "Bar of Claim Status", caption = " Figure X", y = "Number of Claim Status")
ggplotly(sta)
verified<-ggplot(data = df) + geom_bar(mapping = aes(y = verified_status, fill = claim_status)) + labs(title = "Bar of Verified Status", caption = " Figure X", y = "Number of Verified Status")
ggplotly(verified)
author<-ggplot(data = df) + geom_bar(mapping = aes(y = author_ban_status, fill = verified_status)) + labs(title = "Bar of Author Ban Status", caption = " Figure X", y = "Number of Author ban")
ggplotly(author)

#---------------------------------
# VISUALIZATIONS OF NUMERIC VARIABLES
#---------------------------------
sec<-ggplot(df, aes(x=video_duration_sec, fill= claim_status)) + geom_histogram( position="identity") + scale_color_brewer() +
  labs(title = "Distribution of video duration by Seconds ", caption = " Figure X", x= "Seconds")
ggplotly(sec)
view<-ggplot(df, aes(x=video_view_count, fill= claim_status)) + geom_histogram( position="identity") + scale_color_brewer() +
  labs(title = "Distribution of Video's Views  ", caption = " Figure X", x= "Number of views")
ggplotly(view)
like<-ggplot(df, aes(x=video_like_count, fill= claim_status)) + geom_histogram( position="identity") + scale_color_brewer() +
  labs(title = "Distribution of Video's like  ", caption = " Figure X", x= "Number of likes")
ggplotly(like)
share<-ggplot(df, aes(x=video_share_count, fill= claim_status)) + geom_histogram( position="identity") + scale_color_brewer() +
  labs(title = "Distribution of Video's share  ", caption = " Figure X", x= "Number of share")
ggplotly(share)
download<-ggplot(df, aes(x=video_download_count, fill= claim_status)) + geom_histogram( position="identity") + scale_color_brewer() +
  labs(title = "Distribution of Video's download  ", caption = " Figure X", x= "Number of downloads")
ggplotly(download)
comment<-ggplot(df, aes(x=video_comment_count, fill= claim_status)) + geom_histogram( position="identity") + scale_color_brewer() +
  labs(title = "Distribution of Video's comment  ", caption = " Figure X", x= "Number of comments")
ggplotly(comment)

#---------------------------------
# Handling Outliers
#---------------------------------
#Boxplot video_duration_sec
duration1<-ggplot(df, aes(x=claim_status, y=video_duration_sec, fill=claim_status)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of Video duration by seconds")
ggplotly(duration1)
boxplot.stats(df$video_duration_sec)$out

#Boxplot video_view_count
view2<-ggplot(df, aes(x=claim_status, y=video_view_count, fill=claim_status)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of views")
ggplotly(view2)
boxplot.stats(df$video_view_count)$out

#Boxplot video_like_count
like2<-ggplot(df, aes(x=claim_status, y=video_like_count, fill=claim_status)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of like")
ggplotly(like2)
boxplot.stats(df$video_like_count)$out

#Boxplot video_share_count
share2<-ggplot(df, aes(x=claim_status, y=video_share_count, fill=claim_status)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of shares")
ggplotly(share2)
boxplot.stats(df$video_share_count)$out

#Boxplot video_share_count
download2<-ggplot(df, aes(x=claim_status, y=video_download_count, fill=claim_status)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of download")
ggplotly(download2)
boxplot.stats(df$video_download_count)$out

#Boxplot video_share_count
comment2<-ggplot(df, aes(x=claim_status, y=video_comment_count, fill=claim_status)) + geom_boxplot() +
  guides(fill=FALSE) + coord_flip() + ggtitle("Boxplot of comment")
ggplotly(comment2)
boxplot.stats(df$video_comment_count)$out

#---------------------------------
# CORRELATION MATRIX
#---------------------------------
# correlation between only numerical variables
corr<-cor(df[,unlist(lapply(df, is.numeric))])
corr
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

#---------------------------------
# VERIFY SKEWNESS AND KURTOSIS
#---------------------------------
#Calculate Means of each column
df_num<-select_if(df, is.numeric)
ggpairs(df_num)
sapply(df_num, mean)
sapply(df_num, median)
sapply(df_num, sd)
moments::kurtosis(df_num)
moments::skewness(df_num)

#---------------------------------
# PERFORM CATEGORICAL DATA BY DUMMY
#---------------------------------
dummy<-dummyVars(~ claim_status + verified_status + author_ban_status, data = df, fullRank = T)
table <- data.frame(predict(dummy, newdata = df))
df1<- merge(df, table, by = 0)
#set "Row.Names" column as row names
df1 <- df1 %>% column_to_rownames(., var = 'Row.names')
head(df1)

#=================================
# LOGISTIC REGRESSION 
#=================================

#---------------------------------
# SPLITTING DATA INTO TRAINING AND TEST SETS
#---------------------------------
set.seed(123)
#Use 70% of dataset as training set and remaining 30% as testing set
splitting <- sample(c(TRUE, FALSE), nrow(df1), replace=TRUE, prob=c(0.7,0.3))
train  <- df1[splitting, ]
test   <- df1[!splitting, ]
head(train)

#---------------------------------
# MODEL 1
#---------------------------------
model <- glm( claim_status.opinion ~ video_duration_sec + video_view_count + video_like_count + video_share_count +
               video_download_count + video_comment_count + verified_status.verified + author_ban_status.banned +
               author_ban_status.under.review, family=binomial(),data= train)
summary(model)

anova(model, test = "Chisq")

caret::varImp(model)

car::vif(model)

coef(model)

#odds ratio and 95% CI
exp(cbind(OR = coef(model), confint(model)))

pR2(model)

#---------------------------------
# MODEL 2
#---------------------------------
model2 <- glm( claim_status.opinion ~ video_view_count + video_like_count + video_download_count + 
                video_comment_count + verified_status.verified + author_ban_status.banned +
                author_ban_status.under.review, family=binomial(),data= train)
summary(model2)

anova(model2, test = "Chisq")

caret::varImp(model2)

car::vif(model2)

coef(model2)

#odds ratio and 95% CI
exp(cbind(OR = coef(model2), confint(model2)))

pR2(model2)



#---------------------------------
# PREDICTIONS
#---------------------------------
predic <- predict(model2, test, type="response")
predic <- ifelse(predic> 0.5, 1, 0)

#---------------------------------
# EVALUATION
#---------------------------------
ClassError <- mean(predic != test$claim_status.opinion )
print(paste('Accuracy', 1-ClassError))

# ROC-AUC Curve
ROCPred <- prediction(predic, test$claim_status.opinion)
ROCPer <- performance(ROCPred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc
# Plotting curve
roc = performance(ROCPred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1) 
legend(.6, .4, auc, title = "AUC", cex = 1)

confusionMatrix(table(predic, test$claim_status.opinion))

#=================================
# NAIVE BAYES MACHINE LEARNING
#=================================

#---------------------------------
# LOADING DATA
#---------------------------------
tik_tex<- df %>% select(claim_status,video_transcription_text)
str(tik_tex)
str(tik_tex$claim_status)
table(tik_tex$claim_status)

#---------------------------------
# TEXT MINING
#---------------------------------

tik_corpus<- VCorpus(VectorSource(tik_tex$video_transcription_text))
print(tik_corpus)

#check out the first few messages in the new corpus
inspect(tik_corpus[1:3])
as.character(tik_corpus[[3]])

#clean to all lower case letter
tik_corpus_clean <- tm_map(tik_corpus, content_transformer(tolower))
#see if the data was transformed
as.character(tik_corpus[[3]])
as.character((tik_corpus_clean[[3]]))

#remove the numbers
tik_corpus_clean <- tm_map(tik_corpus_clean, removeNumbers)
as.character((tik_corpus_clean[[3]]))

#remove to/ and/ but/ or
tik_corpus_clean <- tm_map(tik_corpus_clean, removeWords, stopwords())

#remove punctuation
tik_corpus_clean <- tm_map(tik_corpus_clean, removePunctuation)
as.character((tik_corpus_clean[[3]]))

tik_corpus_clean <- tm_map(tik_corpus_clean, stemDocument)

#remove the white space
tik_corpus_clean <- tm_map(tik_corpus_clean, stripWhitespace)
as.character((tik_corpus_clean[[3]]))

#Perform tokenization
tik_dtm <- DocumentTermMatrix(tik_corpus_clean)

#---------------------------------
# DATA PREPARATION
#---------------------------------
#Split the dataset in training and testing

tik_dtm_train <- tik_dtm[1:14313, ]
tik_dtm_test <- tik_dtm[14314:19084, ]

#save vectors labeling rows in the training and testing vectors
tik_train_labels <- tik_tex[1:14313, ]$claim_status
tik_test_labels <- tik_tex[14314:19084,]$claim_status

prop.table(table(tik_train_labels))
prop.table(table(tik_test_labels))

#---------------------------------
# VISUALIZATION
#---------------------------------
#create a wordcloud of frequency of the words
wordcloud(tik_corpus_clean, max.words = 100, random.order = FALSE)
#compare wordclouds between claim and opinion
claim<- subset(tik_tex, claim_status =="claim")
opinion<- subset(tik_tex, claim_status=="opinion")
wordcloud(claim$video_transcription_text, max.words = 100)
wordcloud(opinion$video_transcription_text, max.words = 100)

#---------------------------------
# PREPARATION FOR NAIVE BAYES
#---------------------------------
#remove words from the matix that appear less than 5 times
tik_freq_words <- findFreqTerms(tik_dtm_train,5)
str(tik_freq_words)
#limit the document term matrix to only include words in the tik_freq_vector
tik_dtm_freq_train <- tik_dtm_train[ , tik_freq_words]
tik_dtm_freq_test <- tik_dtm_test[ , tik_freq_words]
#convert the matrix to "yes" and "no" categorical variables
counts <- function(x){
  x <- ifelse(x>0, "Yes", "No")
}
#replace values grater than 0 with yes and values not greater than 0 with no
tik_train <-apply(tik_dtm_freq_train, MARGIN = 2, counts)
tik_test <-apply(tik_dtm_freq_test, MARGIN = 2, counts)

#---------------------------------
# TRAIN MODEL ON THE DATA
#---------------------------------
tik_classifier <- naiveBayes(tik_train, tik_train_labels)

#---------------------------------
# PREDICT AND EVALUATE THE MODEL
#---------------------------------
tik_test_pred <- predict(tik_classifier, tik_test)

CrossTable(tik_test_pred, tik_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('PREDICTED', 'ACTUAL'))

#the 10 most frequent words from each claim status
wordcloud(claim$video_transcription_text, max.words = 10, scale = c(3, 0.5))
wordcloud(opinion$video_transcription_text, max.words = 10, scale = c(3, 0.5))
