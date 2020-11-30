# Load the package/library
library(ggplot2)
library(tidyverse)
library(gmodels)
library(ggmosaic)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(fpc)
library(data.table)
library(plyr)
library(dplyr)
library(rattle)
library(RWeka)
library(partykit)
library(corrplot)

# Load dataset
online_shoppers_intention <- read.csv("~/online_shoppers_intention.csv")
shopper_data <- online_shoppers_intention

# Mengecek berapa variabel yang ada
ncol(shopper_data)

#Mengecek berapa banyak row dataset
nrow(shopper_data)

# Melihat dataset pada record paling atas
head(shopper_data)

# Melihat variabel dataset
str(shopper_data)

# Melakukan cek missing value dan menghilangkannya
colSums(is.na(shopper_data))

# Menampilkan sumaries atau analisis deskriptif dari dataset
summary(shopper_data)

# Melihat korelasi keseluruhan antar variabel
data_korr <- shopper_data
str(data_korr)
data_korr$Month <- as.numeric(data_korr$Month)
data_korr$VisitorType <- as.numeric(data_korr$VisitorType)
data_korr$Revenue <- as.numeric(data_korr$Revenue)
data_korr$Weekend <- as.numeric(data_korr$Weekend)
cor(data_korr)


# Melakukan check terhadap disribusi var target yaitu Revenue
summary(shopper_data$Revenue)
CrossTable(shopper_data$Revenue)

# Membuat sebuah binary var dependent utk potensi model regresi
shopper_data <- shopper_data %>%
  mutate(Revenue_binary = ifelse(Revenue == "FALSE", 0, 1))

# Cek distribusi dari var target
hist(shopper_data$Revenue_binary, xlab = "binary Revenue", ylab = "Frekuensi")
summary(shopper_data$Revenue_binary)

## default theme for ggplot
theme_set(theme_bw())

## setting default parameters for mosaic plots
mosaic_theme = theme(axis.text.x = element_text(angle = 90,
                                                hjust = 1,
                                                vjust = 0.5),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())

## Analisis Univariate
# Administrative
summary(shopper_data$Administrative)
shopper_data %>% 
  ggplot() +
  aes(x = Administrative) +
  geom_bar() +
  facet_grid(Revenue ~ .,
             scales = "free_y")

# Administrative duration
summary(shopper_data$Administrative_Duration)
shopper_data %>% 
  ggplot() +
  aes(x = Administrative_Duration) +
  geom_histogram(bins = 50) +
  facet_grid(Revenue ~ .,
             scales = "free_y")

# Informational
summary(shopper_data$Informational)
shopper_data %>% 
  ggplot() +
  aes(x = Informational) +
  geom_bar() +
  facet_grid(Revenue ~ .,
             scales = "free_y")

# Informational Duration
summary(shopper_data$Informational_Duration)
shopper_data %>% 
  ggplot() +
  aes(x = Informational_Duration) +
  geom_histogram(bins = 50) +
  facet_grid(Revenue ~ .,
             scales = "free_y")

# Product Related
summary(shopper_data$ProductRelated)
shopper_data %>% 
  ggplot() +
  aes(x = ProductRelated) +
  geom_bar() +
  facet_grid(Revenue ~ .,
             scales = "free_y")

# Product Related Duration
summary(shopper_data$ProductRelated_Duration)
shopper_data %>% 
  ggplot() +
  aes(x = ProductRelated_Duration) +
  geom_histogram(bins = 100) +
  facet_grid(Revenue ~ .,
             scales = "free_y")

# Bounce Rates
summary(shopper_data$BounceRates)
shopper_data %>% 
  ggplot() +
  aes(x = BounceRates) +
  geom_histogram(bins = 100) +
  facet_grid(Revenue ~ .,
             scales = "free_y")

# Exit Rates
summary(shopper_data$ExitRates)
shopper_data %>% 
  ggplot() +
  aes(x = ExitRates) +
  geom_histogram(bins = 100) +
  facet_grid(Revenue ~ .,
             scales = "free_y")

# Page Value
summary(shopper_data$PageValues)
shopper_data %>% 
  ggplot() +
  aes(x = PageValues) +
  geom_histogram(bins = 50) +
  facet_grid(Revenue ~ .,
             scales = "free_y")

# Special Day
summary(shopper_data$SpecialDay)
shopper_data %>% 
  ggplot() +
  aes(x = SpecialDay) +
  geom_bar() +
  facet_grid(Revenue ~ .,
             scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

## Analisis Univariate Kategorik
# Month
CrossTable(shopper_data$Month, shopper_data$Revenue)
shopper_data %>% 
  ggplot() +
  aes(x = Month, Revenue = ..count../nrow(shopper_data), fill = Revenue) +
  geom_bar() +
  ylab("relative frequency")
month_table <- table(shopper_data$Month, shopper_data$Revenue)
month_tab <- as.data.frame(prop.table(month_table, 2))
colnames(month_tab) <-  c("Month", "Revenue", "perc")

ggplot(data = month_tab, aes(x = Month, y = perc, fill = Revenue)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) + 
  xlab("Month")+
  ylab("Percent")

# Operating System
CrossTable(shopper_data$OperatingSystems, shopper_data$Revenue)
shopper_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(Revenue, OperatingSystems), fill = Revenue)) +
  mosaic_theme +
  xlab("OS Types") +
  ylab(NULL)

# Browser Type
CrossTable(shopper_data$Browser, shopper_data$Revenue)
shopper_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(Revenue, Browser), fill = Revenue)) +
  mosaic_theme +
  xlab("Broswer Types") +
  ylab(NULL)

# Region
CrossTable(shopper_data$Region, shopper_data$Revenue)
shopper_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(Revenue, Region), fill = Revenue)) +
  mosaic_theme +
  xlab("Regions") +
  ylab(NULL)

# Traffic Type
CrossTable(shopper_data$TrafficType, shopper_data$Revenue)
shopper_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(Revenue, TrafficType), fill = Revenue)) +
  mosaic_theme +
  xlab("Traffic Type") +
  ylab(NULL)

# Visitor Type
CrossTable(shopper_data$VisitorType, shopper_data$Revenue)
shopper_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(Revenue, VisitorType), fill = Revenue)) +
  mosaic_theme +
  xlab("Visitor Type") +
  ylab(NULL)

# Weekend
CrossTable(shopper_data$Weekend, shopper_data$Revenue)
shopper_data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(Revenue, Weekend), fill = Revenue)) +
  mosaic_theme +
  xlab("Weekend") +
  ylab(NULL)

## Preparation Data Analisis
## Konversi data kategorik ke ordinal
shopper_data$OperatingSystems <- factor(shopper_data$OperatingSystems, order = TRUE, levels = c(6,3,7,1,5,2,4,8))
shopper_data$Browser <- factor(shopper_data$Browser, order = TRUE, levels = c(9,3,6,7,1,2,8,11,4,5,10,13,12))
shopper_data$Region <- factor(shopper_data$Region, order = TRUE, levels = c(8,6,3,4,7,1,5,2,9))
shopper_data$TrafficType <- factor(shopper_data$TrafficType, order = TRUE, levels = c(12,15,17,18,13,19,3,9,1,6,4,14,11,10,5,2,20,8,7,16))

shopper_data$Month <- factor(shopper_data$Month, order = TRUE, levels =c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'))
shopper_data$Month_Numeric <-mapvalues(shopper_data$Month, from = c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'), to = c(1,2,3,4,5,6,7,8,9,10))

shopper_data$VisitorType <- factor(shopper_data$VisitorType, order = TRUE, levels = c('Returning_Visitor', 'Other', 'New_Visitor'))
shopper_data$VisitorType_Numeric <-mapvalues(shopper_data$VisitorType, from = c("Returning_Visitor", "Other", "New_Visitor"), to = c(1,2,3))

# Membuat Appropriate Dummy Variables
shopper_data <- shopper_data %>%
  mutate(Weekend_binary = ifelse(Weekend == "FALSE",0,1))

# Split Data
shopper_data_class <- shopper_data[-c(19:22)]

set.seed(1984)
training <- createDataPartition(shopper_data_class$Revenue, p = 0.8, list=FALSE)

train_data <- shopper_data_class[training,]
test_data <- shopper_data_class[-training,]

head(test_data)

## Classification ##

## Decision Tree
model_dt<- rpart(Revenue ~ . , data = train_data, method="class")
rpart.plot(model_dt)
fancyRpartPlot(model_dt)

## Cross Validation
myControl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = FALSE
)

str(train_data)
train_data$Weekend <- as.factor(train_data$Weekend)
train_data$Revenue <- as.factor(train_data$Revenue)
test_data$Weekend <- as.factor(test_data$Weekend)
test_data$Revenue <- as.factor(test_data$Revenue)

## Model with cross validation
dtree_cv <- train(Revenue~., data=train_data,
                  method='rpart',
                  trControl=myControl)

confusionMatrix(predict(dtree_cv, newdata=test_data) %>% 
                  as.factor(),test_data$Revenue %>% 
                  as.factor())

recall(predict(dtree_cv, newdata=test_data) %>% 
         as.factor(),test_data$Revenue %>% 
         as.factor())

precision(predict(dtree_cv, newdata=test_data) %>% 
            as.factor(),test_data$Revenue %>% 
            as.factor())

F_meas(predict(dtree_cv, newdata=test_data) %>% 
         as.factor(),test_data$Revenue %>% 
         as.factor())

## Random Forest
library(randomForest)
rf <- randomForest(Revenue~., data = train_data)
print(rf)

# variable important
varImpPlot(rf)

## Prediksi dan validasi dengan cross validation
rf_cv <- train(Revenue~., data=train_data,
               method='rf',
               trControl=myControl)

confusionMatrix(predict(rf_cv, newdata=test_data) %>% 
                  as.factor(),test_data$Revenue %>% 
                  as.factor())

recall(predict(rf_cv, newdata=test_data) %>% 
         as.factor(),test_data$Revenue %>% 
         as.factor())

precision(predict(rf_cv, newdata=test_data) %>% 
            as.factor(),test_data$Revenue %>% 
            as.factor())

F_meas(predict(rf_cv, newdata=test_data) %>% 
         as.factor(),test_data$Revenue %>% 
         as.factor())

## Naive Bayes
library(naivebayes)
nb <- naiveBayes(Revenue~., data = train_data)
print(nb)

## Model with cross validation
nb_cv <- train(Revenue~., data=train_data,
               method='naive_bayes',
               trControl=myControl)
print(nb_cv)

# prediksi dan evaluasi dari CV
confusionMatrix(predict(nb_cv, newdata=test_data) %>% 
                  as.factor(),test_data$Revenue %>% 
                  as.factor())

recall(predict(nb_cv, newdata=test_data) %>% 
         as.factor(),test_data$Revenue %>% 
         as.factor())

precision(predict(nb_cv, newdata=test_data) %>% 
            as.factor(),test_data$Revenue %>% 
            as.factor())

F_meas(predict(nb_cv, newdata=test_data) %>% 
         as.factor(),test_data$Revenue %>% 
         as.factor())

## Support Vector Machine (SVM)
library(e1071)
svmLin <- svm(Revenue~., data = train_data, kernel = 'linear', cost = 0.1)
summary(svmLin)

# prediksi dan validasi dari Control validation
svmLin_cv <- train(Revenue~., data=train_data,
                   method='svmLinear',
                   trControl=myControl)
confusionMatrix(predict(svmLin_cv, newdata=test_data) %>% 
                  as.factor(),test_data$Revenue %>% 
                  as.factor())

confusionMatrix(predict(svmLin_cv, newdata=test_data) %>% 
                  as.factor(),test_data$Revenue %>% 
                  as.factor())

recall(predict(svmLin_cv, newdata=test_data) %>% 
         as.factor(),test_data$Revenue %>% 
         as.factor())

precision(predict(svmLin_cv, newdata=test_data) %>% 
            as.factor(),test_data$Revenue %>% 
            as.factor())

F_meas(predict(svmLin_cv, newdata=test_data) %>% 
         as.factor(),test_data$Revenue %>% 
         as.factor())
