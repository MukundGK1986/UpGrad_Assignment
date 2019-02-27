
library(tidyverse)
library(caret)
library(DMwR)
library(vegan)
library(ROCR)
rm(list = ls())

bank_data = read.csv("bank-additional-full.csv", sep=";", na.strings = 999)
str(bank_data)
summary(bank_data)
head(bank_data,3)

## convert the Age into categorical. Divide the age in to 3 categories. 
## 17-30 -> YOUNG
## 31-59 -> MIDDLE
## >=60  -> SENIOR

for(i in 1:length(bank_data$age)){
  ifelse(bank_data$age[i] <= 30, bank_data$age[i] <- 'Youth',
         ifelse((bank_data$age[i] > 30) && (bank_data$age[i] < 60), bank_data$age[i] <- 'Middle',
                bank_data$age[i] <- 'Senior'))
}

table(bank_data$age)

str(bank_data)
bank_data$age <- as.factor(bank_data$age)

## Made a Copy of the Bank Data. This will helo for visualization. 
bank_data_copy <- bank_data

bank_data <- bank_data[-c(8:11)]

## Outlier detection for Numeric Variables
outliers <- boxplot(bank_data$emp.var.rate,plot = TRUE)$out
## No Outliers

outliers <- boxplot(bank_data$cons.price.idx,plot = TRUE)$out
## No Outliers

outliers <- boxplot(bank_data$cons.conf.idx,plot = TRUE)$out
## Outliers are present. 
length(outliers) ## 447 outliers are present. 
summary(bank_data$cons.conf.idx)
## Outliers detected but decided not to remove them. 
max(outliers)

outliers <- boxplot(bank_data$euribor3m,plot = TRUE)$out
## No Outliers

outliers <- boxplot(bank_data$nr.employed,plot = TRUE)$out
## No Outliers

sum(is.na(bank_data))
colnames(bank_data)[colSums(is.na(bank_data)) > 0]
## We have found the NA's in the column 'pdays' which are not exactly the NAs. Instead they are 999
## which mean that the client was not previously contacted. If we see the ratio of NAs against the 
## total number of records, then it comes to around 96%. This means that in the column we only have
## 4% of actual information. Below is the calculation for the same. 

(100 * 39673)/41188

bank_data <- bank_data[-c(9)]

write.csv(bank_data,"Upgrad_data.csv",row.names = FALSE)

## Splitting the data in the ratio of 70:30 

set.seed(8686816)

train_rows <- createDataPartition(bank_data$y, p = 0.7,list=F)
train_data <- bank_data[train_rows, ]
val_data <- bank_data[-train_rows, ]



## Standardization of Numeric Data

Mean_train <- apply(train_data[,c(8,9,11:15)], MARGIN = 2, mean)
SD_train <- apply(train_data[,c(8,9,11:15)], MARGIN = 2, sd)

train_data_std <- data.frame(decostand(train_data[,c(8,9,11:15)],"standardize"),
                             train_data[,c(1:7,10,16)])


val_data_std <- data.frame(decostand(val_data[,c(8,9,11:15)],mean = Mean_train,
                                      sd = SD_train, "standardize"),val_data[,c(1:7,10,16)])




## Writing the Train and Validation data frames to CSV files to use in Python. 
write.csv(train_data_std,"Upgrad_Train_Std.csv",row.names = FALSE)
write.csv(val_data_std,"Upgrad_Val_Std.csv",row.names = FALSE)



## VISUALIZATIONS

table(bank_data_copy$age)

bank_labels <- c("No","Yes")

bank_data_copy %>%
  select(age,y) %>%
  group_by(age,y) %>%
  dplyr :: summarise(n = n()) %>%
  
  ggplot() +
  geom_col(aes(x=reorder(age, -n),y=n,fill = age)) +
  xlab("Age") + ylab("Count") + 
  facet_grid(~y,scales = "free") +
  guides(fill = guide_legend(title = "Age Categories"))  +
  ggtitle("Age vs Response")  +
  theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x.bottom = element_text(face = "bold"), 
        axis.title.y.left = element_text(face = "bold"), 
        axis.text.x.bottom = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  
  geom_text(aes(x = age, y = n, label = n, vjust = -0.5))



bank_data_copy %>%
  select(marital,y) %>%
  filter(marital != "unknown") %>%
  group_by(marital,y) %>%
  dplyr :: summarise(n = n()) %>%
  
  ggplot() +
  geom_col(aes(x=reorder(marital, -n),y=n,fill = marital)) +
  xlab("Marital Status") + ylab("Count") + 
  facet_grid(~y,scales = "free") +
  guides(fill = guide_legend(title = "Marital Status"))  +
  ggtitle("Marital Status vs Response")  +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x.bottom = element_text(face = "bold"), 
        axis.title.y.left = element_text(face = "bold"), 
        axis.text.x.bottom = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  
  geom_text(aes(x = marital, y = n, label = n, vjust = -0.5))




bank_data_copy %>%
  select(education,y) %>%
  filter(education != "unknown") %>% 
  filter(education != "illiterate") %>%
  group_by(education,y) %>%
  dplyr :: summarise(n = n()) %>%
  
  ggplot() +
  geom_col(aes(x=reorder(education, -n),y=n,fill = education)) +
  #coord_flip() + 
  xlab("Education") + ylab("Count") + 
  facet_grid(~y,scales = "free") +
  guides(fill = guide_legend(title = "Education Levels"))  +
  ggtitle("Education Level vs Response")  +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x.bottom = element_text(face = "bold"), 
        axis.title.y.left = element_text(face = "bold"), 
        axis.text.x.bottom = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  
  geom_text(aes(x = education, y = n, label = n, vjust = -0.5))







