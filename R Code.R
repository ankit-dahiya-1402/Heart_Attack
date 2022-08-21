#Remove all variables stored previously
rm(list = ls())

# First we will load the dataset.
data <- read.csv("C:/Users/91880/OneDrive/Desktop/R/Heart Disease/heart.csv")

# Let's see the dataset to understand it.
View(data)

# Install and Load all the required libraries
library(tidyverse)
library(ggplot2)
library(corrplot)



heart <- data %>% 
         mutate(sex     = if_else(sex==1,"MALE","FEMALE"),
                fbs     = if_else(fbs==1,">120","<-120"),
                exang   = if_else(exang==1,"YES","NO"),
                cp      = if_else(cp==1,"ATYPICAL ANGINA",
                                 if_else(cp==2,"NON-ANGINAL PAIN","ASYMPTOMATIC")),
                restecg = if_else(restecg==0,"NORMAL",
                                      if_else(restecg==1,"ABNORMALITY","PROBABLE OR DEFINITE")),
                slope   = as.factor(slope),
                ca      = as.factor(ca),
                thal    = as.factor(thal),
                target   = if_else(target==1,"YES","NO") ) %>% 
      mutate_if(is.character,as.factor)%>%
      dplyr :: select(target,sex,fbs,exang,cp,restecg,slope,ca,thal,everything())

# age column is written as ï..age rename it to age

colnames(heart)[10]<-"age" # here 10 is ï..age column index

# Let's see the cleaned data.
View(heart)

glimpse(heart)


# To find the proportion of having heart disease
prop.table(table(heart$target))

# Data visualization

ggplot(heart,aes(target,fill=target))+
  geom_bar()+
  xlab("Heart Disease")+
  ylab("Number of Patient") +
  scale_fill_discrete(name="Heart Disease",labels= c("Absence","Presence")) +
  labs(title = "Number of patient having/not having Heart Disease",subtitle = "Figure 1")

ggplot(heart,aes(target,fill=sex))+
  geom_bar()+
  xlab("Heart Disease")+
  ylab("Number og Patient")+
  scale_fill_discrete(name="Sex")+
  labs(title = "Showing trend of disease amoung different Gender",subtitle = "Figure 2")

ggplot(heart,aes(target,fill=restecg))+
  geom_bar()+
  xlab("Heart Disease")+
  ylab("Number og Patient")+
  scale_fill_discrete(name="Resting Electrocardiographic")+
  labs(title = "Counting different types of Resting Electrocardiographic",subtitle = "Figure 3")


# Count the frequency of values of age
heart %>%
  group_by(age)%>%
  count()%>%
  filter(n>10)%>%
  ggplot()+
  geom_col(aes(age,n),fill='blue')+
  xlab("Age")+
  ylab("Number of Patient")+
  labs(title = "Age Analysis",subtitle = "Figure 4")

ggplot(heart) + geom_bar(aes(age))

# Compare blood pressure across the chest pain

ggplot(heart,aes(x=sex,y= trestbps)) +
  geom_boxplot(fill="purple") +
  xlab('Sex')+
  ylab('BP')+
  facet_grid(~cp)+
  labs(title = "Comparision of blood  pressure arcoss the chest pain",subtitle = "Figure 5")

#Co-relation

corr_heart <- cor(heart[,10:14])
corr_heart

corrplot(corr_heart,method = "circle",type = 'full')


















