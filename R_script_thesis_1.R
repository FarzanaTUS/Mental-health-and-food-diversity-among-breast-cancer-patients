rm(list=ls())
getwd()
library(haven)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggpubr)
install.packages("descriptr")
library(descriptr)
library(grid)

BC_M_N<- read_excel("E:/Manuscripts/Thesis_BC/Analyzed_data.xlsx")

BC_M_N$Anxiety<- as.factor(BC_M_N$Anxiety)
BC_M_N$Anxiety_level<- as.factor(BC_M_N$Anxiety_level)
BC_M_N$Depression<- as.factor(BC_M_N$Depression)
BC_M_N$Depression_level<- as.factor(BC_M_N$Depression_level)
BC_M_N$food_diversity<- as.factor(BC_M_N$food_diversity)



####categorization
class(BC_M_N$Age)
BC_M_N$Age_cat<- ifelse(BC_M_N$Age>=20 &BC_M_N$Age<=39, "20-39",
                    ifelse(BC_M_N$Age>=40 & BC_M_N$Age<=59, "40-59",
                    ifelse(BC_M_N$Age>=60 & BC_M_N$Age<=79, "60-79",
                    ifelse(BC_M_N$Age>=80,"80 or above", NA))))

class(BC_M_N$"Monthly family income")
BC_M_N$MI_cat<- ifelse(BC_M_N$"Monthly family income"<=50000, "Upto 50000",
                        ifelse(BC_M_N$"Monthly family income">50000 & BC_M_N$"Monthly family income"<=100000, "50000-100000",
                               ifelse(BC_M_N$"Monthly family income">100000, "Above 1000", NA)))


#### Character to factor
BC_M_N$Religion<-as.factor(BC_M_N$Religion)
BC_M_N$Distric<-as.factor(BC_M_N$Distric)
BC_M_N$"Education level"<-as.factor(BC_M_N$"Education level")
BC_M_N$"Marital status"<-as.factor(BC_M_N$"Marital status")
BC_M_N$"NChildren_cat"<-as.factor(BC_M_N$"NChildren_cat")
BC_M_N$"Employment status"<-as.factor(BC_M_N$"Employment status")
BC_M_N$"Family structure"<-as.factor(BC_M_N$"Family structure")
BC_M_N$"Menstrual status"<-as.factor(BC_M_N$"Menstrual status")


####Proportion of anxiety, depression and dietary diversity

class(BC_M_N$Anxiety)
levels(BC_M_N$Anxiety)
table(BC_M_N$Anxiety, exclude = NULL)
addmargins(table(BC_M_N$Anxiety))
round(prop.table(table(BC_M_N$Anxiety))*100)

class(BC_M_N$Anxiety_level)
levels(BC_M_N$Anxiety_level)
table(BC_M_N$Anxiety_level, exclude = NULL)
addmargins(table(BC_M_N$Anxiety_level))
round(prop.table(table(BC_M_N$Anxiety_level))*100)

class(BC_M_N$Depression)
levels(BC_M_N$Depression)
table(BC_M_N$Depression, exclude = NULL)
addmargins(table(BC_M_N$Depression))
round(prop.table(table(BC_M_N$Depression))*100)

class(BC_M_N$Depression_level)
levels(BC_M_N$Depression_level)
table(BC_M_N$Depression_level, exclude = NULL)
addmargins(table(BC_M_N$Depression_level))
round(prop.table(table(BC_M_N$Depression_level))*100)

class(BC_M_N$food_diversity)
levels(BC_M_N$food_diversity)
table(BC_M_N$food_diversity, exclude = NULL)
addmargins(table(BC_M_N$food_diversity))
round(prop.table(table(BC_M_N$food_diversity))*100)


############Socio-demographic characteristics#################

#####Age

addmargins(table(BC_M_N$Age_cat))
prop.table(table(BC_M_N$Age_cat))*100

table(BC_M_N$Age_cat,BC_M_N$Anxiety)
prop.table(table(BC_M_N$Age_cat,BC_M_N$Anxiety),2)*100

BC_M_N %>% 
  select(Anxiety, Age_cat) %>% 
  table() %>% 
  chisq.test()

table(BC_M_N$Age_cat,BC_M_N$Depression)
prop.table(table(BC_M_N$Age_cat,BC_M_N$Depression),2)*100

BC_M_N %>% 
  select(Depression, Age_cat) %>% 
  table() %>% 
  chisq.test()


table(BC_M_N$Age_cat,BC_M_N$food_diversity)
prop.table(table(BC_M_N$Age_cat,BC_M_N$food_diversity),2)*100

BC_M_N %>% 
  select(food_diversity, Age_cat) %>% 
  table() %>% 
  chisq.test()


#####Religion

addmargins(table(BC_M_N$Religion))
prop.table(table(BC_M_N$Religion))*100

table(BC_M_N$Religion,BC_M_N$Anxiety)
prop.table(table(BC_M_N$Religion,BC_M_N$Anxiety),2)*100

BC_M_N %>% 
  select(Anxiety, Religion) %>% 
  table() %>% 
  chisq.test()

table(BC_M_N$Religion,BC_M_N$Depression)
prop.table(table(BC_M_N$Religion,BC_M_N$Depression),2)*100

BC_M_N %>% 
  select(Depression, Religion) %>% 
  table() %>% 
  chisq.test()


table(BC_M_N$Religion,BC_M_N$food_diversity)
prop.table(table(BC_M_N$Religion,BC_M_N$food_diversity),2)*100

BC_M_N %>% 
  select(food_diversity, Religion) %>% 
  table() %>% 
  chisq.test()

###District

addmargins(table(BC_M_N$Distric))
prop.table(table(BC_M_N$Distric))*100

table(BC_M_N$Distric, BC_M_N$Anxiety)
prop.table(table(BC_M_N$Distric, BC_M_N$Anxiety),2)*100

BC_M_N %>% 
  select(Anxiety, Distric) %>% 
  table() %>% 
  chisq.test()

table(BC_M_N$Distric, BC_M_N$Depression)
prop.table(table(BC_M_N$Distric, BC_M_N$Depression),2)*100

BC_M_N %>% 
  select(Depression, Distric) %>% 
  table() %>% 
  chisq.test()


table(BC_M_N$Distric, BC_M_N$food_diversity)
prop.table(table(BC_M_N$Distric, BC_M_N$food_diversity),2)*100

BC_M_N %>% 
  select(food_diversity, Distric) %>% 
  table() %>% 
  chisq.test()


####Education level 

addmargins(table(BC_M_N$"Education level"))
prop.table(table(BC_M_N$"Education level"))*100

table(BC_M_N$"Education level", BC_M_N$Anxiety)
prop.table(table(BC_M_N$"Education level", BC_M_N$Anxiety),2)*100

BC_M_N %>% 
  select(Anxiety, "Education level") %>% 
  table() %>% 
  chisq.test()

table(BC_M_N$"Education level", BC_M_N$Depression)
prop.table(table(BC_M_N$"Education level", BC_M_N$Depression),2)*100

BC_M_N %>% 
  select(Depression, "Education level") %>% 
  table() %>% 
  chisq.test()


table(BC_M_N$"Education level", BC_M_N$food_diversity)
prop.table(table(BC_M_N$"Education level", BC_M_N$food_diversity),2)*100

BC_M_N %>% 
  select(food_diversity, "Education level") %>% 
  table() %>% 
  chisq.test()

######Marital status 

addmargins(table(BC_M_N$"Marital status"))
prop.table(table(BC_M_N$"Marital status"))*100

table(BC_M_N$"Marital status", BC_M_N$Anxiety)
prop.table(table(BC_M_N$"Marital status", BC_M_N$Anxiety),2)*100

BC_M_N %>% 
  select(Anxiety, "Marital status") %>% 
  table() %>% 
  chisq.test()

table(BC_M_N$"Marital status", BC_M_N$Depression)
prop.table(table(BC_M_N$"Marital status", BC_M_N$Depression),2)*100

BC_M_N %>% 
  select(Depression, "Marital status") %>% 
  table() %>% 
  chisq.test()


table(BC_M_N$"Marital status", BC_M_N$food_diversity)
prop.table(table(BC_M_N$"Marital status", BC_M_N$food_diversity),2)*100

BC_M_N %>% 
  select(food_diversity, "Marital status") %>% 
  table() %>% 
  chisq.test()

#####Employment status 

addmargins(table(BC_M_N$"Employment status"))
prop.table(table(BC_M_N$"Employment status"))*100

table(BC_M_N$"Employment status", BC_M_N$Anxiety)
prop.table(table(BC_M_N$"Employment status", BC_M_N$Anxiety),2)*100

BC_M_N %>% 
  select(Anxiety, "Employment status") %>% 
  table() %>% 
  chisq.test()

table(BC_M_N$"Employment status", BC_M_N$Depression)
prop.table(table(BC_M_N$"Employment status", BC_M_N$Depression),2)*100

BC_M_N %>% 
  select(Depression, "Employment status") %>% 
  table() %>% 
  chisq.test()


table(BC_M_N$"Employment status", BC_M_N$food_diversity)
prop.table(table(BC_M_N$"Employment status", BC_M_N$food_diversity),2)*100

BC_M_N %>% 
  select(food_diversity, "Employment status") %>% 
  table() %>% 
  chisq.test()

####Family structure 

addmargins(table(BC_M_N$"Family structure"))
prop.table(table(BC_M_N$"Family structure"))*100

table(BC_M_N$"Family structure", BC_M_N$Anxiety)
prop.table(table(BC_M_N$"Family structure", BC_M_N$Anxiety),2)*100

BC_M_N %>% 
  select(Anxiety, "Family structure") %>% 
  table() %>% 
  chisq.test()


###Menstrual status 

addmargins(table(BC_M_N$"Menstrual status"))
prop.table(table(BC_M_N$"Menstrual status"))*100

table(BC_M_N$"Menstrual status", BC_M_N$Anxiety)
prop.table(table(BC_M_N$"Menstrual status", BC_M_N$Anxiety),2)*100

BC_M_N %>% 
  select(Anxiety, "Menstrual status") %>% 
  table() %>% 
  chisq.test()

table(BC_M_N$"Menstrual status", BC_M_N$Depression)
prop.table(table(BC_M_N$"Menstrual status", BC_M_N$Depression),2)*100

BC_M_N %>% 
  select(Depression, "Menstrual status") %>% 
  table() %>% 
  chisq.test()


table(BC_M_N$"Menstrual status", BC_M_N$food_diversity)
prop.table(table(BC_M_N$"Menstrual status", BC_M_N$food_diversity),2)*100

BC_M_N %>% 
  select(food_diversity, "Menstrual status") %>% 
  table() %>% 
  chisq.test()

####Monthly family income
addmargins(table(BC_M_N$MI_cat))
prop.table(table(BC_M_N$MI_cat))*100

table(BC_M_N$MI_cat,BC_M_N$Anxiety)
prop.table(table(BC_M_N$MI_cat,BC_M_N$Anxiety),2)*100

BC_M_N %>% 
  select(Anxiety, MI_cat) %>% 
  table() %>% 
  chisq.test()

table(BC_M_N$MI_cat,BC_M_N$Depression)
prop.table(table(BC_M_N$MI_cat,BC_M_N$Depression),2)*100

BC_M_N %>% 
  select(Depression, MI_cat) %>% 
  table() %>% 
  chisq.test()


table(BC_M_N$MI_cat,BC_M_N$food_diversity)
prop.table(table(BC_M_N$MI_cat,BC_M_N$food_diversity),2)*100

BC_M_N %>% 
  select(food_diversity, MI_cat) %>% 
  table() %>% 
  chisq.test()




#########Logistics

####Anxiety_Model

Log_Anxiety_Age<-glm(BC_M_N$Anxiety_Log ~ BC_M_N$Age_cat,
                 family=binomial(link=logit))
summary(Log_Anxiety_Age)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_Age), confint(Log_Anxiety_Age)))


Log_Anxiety_Religion<-glm(BC_M_N$Anxiety_Log ~ BC_M_N$Religion,
                     family=binomial(link=logit))
summary(Log_Anxiety_Religion)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_Religion), confint(Log_Anxiety_Religion)))


Log_Anxiety_Distric<-glm(BC_M_N$Anxiety_Log ~ BC_M_N$Distric,
                          family=binomial(link=logit))
summary(Log_Anxiety_Distric)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_Distric), confint(Log_Anxiety_Distric)))


Log_Anxiety_EducationLevel<-glm(BC_M_N$Anxiety_Log ~ BC_M_N$"Education level",
                         family=binomial(link=logit))
summary(Log_Anxiety_EducationLevel)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_EducationLevel), confint(Log_Anxiety_EducationLevel)))

BC_M_N$"Marital status"<- relevel(BC_M_N$"Marital status", ref = "Married")
Log_Anxiety_MaritalStatus <-glm(BC_M_N$Anxiety_Log ~ BC_M_N$"Marital status",
                                family=binomial(link=logit))
summary(Log_Anxiety_MaritalStatus)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_MaritalStatus), confint(Log_Anxiety_MaritalStatus)))

BC_M_N$"Employment status"<- relevel(BC_M_N$"Employment status", ref = "Employed")
Log_Anxiety_EmploymentStatus  <-glm(BC_M_N$Anxiety_Log ~ BC_M_N$"Employment status",
                                family=binomial(link=logit))
summary(Log_Anxiety_EmploymentStatus)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_EmploymentStatus), confint(Log_Anxiety_EmploymentStatus)))

Log_Anxiety_FamilyStructure  <-glm(BC_M_N$Anxiety_Log ~ BC_M_N$"Family structure",
                                    family=binomial(link=logit))
summary(Log_Anxiety_FamilyStructure)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_FamilyStructure), confint(Log_Anxiety_FamilyStructure)))


Log_Anxiety_Menstrualtatus  <-glm(BC_M_N$Anxiety_Log ~ BC_M_N$"Menstrual status",
                                   family=binomial(link=logit))
summary(Log_Anxiety_Menstrualtatus)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_Menstrualtatus), confint(Log_Anxiety_Menstrualtatus)))


class(BC_M_N$MI_cat)
BC_M_N$MI_cat<- as.factor(BC_M_N$MI_cat)
levels(BC_M_N$MI_cat)
BC_M_N$MI_cat<- relevel(BC_M_N$MI_cat, ref = "Upto 50000")
Log_Anxiety_MI <-glm(BC_M_N$Anxiety_Log ~ BC_M_N$MI_cat,
                                  family=binomial(link=logit))
summary(Log_Anxiety_MI)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_MI), confint(Log_Anxiety_MI)))


Log_Anxiety_CoSurgery  <-glm(BC_M_N$Anxiety_Log ~ BC_M_N$i."Cost of surgery",
                                  family=binomial(link=logit))
summary(Log_Anxiety_CoSurgery)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_CoSurgery), confint(Log_Anxiety_CoSurgery)))


Log_Anxiety_Year_cancer  <-glm(BC_M_N$Anxiety_Log ~ BC_M_N$"Year first diagnosed with cancer",
                             family=binomial(link=logit))
summary(Log_Anxiety_Year_cancer)
exp(cbind(Odds_RAtio=coef(Log_Anxiety_Year_cancer), confint(Log_Year_cancer)))


Log_Anxiety<-glm(BC_M_N$Anxiety ~ BC_M_N$Age_cat
             +BC_M_N$Distric,
             family=binomial(link=logit))
summary(Log_Anxiety)
exp(cbind(Odds_RAtio=coef(Log_Anxiety), confint(Log_Anxiety)))


#####For Depression

Log_Depression_Age<-glm(BC_M_N$Depression_Log ~ BC_M_N$Age_cat,
                     family=binomial(link=logit))
summary(Log_Depression_Age)
exp(cbind(Odds_RAtio=coef(Log_Depression_Age), confint(Log_Depression_Age)))


Log_Depression_Religion<-glm(BC_M_N$Depression_Log ~ BC_M_N$Religion,
                          family=binomial(link=logit))
summary(Log_Depression_Religion)
exp(cbind(Odds_RAtio=coef(Log_Depression_Religion), confint(Log_Depression_Religion)))


Log_Depression_Distric<-glm(BC_M_N$Depression_Log ~ BC_M_N$Distric,
                         family=binomial(link=logit))
summary(Log_Depression_Distric)
exp(cbind(Odds_RAtio=coef(Log_Depression_Distric), confint(Log_Depression_Distric)))


Log_Depression_EducationLevel<-glm(BC_M_N$Depression_Log ~ BC_M_N$"Education level",
                                family=binomial(link=logit))
summary(Log_Depression_EducationLevel)
exp(cbind(Odds_RAtio=coef(Log_Depression_EducationLevel), confint(Log_Depression_EducationLevel)))

BC_M_N$"Marital status"<- relevel(BC_M_N$"Marital status", ref = "Married")
Log_Depression_MaritalStatus <-glm(BC_M_N$Depression_Log ~ BC_M_N$"Marital status",
                                family=binomial(link=logit))
summary(Log_Depression_MaritalStatus)
exp(cbind(Odds_RAtio=coef(Log_Depression_MaritalStatus), confint(Log_Depression_MaritalStatus)))

BC_M_N$"Employment status"<- relevel(BC_M_N$"Employment status", ref = "Employed")
Log_Depression_EmploymentStatus  <-glm(BC_M_N$Depression_Log ~ BC_M_N$"Employment status",
                                    family=binomial(link=logit))
summary(Log_Depression_EmploymentStatus)
exp(cbind(Odds_RAtio=coef(Log_Depression_EmploymentStatus), confint(Log_Depression_EmploymentStatus)))

Log_Depression_FamilyStructure  <-glm(BC_M_N$Depression_Log ~ BC_M_N$"Family structure",
                                   family=binomial(link=logit))
summary(Log_Depression_FamilyStructure)
exp(cbind(Odds_RAtio=coef(Log_Depression_FamilyStructure), confint(Log_Depression_FamilyStructure)))


Log_Depression_MenstrualStatus  <-glm(BC_M_N$Depression_Log ~ BC_M_N$"Menstrual status",
                                  family=binomial(link=logit))
summary(Log_Depression_MenstrualStatus)
exp(cbind(Odds_RAtio=coef(Log_Depression_MenstrualStatus), confint(Log_Depression_MenstrualStatus)))


class(BC_M_N$MI_cat)
BC_M_N$MI_cat<- as.factor(BC_M_N$MI_cat)
levels(BC_M_N$MI_cat)
BC_M_N$MI_cat<- relevel(BC_M_N$MI_cat, ref = "Upto 50000")
Log_Depression_MI <-glm(BC_M_N$Depression_Log ~ BC_M_N$MI_cat,
                     family=binomial(link=logit))
summary(Log_Depression_MI)
exp(cbind(Odds_RAtio=coef(Log_Depression_MI), confint(Log_Depression_MI)))


Log_Depression_CoSurgery  <-glm(BC_M_N$Depression_Log ~ BC_M_N$"Cost of surgery",
                             family=binomial(link=logit))
summary(Log_Depression_CoSurgery)
exp(cbind(Odds_RAtio=coef(Log_Depression_CoSurgery), confint(Log_Depression_CoSurgery)))


Log_Depression_Year_cancer  <-glm(BC_M_N$Depression_Log ~ BC_M_N$"Year first diagnosed with cancer",
                               family=binomial(link=logit))
summary(Log_Depression_Year_cancer)
exp(cbind(Odds_RAtio=coef(Log_Depression_Year_cancer), confint(Log_Year_cancer)))


Log_Depression<-glm(BC_M_N$Depression ~ BC_M_N$"Menstrual status",
                 family=binomial(link=logit))
summary(Log_Depression)
exp(cbind(Odds_RAtio=coef(Log_Depression), confint(Log_Depression)))


#####Food Diversity

Log_food_diversity_Age<-glm(BC_M_N$food_diversity ~ BC_M_N$Age_cat,
                     family=binomial(link=logit))
summary(Log_food_diversity_Age)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_Age), confint(Log_food_diversity_Age)))


Log_food_diversity_Religion<-glm(BC_M_N$food_diversity ~ BC_M_N$Religion,
                          family=binomial(link=logit))
summary(Log_food_diversity_Religion)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_Religion), confint(Log_food_diversity_Religion)))


Log_food_diversity_Distric<-glm(BC_M_N$food_diversity ~ BC_M_N$Distric,
                         family=binomial(link=logit))
summary(Log_food_diversity_Distric)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_Distric), confint(Log_food_diversity_Distric)))


Log_food_diversity_EducationLevel<-glm(BC_M_N$food_diversity ~ BC_M_N$"Education level",
                                family=binomial(link=logit))
summary(Log_food_diversity_EducationLevel)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_EducationLevel), confint(Log_food_diversity_EducationLevel)))

BC_M_N$"Marital status"<- relevel(BC_M_N$"Marital status", ref = "Married")
Log_food_diversity_MaritalStatus <-glm(BC_M_N$food_diversity ~ BC_M_N$"Marital status",
                                family=binomial(link=logit))
summary(Log_food_diversity_MaritalStatus)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_MaritalStatus), confint(Log_food_diversity_MaritalStatus)))

BC_M_N$"Employment status"<- relevel(BC_M_N$"Employment status", ref = "Employed")
Log_food_diversity_EmploymentStatus  <-glm(BC_M_N$food_diversity ~ BC_M_N$"Employment status",
                                    family=binomial(link=logit))
summary(Log_food_diversity_EmploymentStatus)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_EmploymentStatus), confint(Log_food_diversity_EmploymentStatus)))

Log_food_diversity_FamilyStructure  <-glm(BC_M_N$food_diversity ~ BC_M_N$"Family structure",
                                   family=binomial(link=logit))
summary(Log_food_diversity_FamilyStructure)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_FamilyStructure), confint(Log_food_diversity_FamilyStructure)))


Log_food_diversity_MenstrualStatus  <-glm(BC_M_N$food_diversity ~ BC_M_N$"Menstrual status",
                                  family=binomial(link=logit))
summary(Log_food_diversityn_MenstrualStatus)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_MenstrualStatus), confint(Log_food_diversity_MenstrualStatus)))


class(BC_M_N$MI_cat)
BC_M_N$MI_cat<- as.factor(BC_M_N$MI_cat)
levels(BC_M_N$MI_cat)
BC_M_N$MI_cat<- relevel(BC_M_N$MI_cat, ref = "Upto 50000")
Log_food_diversity_MI <-glm(BC_M_N$food_diversity ~ BC_M_N$MI_cat,
                     family=binomial(link=logit))
summary(Log_food_diversity_MI)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_MI), confint(Log_food_diversity_MI)))


Log_food_diversity_CoSurgery  <-glm(BC_M_N$food_diversity ~ BC_M_N$"Cost of surgery",
                             family=binomial(link=logit))
summary(Log_food_diversity_CoSurgery)
exp(cbind(Odds_RAtio=coef(Log_food_diversityn_CoSurgery), confint(Log_food_diversity_CoSurgery)))


Log_food_diversity_Year_cancer  <-glm(BC_M_N$food_diversity ~ BC_M_N$"Year first diagnosed with cancer",
                               family=binomial(link=logit))
summary(Log_food_diversity_Year_cancer)
exp(cbind(Odds_RAtio=coef(Log_food_diversity_Year_cancer), confint(Log_Year_cancer)))


Log_food_diversity<-glm(BC_M_N$food_diversity ~ BC_M_N$"Menstrual status",
                 family=binomial(link=logit))
summary(Log_food_diversity)
exp(cbind(Odds_RAtio=coef(Log_food_diversity), confint(Log_food_diversity)))


##Graph

