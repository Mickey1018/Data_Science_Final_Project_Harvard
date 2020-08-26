#install packages that have not been downloaded
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")

#import any necessary library
library(tidyverse)
library(caret)
library(data.table)
library(readr)
library(dslabs)
library(caret)
library(lubridate)
library(corrplot)
library(knitr)
library(plyr)
library(DMwR)
library(randomForest)
library(rpart)

#Read csv data set
#change data set into data frame
seismic <- as.data.frame(read_csv("data/seismic_bumps.csv"))


#get an overview on the data set
summary(seismic) #no NAs are observed, nbumps6,7,89 not useful
head(seismic)



#Variable Analysis
variable_summary<-seismic %>% summarise_all(funs("Total" = n(),
                     "Filled" = sum(!is.na(.)),
                     "Nulls" = sum(is.na(.)),
                     "Cardinality" = length(unique(.)))) %>%
                     melt() %>%
                    separate(variable, into = c('variable', 'measure'), sep="_") %>%
                    spread(measure, value)  %>%
                    mutate(Uniqueness = format(round(Cardinality/Total,1), nsmall = 1))

#Create table of variable_summary
variable_summary[,c(1,2,3,4,5,6)]


#Create data frame for summarizing variable types
names<- variable_summary$variable[-8] #remove id
data.frame(
  Variable = names, 
  Type = c('binary',
           'numeric',
           'numeric',
           'numeric',
           'numeric',
           'catagorical',
           'numeric',
           'catagorical',
           'catagorical',
           'catagorical',
           'catagorical',
           'catagorical',
           'catagorical',
           'catagorical',
           'catagorical',
           'catagorical',
           'catagorical',
           'catagorical',
           'catagorical')
  )



#test the correctness of the data set
seismic %>% 
  mutate(total = nbumps2+nbumps3+nbumps4+nbumps5+nbumps6+nbumps7+nbumps89) %>%
  mutate(diff = total - nbumps) %>%
  filter(diff!=0) %>% 
  dplyr::summarize(n=n()) %>%
  pull(n) #2 incorrect observations are found


#extract the index of incorrect data set
incorrect_index<- 
  seismic %>% 
  mutate(total = nbumps2+nbumps3+nbumps4+nbumps5+nbumps6+nbumps7+nbumps89) %>%
  mutate(diff = total - nbumps) %>%
  filter(diff!=0) %>% 
  pull(id)

#filter out the incorrect observations
#correct the data set
corrected_seismic<-
  seismic %>% 
  filter(id!=incorrect_index) %>%
  select(-nbumps6,-nbumps7,-nbumps89,-id)


#number of positive class
sum(corrected_seismic$class==1)#169

#number of negative class
sum(corrected_seismic$class==0)#2413





## 2.2 Data Exploration and Visualization

### 2.2.1 Distribution of seismic (result of shift seismic hazard assessment) on mine with hazardous and non-hazardous state
corrected_seismic %>%
  filter(class==1) %>%
  ggplot(aes(seismic)) +
  geom_bar(width = 0.1,fill="red",col="black") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  xlab("a - lack of hazard, b - low hazard") +
  ggtitle("seismic distribution on hazardous state \n(class 1)")

corrected_seismic %>%
  filter(class==0) %>%
  ggplot(aes(seismic)) +
  geom_bar(width = 0.1,fill="red",col="black") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  xlab("a - lack of hazard, b - low hazard") +
  ggtitle("seismic distribution on non-hazardous state \n(class 0)")



### 2.2.2 Distribution of seismoacoustic (result of shift seismic hazard assessment) on mine with hazardous and non-hazardous state 
corrected_seismic %>%
  filter(class==1) %>%
  ggplot(aes(seismoacoustic)) +
  geom_bar(width = 0.2,fill="red",col="black") +
  xlab("a - lack of hazard, b - low hazard, c - high hazard") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("seismoacoustic distribution on hazardous state \n(class 1)")

corrected_seismic %>%
  filter(class==0) %>%
  ggplot(aes(seismoacoustic)) +
  geom_bar(width = 0.2,fill="red",col="black") +
  xlab("a - lack of hazard, b - low hazard, c - high hazard") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("seismoacoustic distribution on non-hazardous state \n(class 0)")



### 2.2.3 Distribution of shift type on mine with hazardous and non-hazardous state
corrected_seismic %>%
  filter(class==1) %>%
  ggplot(aes(shift)) +
  geom_bar(width = 0.1,fill="red",col="black") +
  xlab("type of a shift (W - coal-getting, N -preparation shift)") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("shift type distribution on hazardous state \n(class 1)")
  #proportion of N is smaller in negative class

corrected_seismic %>%
  filter(class==0) %>%
  ggplot(aes(shift)) +
  geom_bar(width = 0.1,fill="red",col="black") +
  xlab("type of a shift (W - coal-getting, N -preparation shift)") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("shift type distribution on non-hazardous state \n(class 0)")
  #proportion of N is higher in negative class



### 2.2.4 Seismic energy recorded in previous shift (genergy) in mine with hazardous and non-hazardous state
corrected_seismic %>% 
  mutate(genergy_tran = genergy+1) %>%
  ggplot(aes(genergy_tran)) +
  geom_boxplot(fill="white") +
  scale_x_continuous(trans = "log10") +
  xlab("log(seismic energy in previous shift) / J") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  facet_grid(class~.) +
  ggtitle("genergy in mine with hazardous state (class 1) and non-hazardous state (class 0)")



### 2.2.5 Deviation of Seismic energy recorded in previous shift (gdenergy) in mine with hazardous and non-hazardous state
corrected_seismic %>% 
  mutate(gdenergy_absolute = ifelse(gdenergy<0,-gdenergy,gdenergy)) %>%
  mutate(gdenergy_tran = ifelse(gdenergy_absolute==0,gdenergy_absolute+1,gdenergy_absolute)) %>%
  ggplot(aes(gdenergy_tran)) +
  geom_boxplot(fill="white") +
  scale_x_continuous(trans = "log10") +
  xlab("log(deviation of seismic energy in previous shift) / J") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  facet_grid(class~.) +
  ggtitle("gdenergy in mine with hazardous state (class 1) and non-hazardous state (class 0)")




### 2.2.6 Number of pulses recorded in previous shift (gpuls) in mine with hazardous and non-hazardous state
#box plot
corrected_seismic %>% 
  ggplot(aes(gpuls)) +
  geom_boxplot(fill="white") +
  scale_x_continuous(trans = "log10") +
  xlab("Number of pulses in previous shift") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  facet_grid(class~.) +
  ggtitle("gpuls in mine with hazardous state (class 1) and non-hazardous state (class 0)")

#density plot
mu_gpuls <- ddply(corrected_seismic,"class", summarise, grp.mean=mean(gpuls))
corrected_seismic %>% 
  mutate(class = as.character(class)) %>%
  ggplot(aes(gpuls,fill=class)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu_gpuls, aes(xintercept=grp.mean, color=class),
             linetype="dashed") +
  scale_x_continuous(trans = "log10") +
  xlab("log(Number of pulses in previous shift)") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("gpuls in mine with hazardous state (class 1) and non-hazardous state (class 0)")



### 2.2.7 Deviation of number of pulses recorded in previous shift (gdpuls) in mine with hazardous and non-hazardous state
#box plot
corrected_seismic %>% 
  mutate(gdpuls_absolute = ifelse(gdpuls<0,-gdpuls,gdpuls)) %>%
  mutate(gdpuls_tran = ifelse(gdpuls_absolute==0,gdpuls_absolute+1,gdpuls_absolute)) %>%
  ggplot(aes(gdpuls_tran)) +
  geom_boxplot(fill="white") +
  scale_x_continuous(trans = "log10") +
  xlab("Deviation of number of pulses in previous shift") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  facet_grid(class~.) +
  ggtitle("gdpuls in mine with hazardous state (class 1) and non-hazardous state (class 0)")

#density plot
mu_gdpuls <- 
  ddply(corrected_seismic %>% 
        mutate(gdpuls_absolute = ifelse(gdpuls<0,-gdpuls,gdpuls)),
      "class", summarise, grp.mean=mean(gdpuls_absolute))

corrected_seismic %>% 
  mutate(class = as.character(class)) %>%
  mutate(gdpuls_absolute = ifelse(gdpuls<0,-gdpuls,gdpuls)) %>%
  mutate(gdpuls_tran = ifelse(gdpuls_absolute==0,gdpuls_absolute+1,gdpuls_absolute)) %>%
  ggplot(.,aes(x=gdpuls_tran,fill=class)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu_gdpuls, aes(xintercept=grp.mean, color=class),
             linetype="dashed") +  
  scale_x_continuous(trans = "log10") +
  xlab("log(Deviation of number of pulses in previous shift)") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("gdpuls in mine with hazardous state (class 1) and non-hazardous state (class 0)")



### 2.2.8 Distribution of ghazard (result of shift seismic hazard assessment) on mine with hazardous and non-hazardous state
corrected_seismic %>%
  filter(class==1) %>%
  ggplot(aes(ghazard)) +
  geom_bar(width = 0.1,fill="red",col="black") +
  xlab("a - lack of hazard, b - low hazard") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("ghazard distribution on mine with hazardous state (class 1)")
  

corrected_seismic %>%
  filter(class==0) %>%
  ggplot(aes(ghazard)) +
  geom_bar(width = 0.1,fill="red",col="black") +
  xlab("a - lack of hazard, b - low hazard, c - high hazard") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("ghazard distribution on mine with non-hazardous state(class 0)")
  #seems the ratios are the same on both classes




### 2.2.9 Number of seismic bumps with different energy levels in previous shift on mine with hazardous and non-hazardous state 
corrected_seismic %>%
  select(class,nbumps,nbumps2,nbumps3,nbumps4,nbumps5) %>%
  group_by(class) %>%
  summarize(sum(nbumps),sum(nbumps2),sum(nbumps3),sum(nbumps4),sum(nbumps5))

###create two data frames for each class   
bumps_c0<- data.frame(bumps_sum = c(1856,848,847,150,11))
bumps_c1<- data.frame(bumps_sum = c(362,168,168,25,1))

###plot the positive class and negative class result
ggplot(bumps_c1, aes(x=c('total no. of bumps',
                         'no. of bumps in \nenergy range [10^2, 10^3)',
                         'no. of bumps in \nenergy range [10^3, 10^4)',
                         'no. of bumps in \nenergy range [10^4, 10^5)',
                         'no. of bumps in \nenergy range [10^5, 10^6)'),
                     y=bumps_sum)) +
  geom_col(fill="red",color="black", width = 0.2) +
  xlab("") +
  ylab("count") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Number of seismic bumps with different energy range on \nmine with hazardous state (class 1)")

ggplot(bumps_c0, aes(x=c('total no. of bumps',
                         'no. of bumps in \nenergy range [10^2, 10^3)',
                         'no. of bumps in \nenergy range [10^3, 10^4)',
                         'no. of bumps in \nenergy range [10^4, 10^5)',
                         'no. of bumps in \nenergy range [10^5, 10^6)'), 
                     y=bumps_sum)) +
  geom_col(fill="red",color="black", width = 0.2) +
  xlab("") +
  ylab("count") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Number of seismic bumps with different energy range on \nmine with non-hazardous state (class 0)")



### 2.2.10 Total energy of seismic bumps in previous shift on mine with hazardous and non-hazardous state
mu_energy <- ddply(corrected_seismic,"class", summarise, grp.mean=mean(energy))
corrected_seismic %>%
  mutate(class = as.character(class)) %>%
  mutate(energy_tran = energy+1) %>%
  ggplot(aes(energy_tran,fill=class)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu_energy, aes(xintercept=grp.mean, color=class),
             linetype="dashed") +
  scale_x_continuous(trans = "log10") +
  xlab("log(Total energy of seismic bumps in previous shift) / J") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Total energy of seismic bumps in previous shift on mine with\nhazardous state (class 1) and non-hazardous state (class 0))")



### 2.2.11 Maximum energy of seismic bumps in previous shift on mine with hazardous and non-hazardous state
mu_maxenergy <- ddply(corrected_seismic,"class", summarise, grp.mean=mean(maxenergy))
corrected_seismic %>%
  mutate(class = as.character(class)) %>%
  mutate(maxenergy_tran = maxenergy+1) %>%
  ggplot(aes(maxenergy_tran,fill=class)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu_maxenergy, aes(xintercept=grp.mean, color=class),
             linetype="dashed") +
  scale_x_continuous(trans = "log10") +
  xlab("log(Maxium energy of the seismic bumps in previous shift) / J") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Maximum energy of the seismic bumps in previous shift on mine with\nhazardous state (class 1) and non-hazardous state (class 0)")


#Ratio of maximum energy to total energy
corrected_seismic %>%
  mutate(maxenergy=maxenergy+1) %>%
  mutate(energy=energy+1) %>%
  mutate(max_to_total = maxenergy/energy) %>%
  summarize(mean(max_to_total))



### 2.2.12 Correlation between attributes
NumericVariables<- corrected_seismic %>%
  select(genergy,gpuls,gdenergy,gdpuls,energy,maxenergy)
corrplot.mixed(cor(NumericVariables), lower.col = "black", number.cex = .7)



## 2.3 Modeling Approach

### 2.3.2 Model-Logistic Regression (Imbalance class)
model_glm <- glm(class ~ ., data = corrected_seismic, family = "binomial")
predict_glm <- predict(model_glm, type = "response")
class_glm <- ifelse(predict_glm >= mean(corrected_seismic$class), 1, 0)
mean(class_glm == corrected_seismic$class)#0.735



### 2.3.3 Model- Zero (Imbalance class)
model_zero <- 0
mean(model_zero == corrected_seismic$class)#0.935



### 2.3.4 Resampling 
corrected_seismic$class <- as.factor(corrected_seismic$class)
corrected_seismic$seismic <- as.factor(corrected_seismic$seismic)
corrected_seismic$seismoacoustic <- as.factor(corrected_seismic$seismoacoustic)
corrected_seismic$shift <- as.factor(corrected_seismic$shift)
corrected_seismic$ghazard <- as.factor(corrected_seismic$ghazard)

re_seismic <- SMOTE(class ~ ., corrected_seismic, perc.over = 200, k = 5, perc.under=150)
sum(re_seismic$class==1)#507
sum(re_seismic$class==0)#507


### 2.3.5 Model - Zero (Balance class)
mean(model_zero == re_seismic$class)




### 2.3.6 Creating Data Partition
y <- re_seismic$class
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
re_seismic_train <- re_seismic %>% slice(-test_index)
re_seismic_test <- re_seismic %>% slice(test_index)


### 2.3.7 Model - Logistic Regression
set.seed(1)
fit_glm<- train(class ~ ., method = "glm", data = re_seismic_train, family = "binomial")
predict_glm_2 <- 
  re_seismic_test %>%
  mutate(y_hat = predict(fit_glm, newdata = re_seismic_test)) %>%
  pull(y_hat) %>%
  factor(levels = levels(re_seismic_test$class))

cm_glm_2 <- confusionMatrix(predict_glm_2, re_seismic_test$class) #0.7028


### 2.3.8 Model - Decision Tree
set.seed(1)
fit_rpart <- train(class~., data = re_seismic_train, method = "rpart") 
#plot(fit_rpart)
predict_rpart <- 
  re_seismic_test %>%
  mutate(y_hat = predict(fit_rpart, newdata = re_seismic_test)) %>%
  pull(y_hat) %>%
  factor(levels = levels(re_seismic_test$class))

cm_rpart <- confusionMatrix(predict_rpart, re_seismic_test$class) #0.7579


### 2.3.9 Model - Random Forest
set.seed(1)
fit_rf <- randomForest(class~., data = re_seismic_train) 
#plot(fit_rf)
predict_rf <- 
  re_seismic_test %>%
  mutate(y_hat = predict(fit_rf, newdata = re_seismic_test)) %>%
  pull(y_hat) %>%
  factor(levels = levels(re_seismic_test$class))

cm_rf <- confusionMatrix(predict_rf, re_seismic_test$class) #0.8209


















###Confusion Matrix###
cm_Sex<-confusionMatrix(data = guess_by_sex, reference = test_set$Survived)
cm_Pclass<- confusionMatrix(data = guess_by_Pclass, reference = test_set$Survived)
cm_Sex_Pclass<- confusionMatrix(data = guess_by_Sex_Pclass, reference = test_set$Survived)

cm_Sex$byClass[c("Sensitivity","Specificity", "Prevalence", "Balanced Accuracy")]
cm_Pclass$byClass[c("Sensitivity","Specificity", "Prevalence", "Balanced Accuracy")]
cm_Sex_Pclass$byClass[c("Sensitivity","Specificity", "Prevalence", "Balanced Accuracy")]

###F1 Score###
F1_Sex<-F_meas(data = guess_by_sex, reference = test_set$Survived)
F1_Pclass<- F_meas(data = guess_by_Pclass, reference = test_set$Survived)
F1_Sex_Pclass<- F_meas(data = predict(fit_rf, newdata = re_seismic_test), reference = re_seismic_test$Sclass)





