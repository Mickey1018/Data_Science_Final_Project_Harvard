#install packages that have not been downloaded
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

#import any necessary library
library(tidyverse)
library(caret)
library(data.table)
library(readr)
library(dslabs)
library(caret)
library(lubridate)

#Read csv data set
#change data set into data frame
seismic <- as.data.frame(read_csv("data/seismic_bumps.csv"))

#get an overview on the data set
summary(seismic) #no NAs are observed
head(seismic)

#test the correctness of the numbers of bumps recorded
seismic %>% 
  mutate(total = nbumps2+nbumps3+nbumps4+nbumps5+nbumps6+nbumps7+nbumps89) %>%
  mutate(diff = total - nbumps) %>%
  filter(diff!=0) %>% 
  summarize(n=n()) %>%
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
  filter(id!=incorrect_index)

#number of positive class
sum(corrected_seismic$class==1)#169

#number of negative class
sum(corrected_seismic$class==0)#2413



#Date exploration
##Ratio between shift on positive and negative class 
corrected_seismic %>%
  filter(class==1) %>%
  ggplot(aes(shift)) +
  geom_bar(width = 0.3) #proportion of N is smaller in negative class

corrected_seismic %>%
  filter(class==0) %>%
  ggplot(aes(shift)) +
  geom_bar(width = 0.3) #proportion of N is higher in negative class

##









corrected_seismic %>% 
  group_by(ghazard) %>%
  summarize(mean(energy))
  

#distribution of number of bumps given that hazardous state
seismic %>% 
  filter(class == 1) %>% 
  ggplot(aes(nbumps)) +
  geom_bar(width = 0.5)

#distribution of number of bumps given that non-hazardous state
seismic %>% 
  filter(class == 0) %>% 
  ggplot(aes(nbumps)) +
  geom_bar(width = 0.5)







#Analysis on covid_jpn_prefecture data set
covid_jpn_prefecture %>% ggplot(aes(Prefecture,Positive)) +  
  geom_bar(width = 0.5, stat = "identity", color = "blue") + 
  coord_flip()
