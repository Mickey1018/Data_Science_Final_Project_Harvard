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
corrected_seismic %>% 
  ggplot(aes(gpuls)) +
  geom_boxplot(fill="white") +
  scale_x_continuous(trans = "log10") +
  xlab("Number of pulses in previous shift") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  facet_grid(class~.) +
  ggtitle("gpuls on hazardous state (class 1) and non-hazardous state (class 0)")


corrected_seismic %>% 
  mutate(class = as.character(class)) %>%
  ggplot(aes(gpuls,fill=class)) +
  geom_density(alpha=0.4) +
  scale_x_continuous(trans = "log10") +
  xlab("Number of pulses in previous shift") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("gpuls on hazardous state (class 1) and non-hazardous state (class 0)")



### 2.2.7 Deviation of number of pulses recorded in previous shift (gdpuls) in mine with hazardous and non-hazardous state
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
  ggtitle("gdpuls on hazardous state (class 1) and non-hazardous state (class 0)")


corrected_seismic %>% 
  mutate(class = as.character(class)) %>%
  mutate(gdpuls_absolute = ifelse(gdpuls<0,-gdpuls,gdpuls)) %>%
  mutate(gdpuls_tran = ifelse(gdpuls_absolute==0,gdpuls_absolute+1,gdpuls_absolute)) %>%
  ggplot(.,aes(x=gdpuls_tran,fill=class)) +
  geom_density(alpha=0.4) +
  scale_x_continuous(trans = "log10") +
  xlab("Deviation of number of pulses in previous shift") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("gdpuls on hazardous state (class 1) and non-hazardous state (class 0)")



##Ratio between ghazard on positive class and negative class
corrected_seismic %>%
  filter(class==1) %>%
  ggplot(aes(ghazard)) +
  geom_bar(width = 0.1,fill="red",col="black") +
  xlab("a - lack of hazard, b - low hazard") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("ghazard distribution on hazardous state (class 1)")
  

corrected_seismic %>%
  filter(class==0) %>%
  ggplot(aes(ghazard)) +
  geom_bar(width = 0.2,fill="red",col="black") +
  xlab("a - lack of hazard, b - low hazard, c - high hazard") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("ghazard distribution on non-hazardous state(class 0)")
  #seems the ratios are the same on both classes



##effects by number of bumps with diff. energy range
###In each class, calculate total numbers of bumps with diff. energy range 
corrected_seismic %>%
  select(class,nbumps,nbumps2,nbumps3,nbumps4,nbumps5) %>%
  group_by(class) %>%
  summarize(sum(nbumps),sum(nbumps2),sum(nbumps3),sum(nbumps4),sum(nbumps5))
###create two data frames for each class   
bumps_c0<- data.frame(bumps_sum = c(1856,848,847,150,11))
bumps_c1<- data.frame(bumps_sum = c(362,168,168,25,1))
###plot the positive class and negative class result
ggplot(bumps_c1, aes(x=row.names(bumps_c1), y=bumps_sum)) +
  geom_col(fill="red",color="black", width = 0.3) +
  xlab("summed bumps with diff. energy range") +
  ylab("count") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("Proportion of bumps with diff. energy range on hazardous state \n(class 1)")
ggplot(bumps_c0, aes(x=row.names(bumps_c0), y=bumps_sum)) +
  geom_col(fill="red",color="black", width = 0.3) +
  xlab("summed bumps with diff. energy range") +
  ylab("count") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("Proportion of bumps with diff. energy range on non-hazardous state \n(class 0)")



##energy on positive class and negative class
corrected_seismic %>%
  ggplot(aes(energy)) +
  geom_boxplot() +
  scale_x_continuous(trans = "log10") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  facet_grid(class~.) +
  ggtitle("energy on hazardous state (class 1) and non-hazardous state (class 0))")



##maxenergy on positive class and negative class
corrected_seismic %>%
  ggplot(aes(maxenergy)) +
  geom_boxplot() +
  scale_x_continuous(trans = "log10") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  facet_grid(class~.) +
  ggtitle("maxenergy on hazardous state (class 1) and non-hazardous state (class 0))")



##explore correlation between numeric variables
NumericVariables<- corrected_seismic %>%
  select(genergy,gpuls,gdenergy,gdpuls,energy,maxenergy)
corrplot.mixed(cor(NumericVariables), lower.col = "black", number.cex = .7)





