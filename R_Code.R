if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(readr)
library(dslabs)
library(caret)
library(lubridate)

#Read csv data first, and then change them into data frame
seismic <- as.data.frame(read_csv("data/seismic_bumps.csv"))
summary(seismic)

#Analysis on covid_jpn_prefecture data set
covid_jpn_prefecture %>% ggplot(aes(Prefecture,Positive)) +  
  geom_bar(width = 0.5, stat = "identity", color = "blue") + 
  coord_flip()
