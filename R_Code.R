if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(readr)


covid_jpn_prefecture <- read_csv("data/covid_jpn_prefecture.csv")
covid_jpn_metadata <- read_csv("data/covid_jpn_metadata.csv")
covid_jpn_total <- read_csv("data/covid_jpn_total.csv")
