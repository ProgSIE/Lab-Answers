

library(ggplot2)
library(dplyr)

#**********************************************************
# 1. Reading in data
#**********************************************************

#***************************
# 1.1 Set working directory
#***************************

# For example, an Mac user
#traindir <- "~/Google Drive/My Drive/UVA/Courses/ProgSIE/Data/TrainData/"
#sourcedir <-"~/Google Drive/My Drive/UVA/Courses/ProgSIE/"

# or a Windows user
traindir <- "D:/sys3501/R/TrainData"
sourcedir <- "D:/sys3501/R"

# set the working directory to traindir
setwd(traindir)
#check the current working directory
#it should be same as your traindir directory
getwd()

# list the files
dir()


#***************************
# 1.2 loading the data
#***************************

# Read in the accident files one at at time
# for the first Lab questions we will 
# only use data from 2020

accident_data_20 <- read.csv("RailAccidents20.csv")

# Q6 
x <- "a"
typeof(x)

# Q9
x <- c(2,2,4,5,7,10,11,9,10)
x[5]

# Q14
# Total number of train accidents occurred in 2020
nrow(accident_data_20)

# Q15
ncol(accident_data_20)

# Q16
accident_data_20$INCDTNO[which.max(accident_data_20$TOTINJ)]

# Q17 
which.max(accident_data_20$ACCDMG)

# Q18
accident_data_20[955, "TYPE"]
accident_data_20[955, "TOTINJ"]
accident_data_20[955, "TOTKLD"]
accident_data_20[955, "COUNTY"]

# Q19
max(accident_data_20$ACCDMG)

# Q20
max(accident_data_20$TRNSPD)

# Q21
max(accident_data_20$TOTKLD)

# Q22
max(accident_data_20$TOTINJ)

# Q23
max(accident_data_20$ACCDMG)

# Q24
median(accident_data_20$TOTINJ)

# Q25
sum(accident_data_20$ACCDMG[accident_data_20$TOTINJ >= 1 | accident_data_20$TOTKLD >= 1])
