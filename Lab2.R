
library(ggplot2)
library(dplyr)
library(psych)
library(lattice)
library(gplots)
library(stringi)

#**********************************************************
# 1. Reading in data
#**********************************************************

#***************************
# 1.1 Set working directory
#***************************

traindir <- "D:/sys3501/R/TrainData"
sourcedir <- "D:/sys3501/R"


# set the working directory to traindir
setwd(traindir)
#check the current working directory
#it should be same as your traindir directory
getwd()


#***************************
# 1.1 loading all years data
#***************************

# You will need to read in all 20 years of the data 
# You will put the data into a data structure called a list

# To do this you will use code I have written, AccidentInput.R 
# Put that file in your working directory and then source it:
setwd(sourcedir)
source("FileInput.R")

# Now use it to read in all the data. You must have ALL and ONLY the rail accident data
# files in one directory. Call that directory and its path, path.
# You can give your data a name
# In my examples below I use acts as the name for data sets
# Then use the command

setwd(traindir)
my.files <- list.files(traindir)

# get only csv files
library(stringi)
my.csv.files <- my.files[which(stri_detect_fixed(my.files,"csv")==TRUE)]

# 2 different ways of loading multiple files in a directory

# the first uses the lapply function
acts <- lapply(my.csv.files, read.csv)

# the second uses file.inputl function from FileInput.R
acts <- file.inputl(traindir) 

# the argument for the list.files and file.inputl functions is the
# specification of the path to your file.  In my case traindir

# Before we put all the data into one data frame
# we must clean the data

##################################################
#
#	2. Data Cleaning
#
##################################################

#Check the number of columns in a few of the different years of data.  
#Are they the same?  Different?  

#*******************************
# 2.1 combine all years of data
#*******************************
# Now combine the data frames for all 23 years
# Use combine.data() function from AccidentInput.R
# which takes as an argument the list of dataframes
totacts <- combine.data(acts)

#check the number of rows and columns in this merged dataset
dim(totacts)

# Q6 correct ans is conditional box plots, bar plots, and heat maps
# but due to regrading all previous and correct ans are given 1 point

# Q10
summary(totacts$HIGHSPD)

# Q11
selected_years = totacts %>% filter(YEAR %in% c(1:22))
# box plot of equipment damage based on each year
ggplot(as.data.frame(selected_years), aes(EQPDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total EQUIPMENT Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  facet_wrap(~YEAR)

# Q12
selected_years = totacts %>% filter(YEAR %in% c(1:22))
# box plot of total killed based on each year
ggplot(as.data.frame(selected_years), aes(TOTKLD)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total KILLED in the selected years") + 
  labs(x = "Number") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  facet_wrap(~YEAR)


# Q13
selected_years = totacts %>% filter(YEAR %in% c(1:22))
# box plot of track damage based on each year
ggplot(as.data.frame(selected_years), aes(TRKDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Track Damage in the selected years") + 
  labs(x = "Number") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  facet_wrap(~YEAR)

# Q14
selected_years = totacts %>% filter(YEAR %in% c(1:22))
# box plot of total injury based on each year
ggplot(as.data.frame(selected_years), aes(TOTINJ)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Injury in the selected years") + 
  labs(x = "Number") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  facet_wrap(~YEAR)

# Q15 
totacts$INCDTNO[which.max(totacts$TOTINJ)]

# Q16
selected_years = totacts %>% filter(YEAR %in% c(1:22))
# box plot of carsdmg based on each year
ggplot(as.data.frame(selected_years), aes(CARSDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("CARSDMG in the selected years") + 
  labs(x = "Number") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  facet_wrap(~YEAR)

# Q17 
# Here 65 and 75 are very close, so we regrade as both correct
selected_years = totacts %>% filter(YEAR %in% c(1:22))
# histogram of TEMP based on each year
ggplot(as.data.frame(selected_years), aes(TEMP)) + 
  geom_histogram(fill = NA, color = 'Steelblue', bins = nclass.Sturges(selected_years$TEMP)) + 
  ggtitle("TEMP in selected years") + 
  labs(x = "TEMP", y = "Frequency") + 
  facet_wrap(~YEAR)

# to see the actual numbers
sum(totacts$TEMP == 65)
sum(totacts$TEMP == 75)

# Q18, Q19, Q20
pairs.panels(totacts[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])
# Q18 ans: 0.49
# Q19 ans: The robust regression fit (red line) between TOTKLD and TRKDMG shows 0 slope
# Q20 ans: There is one extreme point in the scatter plots of TOTINJ with more than 500 injuries

# Q21
totacts$IYR[which(totacts$ACCDMG == max(totacts$ACCDMG))]

# Q22 ans: type derailment (1) and it involved two different railroads
max_accdmg <- totacts[which(totacts$ACCDMG == max(totacts$ACCDMG)),]
max_accdmg$TYPE
max_accdmg$RAILROAD

# Q23
max(totacts$ACCDMG)

# Q24
totacts$IYR[which(totacts$TOTKLD == max(totacts$TOTKLD))]

# Q25
max(totacts$TOTKLD)

# 26
nrow(totacts[totacts$ACCDMG > 1500000, ])

# Q27 
sum(totacts$TOTKLD >= 1)

# Q28
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", 
                                                "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", 
                                                "GradeX", "Obstruction", "Explosive", "Fire","Other",
                                                "SeeNarrative" ))

# Add color, a title, and change the text size and rotate text
ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) +
  geom_bar(stat="identity",fill= "steelblue")+ 
  ggtitle("Accident Frequency by Type") +
  labs(x = "Type of Accident")+
  theme(axis.text.x = element_text(size = 8,  angle = 45))


# Q29
totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor
totacts$Cause <- factor(totacts$Cause)

# use table() and barplot() to see it.
table(totacts$Cause)

# Use barplot() to graph frequencies corresponding to different types of trains
ggplot(as.data.frame(table(totacts$Cause)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity") +
  labs(x = "Cause")+
  theme(axis.text.x = element_text(size = 8,  angle = 45))


# Q30
ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

upper_whisker <- boxplot(totacts$ACCDMG)$stats[5]
print(upper_whisker)

# Q31
sum(totacts$ACCDMG > upper_whisker)

# Q32
round(sum(totacts$ACCDMG > 162800) / nrow(totacts) * 100)

# Q33
round((sum(totacts$ACCDMG[totacts$ACCDMG > 162800]) / sum(totacts$ACCDMG))*100)

# Q34
threshold <- 15000000
#number of accidents greater than threshold
sum(totacts$ACCDMG > 	threshold)
#years
totacts$IYR[totacts$ACCDMG > 	threshold]
# months
totacts$IMO[totacts$ACCDMG > 	threshold]
#days
totacts$DAY[totacts$ACCDMG > 	threshold]
#types
totacts$TYPE[totacts$ACCDMG > 	threshold]
#all ACCDMG costs greater than the threshold
max(totacts$ACCDMG[totacts$ACCDMG > 	threshold])

# Q35
extreme <- totacts[totacts$ACCDMG > upper_whisker, ]
se <- extreme[, c("INCDTNO", "YEAR", "MONTH","DAY","TIMEHR","TIMEMIN")]
sum(duplicated(se))
