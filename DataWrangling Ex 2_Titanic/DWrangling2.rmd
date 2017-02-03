library(dplyr)
library(stringr)
library(caret)
library(ggplot2)
library(Hmisc)
library(lattice)
library(VIM)
##Load the data in RStudio
##Save the data set as a CSV file called titanic_original.csv 
##Load it in RStudio into a data frame.
titanic3 <- read.csv("~/Springboard/Titanic Exercise/titanic3.csv", stringsAsFactors=FALSE )

##Load it in RStudio as dplyr table.
sb_ti <-tbl_df(titanic3)
glimpse(sb_ti)
##Port of embarkation- The embarked column has some missing values
View(sb_ti$embarked)
##which are known to correspond to passengers who actually embarked at Southampton. 
#Find the missing values and replace them with S. 
sb_ti$embarked <-str_replace(sb_ti$embarked,'-', "s")
sb_ti$embarked

##Age- Problem (Use Hmisc for missing value imputation`)
sb_ti$age <- with(sb_ti, impute(sb_ti$age, mean))
summary(sb_ti$age)
sb_ti$age

##Lifeboat - Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'
sb_ti <- sb_ti %>% 
  mutate(boat = replace(boat, boat == '', NA))

summary(sb_ti$boat)
sb_ti$boat

# 4- Cabin -  Create a new column has cabin_number which has 1 if there is a cabin number, and 0 otherwise.
sb_ti <- sb_ti %>%
  mutate(cabin, cabin_number = ifelse(cabin == "", "0","1"))
summary(sb_ti$cabin)
sb_ti$cabin

#Write to CSV files
write.csv(titanic3, file ="titanic_original_jk.csv")
write.csv(sb_ti, file ="titanic_clean_jk.csv")