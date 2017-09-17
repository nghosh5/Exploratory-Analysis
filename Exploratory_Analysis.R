

#Loading the data files
#notice that there are multiple CSV files that need to be read and combined later
#Hence I am using a loop to read all files in one go.
rm(list = ls())
setwd("C:/Users/Nabanita/Desktop/Adv Stats/Exploratory Analysis/csv")### setting the working directory path
# Create vector of file names in working direcotry
files <- list.files() 
files <- files[grep("csv", files)]  
#create empty list
lst <- vector("list", length(files))
#Read files in to list
for(i in 1:length(files)) {
  lst[[i]] <- read.csv(files[i],header = T)
}
#Examining data
for(i in 1:length(files)) {
View(lst[i])  
str(lst[i])
}
#Performing initial data examination, we notice that first 3 rows give general information on data
#Also, the column names that we see in CSV file look very messy and not meaningful
#Hence we import the data again, skipping first 3 rows, as of now. The year information that gets lost in this stage would be compensated at a later stage
#We intend to get meaningful column names that are seen in row number 4 of every CSV file
library(zoo)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

#Read files in to list
for(i in 1:length(files)) {
  lst[[i]] <- read.csv(files[i],skip=3,header = T, nrows=20000)
}

#Examining data again:
for(i in 1:length(files)) {
  View(lst[i])  
  str(lst[i])
}
#We see that the data looks relatively clean in terms of meaningful column names
#We further notice that some of source files have additional columns
#Since we are interested in combining the data at a later stage, we drop off all the uncommon columns
#Viewing the data and examining structure in further detail, we see that first 6 columns are common across all sources
#Also, we see that column names are not consistent. That is, they represent same information but have some difference in names

########################################
# Renaming columns #####################
#######################################
colnames <- c("State","City","Population","Tot_law_enforce_employee","Tot_officers","Tot_Civillians") 
for(i in 1:length(files))
{
  keepcols<-c(1:6)
  lst[[i]]<-lst[[i]][,keepcols]
  colnames(lst[[i]]) <- colnames
}
########################################
#Bringing back year information
# Creating a new Column with Year Value#
#######################################

X<-2007
for(i in 1:9)
{
  lst[[i]]["Year"]<- X
  X<-X+1
}


#replacing sentences with NA
for(i in length(files)) {
  lst[[i]]$State[nchar(as.character(lst[[i]]$State)) > 20] <- NA
}
########################################
# Formatting the data even further#####
#######################################
for(i in 1:9)
{ #Removing comma from population to convert it into numeric
  lst[[i]]$Population<-gsub("\\,", "", lst[[i]]$Population)
  #Removing the white spaces from State AND city if there is any
  lst[[i]]$City <- gsub("^\\s+|\\s+$", "", lst[[i]]$City)
  lst[[i]]$State <- gsub("^\\s+|\\s+$", "", lst[[i]]$State)
  #Filling the blank rows in State Column with the preceding value
  lst[[i]]$State[lst[[i]]$State == ""] <- NA
  lst[[i]]$State <- na.locf(lst[[i]]$State)
  # delete row with missing values after replacing blanks with NA
  lst[[i]][lst[[i]]==""]<-NA
  lst[[i]]<-lst[[i]][complete.cases(lst[[i]]),]

}


#We achieved uniformity in column names across all sources and they have same number of columns
#Further examining data for messiness in rows
#Converting  column type to their classes

for(i in 1:length(files)) 
  {
  # Converting to factors
  lst[[i]]$State<-as.factor(lst[[i]]$State)
  lst[[i]]$City<-as.factor(lst[[i]]$City)
  lst[[i]]$Year<-as.factor(lst[[i]]$Year)
  # Converting to numeric
  lst[[i]]$Population<-as.numeric(lst[[i]]$Population)
  lst[[i]]$Tot_law_enforce_employee<-as.numeric(lst[[i]]$Tot_law_enforce_employee)
  lst[[i]]$Tot_officers<-as.numeric(lst[[i]]$Tot_officers)
  lst[[i]]$Tot_Civillians<-as.numeric(lst[[i]]$Tot_Civillians)
  
}
## Checking for duplicates
for(i in 1:length(files)) 
{
duplicates<-as.data.frame(lst[[i]][duplicated(lst[[i]]),])
}
nrow(duplicates)
# Zero rows found in duplicates data set


#Checking if there are any duplicates:
#Examining value of x, we see that there are no duplicates in any of the lists
#Checking whether the changes have been implemented in classes
str(lst[[1]])
View(lst[[9]])
#####################################
# Combining files into a Single data frame using rbind#
#####################################

df.cat1 <- do.call(rbind, lst[1:9])
View(df.cat1)
str(df.cat1)
############################################################
# Relation Between Population and Law Enforcement Employees#
###########################################################

cor(df.cat1$Population, df.cat1$Tot_law_enforce_employee)
#Correlation is 0.0075
#The number indicates that there is weak correlation between Population and Total law enforcement employees
#Checking the plots:

plot(Population, Tot_law_enforce_employee, main="Law Enforcement Vs Population", 
     xlab="Population ", ylab="Law Enforcement Employees ", pch=19)
abline(lm(Tot_law_enforce_employee~Population, data=df.cat1), col="Red",lwd=3)

#From the plot, we see that increase in population does not necessarily increase number of total law enforcement employees
#The model line indicating relationship has a very small slope and high intercept
#Looking at the intercept, we can say that every city has atleast 200 law enforcement employees, regardless of population

#We also see that range of Population is quite high
#Examining Population separately

summary(Population)
#We see that 75% of data has population upto 14740 and maximum is nearly 8 million
#Therefore, the poulation data is slightly right skewed

#We have categorised data into bins, for a better understanding of distribution
for(i in 1:nrow(df.cat1)){
  if(df.cat1$Population[i] <= 2145)
  {
    df.cat1$PopBin[i]<-"<=2000"
  }else if (df.cat1$Population[i] > 2145 && df.cat1$Population[i] <= 5330)
  {
    df.cat1$PopBin[i]<-"<=5000"
  }else if (df.cat1$Population[i]>5330 && df.cat1$Population[i]<=14740)
  {
    df.cat1$PopBin[i]<-"<=15000"
  } else if(df.cat1$Population[i]>15000)
  {
    df.cat1$PopBin[i]<-">15000"
  }
}
table(df.cat1$PopBin)

#We see that the frequency of each of these categories is nearly same
#This indicates that some cities are very highly populated as compared to other cities in the datasets
#We now intend to obtain average number of law enforcement employees for each of these categories of population
#We are interested to see if there is any increase in number of law enforcement employees for highly populated cities



boxplot(Tot_law_enforce_employee~PopBin, data=df.cat1, main="Relation between Population & Law Enforcement Employee", col=c("Red","Blue","Green","Yellow"), xlab="Population", ylab="Law Enforcement Employees")

#From the plot we see that number of law enforcement employees does not increase highly with such high increase in population
#The median number of total law enforcement employees shows a small step increase with increase in population
#Thus, combining all above observations, we conclude that there is no significant correlation in the mumber of total law enforcement employees and population of a city


############################################################
##### Aggregating Data to get average Population      ######
############################################################

City1 <- df.cat1 %>% group_by(State,City) %>% summarize( avgpop = mean(Population))   %>% arrange(-avgpop)
City1
#We get four cities with average population > 2M
#New York, Los Angeles,Chicago, Houston, 
detach(df.cat1)
#Subsetting the data set based on the new list of Cities
attach(City1)
newdata <- City1[ which( avgpop > 2000000),]
df.cat2<-merge(df.cat1, newdata, by=c("State","City") )
detach(City1)

############################################################
##### Trend Analysis of 4 cities with population > 2M ######
############################################################

library(ggplot2)

#####PLotting Total Civillian Trend Among the largely populated Cities###
Tot_Civillians <- subset( df.cat2, select = c(Year,City,Tot_Civillians))
class(Tot_Civillians$Year)
Tot_Civillians$Year<-as.Date((as.character(Tot_Civillians$Year)),format="%Y")
class(Tot_Civillians$Year) 

attach(Tot_Civillians)
ggplot(data=Tot_Civillians,
aes(x=Year,y=Tot_Civillians)) + geom_line(aes(color=City),
                                                 size=1.25) +
scale_x_date("Year") + 
  scale_y_continuous("Tot_Civillians")
#Top most civillians has been observed in Chicago 
#Los Angeles has seen a dip in total civillians from 2010 onwards
#New York is the thirdmost in terms of total civillians and has peaks in only in 2007 and 2014
#Total Civillians of Houston remain consisten from 2007 to 2015
detach(Tot_Civillians)


#Plotting Total officers Staffing Trend Among the largely populated Cities###
Tot_officers <- subset( df.cat2, select = c(Year,City,Tot_officers))
Tot_officers$Year<-as.Date((as.character(Tot_officers$Year)),format="%Y")

class(Tot_officers$Year)

attach(Tot_officers)
ggplot(data=Tot_officers,
       aes(x=Year,y=Tot_officers)) + geom_line(aes(color=City),
                                                           size=1.25) +
  scale_x_date("Year") + 
  scale_y_continuous("Tot_officers")
#Los Angeles has the highest total officers followed by Houston and New York
#Chicago as the lowest number of total officers as compared to other cities
detach(Tot_officers)

#PLotting Total Law Enforcement Staffing Trend Among the largely populated Cities###
Tot_law_enforce_employee <- subset( df.cat2, select = c(Year,City,Tot_law_enforce_employee))
Tot_law_enforce_employee$Year<-as.Date((as.character(Tot_law_enforce_employee$Year)),format="%Y")

class(Tot_law_enforce_employee$Year)

attach(Tot_law_enforce_employee)
ggplot(data=Tot_law_enforce_employee,
       aes(x=Year,y=Tot_law_enforce_employee)) + geom_line(aes(color=City),
                                                           size=1.25) +
  scale_x_date("Year") + 
  scale_y_continuous("Tot_law_enforce_employee")
# New York and Houston have the highest total law enforcement employees
# Peaks has been observed in Houston from 2009 to 2011
# Los Angeles and New York have much lower count of officers as compared to other two cities
# From 2010 onwards, dip in law enforcement employees has been observed in Chicago and Los Angeles


#Law enforcement staffing year on year trend of Chicago and Los Angeles are similar 
#Year on Year law enforcement employees of New York and Houston are much higher ( approximately 300 enforcement employees  more than that of Chicago)
detach(Tot_law_enforce_employee)
#Thank you!
