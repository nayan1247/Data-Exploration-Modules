library(pacman)

#Load Libraries
library(data.table)

#----------------------- Functions to implement the required functionality

#Mean of the Continuous Variable after removing NA
meanNA <- function(x)
{
  if(class(x)=="numeric" | class(x)=="integer" )
  {
    round(mean(x,na.rm=TRUE),2)
  }
  else{
    NA
  }
}

# Number of levels for the categorical variable
levelsCat <- function(x)
{
  if(class(x)=="factor")
  {
    length(unique(x))
  }
  else{
    NA
  }
}

#Max of the Continuous Variable after removing NA
maxNA <- function(x)
{
  if(class(x)=="numeric" | class(x)=="integer")
  {
    max(x,na.rm=TRUE)
  }
  else{
    NA
  }
}

#Min of the Continuous Variable after removing NA
minNA <- function(x)
{
  if(class(x)=="numeric" | class(x)=="integer")
  {
    min(x,na.rm=TRUE)
  }
  else{
    NA
  }
}

#Standard Deviation of the Continuous Variable after removing NA
sdNA <- function(x)
{
  if(class(x)=="numeric" | class(x)=="integer")
  {
    round(sd(x,na.rm=TRUE),2)
  }
  else{
    NA
  }
}

#Unique Values of the variable
uniqueValuesNA <- function(x){
  return(length(unique(x)))
}


#------------------Create a Summary of the Data Set
generateSummary<-function(df,threshold)
{
  #Convert to Categorical (factor) if # of levels less than threshold
  df.dt<-as.data.table(df)
  var=names(df.dt)[10]
  for (var in names(df.dt))
  {
    if (length(unique(df.dt[[var]])) < threshold & class(df.dt[[var]])=="numeric" | length(unique(df.dt[[var]])) < threshold & class(df.dt[[var]])=="integer")
    {
      df.dt[[var]]<-as.factor(df.dt[[var]])
    }
  }
  
  #Basic Summary Metrics
  names<-names(df.dt)
  missingVal<-sapply(df.dt, function(x) sum(is.na(x)))
  typeVar<-sapply(df.dt, function(x) class(x))
  meanVar<-sapply(df.dt, function(x) meanNA(x))
  maxVal<-sapply(df.dt, function(x) maxNA(x))
  minVal<-sapply(df.dt, function(x) minNA(x))
  sdVal<-sapply(df.dt, function(x) sdNA(x))
  numLevels<-sapply(df.dt, function(x) levelsCat(x))
  uniqueValues<-sapply(df.dt,function(x) uniqueValuesNA(x))
  
  
  #finalDF
  finalSummary<-data.frame()
  finalSummary<-rbind(finalSummary,typeVar,missingVal,meanVar,maxVal,minVal,sdVal,numLevels,uniqueValues)
  names(finalSummary)<-names
  
  row.names(finalSummary)<-c("typeVar","missingVal","meanVar","maxVal","minVal","sdVal","numLevels","uniqueValues")
  t(finalSummary)
}

#----------------------------Test meh function
mtcarsDT<-as.data.table(mtcars)
a<-generateSummary(mtcarsDT,5)
b<-generateSummary(airquality,5)

View(b)
View(a)
