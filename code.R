library(pacman)

#Load Libraries
pacman::p_load(data.table)

#Functions
meanNA <- function(x)
{
  if(class(x)=="numeric" | class(x)=="integer" )
  {
    mean(x,na.rm=TRUE)
  }
  else{
    NA
  }
}


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


#Create a Summary of the Data Set
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
  
  #str(df.dt)
  
  #Basic Summary Metrics
  names<-names(df.dt)
  missingVal<-sapply(df.dt, function(x) sum(is.na(x)))
  typeVar<-sapply(df.dt, function(x) class(x))
  meanVar<-sapply(df.dt, function(x) meanNA(x))
  maxVal<-sapply(df.dt, function(x) maxNA(x))
  minVal<-sapply(df.dt, function(x) minNA(x))
  sdVal<-sapply(df.dt, function(x) sdNA(x))
  numLevels<-sapply(df.dt, function(x) levelsCat(x))
  
  
  #finalDF
  finalSummary<-data.frame()
  finalSummary<-rbind(finalSummary,missingVal,typeVar,maxVal,minVal,sdVal,numLevels)
  names(finalSummary)<-names
  
  row.names(finalSummary)<-c("missingVal","typeVar","maxVal","minVal","sdVal","numLevels")
  finalSummary
}


generateSummary(mtcars,5)
generateSummary(airquality,5)
