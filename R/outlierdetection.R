outlierdetection <- function(data,k='default')
{
  #creating a local function
  ub <- function(x)
  {
    x=x[!is.na(x)]
    q3=quantile(x,.75)
    q1=quantile(x,.25)
    res=min(max(x),(q3+1.5*(q3-q1)))
    count_ub=0 #to count values above upper boundary

    for (i in x)
    {
      if(i>res) #if the value is greater than the upper boundary,we count it as outlier
      {
        count_ub=count_ub+1
      }}

    return(count_ub)
  }

  lb <- function(x)
  {
    x=x[!is.na(x)]
    q3=quantile(x,.75)
    q1=quantile(x,.25)
    res=max(min(x),(q3-1.5*(q3-q1)))
    count_lb=0 #to count values below lower boundary

    for (i in x)
    {
      if(i<res)#if the value is less than the lower boundary,we count it as outlier
      {
        count_lb=count_lb+1
      }}

    return(count_lb)
  }

  if (k=='default'|| k=='yes') #if k is either 'default' or 'yes', execute the below codes
  {
    upperb <- apply(data, 2, ub)
    upperb <- as.matrix(upperb)
    lowerb <- apply(data, 2, lb)
    lowerb <- as.matrix(lowerb)
    outlier<-cbind(upperb,lowerb)
    colnames(outlier) <- c("uppercount","lowercount")
  }

  {if (k=='yes') #if k is strictly yes,execute the below lines for log transformed data
  {
    data1<-log(data+1)
    upperb1 <- apply(data1, 2, ub)
    upperb1 <- as.matrix(upperb1)
    lowerb1 <- apply(data1, 2, lb)
    lowerb1 <- as.matrix(lowerb1)
    outlier1<-cbind(upperb1,lowerb1)
    colnames(outlier1) <- c("log_uppercount","log_lowercount")
    result<-cbind(outlier,outlier1) #column bind the matrices of 'actual data' and 'log transformed data'
    return(result)
  }
    else #if the k is not 'yes',return the matrix for actual data with outlier counts
    {
      return(outlier)
    }}

}



