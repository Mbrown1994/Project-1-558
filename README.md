Project 1 558
================
Mary Brown
10/4/2021

# This code chunk connects to the COVID API and converts the summary information to text, then to json, and lastly to a data frame.

``` r
library(httr)  
library(jsonlite)  
Data<-GET("https://api.covid19api.com/summary")  
get_text<-(content(Data,"text"))  
get_json<-fromJSON(get_text, flatten=TRUE)  
get_df<-as.data.frame(get_json)  
```

# This function returns the number of cases of either confirmed cases, active cases, deaths, or recovered cases (user specified) for the United States

``` r
library(magrittr)  
library(dplyr)  
USAdata<-function(type="all"){  
  outputAPI<-GET("https://api.covid19api.com/total/country/united-states")  
  data<-fromJSON(rawToChar(outputAPI$content))
  if(type !="all"){  
    data<-data %>% select(type)
  }  
  return(data)
    }  
```

# This function returns user specified type by a user specified country in North America (united-states or canada)
