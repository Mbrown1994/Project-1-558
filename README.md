Project 1 558
================
Mary Brown
10/4/2021

## Required packages for this Project

-   Httr  
-   jsonlite  
-   ggplot2  
-   knitr  
-   dplyr
-   magrittr  
-   grid

## This helper function converts columns that contain numeric data stored as character values to numeric data types. I made this function first, in case I would need it moving forward.

``` r
convertToNumeric<-function(vec){  
  if(any(is.na(suppressWarnings(as.numeric(vec))) == TRUE)){  
    output <- vec  
  }  
  else{  
    output<-as.numeric(vec)  
  }  
  return(output)  
}  
```

## This code chunk connects to the COVID API and converts the summary information to text, then to json, and lastly to a data frame. This was just practice to make sure the correct data could be pulled.

``` r
library(httr)  
library(jsonlite)  
Data<-GET("https://api.covid19api.com/summary")  
get_text<-(content(Data,"text"))  
get_json<-fromJSON(get_text, flatten=TRUE)  
get_df<-as.data.frame(get_json)  
```

## This code chunk interacts with the countries endpoint of the COVID API.

``` r
CountriesEndpoint<-function(Country="all")  
  outputAPI<-fromJSON("https://api.covid19api.com/countries")  
  output<-outputAPI$content  
  if(Country %in% "Country")
  if (Country !="all"){  
  if (Country %in% output$Slug){  
    output<-output %>%  
      filter(Slug == Country)  
    }  
    else if (Country %>% output$ISO2){  
      output<-output %>%  
        filter(ISO2 == Country)  
    }  
    return(output)  
}  
```

## This function returns user specified type by a user specified country: either the United States or Canada

``` r
UnitedCanada<-function(type="all", Country="all"){  
  outputAPI<-GET("https://api.covid19api.com/total/country/united-states")  
  data<-fromJSON(rawToChar(outputAPI$content))  
  if (type %in% c("Deaths", "Recovered", "Active", "Confirmed", "Country")){  
    if (Country == "Canada"){  
      baseurl <- "https://api.covid19api.com/total/country/"  
      fullURL <- paste0(baseurl, Country)  
      data <- fromJSON(fullURL)  
      data <- data %>% select(type)  
    }  
    else if (Country == "United States"){  
      baseurl <- "https://api.covid19api.com/total/country/"  
      fullURL <- paste0(baseurl, Country)  
      data <- fromJSON(fullURL)  
      data <- data %>% select(type)  
    }  
  }  
  return(data)  
}  
```

## This function utilizes the Status endpoint for the COVID API. The data is for Australia with the status being user specified (Deaths, Confirmed, Recovered).

``` r
StatusEndpoint<-function(type="all", Country="Australia"){  
  outputAPI<-GET("https://api.covid19api.com/dayone/country/australia/status/confirmed/live")  
  data<-fromJSON(rawToChar(outputAPI$content))  
  if (type %in% "Status"){  
    if (type == "Deaths"){  
      baseurl <- "https://api.covid19api.com/dayone/country/australia/status/"  
      fullURL <- paste0(baseurl, deaths, "/live")  
      data<-fromJSON(fullURL)  
      data<- data %>% select(type)  
    }  
    if (type == "Confirmed"){  
      baseurl <- "https://api.covid19api.com/dayone/country/australia/status/"  
      fullURL <- paste0(baseurl, confirmed, "/live")  
      data<-fromJSON(fullURL)  
      data<-data %>% select(type)  
    }  
    if (type == "Recovered"){  
      baseurl <- "https://api.covid19api.com/dayone/country/australia/status/"  
      fullURL<-paste0(baseurl, recovered, "/live")  
      data <-fromJSON(fullURL)  
      data<-data %>% select(type)  
    }   
  }
    return(data)
}
```

## Pull data from at least two endpoints. For this section, I used the status endpoint function that I made and the USA/Canada function.

``` r
AustraliaConfirmed<-StatusEndpoint("Confirmed", "Australia")  
UnitedStatesConfirmed<- UnitedCanada("Confirmed", "United States")  
UnitedStatesDeaths<- UnitedCanada("Deaths", "United States")  
UnitedStatesRecovered<- UnitedCanada("Recovered", "United States")
CanadaConfirmed<- UnitedCanada("Confirmed", "Canada")  
CanadaDeaths<- UnitedCanada("Deaths", "Canada")  
CanadaRecovered<- UnitedCanada("Recovered", "Canada")  
UnitedStatesAll<-UnitedCanada("all", "United States")
```

## I created three new variables - the mean of cases in Australia, non-active cases for the United States, and the amount that have survived for the United States.

``` r
library(dplyr)
Summary1<-mutate(AustraliaConfirmed, Average=mean(Cases)) 
Summary2<-mutate(UnitedStatesAll, NonActiveCases=(Confirmed-Active))  
Summary3<-mutate(UnitedStatesAll, Survived=(Confirmed-Deaths))
```

## Create a few contingecy tables for comparison. The first contingency table looks at the Provinces in Australia, the second table looks at the dates for Australia and the Provinces for Australia. Table 4 looks at the conutry Australia, the provinces, and dates. Contingency tables are useful for summarization of categorical variables - I don’t think I had the best data to work with here to make my contingency tables but an example of a 2-way contingency table would be table two and example of a three way table would be Table 4.

``` r
Table1<-table(AustraliaConfirmed$Province)  
Table2<-table(AustraliaConfirmed$Date, AustraliaConfirmed$Province)
Table4<-table(AustraliaConfirmed$Country, AustraliaConfirmed$Date, AustraliaConfirmed$Province)  
```

## Some numerical summaries for quantitative variables at each setting of some of the categorial variables. Below you can view the mean, median, and standard deviations for the different Summaries that I created.

``` r
Summary2 %>% summarise(avg=mean(Confirmed), med=median(Confirmed), sd=sd(Confirmed))  
```

    ##        avg      med       sd
    ## 1 17387834 13454278 14725466

``` r
Summary2 %>% summarise(avg=mean(Deaths), med=median(Deaths), sd=sd(Deaths))  
```

    ##        avg    med       sd
    ## 1 328971.5 268956 233302.3

``` r
Summary3 %>% summarise(avg=mean(Survived), med=median(Survived), sd=sd(Survived))  
```

    ##        avg      med       sd
    ## 1 17058863 13185322 14494116

``` r
Summary3 %>% summarise(avg=mean(Active), med=median(Active), sd=sd(Active)) 
```

    ##        avg     med       sd
    ## 1 16234941 8127028 14990047

``` r
Summary3 %>% summarise(avg=mean(Recovered), med=median(Recovered), sd=sd(Recovered))
```

    ##        avg med      sd
    ## 1 796429.2   0 1422295

## This section shows a boxplot for the deaths in the United States. The line thrrough the boxplot marks the median which is the mid-point of the data. I utilized width and fill settings to make the graph look more presentable with such large data.

``` r
library(ggplot2)
p<-ggplot(Summary2, aes(x=Country, y=Deaths)) +  
  geom_boxplot(width=0.7, aes(fill=Country)) 
print(p)  
```

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

## This second graph is barplot that explores the total cases by specific provinces in Australia. It’s interesting to see how many more cases are in specific provinces. I utilized stat, width, color, and fill for this plot. I also changed the angle of the x-axis labels to make them easier to read.

``` r
g <- ggplot(data=Summary1, aes(x = Province, y = Cases)) +  
  geom_bar(stat = "identity", width=0.7, col = "purple", fill = "pink") + theme(axis.text.x = element_text(angle=90)) 
print(g)
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

## The below graph is a histogram showing the recovered cases for the United States. I added a mean line showing the average. The data frame for this was so large that I decided to subset it making a smaller data frame from rows 1:55 and columns 1:12.

``` r
RecovUS<-Summary3[c(1:55),c(1:12)]
s<-ggplot(data=RecovUS, aes(x=Recovered)) +  
  geom_histogram(binwidth=2, color="blue", fill="lightblue") +  
  geom_vline(aes(xintercept=mean(Recovered)),  
             color="black", linetype="dashed", size=1)
print(s)
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

## The below graph is a scatterplot showing the survived cases vs deaths within the United States.

``` r
q<-ggplot(data = Summary3, aes(x = Survived, y = Deaths)) +  
  geom_point(shape=10, color="orange") + geom_smooth(method=lm)  
print(q)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

## The below graph is a line plot showing the deaths vs confirmed cases in the United States. Since the data set for this was so large, I decided to subset it - making a smaller data frame of rows 1:55 and columns 1:12 (RecovUS)

``` r
library(grid)
ggplot(data=RecovUS, aes(x=Deaths, y=Confirmed, group=1)) +  
  geom_line(color = "orange", arrow=arrow()) + 
  geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->
