Project Two
================
Maks Nikiforov and Mark Austin


## Purpose

This repository contains files needed for analysis of an [Online News Popularity](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity) data set.  renderProjectTwo.R contains render code that works with input file ProjectTwo.Rmd to produce different github document files for each of the 6 channels.  Data summaries and modeling are presented for each channel.  

Github pages is enabled to allow viewing the channel analysis pages as linked html files.  


## Required R Packages

The following R packages are required to run R code used in this
project and/or create the linked channel documents.

-   `tidyverse` The tidyverse package is used for data handling and
    plotting.  
    
-   `caret` The caret package is used for working with data and models.    

-   `corrplot` The corrplot package is used to produce a visual correlation plot.  

-   `knitr` The knitr package is used for document image handling.  

-   `rmarkdown` The rmarkdown package is used by a render program to
    render this document.  
    
-   `doParallel` The doParallel package is used to enable [parallel processing with the caret package](https://topepo.github.io/caret/parallel-processing.html)

-   `lares` The lares package provides the `corr_cross()` function to compute and return the highest and most significant variable correlations in a data frame. 

-   `rvest` The rvest package provides the functions `read_html()` and `html_text()` to scrape the titles of top ten Mashable articles in each category of the online news popularity data set.
    

## Analyses by Channel  

[Business articles is available here](data_channel_is_bus.md)  

[Entertainment articles is available here](data_channel_is_entertainment.md)   
  
[Lifestyle articles is available here](data_channel_is_lifestyle.md)  

[Social Media articles is available here](data_channel_is_socmed.md)  

[Tech articles is available here](data_channel_is_tech.md)  

[World articles is available here](data_channel_is_world.md)  


## Render Code Used to Create Analyses  

``` r
library(tidyverse)
library(rmarkdown)

##Read data then extract data channel variables
fullData<-read_csv("./data/OnlineNewsPopularity.csv")

##Extract data channel variables from column names
channels<-names(fullData)[startsWith(names(fullData),"data_channel")]
channels

##Create .md filenames
output_file <- paste0(channels, ".md")
output_file

#create a list for each channel with just the channel name parameter
params = lapply(channels, FUN = function(x){list(channel = x)})


#put into a data frame 
reports <- tibble(output_file, params)
reports

#use render to create report for each channel
# by passing channel parameter
apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "ProjectTwo.Rmd",
               output_format = "github_document",
               output_file = x[[1]],
               output_options = list(
                 html_preview = FALSE,
                 df_print = "tibble",
                 toc = TRUE
               ),
               params = x[[2]])
      })

```


