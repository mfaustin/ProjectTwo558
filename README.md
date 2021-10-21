Project Two
================
Maksim Nikiforov and Mark Austin


## Purpose


## Required R Packages

The following R packages are required to run R code used in this
project and/or create this document.

-   `tidyverse` The tidyverse package is used for data handling and
    plotting.
    
-   `caret` The caret package is used for working with data and models.    

-   `knitr` The knitr package is used for document image handling.

-   `rmarkdown` The rmarkdown package is used by a render program to
    render this document.
    

## Analyses

[Lifestyle articles is available here](data_channel_is_lifestyle.html)  

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
reports <- tibble(output_files, params)
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


