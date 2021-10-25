#########################
#  Authors-Mark Austin
#    Maks Nikiforov
#
#  Due Date 10/31/21
#
#  Purpose-Render 
#   channel .md 
#   files for 558 Project 2
#  
#########################

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



