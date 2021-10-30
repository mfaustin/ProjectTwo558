Analysis for the Social Media Channel
================
Maks Nikiforov and Mark Austin
Due 10/31/2021

-   [Data Import](#data-import)
-   [Introduction](#introduction)
-   [Summarizations](#summarizations)
    -   [Numerical Summaries](#numerical-summaries)
    -   [Contingency Table](#contingency-table)
    -   [Plots](#plots)
-   [Modeling](#modeling)
    -   [Splitting Data](#splitting-data)
    -   [Linear Regression Models](#linear-regression-models)
    -   [Random Forest Model](#random-forest-model)
    -   [Boosted Tree Model](#boosted-tree-model)
-   [Model Comparisons](#model-comparisons)
-   [References](#references)

## Data Import

Data was imported first to allow for a more automated introduction.

``` r
# Read all data into a tibble
fullData<-read_csv("./data/OnlineNewsPopularity.csv")

# Eliminate non-predictive variables
reduceVarsData<-fullData %>% select(-url,-timedelta)

#test code for pre markdown automation
#params$channel<-"data_channel_is_bus"

#filter by the current params channel
channelData<-reduceVarsData %>% filter(eval(as.name(params$channel))==1) 

# URL data for top ten articles in each category
channelDataURL <- fullData %>% filter(eval(as.name(params$channel))==1)

###Can now drop the data channel variables 
channelData<-channelData %>% select(-starts_with("data_channel"))
```

## Introduction

This page offers an exploratory data analysis of Social Media articles
in the [online news popularity data
set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).
The top ten articles in this category, based on the number of shares on
social media, include the following titles:

| Shares | Article title                                                       |
|-------:|:--------------------------------------------------------------------|
| 122800 | World’s First Sprout-Powered Battery Just Lit Up a Christmas Tree   |
|  59000 | Facebook Hashtags Not Open to Advertising – Yet                     |
|  57600 | 5 Fascinating Facts We Learned From Reddit This Week                |
|  57000 | The Most Memorable Brand Wins and Fails of 2013                     |
|  54100 | Even Superman Can’t Resist Photoshopping His Pics                   |
|  53100 | Watch the First-Ever YouTube Music Awards Here                      |
|  51900 | The 100 Most Beautiful Songs in the World, According to Reddit      |
|  47700 | Rebooted NASA Spacecraft Begins a New Mission 36 Years After Launch |
|  47400 | ‘Doctor Who’ Crash Course: 12 Essential Episodes                    |
|  41900 | The Tech Behind the Film ‘Gravity’                                  |

Two variables - `url` and `timedelta` - are non-predictive and have been
removed. The remaining 53 variables comprise 2323 observations, which
makes up 5.9 percent of the original data set. Fernandes et al., who
sourced the data, concentrated on article characteristics such as
verbosity and the polarity of content, publication day, the quantity of
included media, and keyword attributes (Fernandes et al., 2015). A
subset of these variables and the correlations between them are explored
in subsequent sections.

The broader purpose of this analysis is predicated on using supervised
learning to predict a target variable - `shares`. To this end, the final
sections outline four unique models for conducting such predictions and
an assessment of their relative performance. Two models are rooted in
multiple linear regression analysis, which assesses relationships
between a response variable and two or more predictors. The remaining
models are based on random forest and boosted tree techniques. The
random forest method averages results from multiple decision trees which
are fitted with a random parameter subset. The boosted tree method
spurns averages in favor of results that stem from weighted iterations
(James et al., 2021).

## Summarizations

### Numerical Summaries

The first table summarizes information for article shares grouped by
whether an article was a weekend article or not. This summary gives an
idea of the center and spread of `shares` across type of day group
levels.

``` r
channelData %>% 
  mutate(dayType=ifelse(is_weekend,"Weekend","Weekday")) %>%
  group_by(dayType) %>% 
  summarise(Avg = mean(shares), Sd = sd(shares), 
    Median = median(shares), IQR =IQR(shares)) %>% kable()
```

| dayType |      Avg |       Sd | Median |  IQR |
|:--------|---------:|---------:|-------:|-----:|
| Weekday | 3579.021 | 5525.015 |   2100 | 2400 |
| Weekend | 3948.079 | 5516.842 |   2400 | 2600 |

The next tables gives expands on the idea of the first table by grouping
`shares` by each day of the week. This summary gives an idea of the
center and spread of `shares` across day of the week group levels.

``` r
dowData<-channelData %>% select(starts_with("weekday_is"),shares) %>%
  mutate(dayofWeek=case_when(as.logical(weekday_is_monday)~"Monday",
                             as.logical(weekday_is_tuesday)~"Tuesday",
                             as.logical(weekday_is_wednesday)~"Wednesday",
                             as.logical(weekday_is_thursday)~"Thursday",
                             as.logical(weekday_is_friday)~"Friday",
                             as.logical(weekday_is_saturday)~"Saturday",
                             as.logical(weekday_is_sunday)~"Sunday")) %>%
  select(dayofWeek,shares)

dowLevels<-c("Monday","Tuesday","Wednesday",
             "Thursday","Friday","Saturday","Sunday")
dowData$dayofWeek<-factor(dowData$dayofWeek,levels = dowLevels)

dowData %>%  
  group_by(dayofWeek) %>% 
  summarise(Avg = mean(shares), Sd = sd(shares), 
    Median = median(shares), IQR =IQR(shares)) %>% kable()
```

| dayofWeek |      Avg |       Sd | Median |  IQR |
|:----------|---------:|---------:|-------:|-----:|
| Monday    | 4010.442 | 6045.547 |   2300 | 2800 |
| Tuesday   | 3503.290 | 6854.140 |   1900 | 2300 |
| Wednesday | 3508.510 | 5203.778 |   2100 | 2400 |
| Thursday  | 3092.168 | 3177.576 |   2000 | 2200 |
| Friday    | 4012.880 | 5845.955 |   2200 | 2525 |
| Saturday  | 3508.711 | 4119.288 |   2400 | 2100 |
| Sunday    | 4525.350 | 6913.255 |   2500 | 3100 |

The table below highlights variables with the highest and most
significant correlations in the data set. This output may be considered
when analyzing covariance to control for potentially confounding
variables.

``` r
# Display top 10 highest correlations
covarianceDF <- corr_cross(df = channelData, max_pvalue = 0.05, top = 10, plot = 0) %>% 
  select(key, mix, corr, pvalue) %>% rename("Variable 1" = key, "Variable 2" = mix, 
                                            "Correlation" = corr, "p-value" = pvalue) 

# Display non-zero p-values
covarianceDF[4] <- format.pval(covarianceDF[4])

kable(covarianceDF)
```

| Variable 1                    | Variable 2                      | Correlation | p-value       |
|:------------------------------|:--------------------------------|------------:|:--------------|
| kw\_max\_min                  | kw\_avg\_min                    |    0.976238 | &lt; 2.22e-16 |
| n\_unique\_tokens             | n\_non\_stop\_unique\_tokens    |    0.924183 | &lt; 2.22e-16 |
| kw\_max\_avg                  | kw\_avg\_avg                    |    0.879522 | &lt; 2.22e-16 |
| rate\_positive\_words         | rate\_negative\_words           |   -0.856612 | &lt; 2.22e-16 |
| kw\_min\_min                  | kw\_max\_max                    |   -0.851158 | &lt; 2.22e-16 |
| n\_non\_stop\_words           | average\_token\_length          |    0.801155 | &lt; 2.22e-16 |
| self\_reference\_max\_shares  | self\_reference\_avg\_sharess   |    0.794608 | &lt; 2.22e-16 |
| self\_reference\_min\_shares  | self\_reference\_avg\_sharess   |    0.789028 | &lt; 2.22e-16 |
| global\_rate\_negative\_words | rate\_negative\_words           |    0.776907 | &lt; 2.22e-16 |
| title\_subjectivity           | abs\_title\_sentiment\_polarity |    0.732438 | &lt; 2.22e-16 |

### Contingency Table

The following contingency table displays counts and sums for the number
of article shares within given ranges by the day of week shared. Share
ranges were selected to illustrate lower, medium, and higher ranges of
shares. Examining these counts can show possible patterns of shares by
day or week and the range grouping for shares.

``` r
##dig.lab is needed to avoid R defaulting to scientific notation
kable(addmargins(table
                 (dowData$dayofWeek,cut(dowData$shares,
                  c(0,200,1000,10000,860000),dig.lab = 7))))
```

|           | (0,200\] | (200,1000\] | (1000,10000\] | (10000,860000\] |  Sum |
|:----------|---------:|------------:|--------------:|----------------:|-----:|
| Monday    |        2 |          34 |           279 |              22 |  337 |
| Tuesday   |        0 |          62 |           374 |              22 |  458 |
| Wednesday |        2 |          40 |           352 |              22 |  416 |
| Thursday  |        3 |          54 |           388 |              18 |  463 |
| Friday    |        0 |          33 |           273 |              26 |  332 |
| Saturday  |        0 |           7 |           165 |               8 |  180 |
| Sunday    |        0 |           9 |           116 |              12 |  137 |
| Sum       |        7 |         239 |          1947 |             130 | 2323 |

### Plots

The following histogram looks at the distribution of `shares`. A pseudo
log y scale with modified y break values was used so that article
`shares` with low frequency will appear. We can tell from the histogram
whether `shares` has a symmetric or skewed distribution. The
distribution is symmetric if the tails are the same around the center.
The distribution is right skewed if there is a long left tail and right
skewed if there is a long right tail.

``` r
###creating histogram of shares data 
##scales comma was used to avoid the default scientific notation
##pseudo log with breaks was used to make low frequency values 
## more visisble
g <- ggplot(channelData, aes( x = shares))
g + geom_histogram(binwidth=12000,color = "brown", fill = "green", 
  size = 1)  + labs(x="Article Shares", y="Pseudo Log of Count",
  title = "Histogram of Article Shares") +
  scale_y_continuous(trans = "pseudo_log",
                     breaks = c(0:3, 2000, 6000),minor_breaks = NULL) +
  scale_x_continuous(labels = scales::comma) 
```

![](images/socmed/histogram%20of%20shares-1.png)<!-- -->

Fernandes et al. highlight several variables in their random forest
model (Fernandes et al., 2015). The following variables from their top
11 were included in the following correlation plot with variables in ()
being renamed for this plot:
`shares`,`kw_min_avg`,`kw_max_avg`,`LDA_03`,`self_reference_min_shares`(`srmin_shares`),`kw_avg_max`,`self_reference_avg_sharess`(`sravg_shares`),`LDA_02`,`kw_avg_min`,`LDA_01`,`n_non_stop_unique_tokens`(`n_nstop_utokens`).  
The plot shows correlation with the response variable `shares` and the
other various combinations. Larger circles indicate stronger positive
(blue) or negative (red) correlation with correlation values on the
lower portion of the plot.

``` r
##Reduce variable name length for later plotting
## Otherwise var names overwrite Title no matter
##  how many other size tweaks were made
corrData<-channelData %>% 
  mutate(sravg_shares=self_reference_avg_sharess,
         srmin_shares=self_reference_min_shares,
         n_nstop_utokens=n_non_stop_unique_tokens)

Correlation<-cor(select(corrData, shares, kw_min_avg,
        kw_max_avg, LDA_03, srmin_shares,
        kw_avg_max, sravg_shares, LDA_02,
        kw_avg_min, LDA_01, n_nstop_utokens),
        method = "spearman")

corrplot(Correlation,type="upper",tl.pos="lt", tl.cex = .70)
corrplot(Correlation,type="lower",method="number",
         add=TRUE,diag=FALSE,tl.pos="n",tl.cex = .70,number.cex = .75,
         title = 
           "Correlation Plot of Shares and Variables of Interest",
         mar=c(0,0,.50,0),cex.main = .75)
```

![](images/socmed/corrplot-1.png)<!-- -->

The following two scatterplots illustrate the relationship between
response article shares `shares` and predictor average keyword (max
shares) `kw_max_ave`. `kw_max_ave` was chosen because it was one of the
potential predictors examined in the previous correlation plot.

Both scatterplots plot these variables and add a simple linear
regression line to the graph.

For either graph, an upward relationship indicates higher average
keyword values tend towards more article shares. A negative relation
would indicate a lower average keyword values tend towards more article
shares.

In addition, both graphs use differing color for weekday and weekend
articles so that we can spot any possible trends with those values too.

The first scatterplot uses the default R generated axes so that
potential outliers or significant observations can be observed.

The second scatterplot reduces the scale of both axes to make it easier
to spot relationships for the majority of data that occur within these
bounds.

``` r
###Create new factor version of weekend variable 
### to use later in graphs
scatterData<-channelData %>% 
  mutate(dayType=ifelse(is_weekend,"Weekend","Weekday"))
scatterData$dayType<-as.factor(scatterData$dayType)

###First scatter plot with ALL data 
g<-ggplot(data = scatterData,
          aes(x= kw_max_avg,y=shares))
g + geom_point(aes(color=dayType)) +
  geom_smooth(method = lm) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(x="Avg. keyword (max. shares)", y="Article Shares",
       title = "Scatter Plot of Article Shares Versus Avg. keyword (max. shares)",color="") 
```

![](images/socmed/scatterplots%20-1.png)<!-- -->

``` r
###Second scatter plot with reduced axes
g<-ggplot(data = scatterData,
          aes(x= kw_max_avg,y=shares))
g + geom_point(aes(color=dayType)) +
  geom_smooth(method = lm) +
  ylim(0,10000) +
  xlim(0,20000) +
    labs(x="Avg. keyword (max. shares)", y="Article Shares",
       title = "Scatter Plot of Article Shares Versus Avg. keyword (max. shares)",
       color="")
```

![](images/socmed/scatterplots%20-2.png)<!-- -->

The bar plot below shows cumulative article publications for each day of
the week, with higher bars indicating more publications. However, days
with the largest number of publications are not necessarily ones with
the most article shares, as seen in the subsequent box plot.

``` r
# Subset columns to include only weekday_is_*
weekdayData <- channelData %>% select(starts_with("weekday_is"))

# Calculate sum of articles published in each week day
articlesPublished <- lapply(weekdayData, function(c) sum(c=="1"))

# Use factor to set specific order in bar plot
weekPubDF <- data.frame(weekday=c("Monday", "Tuesday", "Wednesday", 
                           "Thursday", "Friday", "Saturday", "Sunday"),
                count=articlesPublished)
weekPubDF$weekday = factor(weekPubDF$weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                           "Thursday", "Friday", "Saturday"))

# Create bar plot with total publications by day
weekdayBar <- ggplot(weekPubDF, aes(x = weekday, y = articlesPublished)) + geom_bar(stat = "identity", color = "#123456", fill = "#0072B2") 
weekdayBar + labs(x = "Day", y = "Number published",
       title = "Article publications by day of week")
```

![](images/socmed/bar%20plot-1.png)<!-- -->

The boxplot below examines the day of article publication
(Monday-Sunday) and the associated distribution of article `shares`. The
median line indicates the center of the distribution of `shares`, and
comparatively high medians indicate days that have relatively high
circulation of Mashable articles in social media networks. For days in
which the median is closer to the lower quartile (and where the upper
whisker may be taller than the lower whisker), the distribution is
skewed to the right. Conversely, a median that is closer to the upper
quartile indicates a distribution that is skewed to the left. Days with
relatively taller boxplots also have greater variability of `shares`.

``` r
# Subset columns to include only weekday_is_*, shares,
# create categorical variable, "day", denoting day of week (Mon-Sun)
medianShares <- channelData %>% select(starts_with("weekday_is"), shares) %>% mutate(day = NA)

# Populate "day"
for (i in 1:nrow(medianShares)) {
  if (medianShares$weekday_is_monday[i] == 1) {
    medianShares$day[i] = "Monday"
  }
  else if (medianShares$weekday_is_tuesday[i] == 1) {
    medianShares$day[i] = "Tuesday"
  }
  else if (medianShares$weekday_is_wednesday[i] == 1) {
    medianShares$day[i] = "Wednesday"
  }
  else if (medianShares$weekday_is_thursday[i] == 1) {
    medianShares$day[i] = "Thursday"
  }
  else if (medianShares$weekday_is_friday[i] == 1) {
    medianShares$day[i] = "Friday"
  }
  else if (medianShares$weekday_is_saturday[i] == 1) {
    medianShares$day[i] = "Saturday"
  }
  else if (medianShares$weekday_is_sunday[i] == 1) {
    medianShares$day[i] = "Sunday"
  }
  else {
    medianShares$day[i] = NA
  }
}

# Transform "day" into factor with levels to control order of boxplots
medianShares$day <- factor(medianShares$day, 
                           levels = c("Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday", "Saturday", "Sunday"))
```

``` r
# Plot distribution of shares for each day of the week
sharesBox <- ggplot(medianShares, aes(x = day, y = shares, fill = day))

sharesBox + geom_boxplot(outlier.shape = NA) + 
  # Exclude extreme outliers, limit range of y-axis
  coord_cartesian(ylim = quantile(medianShares$shares, c(0.1, 0.95))) +
  # Remove legend after coloration
  theme(legend.position = "none") +
  labs(x = "Day", y = "Shares",
       title = "Distribution of article shares for each publication day") + scale_fill_brewer(palette = "Spectral")
```

![](images/socmed/boxpot%20-1.png)<!-- -->

For the empirical cumulative distribution function (ECDF) below, the
`dplyr` ranking function `ntile()` divides `shares` into four groups.
Observations with the fewest shares are placed into group 1, those with
the most shares are placed into group 4, and intermediaries reside in
groups 2 and 3. The horizontal axis lists word count, and the vertical
axis lists the percentage of content with that word count. A divergence
of the colored lines suggests that the number of words differs in
content with the fewest and most shares. At any given percentage of
content (y-value), curves further to the right correspond to more words
within the associated `shares` group. Groups with curves that are
further to the left indicate fewer words in that percentage of content.

``` r
# Create variable to for binning the shares
binnedShares <- channelData %>% mutate(shareQuantile = ntile(channelData$shares, 4))
binnedShares <- binnedShares %>% mutate(totalMedia = num_imgs + num_videos)

# Render and label word count ECDF, group by binned shares
avgWordHisto <- ggplot(binnedShares, aes(x = n_tokens_content, colour = shareQuantile))
avgWordHisto + stat_ecdf(geom = "step", aes(color = as.character(shareQuantile))) +
  labs(title="ECDF - Number of words in the article \ngrouped by article shares (ranked)",
     y = "ECDF", x="Word count") + xlim(0,2000) + 
  scale_colour_brewer(palette = "Spectral", name = "Article shares \n(group rank)")
```

![](images/socmed/ecdf-1.png)<!-- -->

## Modeling

### Splitting Data

Per project requirements, the data for each channel are split with 70%
of the data becoming training data and 30% of the data becoming test
data.

``` r
#Using set.seed per suggestion so that work will be reproducible
set.seed(20)

dataIndex <-createDataPartition(channelData$shares, p = 0.7, list = FALSE)

channelTrain <-channelData[dataIndex,]
channelTest <-channelData[-dataIndex,]
```

### Linear Regression Models

Linear regression models describe a linear relationship between a
response variable and one or more explanatory variables. Models with one
explanatory variable are called simple linear regression models and
models with more than one explanatory variable are called multiple
linear regression models. Multiple linear regression models can include
polynomial and interaction terms. Each explanatory variable has an
associated estimated parameter. All linear regression models are linear
in the parameters.

For linear regression, explanatory variables can be continuous or
categorical. However, response variables are only continuous for linear
regression models.

Linear regression models are fit with training data by minimizing the
sum of squared errors. Model fitting results in a line for simple linear
regression and a saddle for multiple linear regression.

The first linear regression model contains predictors that encompass
content keywords, sentiment and subjectivity, the length of content (the
effects of which were gleaned previously from the ECDF), and link
citations.

``` r
# Parallel cluster setup
cl <- makePSOCKcluster(6)
registerDoParallel(cl)

# Linear regression with subset of predictors (p-value < 0.1) selected after performing 
# least squares fit on the entire set of predictors. 
lmFit1 <- train(shares ~ kw_avg_avg + kw_max_avg + kw_min_avg + 
    num_hrefs + self_reference_min_shares + global_subjectivity + 
    num_self_hrefs + n_tokens_title + n_tokens_content + n_unique_tokens + 
    average_token_length + kw_min_max + num_keywords + kw_max_min + abs_title_subjectivity + 
    global_rate_positive_words, 
    data = channelTrain,
               method = "lm",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", 
                                        number = 5))
stopCluster(cl)
```

The second linear regression model contains main effects for all the
predictors listed earlier in the correlation plot.

``` r
cl <- makePSOCKcluster(6)
registerDoParallel(cl)

# Linear regression 
# Using same vars as in corrplot 
# See corrplot for why these were chosen
lmFit2 <- train(shares ~ kw_min_avg +
        kw_max_avg + LDA_03 + self_reference_min_shares +
        kw_avg_max + self_reference_avg_sharess + LDA_02 +
        kw_avg_min + LDA_01 + n_non_stop_unique_tokens, 
               data = channelTrain,
               method = "lm",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", 
                                        number = 10))


stopCluster(cl)

lmFit2
```

### Random Forest Model

Random forest models aggregate results from many sample decision trees.
Those sample trees are produced using bootstrap samples created using
resampling with replacement. A tree is trained on each bootstrap sample,
resulting in a prediction based on that training sample data. Results
from all bootstrap samples are averaged to arrive at a final prediction.

Both bagging and random forest methods use bootstrap sampling with
decision trees. However, bagging includes all predictors which can lead
to less reduction in variance when strong predictors exist. Unlike
bagging, random forests do not use all predictors but use a random
subset of predictors for each bootstrap tree fit. Random forests usually
have a better fit than bagging models.

In this particular case, the response `shares` is continuous and we are
working with regression trees. The `mtry` tuning parameter controls how
many random predictors are used in the bootstrap samples. An `mtry` of 1
to 30 was chosen as a way to evaluate up to 30 predictors. These values
were chosen to work within available computing constraints. Five fold
cross validation is used to choose the optimal mtry value corresponding
to the lowest RMSE.

``` r
##Run time presented a challenge so parallel processing was used
##Followed Parallel instructions on caret page
##   https://topepo.github.io/caret/parallel-processing.html

##Various mtry values were tried with a 20 minute runtime goal
##  A 20 minute per channel runtime corresponds 
##  to a total of about 2 hours model fit for all 6 channels

##mtry 1:30 was chosen because it was close to 20 minutes
##mtry 1:20 had 10 minute runtime and 1:30 took 30 minutes

##repeatedcv was evaluated but took over 30 minutes 
## thus repeats were not used


cl <- makePSOCKcluster(6)
registerDoParallel(cl)

rfFit <- train(shares ~ ., data = channelTrain,
               method = "rf",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv",
                                number = 5),
               tuneGrid = data.frame(mtry = 1:30))

stopCluster(cl)

rfFit
```

    ## Random Forest 
    ## 
    ## 1628 samples
    ##   52 predictor
    ## 
    ## Pre-processing: centered (52), scaled (52) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 1303, 1302, 1302, 1302, 1303 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared    MAE     
    ##    1    4822.679  0.09627590  2503.748
    ##    2    4771.547  0.10750439  2501.880
    ##    3    4766.368  0.10692717  2498.294
    ##    4    4761.681  0.10728913  2523.412
    ##    5    4770.738  0.10438991  2530.661
    ##    6    4780.182  0.10045061  2536.438
    ##    7    4787.408  0.09890500  2543.214
    ##    8    4798.701  0.09499722  2555.234
    ##    9    4788.466  0.10048097  2544.334
    ##   10    4818.426  0.08868189  2562.268
    ##   11    4831.611  0.08568263  2572.027
    ##   12    4822.275  0.08901293  2567.055
    ##   13    4820.464  0.09266912  2572.488
    ##   14    4838.925  0.08602783  2575.971
    ##   15    4835.983  0.08784034  2581.253
    ##   16    4832.049  0.08875772  2572.530
    ##   17    4851.962  0.08287653  2583.532
    ##   18    4838.720  0.08838009  2574.886
    ##   19    4878.784  0.07739340  2591.915
    ##   20    4869.561  0.08209459  2596.146
    ##   21    4871.233  0.08017346  2594.588
    ##   22    4866.951  0.08074285  2589.955
    ##   23    4904.036  0.07405107  2605.376
    ##   24    4885.679  0.07737309  2595.856
    ##   25    4903.512  0.07343075  2594.956
    ##   26    4907.403  0.07310772  2610.616
    ##   27    4898.035  0.07456466  2614.090
    ##   28    4915.597  0.07152662  2616.756
    ##   29    4922.507  0.07062824  2621.703
    ##   30    4913.313  0.07298555  2608.484
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 4.

After fitting the random forest model, the following variable importance
plot is created. The top ten most important predictors are plotted using
a scale of 0 to 100.

``` r
rfImp <- varImp(rfFit, scale = TRUE)
plot(rfImp,top = 10, main="Random Forest Model\nTop 10 Importance Plot")
```

![](images/socmed/random%20forest%20importance%20plot-1.png)<!-- -->

### Boosted Tree Model

Boosting is a general method whereby decision trees are grown
sequentially using residuals (the differences between observed values
and predicted values of a variable) as the response. Initial prediction
values start at 0 for all combinations of predictors, so that the first
set of residuals matches the observed values in our data. To mitigate
low bias and high variance, contributions from subsequent trees are
scaled with a shrinkage parameter, *λ*. The value of this parameter is
generally small (0.01 or 0.001), which slows tree growth and tampers
overfitting (James et al., 2021).

``` r
# Re-allocate cores for parallel computing
cl <- makePSOCKcluster(6)
registerDoParallel(cl)


# Boosted tree fit with tuneLength (let function decide parameter combinations)
boostedTreeFit <- train(shares ~ ., data = channelTrain,
               method = "gbm",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", 
                                        number = 5),  
               tuneLength = 5)
```

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 26092919.3349             nan     0.1000 108409.4404
    ##      2 25954286.9517             nan     0.1000 118137.3773
    ##      3 25745155.2226             nan     0.1000 97162.1814
    ##      4 25620329.6132             nan     0.1000 91382.9080
    ##      5 25507771.3309             nan     0.1000 25077.8138
    ##      6 25420229.5574             nan     0.1000 14175.6627
    ##      7 25300517.2294             nan     0.1000 75531.6924
    ##      8 25179114.6724             nan     0.1000 35892.6093
    ##      9 25074480.0664             nan     0.1000 20481.2551
    ##     10 24951154.1928             nan     0.1000 18004.1296
    ##     20 24335511.3935             nan     0.1000 -36286.1564
    ##     40 23412444.6736             nan     0.1000 -881.3577
    ##     60 22829400.0676             nan     0.1000 -18864.0229
    ##     80 22450103.0758             nan     0.1000 -25870.8443
    ##    100 22034180.0269             nan     0.1000 -7457.5438

``` r
# Define tuning parameters based on $bestTune from the permutations above
nTrees <- boostedTreeFit$bestTune$n.trees
interactionDepth = boostedTreeFit$bestTune$interaction.depth
minObs = boostedTreeFit$bestTune$n.minobsinnode
shrinkParam <- boostedTreeFit$bestTune$shrinkage

# Boosted tree fit with defined parameters
bestBoostedTree <- train(shares ~ ., data = channelTrain,
               method = "gbm",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", 
                                        number = 5),  
               tuneGrid = expand.grid(n.trees = nTrees, interaction.depth = interactionDepth,
                                      shrinkage = shrinkParam, n.minobsinnode = minObs))
```

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 26085988.3445             nan     0.1000 155686.8197
    ##      2 25781882.3126             nan     0.1000 187633.3507
    ##      3 25696732.9380             nan     0.1000 27075.5392
    ##      4 25546517.6177             nan     0.1000 95638.8114
    ##      5 25437635.4524             nan     0.1000 31618.2713
    ##      6 25300046.3212             nan     0.1000 34491.1051
    ##      7 25229421.3498             nan     0.1000 18512.7325
    ##      8 25130835.5518             nan     0.1000 13914.8991
    ##      9 25030687.9517             nan     0.1000 80218.8198
    ##     10 24968689.3651             nan     0.1000 39996.9295
    ##     20 24339295.9519             nan     0.1000 -28601.8150
    ##     40 23395579.6737             nan     0.1000 37456.1086
    ##     60 22753727.6392             nan     0.1000 -17406.5499
    ##     80 22308387.4733             nan     0.1000 2267.0306
    ##    100 22078809.8372             nan     0.1000 -71981.5425

``` r
stopCluster(cl)
```

## Model Comparisons

After models were fit with training data, we do predictions with testing
data. Finally, RMSE metrics are extracted and compared. The model with
lowest RMSE is presented as the winning model.

``` r
# Predict using test data
predictLM1 <- predict(lmFit1, newdata = channelTest)

# Metrics
RMSELM1 <- postResample(predictLM1, obs = channelTest$shares)["RMSE"][[1]]
RMSELM1
```

    ## [1] 6457.23

``` r
# Store value for model comparison
modelPerformance <- tibble(RMSE = RMSELM1, Model = "Linear regression 1")
```

``` r
predictLM2 <- predict(lmFit2, newdata = channelTest)
RMSELM2<-postResample(predictLM2, channelTest$shares)["RMSE"][[1]]
RMSELM2
```

    ## [1] 7293.085

``` r
modelPerformance <- add_row(modelPerformance, RMSE = RMSELM2, Model = "Linear regression 2")
```

``` r
predictRF <- predict(rfFit, newdata = channelTest)
RMSERF<-postResample(predictRF, channelTest$shares)["RMSE"]
RMSERF
```

    ##     RMSE 
    ## 6248.886

``` r
modelPerformance <- add_row(modelPerformance, RMSE = RMSERF, Model = "Random forest")
```

``` r
# Predict using test data
predictGBM <- predict(bestBoostedTree, newdata = channelTest)

# Metrics
RMSEGBM <- postResample(predictGBM, obs = channelTest$shares)["RMSE"]
RMSEGBM
```

    ##    RMSE 
    ## 6267.83

``` r
modelPerformance <- add_row(modelPerformance, RMSE = RMSEGBM, Model = "Boosted tree")
```

``` r
# Select row with lowest value of RMSE.
selectModel <- modelPerformance %>% slice_min(RMSE)
selectModel
```

    ## # A tibble: 1 x 2
    ##    RMSE Model        
    ##   <dbl> <chr>        
    ## 1 6249. Random forest

Based on the preceding analyses with test data, the Random forest model
yields the lowest RMSE - 6248.8861708.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-10.1007/978-3-319-23485-4_53" class="csl-entry">

Fernandes, K., Vinagre, P., & Cortez, P. (2015). A proactive intelligent
decision support system for predicting the popularity of online news. In
F. Pereira, P. Machado, E. Costa, & A. Cardoso (Eds.), *Progress in
artificial intelligence* (pp. 535–546). Springer International
Publishing.

</div>

<div id="ref-2021" class="csl-entry">

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2021). *An
introduction to statistical learning*. Springer US.
<https://doi.org/10.1007/978-1-0716-1418-1>

</div>

</div>
