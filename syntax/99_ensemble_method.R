
################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition: Online News Popularity Prediction
# Script  : predictor.R
################################################################################
# Authors : Roger Cusco, Matthew Sudmann-Day, Miquel Torrens
# (C) The Unlikelies 2016
################################################################################

################################################################################
# Load, and install if necessary, required packages.
################################################################################
if (!require(randomForest))
{
  install.packages("randomForest")
  library(randomForest)
}
if (!require(caTools))
{
  install.packages("caTools")
  library(caTools)
}
if (!require(dplyr))
{
  install.packages("dplyr")
  library(dplyr)
}
if (!require(gbm))
{
  install.packages("gbm")
  library(gbm)
}
if (!require(assertthat))
{
  install.packages("assertthat")
  library(assertthat)
}
if (!require(maboost))
{
  install.packages("maboost")
  library(maboost)
}

################################################################################
#' prepareData
#'
#' Use features, outlier detection, and hyperparameter optimization brought to
#' you by the Unlikelies to predict online news popularity.
#' 
#' The Unlikelies (c) 2016.02.12
#' Authors: Roger Cusco, Matthew Sudmann-Day, Miquel Torrens
#'
#' DISCLAIMER: Evaluator's eyes only. This function contains confidential
#' procedures for the Kaggle competition results and must not be released or
#' displayed to third parties under any circumstance without express written
#' permission by the authors.
#' 
#' @param test A data frame containing a test set with 61 features.
#' @param train An optional data frame containing a training set with 62 features. If not provided, a default internal training set is used.
#' @param verbose TRUE if the function should print progress.
#' @param store TRUE if the resulting predictions should be saved to a CSV file.
#' @param dest.folder Folder in which the results will be saved.
#' @param visible TRUE if the results should be visible when returned.
#' @param ntree Number of trees in the random forest.
#' @param nodesize Ending node size for the random forest.
#' @param seed Starting random seed for the random forest.
#' @param ... Additional parameters to the model generator randomForest().
################################################################################
prepareData <- function(test, train = NULL, verbose = TRUE, store = FALSE,
                        dest.folder = NULL, visible = TRUE, ntree = NA,
                        nodesize = NA, seed = NA, ...) {
  
  # Required packages
  if (! require(randomForest)) {
    stop('required package not installed: "randomForest"')
  } else {
    if (verbose == TRUE) {
      cat('Loaded package: randomForest\n')  
    }    
  }
  
  if (is.null(train)) {
    train <- NULL
  }
  
  # Assert features
  if (ncol(train) != 62) {
    stop('"train" set does not have the correct amount of features (62).')
  }
  if (ncol(test) != 61) {
    stop('"test" set does not have the correct amount of features (61).')
  }
  
  ##############################################################################
  # Training set
  # Define interesting original variables
  ori.vars <- colnames(train)[! colnames(train) %in% c('url', 'id')]
  nat.vars <- c('popularity', 'timedelta', 'n_tokens_title', 'n_tokens_content',
                'num_hrefs', 'num_self_hrefs', 'num_imgs', 'num_videos',
                'data_channel_is_lifestyle', 'data_channel_is_entertainment',
                'data_channel_is_bus', 'data_channel_is_socmed',
                'data_channel_is_tech', 'data_channel_is_world', 'kw_min_min',
                'kw_max_min', 'kw_avg_min', 'kw_min_max', 'kw_avg_max',
                'kw_min_avg', 'kw_max_avg', 'kw_avg_avg',
                'self_reference_min_shares', 'self_reference_max_shares',
                'self_reference_avg_sharess', 'weekday_is_monday',
                'weekday_is_tuesday', 'weekday_is_wednesday',
                'weekday_is_thursday', 'weekday_is_friday',
                'weekday_is_saturday', 'weekday_is_sunday', 'is_weekend',
                'LDA_01', 'LDA_02', 'LDA_03', 'LDA_04', 'title_subjectivity',
                'title_sentiment_polarity', 'abs_title_sentiment_polarity')
  
  # See if some observations can be considered outliers
  killed <- c()
  extremes <- as.data.frame(matrix(ncol = 0, nrow = 2))
  for (col in nat.vars) {
    if (class(train[, col]) != 'character') {
      # Those in the tails of the distributions may be considered outliers
      thres <- quantile(train[, col], probs = c(0.005, 0.995))
      extremes <- cbind(extremes, as.data.frame(thres))
      colnames(extremes)[ncol(extremes)] <- col
      
      # Only those stricly outside interval (in case of high concentration)
      outliers <- which(train[, col] < thres[1] | 
                          train[, col] > thres[2])
      killed <- unique(c(killed, outliers))
    }
  }
  
  # Report the results
  if (verbose == TRUE) {
    cat(    'Total number of observations ("train"):', nrow(train), '\n')
    cat(    'Number of outliers detected ("train"):', length(killed), '\n')
  }
  
  # Creating new variables
  # Binary for outliers
  train[, 'is_outlier'] <- 0
  train[killed, 'is_outlier'] <- 1
  
  # Binary for timedelta
  train[, 'timedelta_bin'] <- as.numeric(train[, 'timedelta'] >= 400)
  train[, 'timedelta_bin_int'] <- train[, 'timedelta'] *
    train[, 'timedelta_bin']
  
  # Categorical for kw_min_min
  train[, 'kw_min_min_cat0'] <- 0
  train[, 'kw_min_min_cat1'] <- 0
  train[, 'kw_min_min_cat2'] <- 0
  train[which(train[, 'kw_min_min'] ==  -1), 'kw_min_min_cat0'] <- 1
  train[which(train[, 'kw_min_min'] ==   4), 'kw_min_min_cat1'] <- 1
  train[which(train[, 'kw_min_min'] == 217), 'kw_min_min_cat2'] <- 1
  
  # Binary kw_avg_max
  train[, 'kw_avg_max_bin'] <- as.numeric(train[, 'kw_avg_max'] >= 1e5)
  
  # Binary kw_min_avg
  train[, 'kw_min_avg_bin'] <- as.numeric(train[, 'kw_min_avg'] >= 0)
  
  # Binary LDA_0x
  train[, 'LDA_01_bin'] <- as.numeric(train[, 'LDA_01'] < 0.1)
  train[, 'LDA_02_bin'] <- as.numeric(train[, 'LDA_02'] < 0.1)
  train[, 'LDA_03_bin'] <- as.numeric(train[, 'LDA_03'] < 0.1)
  train[, 'LDA_04_bin'] <- as.numeric(train[, 'LDA_04'] < 0.1)
  train[, 'LDA_01_bin_int'] <- train[, 'LDA_01'] * train[, 'LDA_01_bin']
  train[, 'LDA_02_bin_int'] <- train[, 'LDA_02'] * train[, 'LDA_02_bin']
  train[, 'LDA_03_bin_int'] <- train[, 'LDA_03'] * train[, 'LDA_03_bin']
  train[, 'LDA_04_bin_int'] <- train[, 'LDA_04'] * train[, 'LDA_04_bin']
  
  # Categorical title_subjectivity
  q0 <- which(train[, 'title_subjectivity'] <  0.15)
  q1 <- which(train[, 'title_subjectivity'] >= 0.15)
  q2 <- which(train[, 'title_subjectivity'] >= 0.6)
  train[, 'title_subjectivity_cat0'] <- 0
  train[, 'title_subjectivity_cat1'] <- 0
  train[, 'title_subjectivity_cat2'] <- 0
  train[q0, 'title_subjectivity_cat0'] <- 1
  train[q1, 'title_subjectivity_cat1'] <- 1
  train[q2, 'title_subjectivity_cat2'] <- 1
  
  # Categorical title_sentiment_polarity
  q0 <- which(train[, 'title_sentiment_polarity'] <  0)
  q1 <- which(train[, 'title_sentiment_polarity'] == 0)
  q2 <- which(train[, 'title_sentiment_polarity'] >  0)
  train[, 'title_sentiment_polarity_cat0'] <- 0
  train[, 'title_sentiment_polarity_cat1'] <- 0
  train[, 'title_sentiment_polarity_cat2'] <- 0
  train[q0, 'title_sentiment_polarity_cat0'] <- 1
  train[q1, 'title_sentiment_polarity_cat1'] <- 1
  train[q2, 'title_sentiment_polarity_cat2'] <- 1
  
  # Extract the date
  urls <- strsplit(train[, 'url'], '/')
  days <- sapply(urls, `[`, 6)
  years <- sapply(urls, `[`, 4)
  month <- sapply(urls, `[`, 5)
  dates <- paste(years, month, days, sep = '-')
  
  # Date
  day.one <- as.Date('2013-01-01', '%Y-%m-%d')
  train[, 'date'] <- as.Date(dates, '%Y-%m-%d')
  train[, 'since_20130101'] <- as.numeric(train[, 'date'] - day.one)
  
  # Year
  train[, 'year'] <- years
  train[, 'is_2013'] <- as.numeric(train[, 'year'] == '2013')
  train[, 'is_2014'] <- as.numeric(train[, 'year'] == '2014')
  
  # Month
  train[, 'month'] <- month
  train[, 'is_jan'] <- as.numeric(train[, 'month'] == '01')
  train[, 'is_feb'] <- as.numeric(train[, 'month'] == '02')
  train[, 'is_mar'] <- as.numeric(train[, 'month'] == '03')
  train[, 'is_apr'] <- as.numeric(train[, 'month'] == '04')
  train[, 'is_may'] <- as.numeric(train[, 'month'] == '05')
  train[, 'is_jun'] <- as.numeric(train[, 'month'] == '06')
  train[, 'is_jul'] <- as.numeric(train[, 'month'] == '07')
  train[, 'is_aug'] <- as.numeric(train[, 'month'] == '08')
  train[, 'is_sep'] <- as.numeric(train[, 'month'] == '09')
  train[, 'is_oct'] <- as.numeric(train[, 'month'] == '10')
  train[, 'is_nov'] <- as.numeric(train[, 'month'] == '11')
  train[, 'is_dec'] <- as.numeric(train[, 'month'] == '12')
  
  # Day
  train[, 'day'] <- days
  
  # Season
  train[, 'season'] <- '4'  # Winter
  train[which(paste(month, days) > '03 20'), 'season'] <- '1'  # Spring
  train[which(paste(month, days) > '06 20'), 'season'] <- '2'  # Summer
  train[which(paste(month, days) > '09 20'), 'season'] <- '3'  # Autumn
  train[which(paste(month, days) > '12 20'), 'season'] <- '4'  # Winter
  train[, 'is_spring'] <- as.numeric(train[, 'season'] == '1')
  train[, 'is_summer'] <- as.numeric(train[, 'season'] == '2')
  train[, 'is_autumn'] <- as.numeric(train[, 'season'] == '3')
  train[, 'is_winter'] <- as.numeric(train[, 'season'] == '4')
  
  # Daily scores
  # (checked days from beginning to end have some news)
  counts <- tapply(rep(1, nrow(train)), train[, 'date'], sum)
  avg <- tapply(train[, 'popularity'], train[, 'date'], mean)
  sds <- tapply(train[, 'popularity'], train[, 'date'], sd)
  m1 <- match(as.character(train[, 'date']), names(counts))
  m2 <- match(as.character(train[, 'date']), names(avg))
  m3 <- match(as.character(train[, 'date']), names(sds))
  train[, 'day_news'] <- counts[m1]
  train[, 'day_avg_pop'] <- avg[m2]
  train[, 'day_sd_pop'] <- sds[m3]
  
  # New variables
  new.vars <- c('date', 'since_20130101', 'year', 'month', 'day', 'is_2013',
                'is_2014', 'is_jan', 'is_feb', 'is_mar', 'is_apr', 'is_may',
                'is_jun', 'is_jul', 'is_aug', 'is_sep', 'is_oct', 'is_nov',
                'is_dec', 'season', 'timedelta_bin', 'timedelta_bin_int',
                'kw_min_min_cat0', 'kw_min_min_cat1', 'kw_min_min_cat2',
                'kw_avg_max_bin', 'kw_min_avg_bin', 'LDA_01_bin', 'LDA_02_bin',
                'LDA_03_bin', 'LDA_04_bin', 'title_subjectivity_cat0',
                'title_subjectivity_cat1', 'title_subjectivity_cat2',
                'is_outlier', 'title_sentiment_polarity_cat0',
                'title_sentiment_polarity_cat1',
                'title_sentiment_polarity_cat2', 'day_news', 'day_avg_pop',
                'day_sd_pop')
  
  # Standardized features
  std.vars <- c('n_tokens_title', 'n_tokens_content', 'kw_avg_min',
                'kw_max_max', 'kw_min_avg')
  for (col in std.vars) {
    end.col <- paste(col, 'std', sep = '_')
    train[, end.col] <- (train[, col] - mean(train[, col])) / sd(train[, col])
  }
  
  # Logarithmic variables
  log.vars <- c('timedelta', 'kw_max_min', 'kw_min_max', 'kw_avg_max',
                'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares',
                'self_reference_max_shares', 'self_reference_avg_sharess')
  for (col in log.vars) {
    end.col <- paste('log', col, sep = '_')
    train[, end.col] <- log(train[, col] + 1)
  }
  
  # New features added
  std.cols <- paste(std.vars, 'std', sep = '_')
  log.cols <- paste('log', log.vars, sep = '_')
  
  # Final variables
  final.vars <- c(nat.vars, new.vars, std.cols, log.cols)
  
  ##############################################################################
  # Test set
  # See if some observations can be considered outliers
  killed <- c()
  for (col in nat.vars) {
    if (col != 'popularity' && class(test[, col]) != 'character') {
      outliers <- which(test[, col] > max(extremes[, col]) |
                          test[, col] < min(extremes[, col]))
      killed <- unique(c(killed, outliers))
    }
  }
  
  #killed <- killed[-which(killed %in% fives)]
  cat('Total number of observations ("test"):', nrow(test), '\n')
  cat('Number of outliers detected ("test"):', length(killed), '\n')
  
  # Creating new variables (same as for training set)
  # Binary for outliers
  test[, 'is_outlier'] <- 0
  test[killed, 'is_outlier'] <- 1
  
  # Binary for timedelta
  test[, 'timedelta_bin'] <- as.numeric(test[, 'timedelta'] >= 400)
  test[, 'timedelta_bin_int'] <- test[, 'timedelta'] * test[, 'timedelta_bin']
  
  # Categorical for kw_min_min
  test[, 'kw_min_min_cat0'] <- 0
  test[, 'kw_min_min_cat1'] <- 0
  test[, 'kw_min_min_cat2'] <- 0
  test[which(test[, 'kw_min_min'] ==  -1), 'kw_min_min_cat0'] <- 1
  test[which(test[, 'kw_min_min'] ==   4), 'kw_min_min_cat1'] <- 1
  test[which(test[, 'kw_min_min'] == 217), 'kw_min_min_cat2'] <- 1
  
  # Binary kw_avg_max
  test[, 'kw_avg_max_bin'] <- as.numeric(test[, 'kw_avg_max'] >= 1e5)
  
  # Binary kw_min_avg
  test[, 'kw_min_avg_bin'] <- as.numeric(test[, 'kw_min_avg'] >= 0)
  
  # Binary LDA_0x
  test[, 'LDA_01_bin'] <- as.numeric(test[, 'LDA_01'] < 0.1)
  test[, 'LDA_02_bin'] <- as.numeric(test[, 'LDA_02'] < 0.1)
  test[, 'LDA_03_bin'] <- as.numeric(test[, 'LDA_03'] < 0.1)
  test[, 'LDA_04_bin'] <- as.numeric(test[, 'LDA_04'] < 0.1)
  test[, 'LDA_01_bin_int'] <- test[, 'LDA_01'] * test[, 'LDA_01_bin']
  test[, 'LDA_02_bin_int'] <- test[, 'LDA_02'] * test[, 'LDA_02_bin']
  test[, 'LDA_03_bin_int'] <- test[, 'LDA_03'] * test[, 'LDA_03_bin']
  test[, 'LDA_04_bin_int'] <- test[, 'LDA_04'] * test[, 'LDA_04_bin']
  
  # Categorical title_subjectivity
  q0 <- which(test[, 'title_subjectivity'] <  0.15)
  q1 <- which(test[, 'title_subjectivity'] >= 0.15)
  q2 <- which(test[, 'title_subjectivity'] >= 0.6)
  test[, 'title_subjectivity_cat0'] <- 0
  test[, 'title_subjectivity_cat1'] <- 0
  test[, 'title_subjectivity_cat2'] <- 0
  test[q0, 'title_subjectivity_cat0'] <- 1
  test[q1, 'title_subjectivity_cat1'] <- 1
  test[q2, 'title_subjectivity_cat2'] <- 1
  
  # Categorical title_sentiment_polarity
  q0 <- which(test[, 'title_sentiment_polarity'] <  0)
  q1 <- which(test[, 'title_sentiment_polarity'] == 0)
  q2 <- which(test[, 'title_sentiment_polarity'] >  0)
  test[, 'title_sentiment_polarity_cat0'] <- 0
  test[, 'title_sentiment_polarity_cat1'] <- 0
  test[, 'title_sentiment_polarity_cat2'] <- 0
  test[q0, 'title_sentiment_polarity_cat0'] <- 1
  test[q1, 'title_sentiment_polarity_cat1'] <- 1
  test[q2, 'title_sentiment_polarity_cat2'] <- 1
  
  # Extract the date
  urls <- strsplit(test[, 'url'], '/')
  days <- sapply(urls, `[`, 6)
  years <- sapply(urls, `[`, 4)
  month <- sapply(urls, `[`, 5)
  dates <- paste(years, month, days, sep = '-')
  
  # Date
  day.one <- as.Date('2013-01-01', '%Y-%m-%d')
  test[, 'date'] <- as.Date(dates, '%Y-%m-%d')
  test[, 'since_20130101'] <- as.numeric(test[, 'date'] - day.one)
  
  # Year
  test[, 'year'] <- years
  test[, 'is_2013'] <- as.numeric(test[, 'year'] == '2013')
  test[, 'is_2014'] <- as.numeric(test[, 'year'] == '2014')
  
  # Month
  test[, 'month'] <- month
  test[, 'is_jan'] <- as.numeric(test[, 'month'] == '01')
  test[, 'is_feb'] <- as.numeric(test[, 'month'] == '02')
  test[, 'is_mar'] <- as.numeric(test[, 'month'] == '03')
  test[, 'is_apr'] <- as.numeric(test[, 'month'] == '04')
  test[, 'is_may'] <- as.numeric(test[, 'month'] == '05')
  test[, 'is_jun'] <- as.numeric(test[, 'month'] == '06')
  test[, 'is_jul'] <- as.numeric(test[, 'month'] == '07')
  test[, 'is_aug'] <- as.numeric(test[, 'month'] == '08')
  test[, 'is_sep'] <- as.numeric(test[, 'month'] == '09')
  test[, 'is_oct'] <- as.numeric(test[, 'month'] == '10')
  test[, 'is_nov'] <- as.numeric(test[, 'month'] == '11')
  test[, 'is_dec'] <- as.numeric(test[, 'month'] == '12')
  
  # Day
  test[, 'day'] <- days
  
  # Season
  test[, 'season'] <- '4'  # Winter
  test[which(paste(month, days) > '03 20'), 'season'] <- '1'  # Spring
  test[which(paste(month, days) > '06 20'), 'season'] <- '2'  # Summer
  test[which(paste(month, days) > '09 20'), 'season'] <- '3'  # Autumn
  test[which(paste(month, days) > '12 20'), 'season'] <- '4'  # Winter
  test[, 'is_spring'] <- as.numeric(test[, 'season'] == '1')
  test[, 'is_summer'] <- as.numeric(test[, 'season'] == '2')
  test[, 'is_autumn'] <- as.numeric(test[, 'season'] == '3')
  test[, 'is_winter'] <- as.numeric(test[, 'season'] == '4')
  
  # Daily scores
  m1 <- match(as.character(test[, 'date']), names(counts))
  m2 <- match(as.character(test[, 'date']), names(avg))
  m3 <- match(as.character(test[, 'date']), names(sds))
  test[, 'day_news'] <- counts[m1]
  test[, 'day_avg_pop'] <- avg[m2]
  test[, 'day_sd_pop'] <- sds[m3]
  
  # Standardized features
  for (col in std.vars) {
    end.col <- paste(col, 'std', sep = '_')
    test[, end.col] <- (test[, col] - mean(test[, col])) / sd(test[, col])
  }
  
  # Logarithmic variables
  for (col in log.vars) {
    end.col <- paste('log', col, sep = '_')
    test[, end.col] <- log(test[, col] + 1)
  }
  
  ##############################################################################
  # Model
  # Final variables of the model include the original one of the dataset
  model.vars <- unique(c('popularity', final.vars, ori.vars))
  model.varsT <- model.vars[model.vars != 'popularity']

  # Write our final selection of variables to a CSV for later lookup.
  write.csv(model.varsT, file="../data/selectedVariables.csv", row.names=FALSE)  
  
  return(test)
}

################################################################################
#' mode
#
#' @param v A vector of values.
################################################################################
mode <- function(v)
{
	# On a personal note, it's ridiculous that I have to write my own mode function in 2016.

  df <- data.frame(table(v), stringsAsFactors=FALSE)
  df$v <- levels(df$v)
  w <- which.max(df$Freq)
  return(as.numeric(df[w,1]))
}

################################################################################
#' loadDataForSubmission
#'
#' Load the test set and prepare it for submission.
#'
#' @param loadFromCache TRUE if we should attempt to load the output of this function from the cache.
################################################################################
loadDataForSubmission <- function(loadFromCache=FALSE)
{
  cat("LOADING DATA FOR SUBMISSION\n")

  # Load the output of this function from cache if appropriate.
  cacheFile = "../data/cache.loadDataForSubmission.RData"
  if (loadFromCache == TRUE && file.exists(cacheFile))
  {
    load(cacheFile) #data
    cat("  Loaded from cache.\n")
    return(data)
  }
  
  # Load the training and test sets.  The training set is only needed to help prepare the test set.
  load("../data/news_popularity_training.RData") #np.train
  load("../data/news_popularity_test.RData") #np.test

  # Prepare the test data set.
  data <- prepareData(np.test, np.train)
  
  # Ensure correct type-casting of various columns that are problematic for some models.
  data$year <- as.numeric(data$year)
  data$month <- as.numeric(data$month)
  data$day <- as.numeric(data$day)
  data$season <- as.numeric(data$season)
  data$date <- as.Date(data$date)
  
  # Save the result in the cache.
  save(data, file=cacheFile)
  return(data)
}

################################################################################
#' loadAllTrainingData
#'
#' Load and prepare the training data.  Optionally set aside a portion for final
#' cross-validation. Add the 'cvSet' column to uniquely identify sets of the training
#' (non-set-aside) data for internal iterations of smaller sets of cross-validation.
#'
#' @param loadFromCache TRUE if we should attempt to load the output of this function from the cache.
################################################################################
loadAllTrainingData <- function(numSplits, setAside, loadFromCache=FALSE)
{
  cat(paste("LOADING ALL TRAINING DATA {", numSplits, " CV Splits}\n", sep=""))
  
  # Load the output of this function from cache if appropriate.
  cacheFile = paste("../data/cache.loadAllTrainingData_", numSplits, ".RData", sep="")
  if (loadFromCache == TRUE && file.exists(cacheFile))
  {
    load(cacheFile) #data
    cat("  Loaded from cache.\n")
    return(data)
  }
  
  load("../data/news_popularity_training.RData") #np.train
  data <- np.train
  
  truth <- data$popularity
  
  # Perform feature creation against the training set
  data <- prepareData(data[, -ncol(data)], data)
  
  data$year <- as.numeric(data$year)
  data$month <- as.numeric(data$month)
  data$day <- as.numeric(data$day)
  data$season <- as.numeric(data$season)
  data$date <- as.Date(data$date)
  
  # Restore the output variable.
  data$popularity <- truth
  
  if (setAside > 0)
  {
    spl <- sample.split(data, setAside)
    setAsideData <- data[spl,]
    save(setAsideData, file="../data/setAside.RData")
    data <- data[!spl,]
  }
  
  # Split the data up into a number of test sets by putting random numbers [1,4]
  # into the cvSet column.
  data$cvSet <- floor(runif(nrow(data), 1, numSplits + 0.99999))

  # Save the result in the cache.
  save(data, file=cacheFile)
  return(data)
}

################################################################################
#' getSelectedVariables
#'
#' Load our final selection of useful features from a CSV file.
################################################################################
getSelectedVariables <- function()
{
  # removed kw_min_avg_bin
  return(read.csv("../data/selectedVariables.csv", stringsAsFactors=FALSE)[,1])
}

################################################################################
#' splitForCrossValidation
#'
#' Split the outer training (non-set-aside) data into a smaller training and test set.
#' This split is done based on the previously created cvSet column.  Calling this
#' function with all values of index (from 1 to the number of splits) ensure all
#' combinations are used.
#'
#' @param data The complete non-set-aside data set.
#' @param index The index of the split to use.
################################################################################
splitForCrossValidation <- function(data, index)
{
  allIndexes <- c(1:10, 1:10)
  indexPos <- which(allIndexes == index)[1]
  testIndexes <- allIndexes[(0:4)+indexPos]
  
  train <- data[data$cvSet %in% testIndexes,]
  test <- data[!(data$cvSet %in% testIndexes),]

  return(list(train=train, test=test))
}

################################################################################
#' trainPrimaryRfModel
#'
#' Train the primary Random Forest model from which all other models are built.
#'
#' @param data The training data.
#' @param key A key to identify this batch of training data for caching purposes.
#' @param loadFromCache TRUE if we should attempt to load the output of this function from the cache.
################################################################################
trainPrimaryRfModel <- function(data, key, loadFromCache=FALSE)
{
  cat(paste("  TRAINING ORIGINAL RF MODEL {CV ", key, "}\n", sep=""))
  
   # Load the output of this function from cache if appropriate.
  cacheFile = paste("../data/cache.trainOriginalRfModel_", key, ".RData", sep="")
  if (loadFromCache == TRUE && file.exists(cacheFile))
  {
    load(cacheFile) #model
    cat("    Loaded from cache.\n")
    return(model)
  }
  
  vars <- getSelectedVariables()
  set.seed(3050)
  
  # Build the model.
  model <- randomForest(y=as.factor(data$popularity), x=data[, vars],
                        ntree=1200, nodesize=10)
  
  # Save the result in the cache.
  save(model, file=cacheFile)
  return(model)
}

################################################################################
#' trainMultipleRfModels
#'
#' Train a Random Forest model on binary data for every combination of first- and
#' second-choices of the primary Random Forest model.
#'
#' @param data The training data.
#' @param key A key to identify this batch of training data for caching purposes.
#' @param loadFromCache TRUE if we should attempt to load the output of this function from the cache.
################################################################################
trainMultipleRfModels <- function(data, key, loadFromCache=FALSE)
{
  cat(paste("  TRAINING RF MODELS {CV ", key, "} ", sep=""))
  
  # Load the output of this function from cache if appropriate.
  cacheFile = paste("../data/cache.trainMultipleRfModels_", key, ".RData", sep="")  
  if (loadFromCache == TRUE && file.exists(cacheFile))
  {
    load(cacheFile) #models
    cat("\n    Loaded from cache.\n")
    return(models)
  }
  
  # Initialization.
  models <- list()
  vars <- getSelectedVariables()
  
  # Loop through all possible combinations of included classifications.
  for (a1 in 0:1) {
    for (a2 in 0:1) {
      for (a3 in 0:1) {
        for (a4 in 0:1) {
          for (a5 in 0:1) {
            
            s <- c(a1,a2,a3,a4,a5)
            n <- s*(1:5)
            u <- n[n>0]
            
			# Now, we only consider combinations of two classes.
            if (sum(s) == 2)
            {
              key <- paste(u, collapse="")
              cat(paste(key, "...", sep=""))
              
			  # Filter the training data to only the classes we are currently considering.
              tempData <- data[data$popularity %in% u, ]
              
			  # Create the model and put it in the models list.
              models[[key]] <- randomForest(y=as.factor(tempData$popularity), x=tempData[, vars],
                                            ntree=1200, nodesize=10)
            }
          }
        }
      }
    }
  }
  
  # Save the result in the cache.
  save(models, file=cacheFile)
  cat("\n")
  return(models)
}

################################################################################
# 'trainMultipleGbmModels
#'
#' Train a GBM model on binary data for every combination of first- and
#' second-choices of the primary Random Forest model.
#'
#' @param data The training data.
#' @param key A key to identify this batch of training data for caching purposes.
#' @param loadFromCache TRUE if we should attempt to load the output of this function from the cache.
################################################################################
trainMultipleGbmModels <- function(data, key, loadFromCache=FALSE)
{
  cat(paste("  TRAINING GBM MODELS {CV ", key, "} ", sep=""))
  
  # Load the output of this function from cache if appropriate.
  cacheFile = paste("../data/cache.trainMultipleGbmModels_", key, ".RData", sep="")
  if (loadFromCache == TRUE && file.exists(cacheFile))
  {
    load(cacheFile) #models
    cat("\n    Loaded from cache.\n")
    return(models)
  }
  
  models <- list()
  
  # Loop through all possible combinations of included classifications.
  for (a1 in 0:1) {
    for (a2 in 0:1) {
	for (a3 in 0:1) {
        for (a4 in 0:1) {
          for (a5 in 0:1) {
            
            s <- c(a1,a2,a3,a4,a5)
            n <- s*(1:5)
            u <- n[n>0]
            
			# Now, we only consider combinations of two classes.
            if (sum(s) == 2)
            {
              key <- paste(u, collapse="")
              cat(paste(key, "...", sep=""))
              
			  # Filter the training data to only the classes we are currently considering.
              tempData <- data[data$popularity %in% u, ]
              tempData$popularity <- ifelse(tempData$popularity == min(u), 0, 1)
              
 			  # Create the model and put it in the models list.
              models[[key]] <- trainOneGbmModel(tempData)
            }
          }
        }
      }
    }
  }
  
  # Save the result in the cache.
  save(models, file=cacheFile)
  cat("\n")
  return(models)
}

################################################################################
#' trainMultipleMaBoostModels
#'
#' Train an MaBoost model on binary data for every combination of first- and
#' second-choices of the primary Random Forest model.
#'
#' @param data The training data.
#' @param key A key to identify this batch of training data for caching purposes.
#' @param loadFromCache TRUE if we should attempt to load the output of this function from the cache.
################################################################################
trainMultipleMaBoostModels <- function(data, key, loadFromCache=FALSE)
{
  cat(paste("  TRAINING MABOOST MODELS {CV ", key, "} ", sep=""))
  
  # Load the output of this function from cache if appropriate.
  cacheFile = paste("../data/cache.trainMultipleMaBoostModels_", key, ".RData", sep="")  
  if (loadFromCache == TRUE && file.exists(cacheFile))
  {
    load(cacheFile) #models
    cat("\n    Loaded from cache.\n")
    return(models)
  }
  
  models <- list()
  vars <- getSelectedVariables()
  
  # Loop through all possible combinations of included classifications.
  for (a1 in 0:1) {
    for (a2 in 0:1) {
      for (a3 in 0:1) {
        for (a4 in 0:1) {
          for (a5 in 0:1) {
            
            s <- c(a1,a2,a3,a4,a5)
            n <- s*(1:5)
            u <- n[n>0]
            
			# Now, we only consider combinations of two classes.
            if (sum(s) == 2)
            {
              key <- paste(u, collapse="")
              cat(paste(key, "...", sep=""))
              
			  # Filter the training data to only the classes we are currently considering.
              tempData <- data[data$popularity %in% u, ]
              truth <- tempData$popularity
              tempData <- tempData[, vars]
              tempData$popularity <- truth
              
			  # Create the model and put it in the models list.
              models[[key]] <- maboost(as.factor(popularity) ~ ., data = tempData)
            }
          }
        }
      }
    }
  }
  
  # Save the result in the cache.
  save(models, file=cacheFile)
  cat("\n")
  return(models)
}

################################################################################
#' trainOneGbmModel
#'
#' Train a single GBM model.  This function exists to serve trainMultipleGbmModels().
#'
#' @param data The training data.
################################################################################
trainOneGbmModel <- function(data)
{
  vars <- getSelectedVariables()
  vars <- vars[vars!="date"]

  truth <- as.numeric(data$popularity)
  data$popularity <- NULL # Because gbm.fit is sensitive about training/test data matching.
  
  model <- gbm.fit(y=truth, x=data[, vars], interaction.depth=20,
                   distribution="adaboost", n.trees=2000, shrinkage=0.01, bag.fraction=1)
  # model <- gbm.fit(y=as.factor(data$popularity), x=data[, vars], interaction.depth=20,
  #                  distribution="adaboost", n.trees=100, shrinkage=0.1, bag.fraction=1)
  
  return(model)
}

################################################################################
#' trainMetaModel
#'
#' Train a "meta" model, a Random Forest model, which synthesizes the original training data
#' in combination with the output of all other models.
#'
#' @param preds The predictions of all previous models in combination with the original data.
#' @param key A key to uniquely identify the results of this function in the cache.
#' @param loadFromCache TRUE if we should attempt to load the output of this function from the cache.
################################################################################
trainMetaModel <- function(preds, key, loadFromCache=FALSE)
{
  cat(paste("  TRAINING META MODEL {CV ", key, "}\n", sep=""))
  
  # Load the output of this function from cache if appropriate.
  cacheFile = paste("../data/cache.trainMetaModel_", key, ".RData", sep="")
  if (loadFromCache == TRUE && file.exists(cacheFile))
  {
    load(cacheFile) #metaModel
    cat("    Loaded from cache.\n")
    return(metaModel)
  }
  
  # Clean up a few columns before training the meta model.
  colnames(preds)[1:5] <- c("x1","x2","x3","x4","x5")
  preds$popularity <- preds$truth
  preds$correct <- NULL
  preds$truth <- NULL
  preds$cvSet <- NULL
  preds$url <- NULL
  
  # Create the meta model.
  metaModel <- randomForest(factor(popularity) ~ ., preds, ntree=1200, nodeSize=10)

  # Save the model to the cache.
  save(metaModel, file=cacheFile)
  return(metaModel)
}

################################################################################
#' applyPrimaryRfModel
#'
#' Generate productions from the primary Random Forest model.
#'
#' @param model The primary Random Forest model.
#' @param data The test data.
################################################################################
applyPrimaryRfModel <- function(model, data)
{
  cat("  APPLYING ORIGINAL RF MODEL\n")

  # Create predictions from the model requesting results as probabilities of each class.
  rawPreds <- as.data.frame(predict(model, data, type="prob"))
  
  # Find the first choice of the model
  pred <- apply(rawPreds, 1, which.max)
  prob <- apply(rawPreds, 1, max)

  # Find the second choice of the model.
  rawPreds2 <- rawPreds
  for (i in 1:nrow(rawPreds2))
  {
    rawPreds2[i, pred[i]] <- 0
  }
  pred2 <- apply(rawPreds2, 1, which.max)
  prob2 <- apply(rawPreds2, 1, max)
  
  ## TODO - remove this comment:
  ## WE CANNOT ADD COLUMNS TO rawPreds BEFORE THIS LINE
  
  # Copy the results into a data frame of raw predictions.
  rawPreds$id <- data$id
  rawPreds$pred <- pred
  rawPreds$prob <- prob
  rawPreds$pred2 <- pred2
  rawPreds$prob2 <- prob2
  
  # Calculate the probability that one of the other three classes is correct.
  rawPreds$probOther <- 1 - rawPreds$prob - rawPreds$prob2
  
  # Store the ratio of the probability of the first choice to that of the second choice.
  # The conditional is to prevent an overflow.
  rawPreds$probRatio = ifelse(rawPreds$prob2 < 0.0001, 9999999, rawPreds$prob / rawPreds$prob2)
  
  # If we're working with training data, then add two fields for diagnostic purposes:
  # correct, a boolean for correctness of the generated precition, and truth, the actual class.
  if ("popularity" %in% colnames(data))
  {
    rawPreds$correct <- (data$popularity == pred)
    rawPreds$truth <- data$popularity
  }
  
  return(rawPreds)
}

################################################################################
#' applyGbmModels
#'
#' Apply the GBM models to the test data and augment the predictions created thus
#' far with the GBM models' predictions.
#'
#' @param models A list of GBM models created during the training phase.
#' @param data The test data.
#' @param preds A data frame containing the predictions of models that have already been run.
################################################################################
applyGbmModels <- function(models, data, preds)
{
  cat("  APPLYING GBM MODELS ")
  
  data$popularity <- NULL
  
  # Loop through all possible combinations of included classifications.
  for (a1 in 0:1) {
    for (a2 in 0:1) {
      for (a3 in 0:1) {
        for (a4 in 0:1) {
          for (a5 in 0:1) {
            
            s <- c(a1,a2,a3,a4,a5)
            n <- s*(1:5)
            u <- n[n>0]
            
			# Now, we only consider combinations of two classes.
            if (sum(s) == 2)
            {
              key <- paste(u, collapse="")
              cat(paste(key, "...", sep=""))
              
			  # Filter the training data to only the classes we are currently considering.
              tempData <- data[preds$pred %in% u & preds$pred2 %in% u, ]
              if (nrow(tempData) > 0)
              {
			    # Extract the model from the models list and apply it to the selected data.
                model <- models[[key]]
                gbmPreds <- predict(model, tempData, n.trees=2000, type="response")
                
				# Copy the generated predictions into our larger test set.
                for (i in 1:nrow(tempData))
                {
                  preds$gbmProb[data$id==tempData$id[i]] <- gbmPreds[i]
                  preds$gbmPred[data$id==tempData$id[i]] <- ifelse(round(gbmPreds[i])==0,u[1],u[2])
                }
              }
            }
          }
        }
      }
    }
  }
  
  cat("\n")
  return(preds)
}

################################################################################
#' applyMaBoostModels
#'
#' Apply the MaBoost models to the test data and augment the predictions created thus
#' far with the MaBoost models' predictions.
#'
#' @param models A list of MaBoost models created during the training phase.
#' @param data The test data.
#' @param preds A data frame containing the predictions of models that have already been run.
################################################################################
applyMaBoostModels <- function(models, data, preds)
{
  cat("  APPLYING MABOOST MODELS ")
  
  data$popularity <- NULL
  
  # Loop through all possible combinations of included classifications.
  for (a1 in 0:1) {
    for (a2 in 0:1) {
      for (a3 in 0:1) {
        for (a4 in 0:1) {
          for (a5 in 0:1) {
            
            s <- c(a1,a2,a3,a4,a5)
            n <- s*(1:5)
            u <- n[n>0]
            
 			# Now, we only consider combinations of two classes.
            if (sum(s) == 2)
            {
              key <- paste(u, collapse="")
              cat(paste(key, "...", sep=""))
              
			  # Filter the training data to only the classes we are currently considering.
              tempData <- data[preds$pred %in% u & preds$pred2 %in% u, ]
              if (nrow(tempData) > 0)
              {
 			    # Extract the model from the models list and apply it to the selected data.
                model <- models[[key]]
                maBoostPreds <- predict(model, tempData, type="class")
                
				# Copy the generated predictions into our larger test set.
                for (i in 1:nrow(tempData))
                {
                  preds$maBoostPred[data$id==tempData$id[i]] <- as.numeric(levels(maBoostPreds)[maBoostPreds[i]])
                }
              }
            }
          }
        }
      }
    }
  }
  
  cat("\n")
  return(preds)
}

################################################################################
#' applyRfModels
#'
#' Apply the binary Random Forest models to the test data and augment the predictions
#' created thus far with the Random Forest models' predictions.
#'
#' @param models A list of Random Forest models created during the training phase.
#' @param data The test data.
#' @param preds A data frame containing the predictions of models that have already been run.
################################################################################
applyRfModels <- function(models, data, preds)
{
  cat("  APPLYING RF MODELS ")
  
  preds$rfPred <- NA
  
  # Loop through all possible combinations of included classifications.
  for (a1 in 0:1) {
    for (a2 in 0:1) {
      for (a3 in 0:1) {
        for (a4 in 0:1) {
          for (a5 in 0:1) {
            
            s <- c(a1,a2,a3,a4,a5)
            n <- s*(1:5)
            u <- n[n>0]
            
 			# Now, we only consider combinations of two classes.
            if (sum(s) == 2)
            {
              key <- paste(u, collapse="")
              cat(paste(key, "...", sep=""))
              
			  # Filter the training data to only the classes we are currently considering.
              tempData <- data[preds$pred %in% u & preds$pred2 %in% u, ]
              if (nrow(tempData) > 0)
              {
			    # Extract the model from the models list and apply it to the selected data.
                model <- models[[key]]
                rfPreds <- predict(model, tempData, type="prob")

				# Copy the generated predictions into our larger test set.
                for (i in 1:nrow(tempData))
                {
                  #preds$rfPred[data$id==tempData$id[i]] <- rfPreds[i]
                  preds$rfPred[data$id==tempData$id[i]] <- as.numeric(colnames(rfPreds)[which.max(rfPreds[i,])])
                  preds$rfProb[data$id==tempData$id[i]] <- max(rfPreds[i,])
                }
              }
            }
          }
        }
      }
    }
  }

  cat("\n")
  return(preds)
}

################################################################################
#' produceFinalPredictions
#'
#' Generate final predictions based on the original test data and the output of
#' all other models applied to that test data.
#'
#' @param numSplits The number of splits used to generate predictions.
#' @param data The test data.
################################################################################
produceFinalPredictions <- function(numSplits, data)
{
  votes <- data.frame(id=data$id)
  votesOrig <- data.frame(id=data$id)
  votesRf <- data.frame(id=data$id)
  votesMa <- data.frame(id=data$id)
  
  # Use every one of train/test splits.
  for (split in 1:numSplits)
  {
    cat(paste("\nPREDICTING FROM META MODEL ", split, "\n", sep=""))
    
    key <- paste(split, "of", numSplits, sep="")
    
	# Load the primary Random Forest model from cache and preserve its predictions.
    primaryRfModel <- trainPrimaryRfModel(NULL, key, TRUE)
    preds <- applyPrimaryRfModel(primaryRfModel, data)
    
	# Load the binary GBM models from cache and preserve their predictions.
    #gbmModels <- trainMultipleGbmModels(NULL, key, TRUE)
    #preds <- applyGbmModels(gbmModels, data, preds)
  
	# Load the binary MaBoost models from cache and preserve their predictions.
    maBoostModels <- trainMultipleMaBoostModels(NULL, key, TRUE)
    preds <- applyMaBoostModels(maBoostModels, data, preds)

	# Load the binary Random Forest models from cache and preserve their predictions.
    rfModels <- trainMultipleRfModels(NULL, key, TRUE)
    preds <- applyRfModels(rfModels, data, preds)
    
	# Load the meta model from cache.
    metaModel <- trainMetaModel(NULL, key, TRUE)
    
	# Name the columns that contain the original RF votes.  These have numeric column names
	# and cannot be used by some of the models.
    colnames(preds)[1:5] <- c("x1","x2","x3","x4","x5")
	
	# Combine the test data and the predictions generated thus far into a single data frame,
	# eliminating the redundant ID column.
    preds <- cbind(preds[,-6], data)
    
    cat("  APPLYING META MODEL\n")
    col = paste("V", split, sep="")
	
	# Generate votes for each observation, for each split. (rows=observations, columns=splits)
    votes[, col] <- predict(metaModel, preds)
    votesOrig[, col] <- preds$pred
    votesMa[, col] <- preds$maBoostPred
    votesRf[, col] <- preds$rfPred
  }
  
  # Find the mode of the votes and return them as the final predictions.
  modeOfVotes <- as.numeric(apply(votes[, 2:11], 1, mode))
  modeOfVotesOrig <- as.numeric(apply(votesOrig[, 2:11], 1, mode))
  modeOfVotesMa <- as.numeric(apply(votesMa[, 2:11], 1, mode))
  modeOfVotesRf <- as.numeric(apply(votesRf[, 2:11], 1, mode))
  
  #return(list(data.frame(id=data$id, popularity=modeOfVotes, orig=modeOfVotesOrig, ma=modeOfVotesMa, rf=modeOfVotesRf),
  #            votes,votesOrig,votesMa,votesRf))
              
  return(data.frame(id=data$id, popularity=modeOfVotes))
}

#TODO clean up extra vote info above

################################################################################
#' buildMetaModels
#
#' @param numSplits The number of splits used to generate predictions.
#' @param data The training data.
################################################################################
buildMetaModels <- function(numSplits, data)
{
  # Build models and get raw test predictions on data split for cross-validation.
  for (split in 1:numSplits)
  {
    key <- paste(split, "of", numSplits, sep="")
    
    cat(paste("\nTRAINING CROSS VALIDATION GROUP ", split, "\n", sep=""))
    cvData <- splitForCrossValidation(data, split)
    
	# Train the primary Random Forest model, or load it from cache if available.
    primaryRfModel <- trainPrimaryRfModel(cvData$train, key, TRUE)
	# Apply the model to the training data.
    preds <- applyPrimaryRfModel(primaryRfModel, cvData$test)
    
	# Train the binary GBM models, or load them from cache if available.
    #gbmModels <- trainMultipleGbmModels(cvData$train, key, TRUE)
	# Apply the models to the training data.
    #preds <- applyGbmModels(gbmModels, cvData$test, preds)
    
	# Train the binary MaBoost models, or load them from cache if available.
    maBoostModels <- trainMultipleMaBoostModels(cvData$train, key, TRUE)
	# Apply the models to the training data.
    preds <- applyMaBoostModels(maBoostModels, cvData$test, preds)
    
	# Train the binary Random Forest models, or load them from cache if available.
    rfModels <- trainMultipleRfModels(cvData$train, key, TRUE)
	# Apply the models to the training data.
    preds <- applyRfModels(rfModels, cvData$test, preds)

	# Combine the test data and the predictions generated thus far into a single data frame,
	# eliminating the redundant ID column.
    preds <- cbind(preds[,-6], cvData$test)    

	# Train the meta model on the resulting data frame.  The return value is not used because
	# the function writes it to the cache.  It will be read back when needed.
    trainMetaModel(preds, key, TRUE)
  }
}

################################################################################
#' driver
#
#' @param loadFromCache TRUE if we should attempt to load the output of this function from the cache
onp.predict.2 <- function(test)
{
  numSplits <- 10
  setAside <- 0 #0.2
  
  #Train with optional set-aside for cross-validation.
  data <- loadAllTrainingData(numSplits, setAside, TRUE)
  buildMetaModels(numSplits, data)
  return(produceFinalPredictions(numSplits, test))
  
  if (FALSE)
  {
    # Test against the test data.
    load("../data/setAside.RData") #setAsideData
    preds <- produceFinalPredictions(numSplits, setAsideData)
    mean(preds$popularity == setAsideData$popularity)
  
    # Create an entry for submission.
    data <- loadDataForSubmission(FALSE)
    finalPreds <- produceFinalPredictions(numSplits, data)
    print(head(finalPreds))
    path <- "../data/submission6.csv"
    write.csv(finalPreds, path, row.names=FALSE)
    cat(paste("The file for submission, '", path, "', has been written.", sep=""))
  }
  
  return(produceFinalPredictions(numSplits, data))
}

