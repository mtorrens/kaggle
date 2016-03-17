################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition
# Group   : The Unlikelies
# Script  : package_function.R
################################################################################

################################################################################
#' onp.predict
#'
#' Use features, outlier detection, and hyperparameter optimization brought to
#' you by the Unlikelies to predict online news popularity.
#' 
#' The Unlikelies (c) 2016.03.17
#' Authors: Roger Cusco, Matthew Sudmann-Day, Miquel Torrens
#'
#' DISCLAIMER: Evaluator's eyes only. This function contains confidential
#' procedures for the Kaggle competition results and must not be released or
#' displayed to third parties under any circumstance without express written
#' permission by the authors.
#' 
#' @param test A data frame containing a test set with 61 features.
#' @param train An optional data frame containing a training set with 62 features.  If not provided, a default internal training set is used.
#' @param verbose TRUE if the function should print progress.
#' @param store TRUE if the resulting predictions should be saved to a CSV file.
#' @param dest.folder Folder in which the results will be saved.
#' @param visible TRUE if the results should be visible when returned.
#' @param ntree Number of trees in the random forest.
#' @param nodesize Ending node size for the random forest.
#' @param seed Starting random seed for the random forest.
#' @param ... Additional parameters to the model generator randomForest().
#' @return A dataframe containing two columns: id, popularity
#' @examples
#' # predictions <- onp.predict(test)
#' # predictions <- onp.predict(test,train)
#' @export
onp.predict <- function(test, train = NULL, verbose = TRUE, store = FALSE,
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
    train <- np.train
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
  ##############################################################################
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
    cat('Total number of observations ("train"):', nrow(train), '\n')
    cat('Number of outliers detected ("train"):', length(killed), '\n')
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

  ##############################################################################
  # Test set
  ##############################################################################
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

  ##############################################################################
  # Model
  ##############################################################################
  # Final variables of the model include the original one of the dataset
  model.vars <- unique(c('popularity', final.vars, ori.vars))
  model.varsT <- model.vars[model.vars != 'popularity']

  # Define model parameters
  if (is.na(seed)) { seed <- 3090 }
  if (is.na(ntree)) { ntree <- 1200 }
  if (is.na(nodesize)) { nodesize <- 10 }

  # Random Forest
  if (verbose == TRUE) { cat('Callibrating model... ') }
  set.seed(seed)
  rf <- randomForest(y = as.factor(train[, 'popularity']),
                     x = train[, model.varsT],
                     ntree = ntree, nodesize = nodesize,
                     mtry = length(model.vars) ** (1 / 3), ...)
  if (verbose == TRUE) { cat('Done!\n') }

  # Predictions
  if (verbose == TRUE) { cat('Computing predictions... ') }
  preds <- predict(rf, newdata = test)
  result <- cbind(test[, 'id'], preds)
  colnames(result) <- c('id', 'popularity')
  if (verbose == TRUE) { cat('Done!\n') }
  ##############################################################################

  # Save the results in the correct format
  if (store == TRUE) {
    end.path <- ifelse(! is.null(dest.folder), dest.folder, getwd())
    if (! grepl('/$', end.path)) { end.path <- paste(end.path, '/', sep = '') }
    now <- format(Sys.time(), '%Y%m%d_%H%M')
    file <- paste(end.path, 'res_', now, '.csv', sep = '')
    write.csv(result, file = file, row.names = FALSE)
    if (verbose == TRUE) { cat('Written file:', file, '\n') }
  }

  # Return results to upper environment
  if (visible == TRUE) {
    return(result)
  } else {
    return(invisible(result))  
  }  
}
# END OF SCRIPT
