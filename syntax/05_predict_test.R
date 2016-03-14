################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition
# Script  : 05_predict_test.R
################################################################################
# Author   : Miquel Torrens, 2016.02.07
# Modified : Miquel Torrens, 2016.02.08
################################################################################
# source('/Users/miquel/Desktop/bgse/projects/kaggle/syntax/00_start.R')
# source(paste(SYNTAXDIR, '05_predict_test.R', sep = ''))
################################################################################

################################################################################
main.05 <- function() {
################################################################################
  # Print starting time
  bs <- begin.script(paste('[', PROJECT, '] 05_predict_test.R', sep = ''))

  ##############################################################################
  # Load
  file <- paste(DATADIR, 'news_popularity_test.RData', sep = '')
  np.test <- get(load(file = file)); cat('Loaded file:', file, '\n')

  # Training data
  file <- paste(DATADIR, 'news_popularity_training_extended.RData', sep = '')
  np.train <- get(load(file = file)); cat('Loaded file:', file, '\n')
  #load(file = file); cat('Loaded file:', file, '\n')

  # Outlier threshold
  file <- paste(DATADIR, 'outlier_threshold.RData', sep = '')
  extremes <- get(load(file = file)); cat('Loaded file:', file, '\n')

  # Sets of variables
  file <- paste(DATADIR, 'final_variable_list.RData', sep = '')
  load(file = file); cat('Loaded file:', file, '\n')
  ##############################################################################

  ##############################################################################
  # See if some observations can be considered outliers
  #fives <- which(np.test[, 'popularity'] == 5)
  killed <- c()
  for (col in nat.vars) {
    if (col != 'popularity' && class(np.test[, col]) != 'character') {
      outliers <- which(np.test[, col] > max(extremes[, col]) |
                        np.test[, col] < min(extremes[, col]))
      killed <- unique(c(killed, outliers))
    }
  }

  #killed <- killed[-which(killed %in% fives)]
  cat('Total number of observations:', nrow(np.test), '\n')
  cat('Number of outliers detected:', length(killed), '\n')
  ##############################################################################

  ##############################################################################
  # Creating new variables
  # Binary for outliers
  np.test[, 'is_outlier'] <- 0
  np.test[killed, 'is_outlier'] <- 1

  # Binary for timedelta
  np.test[, 'timedelta_bin'] <- as.numeric(np.test[, 'timedelta'] >= 400)
  np.test[, 'timedelta_bin_int'] <- np.test[, 'timedelta'] *
                                     np.test[, 'timedelta_bin']

  # Categorical for kw_min_min
  np.test[, 'kw_min_min_cat0'] <- 0
  np.test[, 'kw_min_min_cat1'] <- 0
  np.test[, 'kw_min_min_cat2'] <- 0
  np.test[which(np.test[, 'kw_min_min'] ==  -1), 'kw_min_min_cat0'] <- 1
  np.test[which(np.test[, 'kw_min_min'] ==   4), 'kw_min_min_cat1'] <- 1
  np.test[which(np.test[, 'kw_min_min'] == 217), 'kw_min_min_cat2'] <- 1

  # Binary kw_avg_max
  np.test[, 'kw_avg_max_bin'] <- as.numeric(np.test[, 'kw_avg_max'] >= 1e5)

  # Binary kw_min_avg
  np.test[, 'kw_min_avg_bin'] <- as.numeric(np.test[, 'kw_min_avg'] >= 0)

  # Binary LDA_0x
  np.test[, 'LDA_01_bin'] <- as.numeric(np.test[, 'LDA_01'] < 0.1)
  np.test[, 'LDA_02_bin'] <- as.numeric(np.test[, 'LDA_02'] < 0.1)
  np.test[, 'LDA_03_bin'] <- as.numeric(np.test[, 'LDA_03'] < 0.1)
  np.test[, 'LDA_04_bin'] <- as.numeric(np.test[, 'LDA_04'] < 0.1)
  np.test[, 'LDA_01_bin_int'] <- np.test[, 'LDA_01'] * np.test[, 'LDA_01_bin']
  np.test[, 'LDA_02_bin_int'] <- np.test[, 'LDA_02'] * np.test[, 'LDA_02_bin']
  np.test[, 'LDA_03_bin_int'] <- np.test[, 'LDA_03'] * np.test[, 'LDA_03_bin']
  np.test[, 'LDA_04_bin_int'] <- np.test[, 'LDA_04'] * np.test[, 'LDA_04_bin']

  # Categorical title_subjectivity
  q0 <- which(np.test[, 'title_subjectivity'] <  0.15)
  q1 <- which(np.test[, 'title_subjectivity'] >= 0.15)
  q2 <- which(np.test[, 'title_subjectivity'] >= 0.6)
  np.test[, 'title_subjectivity_cat0'] <- 0
  np.test[, 'title_subjectivity_cat1'] <- 0
  np.test[, 'title_subjectivity_cat2'] <- 0
  np.test[q0, 'title_subjectivity_cat0'] <- 1
  np.test[q1, 'title_subjectivity_cat1'] <- 1
  np.test[q2, 'title_subjectivity_cat2'] <- 1

  # Categorical title_sentiment_polarity
  q0 <- which(np.test[, 'title_sentiment_polarity'] <  0)
  q1 <- which(np.test[, 'title_sentiment_polarity'] == 0)
  q2 <- which(np.test[, 'title_sentiment_polarity'] >  0)
  np.test[, 'title_sentiment_polarity_cat0'] <- 0
  np.test[, 'title_sentiment_polarity_cat1'] <- 0
  np.test[, 'title_sentiment_polarity_cat2'] <- 0
  np.test[q0, 'title_sentiment_polarity_cat0'] <- 1
  np.test[q1, 'title_sentiment_polarity_cat1'] <- 1
  np.test[q2, 'title_sentiment_polarity_cat2'] <- 1

  # Extract the date
  urls <- strsplit(np.test[, 'url'], '/')
  days <- sapply(urls, `[`, 6)
  years <- sapply(urls, `[`, 4)
  month <- sapply(urls, `[`, 5)
  dates <- paste(years, month, days, sep = '-')

  # Date
  day.one <- as.Date('2013-01-01', '%Y-%m-%d')
  np.test[, 'date'] <- as.Date(dates, '%Y-%m-%d')
  np.test[, 'since_20130101'] <- as.numeric(np.test[, 'date'] - day.one)

  # Year
  np.test[, 'year'] <- years
  np.test[, 'is_2013'] <- as.numeric(np.test[, 'year'] == '2013')
  np.test[, 'is_2014'] <- as.numeric(np.test[, 'year'] == '2014')

  # Month
  np.test[, 'month'] <- month
  np.test[, 'is_jan'] <- as.numeric(np.test[, 'month'] == '01')
  np.test[, 'is_feb'] <- as.numeric(np.test[, 'month'] == '02')
  np.test[, 'is_mar'] <- as.numeric(np.test[, 'month'] == '03')
  np.test[, 'is_apr'] <- as.numeric(np.test[, 'month'] == '04')
  np.test[, 'is_may'] <- as.numeric(np.test[, 'month'] == '05')
  np.test[, 'is_jun'] <- as.numeric(np.test[, 'month'] == '06')
  np.test[, 'is_jul'] <- as.numeric(np.test[, 'month'] == '07')
  np.test[, 'is_aug'] <- as.numeric(np.test[, 'month'] == '08')
  np.test[, 'is_sep'] <- as.numeric(np.test[, 'month'] == '09')
  np.test[, 'is_oct'] <- as.numeric(np.test[, 'month'] == '10')
  np.test[, 'is_nov'] <- as.numeric(np.test[, 'month'] == '11')
  np.test[, 'is_dec'] <- as.numeric(np.test[, 'month'] == '12')

  # Day
  np.test[, 'day'] <- days

  # Season
  np.test[, 'season'] <- '4'  # Winter
  np.test[which(paste(month, days) > '03 20'), 'season'] <- '1'  # Spring
  np.test[which(paste(month, days) > '06 20'), 'season'] <- '2'  # Summer
  np.test[which(paste(month, days) > '09 20'), 'season'] <- '3'  # Autumn
  np.test[which(paste(month, days) > '12 20'), 'season'] <- '4'  # Winter
  np.test[, 'is_spring'] <- as.numeric(np.test[, 'season'] == '1')
  np.test[, 'is_summer'] <- as.numeric(np.test[, 'season'] == '2')
  np.test[, 'is_autumn'] <- as.numeric(np.test[, 'season'] == '3')
  np.test[, 'is_winter'] <- as.numeric(np.test[, 'season'] == '4')

  # Daily scores
  file <- paste(DATADIR, 'daily_scores.RData', sep = '')
  load(file = file); cat('Loaded file:', file, '\n')

  m1 <- match(as.character(np.test[, 'date']), names(counts))
  m2 <- match(as.character(np.test[, 'date']), names(avg))
  m3 <- match(as.character(np.test[, 'date']), names(sds))
  np.test[, 'day_news'] <- counts[m1]
  np.test[, 'day_avg_pop'] <- avg[m2]
  np.test[, 'day_sd_pop'] <- sds[m3]

  # m1 <- match(as.character(np.test[, 'date'] - 1), names(counts))
  # m2 <- match(as.character(np.test[, 'date'] - 1), names(avg))
  # m3 <- match(as.character(np.test[, 'date'] - 1), names(sds))
  # np.test[, 'day_news_lag1'] <- counts[m1]
  # np.test[, 'day_avg_pop_lag1'] <- avg[m2]
  # np.test[, 'day_sd_pop_lag1'] <- sds[m3]

  # m1 <- match(as.character(np.test[, 'date'] - 2), names(counts))
  # m2 <- match(as.character(np.test[, 'date'] - 2), names(avg))
  # m3 <- match(as.character(np.test[, 'date'] - 2), names(sds))
  # np.test[, 'day_news_lag2'] <- counts[m1]
  # np.test[, 'day_avg_pop_lag2'] <- avg[m2]
  # np.test[, 'day_sd_pop_lag2'] <- sds[m3]

  # m1 <- match(as.character(np.test[, 'date'] - 3), names(counts))
  # m2 <- match(as.character(np.test[, 'date'] - 3), names(avg))
  # m3 <- match(as.character(np.test[, 'date'] - 3), names(sds))
  # np.test[, 'day_news_lag3'] <- counts[m1]
  # np.test[, 'day_avg_pop_lag3'] <- avg[m2]
  # np.test[, 'day_sd_pop_lag3'] <- sds[m3]

  # Standardized features
  std.vars <- c('n_tokens_title', 'n_tokens_content', 'kw_avg_min',
                'kw_max_max', 'kw_min_avg')
  for (col in std.vars) {
    end.col <- paste(col, 'std', sep = '_')
    np.test[, end.col] <- (np.test[, col] - mean(np.test[, col])) /
                           sd(np.test[, col])
  }

  # Logarithmic variables
  log.vars <- c('timedelta', 'kw_max_min', 'kw_min_max', 'kw_avg_max',
                'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares',
                'self_reference_max_shares', 'self_reference_avg_sharess')
  for (col in log.vars) {
    end.col <- paste('log', col, sep = '_')
    np.test[, end.col] <- log(np.test[, col] + 1)
  }

  # New features added
  std.cols <- paste(std.vars, 'std', sep = '_')
  log.cols <- paste('log', log.vars, sep = '_')

  # Final variables
  final.vars <- c(nat.vars, new.vars, std.cols, log.cols)

  # Save results
  file <- paste(DATADIR, 'news_popularity_test_extended.RData', sep = '')
  save(np.test, file = file); cat('Saved file:', file, '\n')
  ##############################################################################

  ##############################################################################
  # Run our best model
  # Prune NAs
  np.test <- np.test[complete.cases(np.test), ]
  np.train <- np.train[complete.cases(np.train), ]
  final.vars <- unique(c('popularity', final.vars, ori.vars))
  #final.vars <- unique(c('popularity', final.vars))

  # Random Forest
  cat('Callibrating model... ')
  final.varsT <- final.vars[final.vars != 'popularity']
  ntrees <- 1200
  nodes <- 10
  seed <- 3050
  # 1. n = 100, s = 65, seed = 2100 EST FINAL: 0.5299 (final + original)
  # 2. n = 1200, s = 10, seed = 666 EST FINAL: 0.5289 (final)
  set.seed(seed)
  rf <- randomForest(y = as.factor(np.train[, 'popularity']),
                     x = np.train[, final.varsT],
                     ntree = ntrees, nodesize = nodes)
  cat('Done!\nnt: ', ntrees, ', ns: ', nodes, ', s: ', seed, '\n', sep = '')

  # Predictions
  preds <- predict(rf, newdata = np.test)
  result <- cbind(np.test[, 'id'], preds)
  colnames(result) <- c('id', 'popularity')

  # Save the results in the correct format
  now <- format(Sys.time(), '%Y%m%d_%H%M')
  file <- paste(OUTPUTDIR, 'res_', now, '.csv', sep = '')
  write.csv(result, file = file, row.names = FALSE)
  cat('Written file:', file, '\n')
  ##############################################################################

  # End
  end.script(begin = bs, end = Sys.time())
}
# END OF SCRIPT
