################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition
# Script  : 03_features.R
################################################################################
# Author   : Miquel Torrens, 2016.01.31
# Modified : Miquel Torrens, 2016.02.05
################################################################################
# source('/Users/miquel/Desktop/bgse/projects/kaggle/syntax/00_start.R')
# source(paste(SYNTAXDIR, '03_features.R', sep = ''))
################################################################################

################################################################################
main.03 <- function() {
################################################################################
  # Print starting time
  bs <- begin.script(paste('[', PROJECT, '] 03_features.R', sep = ''))

  # Load data
  file <- paste(DATADIR, 'news_popularity_training.RData', sep = '')
  train <- get(load(file = file)); cat('Loaded file:', file, '\n')

  # Interesting original features (chosen by visual inspection from 02)
  ori.vars <- colnames(train)[! colnames(np.train) %in% c('url', 'id')]
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

  # See if some observations can be considered outliers
  fives <- which(train[, 'popularity'] == 5)
  killed <- c()
  extremes <- as.data.frame(matrix(ncol = 0, nrow = 2))
  for (col in nat.vars) {
    if (class(train[, col]) != 'character') {
      thres <- quantile(train[, col], probs = c(0.005, 0.995))
      extremes <- cbind(extremes, as.data.frame(thres))
      colnames(extremes)[ncol(extremes)] <- col
      #thres <- quantile(np.train[, col], probs = c(0.01, 0.99))
      outliers <- which(train[, col] < thres[1] | 
                        train[, col] > thres[2])
      killed <- unique(c(killed, outliers))
    }
  }

  # Save thresholds
  file <- paste(DATADIR, 'outlier_threshold.RData', sep = '')
  save(extremes, file = file); cat('Saved file:', file, '\n')

  #killed <- killed[-which(killed %in% fives)]
  cat('Total number of observations:', nrow(train), '\n')
  cat('Number of outliers detected:', length(killed), '\n')

  # # We keep the rest
  # tt <- table(np.train[, 'popularity'])
  # print(tt / sum(tt))
  # survivors <- (1:nrow(np.train))[-killed]
  # np.train <- np.train[survivors, ]

  # # Check things are still constant
  # tt <- table(np.train[, 'popularity'])
  # print(tt / sum(tt))  # Nice!

  ##############################################################################
  # Creating new variables
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

  # Binary kw_min_avg
  train[, 'kw_min_avg_bin'] <- as.numeric(train[, 'kw_min_avg'] >= 0)

  # Binary LDA_0x
  train[, 'LDA_01_bin'] <- as.numeric(train[, 'LDA_01'] < 0.1)
  train[, 'LDA_02_bin'] <- as.numeric(train[, 'LDA_02'] < 0.1)
  train[, 'LDA_03_bin'] <- as.numeric(train[, 'LDA_03'] < 0.1)
  train[, 'LDA_04_bin'] <- as.numeric(train[, 'LDA_04'] < 0.1)
  train[, 'LDA_01_bin_int'] <- train[, 'LDA_01'] * train[, 'LDA_01_bin']
  train[, 'LDA_02_bin_int'] <- train[, 'LDA_02'] * train[, 'LDA_02_bin']
  train[, 'LDA_03_bin_int'] <- train[, 'LDA_03'] * train[, 'LDA_03_bin']
  train[, 'LDA_04_bin_int'] <- train[, 'LDA_04'] * train[, 'LDA_04_bin']

  # Categorical title_subjectivity
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

  # Date
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
  train[which(paste(month, days) > '03 20'), 'season'] <- '1'  # Spring
  train[which(paste(month, days) > '06 20'), 'season'] <- '2'  # Summer
  train[which(paste(month, days) > '09 20'), 'season'] <- '3'  # Autumn
  train[which(paste(month, days) > '12 20'), 'season'] <- '4'  # Winter
  train[, 'is_spring'] <- as.numeric(train[, 'season'] == '1')
  train[, 'is_summer'] <- as.numeric(train[, 'season'] == '2')
  train[, 'is_autumn'] <- as.numeric(train[, 'season'] == '3')
  train[, 'is_winter'] <- as.numeric(train[, 'season'] == '4')

  # Daily scores
  # (checked days from beginning to end have some news)
  counts <- tapply(rep(1, nrow(train)), train[, 'date'], sum)
  avg <- tapply(train[, 'popularity'], train[, 'date'], mean)
  sds <- tapply(train[, 'popularity'], train[, 'date'], sd)

  # file <- paste(DATADIR, 'daily_scores.RData', sep = '')
  # save(counts, avg, sds, file = file); cat('Saved file:', file, '\n')

  m1 <- match(as.character(train[, 'date']), names(counts))
  m2 <- match(as.character(train[, 'date']), names(avg))
  m3 <- match(as.character(train[, 'date']), names(sds))
  train[, 'day_news'] <- counts[m1]
  train[, 'day_avg_pop'] <- avg[m2]
  train[, 'day_sd_pop'] <- sds[m3]

  # m1 <- match(as.character(np.train[, 'date'] - 1), names(counts))
  # m2 <- match(as.character(np.train[, 'date'] - 1), names(avg))
  # m3 <- match(as.character(np.train[, 'date'] - 1), names(sds))
  # np.train[, 'day_news_lag1'] <- counts[m1]
  # np.train[, 'day_avg_pop_lag1'] <- avg[m2]
  # np.train[, 'day_sd_pop_lag1'] <- sds[m3]

  # m1 <- match(as.character(np.train[, 'date'] - 2), names(counts))
  # m2 <- match(as.character(np.train[, 'date'] - 2), names(avg))
  # m3 <- match(as.character(np.train[, 'date'] - 2), names(sds))
  # np.train[, 'day_news_lag2'] <- counts[m1]
  # np.train[, 'day_avg_pop_lag2'] <- avg[m2]
  # np.train[, 'day_sd_pop_lag2'] <- sds[m3]

  # m1 <- match(as.character(np.train[, 'date'] - 3), names(counts))
  # m2 <- match(as.character(np.train[, 'date'] - 3), names(avg))
  # m3 <- match(as.character(np.train[, 'date'] - 3), names(sds))
  # np.train[, 'day_news_lag3'] <- counts[m1]
  # np.train[, 'day_avg_pop_lag3'] <- avg[m2]
  # np.train[, 'day_sd_pop_lag3'] <- sds[m3]

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
                'day_sd_pop')#, 'day_news_lag1', 'day_avg_pop_lag1',
                # 'day_sd_pop_lag1', 'day_news_lag2', 'day_avg_pop_lag2',
                # 'day_sd_pop_lag2', 'day_news_lag3', 'day_avg_pop_lag3',
                # 'day_sd_pop_lag3')

  # Standardized features
  #std.vars <- ori.vars[! ori.vars %in% 'popularity']
  std.vars <- c('n_tokens_title', 'n_tokens_content', 'kw_avg_min',
                'kw_max_max', 'kw_min_avg')
  for (col in std.vars) {
    end.col <- paste(col, 'std', sep = '_')
    train[, end.col] <- (train[, col] - mean(train[, col])) / sd(train[, col])
  }

  # Logarithmic variables
  #log.vars <- ori.vars[! ori.vars %in% 'popularity']
  log.vars <- c('timedelta', 'kw_max_min', 'kw_min_max', 'kw_avg_max',
                'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares',
                'self_reference_max_shares', 'self_reference_avg_sharess')
  for (col in log.vars) {
    end.col <- paste('log', col, sep = '_')
    train[, end.col] <- log(train[, col] + 2)
  }

  # New features added
  std.cols <- paste(std.vars, 'std', sep = '_')
  log.cols <- paste('log', log.vars, sep = '_')

  # Final variables
  final.vars <- c(nat.vars, new.vars, std.cols, log.cols)
  np.train <- train

  # Save results
  file1 <- paste(DATADIR, 'final_variable_list.RData', sep = '')
  file2 <- paste(DATADIR, 'news_popularity_training_extended.RData', sep = '')
  save(final.vars, nat.vars, new.vars, std.cols, log.cols, ori.vars,
       file = file1)
  cat('Saved file:', file1, '\n')
  save(np.train, file = file2); cat('Saved file:', file2, '\n')

  # End
  end.script(begin = bs, end = Sys.time())
}
# END OF SCRIPT
