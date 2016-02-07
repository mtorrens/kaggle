################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition
# Script  : 04_models.R
################################################################################
# Author   : Miquel Torrens, 2016.02.05
# Modified : -
################################################################################
# source('/Users/miquel/Desktop/bgse/projects/kaggle/syntax/00_start.R')
# source(paste(SYNTAXDIR, '04_models.R', sep = ''))
################################################################################

################################################################################
main.04 <- function(lr = FALSE, k.nn = FALSE) {
################################################################################
  # Print starting time
  bs <- begin.script(paste('[', PROJECT, '] 03_features.R', sep = ''))

  # Load data
  file1 <- paste(DATADIR, 'news_popularity_training_extended.RData', sep = '')
  #file1 <- paste(DATADIR, 'news_popularity_training.RData', sep = '')
  file2 <- paste(DATADIR, 'final_variable_list.RData', sep = '')
  np.train <- get(load(file = file1)); cat('Loaded file:', file1, '\n')
  load(file = file2); cat('Loaded file:', file2, '\n')

  # Prune NAs
  np.train <- np.train[complete.cases(np.train), ]
  final.vars <- unique(c('popularity', final.vars))

  ##############################################################################
  # Baseline linear regression to see important features
  if (lr == TRUE) {
    cat('Running baseline Linear Regression... ')
    #m01 <- lm(popularity ~ ., data = np.train)
    m01 <- lm(popularity ~ ., data = np.train[, final.vars])
    cat('Done!\n')
    summary(m01)

    cols.out <- c('weekday_is_wednesday', 'weekday_is_sunday',
                  'weekday_is_saturday', 'date', 'is_2013', 'is_2014')
    new.vars <- final.vars[! final.vars %in% cols.out]
    m02 <- lm(popularity ~ ., data = np.train[, new.vars])
    summary(m02)

    # Eliminate some variables
    cols.out <- c('kw_max_min', 'kw_avg_min', 'weekday_is_saturday',
                  'weekday_is_saturday', 'weekday_is_sunday',
                  'abs_title_sentiment_polarity')#, 'weekday_is_monday')
    cols2 <- nat.vars[! nat.vars %in% cols.out]

    # Second model
    m02 <- lm(popularity ~ ., data = np.train[, cols2])
    summary(m02)

    # Check if new variables would make sense
    if (FALSE) {
      for (col in cols2) {
        plot(density(np.train[, col]), main = col, col = 'red', lwd = 2)
        readline('press any key:')
      }
    }

    # Add to previous columns
    cols3 <- c(cols2, 'timedelta_bin', 'kw_min_min_cat0', 'kw_min_min_cat1',
               'kw_min_min_cat2', 'kw_avg_max_bin', 'kw_min_avg_bin',
               'LDA_01_bin', 'LDA_02_bin', 'LDA_03_bin', 'LDA_04_bin',
               'title_subjectivity_cat0', 'title_subjectivity_cat1',
               'title_subjectivity_cat2', 'title_sentiment_polarity_cat0',
               'title_sentiment_polarity_cat1', 'title_sentiment_polarity_cat2')

    # Third model
    m03 <- lm(popularity ~ ., data = np.train[, cols3])
    summary(m03)
  }
  ##############################################################################

  ##############################################################################
  # KNN
  if (k.nn == TRUE) {
    cl <- np.train[, 1]
    train <- np.train[, 2:ncol(np.train)]

    for (k in seq(1, 15, 2)) {
      cat('Computing ', k, '-NN... ', sep = '')
      preds <- knn(cl = cl, train = train, test = train, k = k)
      tt <-  table(preds, cl)
      assign(paste('tt', k, sep = ''), tt)
      assign(paste('perc', k, sep = ''), sum(diag(tt)) / sum(tt))
      cat('Done!\n')
    }

    set.seed(666)
    new.cols <- cols2[cols2 %in% colnames(train)]
    nt <- sample(1:nrow(np.train), floor(0.8 * nrow(np.train)))
    ne <- (1:nrow(np.train))[! (1:nrow(np.train)) %in% nt]
    for (k in seq(1, 55, 2)) {
      cat('Computing ', k, '-NN... ', sep = '')
      preds <- knn(cl = cl[nt], train = train[nt, new.cols], test = train[ne, new.cols], k = k)
      #preds <- knn(cl = cl[nt], train = train[nt, ], test = train[ne, ], k = k)
      tt <-  table(preds, cl[ne])
      assign(paste('ttr', k, sep = ''), tt)
      assign(paste('percr', k, sep = ''), sum(diag(tt)) / sum(tt))
      cat('Done!\n')
    }

    # My K-NN
    source('~/Desktop/bgse/courses/term2/acm/problemSets/PS4/kNN.R')

    labs <- cl
    labs[ne] <- NA

    aa <- kNN(features = train, labels = labs, k = 3, p = 1, action = 'test')
    bb <- kNN(features = train, labels = labs, k = 3, p = 2, action = 'test')
    cc <- kNN(features = train, labels = labs, k = 3, p = Inf, action = 'test')

    sum(diag(table(aa[[1]][ne], cl[ne]))) / sum(table(aa[[1]][ne], cl[ne]))
    sum(diag(table(bb[[1]][ne], cl[ne]))) / sum(table(bb[[1]][ne], cl[ne]))
    sum(diag(table(cc[[1]][ne], cl[ne]))) / sum(table(cc[[1]][ne], cl[ne]))

    # # Mahalanobis
    # dists <- mahalanobis(train, colMeans(train), cov(train))
    # [...]
  }
  ##############################################################################

  ##############################################################################
  # Ordinal Logistic Regression
  set.seed(666)
  #new.cols <- cols2[cols2 %in% colnames(train)]
  nt <- sample(1:nrow(np.train), floor(0.8 * nrow(np.train)))
  ne <- (1:nrow(np.train))[! (1:nrow(np.train)) %in% nt]
  cl <- np.train[, final.vars][, 1]

  olr <- polr(as.factor(popularity) ~ ., data = np.train[nt, final.vars])
  preds <- predict(olr, newdata = np.train[ne, final.vars])
  right <- sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))
  print(right)  # 0.?

  ##############################################################################

  ##############################################################################
  # Random Forest
  final.varsT <- final.vars[final.vars != 'popularity']
  rf <- randomForest(y = as.factor(cl[nt]), x = np.train[nt, final.varsT],
                     ntree = 1000, nodesize = 25)
  # cols5 <- cols3[which(! cols3 %in% c('popularity', 'kw_min_min'))]
  # rf <- randomForest(y = as.factor(cl[nt]), x = np.train[nt, cols5],
  #                    ntree = 5000, nodesize = 5)
  # #rf <- randomForest(y = as.factor(cl[nt]), x = np.train[nt, cols5], ntree = 1000)
  #rf <- randomForest(popularity ~ ., data = np.train[nt, cols3])

  preds <- predict(rf, newdata = np.train[ne, final.varsT])

  #preds <- predict(rf, newdata = np.train[ne, final.vars])
  #preds <- predict(rf, newdata = np.train[ne, cols3])

  sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))  # 0.5234471
                                                               # 0.5307113
  
  # Optimize number of trees and node size                                                             
  comb <- matrix(nrow = length(seq(200, 5000, 200)),
                 ncol = length(seq(5, 50, 5)))
  rownames(comb) <- seq(200, 5000, 200)
  colnames(comb) <- seq(5, 50, 5)
  for (n in seq(200, 5000, 200)) {
    for (s in seq(5, 50, 5)) {
      cat('n:', n, 's:', s)
      trial <- randomForest(y = as.factor(cl[nt]),
                            x = np.train[nt, final.varsT],
                            ntree = n, nodesize = s)
      pr <- predict(trial, newdata = np.train[ne, final.varsT])
      acc <- sum(diag(table(pr, cl[ne]))) / sum(table(pr, cl[ne]))

      comb[which(seq(200, 5000, 200) == n), which(seq(5, 50, 5) == s)] <- acc
      cat('\n')
    }
  }


  ##############################################################################

  ##############################################################################
  # LDA
  cols4 <- c('popularity', 'timedelta', 'n_tokens_title', 'n_tokens_content',
             'num_hrefs', 'num_self_hrefs', 'num_imgs', 'num_videos',
             'data_channel_is_lifestyle', 'data_channel_is_entertainment',
             'data_channel_is_bus', 'data_channel_is_socmed',
             'data_channel_is_tech', 'data_channel_is_world', 'kw_min_min',
             'kw_min_max', 'kw_avg_max', 'kw_min_avg', 'kw_max_avg', 'kw_avg_avg',
             'self_reference_min_shares', 'self_reference_max_shares',
             'self_reference_avg_sharess', 'weekday_is_monday',
             'weekday_is_tuesday', 'weekday_is_wednesday', 'weekday_is_thursday',
             'weekday_is_friday', 'LDA_01', 'LDA_02', 'LDA_03',
             'LDA_04', 'title_subjectivity', 'title_sentiment_polarity',
             'timedelta_bin', 'kw_min_min_cat0', 'kw_avg_max_bin', 'LDA_01_bin',
             'LDA_02_bin', 'LDA_03_bin', 'LDA_04_bin', 'title_subjectivity_cat1',
             'title_subjectivity_cat2', 'title_sentiment_polarity_cat1',
             'title_sentiment_polarity_cat2')
  cm <- cor(np.train[nt, cols3])
  da <- lda(popularity ~ ., data = np.train[nt, cols4])

  res <- predict(da, newdata = np.train[ne, ])
  preds <- res$class

  sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))  # 51%
  ##############################################################################

  ##############################################################################
  # Multinomial logit
  mn <- multinom(popularity ~ ., data = np.train[nt, cols3])
  preds <- predict(mn, newdata = np.train[ne, ])

  sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))

  ##############################################################################

  ##############################################################################
  # SVM
  NULL
  ##############################################################################

  ##############################################################################
  # Gradient Boosting
  library(xgboost)
  library(gbm)

  ##############################################################################

  ##############################################################################
  # SGD
  NULL
  ##############################################################################

  ##############################################################################
  # Neural networks
  NULL
  ##############################################################################

  # End
  end.script(begin = bs, end = Sys.time())
}
# END OF SCRIPT
