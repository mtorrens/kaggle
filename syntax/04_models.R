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
main.04 <- function(lr = FALSE, k.nn = FALSE, svmac = FALSE, nn = FALSE,
                    olreg = FALSE, linda = FALSE, mnomial = FALSE,
                    stocgd = FALSE, do.optimize = FALSE, ncores = 3) {
################################################################################
  # Print starting time
  bs <- begin.script(paste('[', PROJECT, '] 03_features.R', sep = ''))

  ##############################################################################
  # Random Forest (RF)
  # Adaptive Boosting (AdaBoost)
  # SVM with a Radial Basis Function (RBF) kernel
  # K-Nearest Neighbors (KNN) 
  # Naive Bayes (NB)
  ##############################################################################

  # Load data
  file1 <- paste(DATADIR, 'news_popularity_training_extended.RData', sep = '')
  file2 <- paste(DATADIR, 'final_variable_list.RData', sep = '')
  np.train <- get(load(file = file1)); cat('Loaded file:', file1, '\n')
  load(file = file2); cat('Loaded file:', file2, '\n')

  # Prune NAs
  np.train <- np.train[complete.cases(np.train), ]
  final.vars <- unique(c('popularity', final.vars))

  # Converting to numeric
  for (col in final.vars) {
    np.train[, col] <- as.numeric(np.train[, col])  
  }
  
  ##############################################################################
  # Baseline linear regression to see important features
  if (lr == TRUE) {
    cat('Running baseline Linear Regression... ')
    m01 <- lm(popularity ~ ., data = np.train[, final.vars])
    cat('Done!\n')
    summary(m01)
  }

  # Columns non-linearly independent
  cols.out <- c('weekday_is_wednesday', 'weekday_is_sunday',
                'weekday_is_saturday', 'date', 'is_2013', 'is_2014',
                'month', 'is_aug', 'is_jun', 'title_subjectivity_cat1',
                'title_sentiment_polarity_cat2', 'n_tokens_title',
                'n_tokens_content', 'kw_avg_min', 'kw_max_max',
                'kw_min_avg')
  new.vars <- final.vars[! final.vars %in% cols.out]

  # New baseline regression
  if (lr == TRUE) {
    m02 <- lm(popularity ~ ., data = np.train[, new.vars])
    summary(m02)
  }
  ##############################################################################

  ##############################################################################
  # KNN
  set.seed(666)
  nt <- sample(1:nrow(np.train), floor(0.8 * nrow(np.train)))
  ne <- (1:nrow(np.train))[! (1:nrow(np.train)) %in% nt]

  # Define test and training sets
  new.varsT <- new.vars[new.vars != 'popularity']
  cl <- np.train[, 'popularity']

  # Run for different k's
  if (k.nn == TRUE) {
    # Optimize KNN parameters
    train <- np.train[, new.varsT]
    for (k in seq(1, 55, 2)) {
      cat('Computing ', k, '-NN... ', sep = '')
      preds <- knn(cl = cl[nt], train = train[nt, new.varsT],
                   test = train[ne, new.varsT], k = k)
      tt <-  table(preds, cl[ne])
      assign(paste('ttr', k, sep = ''), tt)
      assign(paste('percr', k, sep = ''), sum(diag(tt)) / sum(tt))  # 0.3941667
      cat('Done!\n')
    }
  }
  ##############################################################################

  ##############################################################################
  # Ordinal Logistic Regression
  if (olreg == TRUE) {
    #set.seed(666)
    #new.cols <- cols2[cols2 %in% colnames(train)]
    #nt <- sample(1:nrow(np.train), floor(0.8 * nrow(np.train)))
    #ne <- (1:nrow(np.train))[! (1:nrow(np.train)) %in% nt]
    cl <- np.train[, final.vars][, 1]

    # Not working
    if (FALSE) {
      olr <- polr(as.factor(popularity) ~ ., data = np.train[nt, final.vars])
      preds <- predict(olr, newdata = np.train[ne, final.vars])
      right <- sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))
      print(right)  # 0.?
    }
  }
  ##############################################################################

  ##############################################################################
  # Random Forest
  set.seed(666)
  #final.varsT <- final.vars[final.vars != 'popularity']
  final.varsT <- new.vars[new.vars != 'popularity']
  rf <- randomForest(y = as.factor(cl[nt]),
                     x = np.train[nt, final.varsT],
                     ntree = 1000, nodesize = 25)

  preds <- predict(rf, newdata = np.train[ne, final.varsT])
  sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))  # 0.5234471
                                                               # 0.5307113
                                                               # 0.5331667
  
  # Random Forest
  set.seed(666)
  #rf <- randomForest(y = as.factor(np.train[, 'popularity'] < 2),
  rf <- randomForest(y = as.factor(np.train[nt, 'popularity']),
                     x = np.train[nt, final.varsT],
                     #x = np.train[, ori.varsT],
                     #x = np.train[, new.vars],
                     ntree = 1200, nodesize = 10)

  # Predictions
  preds <- predict(rf, newdata = np.train[ne, ])
  tt <- table(preds, cl[ne])
  res <- sum(diag(tt)) / sum(tt)

  # Optimize parameters of the Random Forest
  if (do.optimize == TRUE) {
    res <- matrix(nrow = 0, ncol = 3)
    colnames(res) <- c('ntrees', 'nodesize', 'accuracy')
    registerDoMC(cores = 3)                                                        
    for (n in rev(seq(100, 1500, 100))) {
      # Parallelize computations on three cores
      new.res <- foreach(s = seq(10, 80, 5)) %dopar% {
      #new.res <- foreach(s = seq(5, 50, 5)) %dopar% {
        set.seed(666)
        rf <- randomForest(y = as.factor(np.train[nt, 'popularity']),
                           x = np.train[nt, final.varsT],
                           #x = np.train[, ori.varsT],
                           ntree = n, nodesize = s)

        # Predictions
        preds <- predict(rf, newdata = np.train[ne, ])
        tt <- table(preds, cl[ne])
        acc <- sum(diag(tt)) / sum(tt)

        # Finish
        cat('n:', n, 's:', s, 'acc:', round(acc, 4), '\n')
        return(c(n, s, acc))
      }

      # Collect results
      rf.accuracy <- cbind(as.numeric(sapply(new.res, `[`, 1)),
                           as.numeric(sapply(new.res, `[`, 2)),
                           as.numeric(sapply(new.res, `[`, 3)))
      colnames(rf.accuracy) <- colnames(res)
      res <- as.data.frame(rbind(res, rf.accuracy))
      file <- paste(TEMPDIR, 'optimal_rf2.RData', sep = '')
      save(res, file = file); cat('Saved file:', file, '\n')
    }

    # Optimal choice
    res[which.max(res[, 'accuracy']), ]
    tapply(res[, 'accuracy'], res[, 'ntrees'], mean)
    tapply(res[, 'accuracy'], res[, 'nodesize'], mean)

    # Optimize seed
    # Parallelize computations on three cores
    res <- matrix(nrow = 0, ncol = 2)
    colnames(res) <- c('seed', 'accuracy')
    registerDoMC(cores = ncores)
    new.res <- foreach(seed = seq(100, 10000, 50)) %dopar% {
      set.seed(seed)
      rf <- randomForest(y = as.factor(np.train[nt, 'popularity']),
                         x = np.train[nt, final.varsT],
                         #x = np.train[, ori.varsT],
                         #ntree = 100, nodesize = 65)
                         ntree = 1200, nodesize = 10)
                         #ntree = 100, nodesize = 5)

      # Predictions
      preds <- predict(rf, newdata = np.train[ne, ])
      tt <- table(preds, cl[ne])
      acc <- sum(diag(tt)) / sum(tt)

      # Finish
      cat('seed', seed, 'acc:', round(acc, 4), '\n')
      return(c(seed, acc))
    }

    # Collect results
    rf.accuracy <- cbind(as.numeric(sapply(new.res, `[`, 1)),
                         as.numeric(sapply(new.res, `[`, 2)))
    colnames(rf.accuracy) <- colnames(res)
    res <- as.data.frame(rbind(res, rf.accuracy))
    save(res, file = paste(TEMPDIR, 'optimal_seed.RData', sep = ''))
    cat('Saved file:', paste(TEMPDIR, 'optimal_seed.RData', sep = ''), '\n')
  }
  ##############################################################################

  ##############################################################################
  # LDA
  if (linda == TRUE) {
    cols4 <- c('popularity', 'timedelta', 'n_tokens_title', 'n_tokens_content',
               'num_hrefs', 'num_self_hrefs', 'num_imgs', 'num_videos',
               'data_channel_is_lifestyle', 'data_channel_is_entertainment',
               'data_channel_is_bus', 'data_channel_is_socmed',
               'data_channel_is_tech', 'data_channel_is_world', 'kw_min_min',
               'kw_min_max', 'kw_avg_max', 'kw_min_avg', 'kw_max_avg',
               'kw_avg_avg', 'self_reference_min_shares',
               'self_reference_max_shares', 'self_reference_avg_sharess',
               'weekday_is_monday', 'weekday_is_tuesday',
               'weekday_is_wednesday', 'weekday_is_thursday',
               'weekday_is_friday', 'LDA_01', 'LDA_02', 'LDA_03',
               'LDA_04', 'title_subjectivity', 'title_sentiment_polarity',
               'timedelta_bin', 'kw_min_min_cat0', 'kw_avg_max_bin',
               'LDA_01_bin', 'LDA_02_bin', 'LDA_03_bin', 'LDA_04_bin',
               'title_subjectivity_cat1', 'title_subjectivity_cat2',
               'title_sentiment_polarity_cat1',
               'title_sentiment_polarity_cat2')
    
    cm <- cor(np.train[nt, cols3])
    da <- lda(popularity ~ ., data = np.train[nt, cols4])
    res <- predict(da, newdata = np.train[ne, ])
    preds <- res$class
    sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))  # 51%
  }
  ##############################################################################

  ##############################################################################
  # Multinomial logit
  if (mnomial == TRUE) {
    mn <- multinom(popularity ~ ., data = np.train[nt, cols3])
    preds <- predict(mn, newdata = np.train[ne, ])
    sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))
  }
  ##############################################################################

  ##############################################################################
  # SVM
  if (svmac == TRUE) {
    library(e1071)

    # Model
    form <- as.formula(as.factor(popularity) ~ .)
    msvm <- svm(form, data = np.train[nt, final.vars], kernel = 'radial')

    # Predictions
    preds <- predict(msvm, newdata = np.train[ne, final.vars])
    tt <- table(preds, cl[ne])
    acc <- sum(diag(tt)) / sum(tt)
  }
  ##############################################################################

  ##############################################################################
  # Adaptive Boosting
  if (adaboost == TRUE) {
    library(maboost)

    # Run model
    mab <- maboost(as.factor(popularity) ~ ., data = np.train[nt, final.vars])

    # Predictions
    preds <- predict(mab, newdata = np.train[ne, final.vars])

    # Check accuracy
    tt <- table(preds, cl[ne])
    acc <- sum(diag(tt)) / sum(tt)
  }
  ##############################################################################

  ##############################################################################
  # SGD
  if (stocgd == TRUE) {
    library(sgd)

    # Model
    msgd <- sgd(as.factor(popularity) ~ ., data = np.train[nt, final.vars],
                model = 'glm', model.control = list(family = 'binomial'),
                sgd.control = list(npasses = 3, pass = TRUE, shuffle = FALSE))

    # # Predictions
    # num.np.test <- np.train[ne, final.varsT]
    # for (col in 1:ncol(num.np.test)) {
    #   num.np.test[, col] <- as.numeric(num.np.test[, col])
    # }
    # preds <- predict(msgd, x_test = num.np.test)

    # # Check accuracy
    # tt <- table(preds, cl[ne])
    # acc <- sum(diag(tt)) / sum(tt)
  }
  ##############################################################################

  ##############################################################################
  # Neural networks
  # Model
  if (nn == TRUE) {
    library(nnet)
    library(neuralnet)

    aux <- np.train[, final.vars]
    for (col in 1:ncol(aux)) {
      aux[, col] <- as.numeric(aux[, col])
    }

    out <- class.ind(np.train[nt, final.vars][, 'popularity'])
    mnn <- nnet(aux[nt, final.varsT], out, size = 3, maxit = 1000,
                softmax = TRUE)
    preds <- predict(mnn, newdata = np.test[ne, final.varsT], type = 'class')
    tt <- table(preds, cl[ne])
    acc <- sum(diag(tt)) / sum(tt)

    #f <- as.formula(paste("as.factor(popularity) ~",
    f <- as.formula(paste("popularity ~",
                    paste(final.varsT, collapse = " + ")))
    mnn <- neuralnet(f, data = aux, hidden = c(5, 3), linear.output = TRUE)

    # Predictions
    preds <- compute(mnn, aux[ne, ])$net.result
    tt <- table(preds, cl[ne])
    acc <- sum(diag(tt)) / sum(tt)
  }
  ##############################################################################

  # End
  end.script(begin = bs, end = Sys.time())
}
# END OF SCRIPT
