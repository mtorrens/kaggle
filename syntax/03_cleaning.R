################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition
# Script  : 03_cleaning.R
################################################################################
# Author   : Miquel Torrens, 2016.01.31
# Modified : -
################################################################################
# source('/Users/miquel/Desktop/bgse/projects/kaggle/syntax/00_start.R')
# source(paste(SYNTAXDIR, '03_cleaning.R', sep = ''))
################################################################################

# Load data
file <- paste(DATADIR, 'news_popularity_training.RData', sep = '')
np.train <- get(load(file = file)); cat('Loaded file:', file, '\n')

# Interesting columns (chosen by visual inspection from 02)
#cols <- colnames(np.train)
cols <- c('popularity', 'timedelta', 'n_tokens_title', 'n_tokens_content',
          'num_hrefs', 'num_self_hrefs', 'num_imgs', 'num_videos',
          'data_channel_is_lifestyle', 'data_channel_is_entertainment',
          'data_channel_is_bus', 'data_channel_is_socmed',
          'data_channel_is_tech', 'data_channel_is_world', 'kw_min_min',
          'kw_max_min', 'kw_avg_min', 'kw_min_max', 'kw_avg_max', 'kw_min_avg',
          'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares',
          'self_reference_max_shares', 'self_reference_avg_sharess',
          'weekday_is_monday', 'weekday_is_tuesday', 'weekday_is_wednesday',
          'weekday_is_thursday', 'weekday_is_friday', 'weekday_is_saturday',
          'weekday_is_sunday', 'is_weekend', 'LDA_01', 'LDA_02', 'LDA_03',
          'LDA_04', 'title_subjectivity', 'title_sentiment_polarity',
          'abs_title_sentiment_polarity')

# Set new data set
np.train <- np.train[, cols]

# See if some observations can be considered outliers
fives <- which(np.train[, 'popularity'] == 5)
killed <- c()
for (col in cols) {
  if (class(np.train[, col]) != 'character') {
    #thres <- quantile(np.train[, col], probs = c(0.005, 0.995))
    thres <- quantile(np.train[, col], probs = c(0.01, 0.99))
    outliers <- which(np.train[, col] < thres[1] | 
                      np.train[, col] > thres[2])
    killed <- unique(c(killed, outliers))
  }
}
killed <- killed[-which(killed %in% fives)]
cat('Total number of observations:', nrow(np.train), '\n')
cat('Number of outliers detected:', length(killed), '\n')

# We keep the rest
tt <- table(np.train[, 'popularity'])
print(tt / sum(tt))
survivors <- (1:nrow(np.train))[-killed]
np.train <- np.train[survivors, ]

# Check things are still constant
tt <- table(np.train[, 'popularity'])
print(tt / sum(tt))  # Nice!


################################################################################
# Fit a linear model to see which features seem to correlate
m01 <- lm(popularity ~ ., data = np.train)
summary(m01)

cols.out <- c('kw_max_min', 'kw_avg_min', 'weekday_is_saturday',
              'weekday_is_saturday', 'weekday_is_sunday',
              'abs_title_sentiment_polarity')#, 'weekday_is_monday')
cols2 <- cols[-which(cols %in% cols.out)]

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

# New variables
np.train[, 'timedelta_bin'] <- 0
np.train[which(np.train[, 'timedelta'] >= 400), 'timedelta_bin'] <- 1

np.train[, 'kw_min_min_cat0'] <- 0
np.train[, 'kw_min_min_cat1'] <- 0
np.train[, 'kw_min_min_cat2'] <- 0
np.train[which(np.train[, 'kw_min_min'] ==  -1), 'kw_min_min_cat0'] <- 1
np.train[which(np.train[, 'kw_min_min'] ==   4), 'kw_min_min_cat1'] <- 1
np.train[which(np.train[, 'kw_min_min'] == 217), 'kw_min_min_cat2'] <- 1

np.train[, 'kw_avg_max_bin'] <- 0
np.train[which(np.train[, 'kw_avg_max'] >= 1e5), 'kw_avg_max_bin'] <- 1

np.train[, 'kw_min_avg_bin'] <- 0
np.train[which(np.train[, 'kw_min_avg'] >= 0), 'kw_min_avg_bin'] <- 1

np.train[, 'LDA_01_bin'] <- 0
np.train[, 'LDA_02_bin'] <- 0
np.train[, 'LDA_03_bin'] <- 0
np.train[, 'LDA_04_bin'] <- 0
np.train[which(np.train[, 'LDA_01'] >= 0.1), 'LDA_01_bin'] <- 1
np.train[which(np.train[, 'LDA_02'] >= 0.1), 'LDA_02_bin'] <- 1
np.train[which(np.train[, 'LDA_03'] >= 0.1), 'LDA_03_bin'] <- 1
np.train[which(np.train[, 'LDA_04'] >= 0.1), 'LDA_04_bin'] <- 1

q0 <- which(np.train[, 'title_subjectivity'] <  0.15)
q1 <- which(np.train[, 'title_subjectivity'] >= 0.15)
q2 <- which(np.train[, 'title_subjectivity'] >= 0.6)
np.train[, 'title_subjectivity_cat0'] <- 0
np.train[, 'title_subjectivity_cat1'] <- 0
np.train[, 'title_subjectivity_cat2'] <- 0
np.train[q0, 'title_subjectivity_cat0'] <- 1
np.train[q1, 'title_subjectivity_cat1'] <- 1
np.train[q2, 'title_subjectivity_cat2'] <- 1

q0 <- which(np.train[, 'title_sentiment_polarity'] <  0)
q1 <- which(np.train[, 'title_sentiment_polarity'] == 0)
q2 <- which(np.train[, 'title_sentiment_polarity'] >  0)
np.train[, 'title_sentiment_polarity_cat0'] <- 0
np.train[, 'title_sentiment_polarity_cat1'] <- 0
np.train[, 'title_sentiment_polarity_cat2'] <- 0
np.train[q0, 'title_sentiment_polarity_cat0'] <- 1
np.train[q1, 'title_sentiment_polarity_cat1'] <- 1
np.train[q2, 'title_sentiment_polarity_cat2'] <- 1

#save(np.train, file = '~/Desktop/prova2.RData')

# Add to previous columns
cols3 <- c(cols2, 'timedelta_bin', 'kw_min_min_cat0', 'kw_min_min_cat1',
           'kw_min_min_cat2', 'kw_avg_max_bin', 'kw_min_avg_bin', 'LDA_01_bin',
           'LDA_02_bin', 'LDA_03_bin', 'LDA_04_bin', 'title_subjectivity_cat0',
           'title_subjectivity_cat1', 'title_subjectivity_cat2',
           'title_sentiment_polarity_cat0', 'title_sentiment_polarity_cat1',
           'title_sentiment_polarity_cat2')

# Third model
m03 <- lm(popularity ~ ., data = np.train[, cols3])
summary(m03)

# KNN
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

# Random Forest
cols5 <- cols3[which(! cols3 %in% c('popularity', 'kw_min_min'))]
rf <- randomForest(y = as.factor(cl[nt]), x = np.train[nt, cols5],
                   ntree = 5000, nodesize = 5)
#rf <- randomForest(y = as.factor(cl[nt]), x = np.train[nt, cols5], ntree = 1000)
#rf <- randomForest(popularity ~ ., data = np.train[nt, cols3])
preds <- predict(rf, newdata = np.train[ne, cols5])
#preds <- predict(rf, newdata = np.train[ne, cols3])

sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))  # 0.5234471

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

# Multinomial logit
mn <- multinom(popularity ~ ., data = np.train[nt, cols3])
preds <- predict(mn, newdata = np.train[ne, ])

sum(diag(table(preds, cl[ne]))) / sum(table(preds, cl[ne]))





