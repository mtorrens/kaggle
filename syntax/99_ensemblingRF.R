################################################################################
source('/Users/miquel/Desktop/bgse/projects/kaggle/syntax/00_start.R')
################################################################################

################################################################################
# Original file
file <- paste(DATADIR, 'news_popularity_test.RData', sep = '')
orig <- get(load(file = file)); cat('Loaded file:', file, '\n')

# Test file
file <- paste(DATADIR, 'news_popularity_test_extended.RData', sep = '')
np.test <- get(load(file = file)); cat('Loaded file:', file, '\n')

# Training data
file <- paste(DATADIR, 'news_popularity_training_extended.RData', sep = '')
np.train <- get(load(file = file)); cat('Loaded file:', file, '\n')

# Outlier threshold
file <- paste(DATADIR, 'outlier_threshold.RData', sep = '')
extremes <- get(load(file = file)); cat('Loaded file:', file, '\n')

# Sets of variables
file <- paste(DATADIR, 'final_variable_list.RData', sep = '')
load(file = file); cat('Loaded file:', file, '\n')
##############################################################################

##############################################################################
# Numeric
for (col in ori.vars) {
  np.train[, col] <- as.numeric(np.train[, col])  
}
for (col in ori.vars[ori.vars != 'popularity']) {
  np.test[, col] <- as.numeric(np.test[, col])  
}

# Linearly independent variables
best.vars <- unique(c(ori.vars, final.vars))
best.varsT <- best.vars[best.vars != 'popularity']
##############################################################################

##############################################################################
master <- function(split = 12000, step = 500, seed = 3050, ...) {
#split = 12000; step = 500; seed = 3050; ntrees = 300; nodesize = 10 # 53.24%
##############################################################################
  np.test2 <- cbind(np.test, rep(NA, nrow(np.test)))
  colnames(np.test2) <- c(colnames(np.test), 'popularity')
  np.test2 <- np.test2[, colnames(np.train)]
  np.total <- rbind(np.train, np.test2)
  np.total <- np.total[order(np.total[, 'timedelta'], decreasing = TRUE), ]
  niter <- round((nrow(np.total) - split) / step, 0)
  results <- data.frame(id = np.test[, 'id'])
  registerDoMC(cores = detectCores())
  it <- 1:niter
  pars <- foreach(it = 1:niter) %dopar% {
    first <- (it - 1) * step + 1
    last <- ifelse(it == niter, nrow(np.total), split + (it - 1) * step)
    Phi <- np.total[first:last, c('id', best.vars)]
    y <- np.total[first:last, 'popularity']
    empty <- which(is.na(y))
    non.empty <- which(! is.na(y))
    Phit <- Phi[non.empty, best.vars]
    Phis <- Phi[empty, best.varsT]
    yt <- y[non.empty]
    ys <- data.frame(id = Phi[empty, 'id'])
    set.seed(seed)
    rf <- randomForest(as.factor(popularity) ~ ., data = Phit, xtest = Phis, ...)
    preds <- rf$predicted
    acc <- sum(as.numeric(yt) == as.numeric(preds)) / length(yt)
    ys[, 'pred'] <- rf$test$predicted    
    return(list(acc = acc, ys = ys))
  }
  accs <- sapply(pars, `[[`, 1)
  for (i in 1:length(pars)) {
    m <- match(results[, 1], pars[[i]][[2]][, 1])
    results[, paste('it', i, sep = '')] <- pars[[i]][[2]][m, 2]
  }
  preds1 <- apply(results, 1, function(x) {
    names(which.max(tapply(accs, x[2:length(x)], sum, na.rm = TRUE)))[1]
  })
  preds2 <- apply(results, 1, function(x) {
    as.numeric(names(tail(sort(table(x[2:length(x)])), 1)))
  })
  return(list(preds1 = preds1, preds2 = preds2)))
}

# Optimize
master2 <- master
master <- compiler::cmpfun(master)

##############################################################################
# Run it
sps <- seq(8000, 14000, 1000)
sts <- seq(500, 2000, 300)
nts <- seq(300, 900, 200)
nss <- c(25, 25, 10)
seed <- 666

aux1 <- expand.grid(sps, sts, nts, nss)
aux2 <- expand.grid(sps, sts)
aux1[, 5] <- NA
aux1[, 6] <- NA
aux2[, 3] <- NA
aux2[, 4] <- NA
colnames(aux1) <- c('split', 'step', 'ntree', 'nodesize', 'wacc', 'vacc')
colnames(aux2) <- c('split', 'step', 'wacc', 'vacc')

for (i in 4:nrow(aux1)) {
  sp <- aux1[i, 1]
  st <- aux1[i, 2]
  nt <- aux1[i, 3]
  ns <- aux1[i, 4]
  cat('nt:', nt, 'ns:', ns, '')
  res <- master(split = sp, step = st, seed = seed, ntree = nt, nodesize = ns)
  aux1[i, 5] <- res[1]
  aux1[i, 6] <- res[2]
}

for (i in 1:nrow(aux2)) {
  sp <- aux2[i, 1]
  st <- aux2[i, 2]
  res <- master(split = sp, step = st, seed = seed)
  aux2[i, 3] <- res[1]
  aux2[i, 4] <- res[2]
}


seeds <- seq(2000, 4000, 50)
for (i in seeds) {
  res <- master(split = 12000, step = 500, seed = i, ntree = 300, nodesize = 10)
}

master(split = 10000, step = 1000, seed = 1234, ntree = 300, mtry = 4)
master(split = 12000, step = 1000, seed = 1234, ntree = 300, mtry = 4)
master(split = 10000, step = 1000, seed = 666, ntree = 400, mtry = 4)
master(split = 12000, step = 500, seed = 666, ntree = 300, nodesize = 10, mtry = 4)
##############################################################################
