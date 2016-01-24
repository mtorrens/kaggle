
################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition
# Script  : 01_load.R
################################################################################
# Author   : Miquel Torrens, 2016.01.24
# Modified : -
################################################################################
# source('/Users/miquel/Desktop/bgse/projects/kaggle/syntax/00_start.R')
# source(paste(SYNTAXDIR, '01_load.R', sep = ''))
################################################################################

################################################################################
main.01 <- function() {
################################################################################
  # Print starting time
  bs <- begin.script(paste('[', PROJECT, '] 01_load.R', sep = ''))

  ##############################################################################
  # Sample data
  file <- paste(INPUTDIR, 'news_popularity_sample.csv', sep = '')
  np.sample <- read.csv(file = file); cat('Loaded file:', file, '\n')

  # Modifications
  np.sample[, 1] <- as.factor(np.sample[, 1])

  # Save
  file <- paste(DATADIR, 'news_popularity_sample.RData', sep = '')
  save(np.sample, file = file); cat('Saved file:', file, '\n')
  ##############################################################################

  ##############################################################################
  # Test data
  file <- paste(INPUTDIR, 'news_popularity_test.csv', sep = '')
  np.test <- read.csv(file = file); cat('Loaded file:', file, '\n')

  # Modifications
  np.test <- factor2char(np.test)

  # Save
  file <- paste(DATADIR, 'news_popularity_test.RData', sep = '')
  save(np.test, file = file); cat('Saved file:', file, '\n')
  ##############################################################################

  ##############################################################################
  # Training data
  file <- paste(INPUTDIR, 'news_popularity_training.csv', sep = '')
  np.train <- read.csv(file = file); cat('Loaded file:', file, '\n')

  # Modifications
  np.train <- factor2char(np.train)

  # Save
  file <- paste(DATADIR, 'news_popularity_training.RData', sep = '')
  save(np.train, file = file); cat('Saved file:', file, '\n')
  ##############################################################################

  # End
  end.script(begin = bs, end = Sys.time())
}
# END OF SCRIPT
