################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition
# Script  : 02_explore.R
################################################################################
# Author   : Miquel Torrens, 2016.01.24
# Modified : Miquel Torrens, 2016.01.31
################################################################################
# source('/Users/miquel/Desktop/bgse/projects/kaggle/syntax/00_start.R')
# source(paste(SYNTAXDIR, '02_explore.R', sep = ''))
################################################################################

# Load data
file <- paste(DATADIR, 'news_popularity_training.RData', sep = '')
np.train <- get(load(file = file)); cat('Loaded file:', file, '\n')

# Segments
ones <- which(np.train[, 'popularity'] == 1)
twos <- which(np.train[, 'popularity'] == 2)
threes <- which(np.train[, 'popularity'] == 3)
fours <- which(np.train[, 'popularity'] == 4)
fives <- which(np.train[, 'popularity'] == 5)

# Summary statistics
sink(paste(TEMPDIR, 'variable_summary.txt', sep = ''))
for (i in 1:ncol(np.train)) {
  if (class(np.train[, i]) != 'character') {
    variable <- colnames(np.train)[i]
    new.liner <- rep('=', 10 + nchar(variable))
    cat('VARIABLE:', variable, '\n')
    cat(new.liner, '\n* Summary:\n', collapse = '', sep = '')
    print(summary(np.train[, variable]))
    cat('* Cluster means:\n')
    print(tapply(np.train[, i], np.train[, 'popularity'], mean))
    cat('* Cluster standard deviations:\n')
    print(tapply(np.train[, i], np.train[, 'popularity'], sd))
    cat('* Cluster medians:\n')
    print(tapply(np.train[, i], np.train[, 'popularity'], median))
    cat('\n')
  }
}
sink()

# Bivariate plots
k <- 1.3
for (i in 1:ncol(np.train)) {
  if (class(np.train[, i]) != 'character') {
    # Plot 1
    variable <- colnames(np.train)[i]
    png(paste(TEMPDIR, 'biv_', variable, '.png', sep = ''))
    dens <- density(np.train[ones, i])
    plot(dens, ylim = c(0, k * max(dens[['y']])), main = variable,
         col = 'red', lwd = 2)
    lines(density(np.train[twos, i]), col = 'orange', lwd = 2)
    lines(density(np.train[threes, i]), col = 'blue', lwd = 2)
    lines(density(np.train[fours, i]), col = 'darkgreen', lwd = 2)
    lines(density(np.train[fives, i]), col = 'black', lwd = 2)
    legend('topright', as.character(1:5),
           col = c('red', 'orange', 'blue', 'darkgreen', 'black'), lwd = 2)
    dev.off()

    # Plot 2
    png(paste(TEMPDIR, 'dens_', variable, '.png', sep = ''))
    plot(density(np.train[, i]), main = variable, col = 'red', lwd = 2)
    dev.off()

    # Boxplot
    png(paste(TEMPDIR, 'box_', variable, '.png', sep = ''))
    np <- np.train[, c('popularity', variable)]
    colnames(np) <- c('popularity', 'variable')
    pt <- ggplot(np, aes(x = popularity, y = variable,
                         group = popularity)) +
            geom_boxplot() +
            ggtitle(variable)
    print(pt)
    #boxplot(np.train[, i])
    #form <- as.formula(paste('popularity ~', variable))
    #boxplot(form, data = np.train, main = variable, col = 'red', lwd = 2)
    dev.off()
  }
}
# END OF SCRIPT
