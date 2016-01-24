################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Script   : functions_generic.R
# Descrip. : generic user-made R functions for general purposes
################################################################################
# Author   : (c) Miquel Torrens, 2015.10.18
# Modified :     Miquel Torrens, 2015.11.13
################################################################################

################################################################################
# Function list
# 1. begin.script()
# 2. end.script()
# 3. cq()
# 4. load.packages()
# 5. check.version()
# 6. factor2char()
# 7. chunk()
################################################################################

################################################################################
begin.script <- function(script = NULL) {
# (c) Miquel Torrens, 2015.01.26
# Visual exposition of the name of a script and the starting time of execution
# - script (string) : character string with a layout-type script name
################################################################################
  # Recording current time
  now <- Sys.time()
  if (is.null(script)) { return(invisible(now)) }

  # Printing starting time
  cat(script, '\n', rep('=', nchar(script)), '\n', sep = '')
  cat('Start: ', format(now, '%d.%m.%Y %H:%M'), '\n', sep = '')
  return(invisible(now))
}

################################################################################
end.script <- function(begin, end = Sys.time()) {
# (c) Miquel Torrens, 2015.01.26
# Visual exposition of the amount of execution time for the current script
# - begin (date) : time in which the execution began
# - end   (date) : time in which the execution ended
################################################################################
  # Calculating hours, minutes and seconds of difference
  difference <- floor(as.numeric(difftime(end, begin, units = 'secs')))
  secs <- 60 * (difference / 60 - floor(difference / 60))
  remaining <- difference - secs
  mins <- floor(remaining / 60)
  mins <- 60 * (mins / 60 - floor(mins / 60))
  remaining <- difference - mins * 60 - secs
  hour <- remaining / 3600

  # Rounding (just for layout)
  secs <- round(secs, 0)
  mins <- round(mins, 0)
  hour <- round(hour, 0)

  # Character string to be printed
  total <- paste(hour, 'H ', mins, "' ", secs, '"', sep = '')
  if (hour == 0) {
    total <- paste(mins, "' ", secs, '"', sep = '')
    #if (mins == 0) {
    #  total <- paste(secs, '"', sep = '')
    #}
  }
  cat('* END Script: ', total, '\n\n', sep = '')
}

################################################################################
cq <- function(...) {
# (c) Miquel Torrens, 2015.02.02
################################################################################
  .Internal(quit(save = 'no', ...))
}

################################################################################
load.packages <- function(pkgs) {
# (c) Miquel Torrens, 2015.10.18
# pkgs (string): vector containing the names of the packages to be loaded
################################################################################
  if (length(pkgs) > 0) {
    for (pkg in pkgs) {
      # Install if necessary
      if (! pkg %in% installed.packages()[, 1]) {
        aux <- try(install.packages(pkg))
        if (class(aux) != 'try-error') {
          cat('Installed package:', pkg, '\n')
        } else {
          stop('unable to install package ', pkg)
        }
      }

      # Load the packages
      aux <- try(library(pkg, character.only = TRUE))
      if (class(aux) != 'try-error') {
        cat('Library:', pkg, '\n')
      } else {
        stop('unable to load package ', pkg)
      }
    }
  }
}

################################################################################
check.version <- function(dev.R = NULL) {
# dev.R (string): version of R under which the project was developed
################################################################################
  if (is.null(dev.R)) { dev.R <- '3.2.1 x86_64' }
  exc.R <- paste(R.Version()['major'], '.', R.Version()['minor'], ' ',
                 R.Version()['arch'], sep = '')
  if (exc.R != dev.R) {
    warning('This project was developed under R version ', dev.R,
            '.\nCurrently using: R ', exc.R)
  }
}

################################################################################
factor2char <- function(df) {
################################################################################
  for (col in 1:ncol(df)) {
    if (is.factor(df[, col])) {
      df[, col] <- as.character(df[, col])
    }
  }
  return(df)
}

################################################################################
chunk <- function(x, n) {
################################################################################
  chunks <- split(x, factor(sort(rank(x) %% n)))
  return(chunks)
}
# END OF SCRIPT
