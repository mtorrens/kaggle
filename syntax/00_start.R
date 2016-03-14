################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition
# Script  : 00_start.R
################################################################################
# Author   : Miquel Torrens, 2016.01.24
# Modified : Miquel Torrens, 2016.02.11
################################################################################
# source('/Users/miquel/Desktop/bgse/projects/kaggle/syntax/00_start.R')
################################################################################

################################################################################
# Read command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Root path
if (is.na(args[1]) || .Platform['OS.type'] != 'unix') {
  PATH <- '/Users/miquel/Desktop/bgse/projects/kaggle/'  # Miquel
  if (.Platform['OS.type'] == 'windows') {
    PATH <- 'C:/OneDrive/BGSE/GitHub/kaggle/'  # Matthew
  }
  #PATH <- '/home/ubuntu/project/'
} else {
  PATH <- paste(system(toString(args[1]), intern = TRUE), '/', sep = '')
}

# Project Name
PROJECT <- 'DS16T2.ACM_KAGGLE_COMP'
################################################################################

################################################################################
# Define relative Paths
DOCDIR <- paste(PATH, 'doc/', sep = '')
DATADIR <- paste(PATH, 'data/', sep = '')
TEMPDIR <- paste(PATH, 'temp/', sep = '')
INPUTDIR <- paste(PATH, 'input/', sep = '')
OUTPUTDIR <- paste(PATH, 'output/', sep = '')
SYNTAXDIR <- paste(PATH, 'syntax/', sep = '')
SCRIPTSDIR <- paste(SYNTAXDIR, 'scripts/', sep = '')

# Create folders
try(dir.create(PATH, showWarnings = FALSE))
try(dir.create(DOCDIR, showWarnings = FALSE))
try(dir.create(DATADIR, showWarnings = FALSE))
try(dir.create(TEMPDIR, showWarnings = FALSE))
try(dir.create(INPUTDIR, showWarnings = FALSE))
try(dir.create(OUTPUTDIR, showWarnings = FALSE))
try(dir.create(SYNTAXDIR, showWarnings = FALSE))
try(dir.create(SCRIPTSDIR, showWarnings = FALSE))

# Project Index
source(paste(SYNTAXDIR, '00_index.R', sep = ''))
################################################################################

################################################################################
# Settings
# Check R version
check.version(dev.R = '3.2.2 x86_64')

# Print starting time
bs <- begin.script(script = paste('[', PROJECT, '] 00_start.R', sep = ''))

# Packages needed
load.packages(pkgs = c('class', 'randomForest', 'MASS', 'nnet', 'ggplot2',
                       'foreach', 'doMC'))

# Stone parameters
today <- format(Sys.time(), '%Y%m%d')

# # CRS
# CRS_GOOGLE <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# Record
end.script(begin = bs, end = Sys.time()); rm(bs)
################################################################################
# END OF SCRIPT
