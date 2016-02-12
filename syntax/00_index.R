################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course  : Advanced Computational Methods
# Project : Kaggle Competition
# Script  : 00_index.R
################################################################################
# Author   : Miquel Torrens, 2016.01.24
# Modified : Miquel Torrens, 2016.02.08
################################################################################
# source('/Users/miquel/Desktop/bgse/projects/kaggle/syntax/00_start.R')
################################################################################

################################################################################
# Generic
source(paste(SCRIPTSDIR, 'functions_generic.R', sep = ''))
source(paste(SCRIPTSDIR, 'package_function.R', sep = ''))
################################################################################

################################################################################
# Load full scripts
source(paste(SYNTAXDIR, '01_load.R', sep = ''))
source(paste(SYNTAXDIR, '02_explore.R', sep = ''))
source(paste(SYNTAXDIR, '03_features.R', sep = ''))
source(paste(SYNTAXDIR, '04_models.R', sep = ''))
source(paste(SYNTAXDIR, '05_predict_test.R', sep = ''))
################################################################################
# END OF SCRIPT
