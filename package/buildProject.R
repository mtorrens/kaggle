
################################################################################

# 1. The working directory should be set to the location of this script.\n")
# 2. Run the following to build the package.
if (!require(devtools))
{
  install.packages("devtools")
  library(devtools)
}
if (!require(roxygen2))
{
  install.packages("roxygen2")
  library(roxygen2)
}


setwd("unlikelies")
devtools::uninstall()

pkgs <- c('survival', 'gbm', 'rpart', 'maboost', 'randomForest', 'caTools', 'dplyr', 'assertthat')
for (pkg in pkgs) {
  devtools::use_package(pkg)  
}
devtools::document()
devtools::check()

################################################################################

# 3. Installation options

#devtools::install()
#devtools::install_github("https://github.com/mtorrens/kaggle/package/unlikelies")
devtools::install_github('mtorrens/kaggle/package/unlikelies', auth_token = 'c4c4e07d3765b7e649a74e633304876c84e69cc8')
#install.packages("unlikelies", type="source", repos=NULL)

library(unlikelies)
load("../../data/news_popularity_test.RData") # loads np.test
onp.predict(np.test)
