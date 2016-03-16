################################################################################
# Load, and install if necessary, required packages.
################################################################################
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

cat("The working directory should be set to the location of this script.")

setwd("unlikelies")

devtools::document()
devtools::install()
devtools::check()

#install.packages("C:\\OneDrive\\BGSE\\GitHub\\kaggle\\package\\unlikelies")
#install.packages("C:\\Users\\Matthew\\Documents\\R\\win-library\\3.2\\unlikelies")

install.packages("unlikelies", type="source", repos=NULL)

load("../../data/news_popularity_test.RData")

library(unlikelies)

onp.predict(np.test)

devtools::install_github("https://github.com/mtorrens/kaggle/package/unlikelies")


devtools::install_github('mtorrens/kaggle/package/unlikelies', auth_token = 'c4c4e07d3765b7e649a74e633304876c84e69cc8')
onp.predict(np.test)
