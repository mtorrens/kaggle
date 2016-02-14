library(devtools)
library(roxygen2)

#create("unlikelies")
setwd("C:\\OneDrive\\BGSE\\GitHub\\kaggle\\package\\unlikelies")

devtools::document()
devtools::install()
devtools::check()

#install.packages("C:\\OneDrive\\BGSE\\GitHub\\kaggle\\package\\unlikelies")
#install.packages("C:\\Users\\Matthew\\Documents\\R\\win-library\\3.2\\unlikelies")

library(unlikelies)
