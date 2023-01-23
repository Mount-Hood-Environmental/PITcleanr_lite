#' @title Loads or installs packages from CRAN
#'
#' @description 
#'
#' @author Mark Roes
#'
#' @param package_list list of required packages
#' 


packageLoadInstall = function(package_list = c("plyr","tidyverse","magrittr", "lubridate","esquisse","readr","janitor","nhdplusTools", "sf", "ggraph", "readxl")){
  
  options(repos = structure(c(cran="https://ftp.osuosl.org/pub/cran/")))  
  
  if (length(setdiff(package_list, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(package_list, rownames(installed.packages())))  
  }  
  lapply(package_list, require, character.only=TRUE)
  
}