# select disk usage information based on a iplist
rm(list = ls())

# library and parameter
library(RMySQL)
dir_data <- 'D:/Data/0427_xiaosong/'
in_name_good <- 'disk_usage_good_ftr.Rda'
in_name_bad <- 'disk_usage_bad_ftr.Rda'
#========================================================================================================
# feature analysis
load(paste(dir_data,in_name_good,sep=''))
na.idx <- colSums(t(is.na(ftr.du))) != 0
ftr.good <- ftr.du[!na.idx,]
load(paste(dir_data,in_name_bad,sep=''))
na.idx <- colSums(t(is.na(ftr.du))) != 0
ftr.bad <- ftr.du[!na.idx,]
