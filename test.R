#test
rm(list = ls())
dir_data <- 'D:/Data/0427_xiaosong/'
###################################################################################################
# ### read and save all disk usage data
# # library and parameter
library(RMySQL)
in_name_all <- 'disk_usage_all.csv'
in_name_good <- 'good_in.csv'
in_name_bad <- 'bad_in.csv'

# read, name, type convert and save
data_all <- read.csv(paste(dir_data,in_name_all,sep=''),sep='\t',header = F)
data_good <- read.csv(paste(dir_data,in_name_good,sep=''),sep='\t',header = F)
data_bad <- read.csv(paste(dir_data,in_name_bad,sep=''),sep='\t',header = F)
names(data_all) <- c('ip','time','used','total')
names(data_good) <- c('ip')
names(data_bad) <- c('ip','f_time')
data_all$time <- as.POSIXct(data_all$time,tz = 'UTC')
data_bad$f_time <- as.POSIXct(data_bad$f_time,tz = 'UTC')
save(data_all,data_good,data_bad,file = paste(dir_data,'alldata_delfactor.Rda',sep=''))
###################################################################################################
### read and save cmdb1104
# library(gdata)
# in_name <- 'cmdb1104_allattr.csv'
# cmdb <- read.table(paste(dir_data,in_name,sep=''))

###################################################################################################
### observe data
# in_name <- 'alldata.Rda'
# load(paste(dir_data,in_name,sep=''))
# 
# sub_good <- subset(data_all,ip %in% data_good$ip[sample(1:nrow(data_good),10)])
# sub_bad <- subset(data_all,ip %in% data_good$ip[sample(1:nrow(data_good),10)])
# sub_good <- sub_good[with(sub_good,order(ip,time)),]
# sub_bad <- sub_bad[with(sub_bad,order(ip,time)),]

