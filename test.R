#test
rm(list = ls())
dir_data <- 'D:/Data/0427_xiaosong/'
###################################################################################################
# ### read and save all disk usage data
# # library and parameter
library(RMySQL)
in_name_all <- 'disk_usage_all.csv'
in_name_bad <- 'flist(0401-1231).Rda'

# read, name, type convert and save
data_all <- read.csv(paste(dir_data,in_name_all,sep=''),sep='\t',header = F)
names(data_all) <- c('ip','time','used','total')

#bad
load(paste(dir_data,in_name_bad,sep=''))
data_bad <- data.bad[,c('ip','f_time','class','fcount')]
data_bad <- subset(data_bad, ip %in% data_all$ip)
data_bad$ip <- factor(data_bad$ip)

#good
uni.all <- unique(data_all$ip)
data_good <- data.frame('ip' = setdiff(uni.all,data_bad$ip))
data_all$time <- as.POSIXct(data_all$time,tz = 'UTC')
save(data_all,data_good,data_bad,file = paste(dir_data,'alldata_delfactor.Rda',sep=''))

# in_name_good <- 'good_in.csv'
# in_name_bad <- 'bad_in.csv'
# data_good <- read.csv(paste(dir_data,in_name_good,sep=''),sep='\t',header = F)
# data_bad <- read.csv(paste(dir_data,in_name_bad,sep=''),sep='\t',header = F)
# names(data_good) <- c('ip')
# names(data_bad) <- c('ip','f_time')
# data_bad$f_time <- as.POSIXct(data_bad$f_time,tz = 'UTC')
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

