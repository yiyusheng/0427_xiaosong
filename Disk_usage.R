# select disk usage information based on a iplist
rm(list = ls())

# library and parameter
library(RMySQL)
dir_data <- 'D:/Data/0427_xiaosong/'
out_name <- 'disk_usage.Rda'

# # read ip list
# csv_file <- 'ip_0401_1231.csv'
# csv_path <- paste(dir_data,csv_file,sep='')
# data.ip_all <- read.csv(csv_path)
# names(data.ip_all) <- c('sn','ip','f_time')
# 
# # # open a connection to remote Mysql server
# # drv<-dbDriver("MySQL")
# # con <- dbConnect(drv, DB,
# #                  groups = "Mysql_conn", 
# #                  default.file = "D:/Git/Config/Mysql_conn")
# # 
# # # create table of ip list to join with disk usage tables
# # name.iptable = '0427_ip'
# # if (!dbExistsTable(con,name.iptable)) {
# #   str.sql <- paste('CREATE TABLE ',name.iptable,' (ip varchar(128));',sep='')
# #   dbSendQuery(con, str.sql)
# #   dbWriteTable(con, name.iptable, ip_list,
# #                row.names = F, overwrite = T) 
# # 
# # # on exit
# #   on.exit(dbRemoveTable(con,name.iptable))
# #   on.exit(dbDisconnect(con))
# 
# # read data, add col name, order by ip, remove zero useage and zero total item
# # convert time, calculate usage_percent
# du_path <- paste(dir_data,du_name,sep='')
# data.du_ori <- read.csv(du_path,sep='\t',header = F)
# names(data.du_ori) <- c('ip','time','usage','total')
# data.du_ori <- data.du_ori[order(data.du_ori$ip),]
# data.du_ori <- subset(data.du_ori, usage != 0 & total != 0)
# data.du_ori$time <- as.POSIXct(data.du_ori$time,tz = 'UTC')
# data.du_ori$usage_percent <- data.du_ori$usage/data.du_ori$total
# 
# # duplicated for date of each ip
# idx.factor <- factor(data.du_ori$ip,levels = unique(data.du_ori$ip))
# idx.dup_each <- tapply(as.Date(data.du_ori$time),idx.factor,duplicated)
# idx.dup <- !(unlist(idx.dup_each))
# data.du_timedup <- data.du_ori[idx.dup,]
# 
# # calculate time dence and item length of each ip.
# idx.factor <- factor(data.du_timedup$ip,levels = unique(data.du_timedup$ip))
# data.ip <- data.frame('ip' = unique(idx.factor))
# # time dence
# t <- tapply(data.du_timedup$time,idx.factor,
#                         function(x) {
#                           it <- mean(x[2:length(x)] - x[1:(length(x)-1)])
#                           units(it) <- 'hours' 
#                           return(it)
#                         })
# data.ip$time_dence[match(data.ip$ip,names(t))] <- as.numeric(t)
# data.ip$time_dence[is.na(data.ip$time_dence)] <- 1000
# # length
# t <- tapply(data.du_timedup$time,idx.factor,length)
# data.ip$item_len[match(data.ip$ip,names(t))] <- as.numeric(t)
# # filter
# data.ip <- subset(data.ip,time_dence<=72 & item_len >= 100)
# # f_time add
# data.ip$f_time <- data.ip_all[match(data.ip$ip,data.ip_all$ip),'f_time']
# data.ip$ip <- factor(as.character(data.ip$ip))
# data.ip$f_time <- as.POSIXct(data.ip$f_time)
# rownames(data.ip) <- NULL
# # filter du based on data.ip
# data.du <- subset(data.du_timedup, ip %in% data.ip$ip)
# data.du$ip <- factor(as.character(data.du$ip))
# save(data.ip,data.du,file = paste(dir_data,out_name,sep=''))

# feature extraction (mean,std of usage during 3ds,7ds,15ds before and after time of failure)
load(paste(dir_data,out_name,sep=''))
extra_ftr <- function(f_ip,f_time,data.du) {
  cur.data.du <- subset(data.du,ip == f_ip)
  itv <- c(-30,-15,-7,-3,0,3,7,15,30)*24*60*60
  time_point <- as.Date(f_time + itv)
  time_rangea <- expand.grid(time_point[1:4]+1,time_point[5]+1)
  time_rangeb <- expand.grid(time_point[5]+1,time_point[6:9]+1)
  time_range <- rbind(time_rangea,time_rangeb)
  names(time_range) <- c('start','end')
  f_ip
}
extra_ftr(data.ip$ip[1],data.ip$f_time[1],data.du)