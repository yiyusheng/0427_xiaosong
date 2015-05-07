# select disk usage information based on a iplist
rm(list = ls())

# library and parameter
library(RMySQL)
dir_data <- 'D:/Data/0427_xiaosong/'
flag_class <- 0 #1 for bad, 0 for good
if (flag_class == 0){
  out_name <- 'disk_usage_good.Rda'
  out_name1 <- 'disk_usage_good_ftr.Rda'
  du_name <- 'disk_usage_good.csv'
}else if (flag_class == 1) {
  out_name <- 'disk_usage_bad.Rda'
  out_name1 <- 'disk_usage_bad_ftr.Rda'
  du_name <- 'disk_usage_bad.csv'
}

#========================================================================================================
# # read ip list
# csv_file <- 'ip_0401_1231.csv'
# csv_path <- paste(dir_data,csv_file,sep='')
# data.ip_all <- read.csv(csv_path)
# names(data.ip_all) <- c('sn','ip','f_time')
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
# if (flag_class == 1){
#   data.ip$f_time <- data.ip_all[match(data.ip$ip,data.ip_all$ip),'f_time']
# } else if (flag_class == 0){
#   data.ip$f_time <- as.POSIXct('2013-10-15')
# }
# data.ip$ip <- factor(as.character(data.ip$ip))
# data.ip$f_time <- as.POSIXct(data.ip$f_time)
# rownames(data.ip) <- NULL
# # filter du based on data.ip
# data.du <- subset(data.du_timedup, ip %in% data.ip$ip)#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^bad
# # else if (flag_class == 0)data.du <- data.du_timedup #^^^^^^^^^^^^^^^^^^^^^^^^^^^good
# data.du$ip <- factor(as.character(data.du$ip))
# save(data.ip,data.du,file = paste(dir_data,out_name,sep=''))
#========================================================================================================
#feature extraction (mean,std of usage during 3ds,7ds,15ds before and after time of failure)
load(paste(dir_data,out_name,sep=''))
assign("data.du",data.du,envir = .GlobalEnv)

extra_ftr <- function(cur_ip,f_time,cur_du) {
  if (unique(cur_du$ip) != cur_ip){
    print('EXTRA_FTR: wrong ip')
    return
  }
  cur.data.du <- cur_du
  itv <- c(-15,-12,-9,-6,-3,0,3,6,9,12,15)*24*60*60
  time_point <- as.POSIXct(as.Date(f_time + itv))
  #time_rangea <- expand.grid(time_point[1:4]+1,time_point[5]+1)
  #time_rangeb <- expand.grid(time_point[5]+1,time_point[6:9]+1)
  #time_range <- rbind(time_rangea,time_rangeb)
  time_range <- cbind(time_point[1:(length(time_point)-1)],time_point[2:length(time_point)])
  names(time_range) <- c('start','end')
  ftr <- matrix(0,nrow = nrow(time_range),ncol = 2)
  for (i in 1:nrow(time_range)) {
    subset.curdu <- subset(cur.data.du, time >= time_range[i,1] & time < time_range[i,2])
    ftr[i,1] <- mean(subset.curdu$usage_percent)
    ftr[i,2] <- sqrt(var(subset.curdu$usage_percent)/length(subset.curdu$usage_percent))
#     ftr[i,3] <- length(subset.curdu$usage_percent)
  }
  return(c(t(ftr)))
}

ftr.du <- data.frame(matrix(,nrow = nrow(data.ip),ncol = 22))
names(ftr.du) <- c('ip','f_time',
                   'b5_mean','b5_std',
                   'b4_mean','b4_std',
                   'b3_mean','b3_std',
                   'b2_mean','b2_std',
                   'b1_mean','b1_std',
                   'a1_mean','a1_std',
                   'a2_mean','a2_std',
                   'a3_mean','a3_std',
                   'a4_mean','a4_std',
                   'a5_mean','a5_std')
ftr.du$ip <- data.ip$ip
ftr.du$f_time <- data.ip$f_time

# add a f_time column for data.du
idx_ft <- match(data.du$ip,data.ip$ip)
idx_list <- tapply(1:length(idx_ft),idx_ft,function(x)return(x))

for (i in 1:nrow(data.ip)){
# for (i in 1:10){
  print(i)
  cur_du <- data.du[idx_list[[i]],]
  ftr.du[i,3:ncol(ftr.du)] <- extra_ftr(ftr.du$ip[i],
                                        ftr.du$f_time[i],
                                        cur_du)
}
save(data.ip,data.du,ftr.du,file = paste(dir_data,out_name1,sep=''))
