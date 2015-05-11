# select disk usage information based on a iplist
rm(list = ls())
dir_data <- 'D:/Data/0427_xiaosong/'
flag_class <- 0 #1 for bad, 0 for good
in_name.all <- 'alldata_delfactor.Rda'
out_name.pre <- 'disk_usage_preprocess.Rda'

#################################################################################################
#@@@ data preprocess :
#@@@ order by (ip,time), remove zero useage and zero total item
#@@@ convert time, calculate usage_percent
#@@@ add f_time
data_preprocess <- function(data,data_good,data_bad,out_name){  
  data <- data[with(data,order(ip,time)),]
  data <- subset(data, used != 0 & total != 0)
  data$ip <- factor(data$ip)
  #set all total as the first one for each ip
  t <- tapply(data$total,data$ip,function(x) {
    if (mean(abs(x-mean(x))) < 10)
      return(rep(x[1],length(x)))
    else
      return(rep(-1,length(x)))
  })
  t <- tapply(data$total,data$ip,var)
  schange.ip <- names(tt)[tt == 'wrong']
  schange <- subset(data,ip %in% schange.ip)
  data$used_percent <- data$used/data$total
  
  # duplicated for date of each ip and daycount record how many items for each (ip,date)
  idx.factor <- factor(data$ip,levels = unique(data$ip))
  idx.dup_each <- tapply(as.Date(data$time),idx.factor,duplicated)
  num.dup_each <- tapply(as.Date(data$time),idx.factor,table)
  idx.dup <- !(unlist(idx.dup_each))
  data.du_timedup <- data[idx.dup,]
  data.du_timedup$daycount <- unlist(num.dup_each)
  
  # calculate time dence and item length of each ip.
  idx.factor <- factor(data.du_timedup$ip,levels = unique(data.du_timedup$ip))
  data.ip <- data.frame('ip' = unique(idx.factor))
  # time dence
  t <- tapply(data.du_timedup$time,idx.factor,
              function(x) {
                it <- mean(x[2:length(x)] - x[1:(length(x)-1)])
                units(it) <- 'hours' 
                return(it)
              })
  data.ip$time_dence[match(data.ip$ip,names(t))] <- as.numeric(t)
  data.ip$time_dence[is.na(data.ip$time_dence)] <- 1000
  # length
  t <- tapply(data.du_timedup$time,idx.factor,length)
  data.ip$item_len[match(data.ip$ip,names(t))] <- as.numeric(t)
  
  # filter ip if time_dence > 72 || item_len <100 
  data.ip <- subset(data.ip,time_dence<=72 & item_len >= 100)
  data.ip$ip <- factor(as.character(data.ip$ip))
  rownames(data.ip) <- NULL
  # filter du based on data.ip
  data.du <- subset(data.du_timedup, ip %in% data.ip$ip)
  data.du$ip <- factor(as.character(data.du$ip))
  
  # add f_time and class
  data.ip$f_time <- as.POSIXct('1970-01-01')
  data.ip$class <- -1
  # good
  idx.goodip <- match(data_good$ip,data.ip$ip)
  idx.gooddata <- match(data.ip$ip,data_good$ip)
  data.ip$f_time[idx.goodip[!is.na(idx.goodip)]] <- as.POSIXct('2013-10-15')
  data.ip$class[idx.goodip[!is.na(idx.goodip)]] <- 0
  # bad
  idx.badip <- match(data_bad$ip,data.ip$ip)
  idx.baddata <- match(data.ip$ip,data_bad$ip)
  data.ip$f_time[idx.badip[!is.na(idx.badip)]] <- data_bad$f_time[idx.baddata[!is.na(idx.baddata)]]
  data.ip$class[idx.badip[!is.na(idx.badip)]] <- 1
  save(data.ip,data.du,file = paste(dir_data,out_name,sep='')) 
}

#========================================================================================================
#feature extraction (mean,std of used during 3ds,7ds,15ds before and after time of failure)
# load(paste(dir_data,out_name,sep=''))
# assign("data.du",data.du,envir = .GlobalEnv)
# feature_extract <- function(data.du,data.ip){
#   
# }
# 
# extra_ftr <- function(cur_ip,f_time,cur_du) {
#   if (unique(cur_du$ip) != cur_ip){
#     print('EXTRA_FTR: wrong ip')
#     return
#   }
#   cur.data.du <- cur_du
#   itv <- c(-15,-12,-9,-6,-3,0,3,6,9,12,15)*24*60*60
#   time_point <- as.POSIXct(as.Date(f_time + itv))
#   #time_rangea <- expand.grid(time_point[1:4]+1,time_point[5]+1)
#   #time_rangeb <- expand.grid(time_point[5]+1,time_point[6:9]+1)
#   #time_range <- rbind(time_rangea,time_rangeb)
#   time_range <- cbind(time_point[1:(length(time_point)-1)],time_point[2:length(time_point)])
#   names(time_range) <- c('start','end')
#   ftr <- matrix(0,nrow = nrow(time_range),ncol = 2)
#   for (i in 1:nrow(time_range)) {
#     subset.curdu <- subset(cur.data.du, time >= time_range[i,1] & time < time_range[i,2])
#     ftr[i,1] <- mean(subset.curdu$used_percent)
#     ftr[i,2] <- sqrt(var(subset.curdu$used_percent)/length(subset.curdu$used_percent))
#     #     ftr[i,3] <- length(subset.curdu$used_percent)
#   }
#   return(c(t(ftr)))
# }
# 
# ftr.du <- data.frame(matrix(,nrow = nrow(data.ip),ncol = 22))
# names(ftr.du) <- c('ip','f_time',
#                    'b5_mean','b5_std',
#                    'b4_mean','b4_std',
#                    'b3_mean','b3_std',
#                    'b2_mean','b2_std',
#                    'b1_mean','b1_std',
#                    'a1_mean','a1_std',
#                    'a2_mean','a2_std',
#                    'a3_mean','a3_std',
#                    'a4_mean','a4_std',
#                    'a5_mean','a5_std')
# ftr.du$ip <- data.ip$ip
# ftr.du$f_time <- data.ip$f_time
# 
# # add a f_time column for data.du
# idx_ft <- match(data.du$ip,data.ip$ip)
# idx_list <- tapply(1:length(idx_ft),idx_ft,function(x)return(x))
# 
# for (i in 1:nrow(data.ip)){
#   # for (i in 1:10){
#   print(i)
#   cur_du <- data.du[idx_list[[i]],]
#   ftr.du[i,3:ncol(ftr.du)] <- extra_ftr(ftr.du$ip[i],
#                                         ftr.du$f_time[i],
#                                         cur_du)
# }
# save(data.ip,data.du,ftr.du,file = paste(dir_data,out_name1,sep=''))
#################################################################################################
#@@@ main function
load(paste(dir_data,in_name.all,sep=''))
data_preprocess(data_all,data_good,data_bad,out_name.pre)
sta.total <- hist(log2(data_all$total),breaks = 0:25)

load(paste(dir_data,out_name.pre,sep=''))
feature_extract(data.du,data.ip)
# data.ip$f_time <- as.POSIXct(data.ip$f_time)
# data.good <- subset(data_all,ip %in% data_good$ip)
# data.bad <- subset(data_all, ip %in% data_bad%ip)
# # f_time add
# if (flag_class == 1){
#   data.ip$f_time <- data.ip_all[match(data.ip$ip,data.ip_all$ip),'f_time']
# } else if (flag_class == 0){
#   data.ip$f_time <- as.POSIXct('2013-10-15')
# }
# data.ip$f_time <- as.POSIXct(data.ip$f_time)