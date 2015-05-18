#FUNCTION SET
rm(list = ls())
#################################################################################################
#@@@ data preprocess :
#@@@ order by (ip,time), remove zero useage and zero total item
#@@@ convert time, calculate usage_percent
#@@@ add f_time
data_preprocess <- function(data,data_good,data_bad,out_name){  
  print(sprintf('before filter: %i items, %i ips',nrow(data),length(unique(data$ip))))
  data <- data[with(data,order(ip,time)),]
  deldata.zero <- subset(data, used == 0 | total == 0)
  data <- subset(data, used != 0 & total != 0)
#   data$ip <- factor(data$ip)
#   deldata.zero$ip <- factor(deldata.zero$ip)
  print(sprintf('filter zero: %i items, %i ips',nrow(data),length(unique(data$ip))))
  print(sprintf('filter zero(del): %i items, %i ips',nrow(deldata.zero),length(unique(deldata.zero$ip))))
  
  #set all total as the first one for each ip
  t <- tapply(data$total,data$ip,function(x) {
    if (mean(abs(x-mean(x))) < 10)
      return(rep(x[1],length(x)))
    else
      return(rep(-1,length(x)))
  })
  data$total <- unlist(t)
#   var.ip <- tapply(data$total,data$ip,var)
  deldata.sto_chg <- subset(data, total == -1)
#   deldata.sto_chg$ip <- factor(deldata.sto_chg$ip)
  data <- subset(data,total != -1)
#   data$ip <- factor(data$ip)
  data$used_percent <- data$used/data$total
  print(sprintf('filter sto_chg: %i items, %i ips',nrow(data),length(unique(data$ip))))
  print(sprintf('filter sto_chg(del): %i items, %i ips',nrow(deldata.sto_chg),length(unique(deldata.sto_chg$ip))))
  
  # duplicated for date of each ip and daycount record how many items for each (ip,date)
#   idx.factor <- factor(data$ip,levels = unique(data$ip))
  idx.dup_each <- tapply(as.Date(data$time),data$ip,duplicated)
  num.dup_each <- tapply(as.Date(data$time),data$ip,table)
  idx.dup <- !(unlist(idx.dup_each))
  deldata.dup <- data[!idx.dup,]
#   deldata.dup$ip <- factor(deldata.dup$ip)
  data.du_timedup <- data[idx.dup,]
#   data.du_timedup$ip <- factor(data.du_timedup$ip)
  data.du_timedup$daycount <- unlist(num.dup_each)
  print(sprintf('filter oneip: %i items, %i ips',nrow(data.du_timedup),length(unique(data.du_timedup$ip))))
  print(sprintf('filter oneip(del): %i items, %i ips',nrow(deldata.dup),length(unique(deldata.dup$ip))))
  
  # calculate time dence and item length of each ip.
#   idx.factor <- factor(data.du_timedup$ip,levels = unique(data.du_timedup$ip))
#   data.ip <- data.frame('ip' = unique(idx.factor))
  data.ip <- data.frame('ip' = levels(data.du_timedup$ip))
  # time dence
  t <- tapply(data.du_timedup$time,data.du_timedup$ip,
              function(x) {
                it <- mean(x[2:length(x)] - x[1:(length(x)-1)])
                units(it) <- 'hours' 
                return(it)
              })
  data.ip$time_dence[match(data.ip$ip,names(t))] <- as.numeric(t)
  data.ip$time_dence[is.na(data.ip$time_dence)] <- 1000
  # length
  t <- tapply(data.du_timedup$time,data.du_timedup$ip,length)
  data.ip$item_len[match(data.ip$ip,names(t))] <- as.numeric(t)
  # filter ip if time_dence > 72 || item_len <100 
  deldata.delip <- subset(data.ip, time_dence >72 | item_len < 100)
  data.ip <- subset(data.ip,time_dence<=72 & item_len >= 100)
#   data.ip$ip <- factor(as.character(data.ip$ip))
  rownames(data.ip) <- NULL
  # filter du based on data.ip
  delip <- setdiff(unique(data.du_timedup$ip),data.ip$ip)
  deldata.tdil <- subset(data.du_timedup, ip %in% delip)
  data.du <- subset(data.du_timedup, ip %in% data.ip$ip)
#   data.du$ip <- factor(data.du$ip)
#   deldata.tdil$ip <- factor(deldata.tdil$ip)
  print(sprintf('filter delip: %i items, %i ips',nrow(data.du),length(unique(data.du$ip))))
  print(sprintf('filter delip(del): %i items, %i ips',nrow(deldata.tdil),length(unique(deldata.tdil$ip))))
  
  # add f_time and class from data_good and data_bad
  data_good$f_time <- as.POSIXct('2013-10-15',tz = 'UTC')
  data_good$class <- 0
  data_good$fcount <- 0
  data_badgood <- rbind(data_bad,data_good)
  #match
  data.ip$f_time <- data_badgood$f_time[match(data.ip$ip,data_badgood$ip)]
  data.ip$class <- data_badgood$class[match(data.ip$ip,data_badgood$ip)]
  data.ip$fcount <- data_badgood$fcount[match(data.ip$ip,data_badgood$ip)]
  
  #save
  save(data.ip,data.du,file = paste(dir_data,out_name,sep='')) 
  save(deldata.dup,deldata.sto_chg,deldata.tdil,deldata.zero,
       file = paste(dir_data,'del_data.Rda',sep=''))
}
#################################################################################################
#@@@trancate data (reserver only data 30 days before failure/10-15)
time_truncate <- function(data.du,data.ip,days_perserve){
  tra <- data.du[,c('ip','time')]
  tra$f_time <- data.ip$f_time[match(tra$ip,data.ip$ip)]
  int.time <- tra$f_time - tra$time
  units(int.time) <- 'secs'
  dp <- days_perserve
  bool.tra <- (int.time <= dp*24*60*60) & (int.time > 0)
  data.du <- data.du[bool.tra,]
  data.ip <- subset(data.ip,ip %in% data.du$ip)
  data.ip$total <- data.du$total[match(data.ip$ip,data.du$ip)]
  return(list(data.du,data.ip))
}
#################################################################################################
#@@@feature extraction (mean,std of used during 3ds,7ds,15ds before and after time of failure)
data_featureB <- function(data.du,data.ip){
  data <- data.du[,c('ip','time','used','used_percent')]
  data$f_time <- data.ip$f_time[match(data$ip,data.ip$ip)]
  data$b_time <- data$f_time - data$time
  units(data$b_time) <- 'days'
  data$b_time <- as.numeric(data$b_time)
  col_name <- c('ip',
                'b1_mean','b1_std',
                'b2_mean','b2_std',
                'b3_mean','b3_std',
                'b4_mean','b4_std',
                'b5_mean','b5_std',
                'b6_mean','b6_std',
                'b7_mean','b7_std',
                'b8_mean','b8_std',
                'b9_mean','b9_std',
                'b10_mean','b10_std')
  ftr <- data.frame(matrix(,nrow = nrow(data.ip),ncol = length(col_name)))
  names(ftr) <- col_name
  ftr$ip <- data.ip$ip
  for (i in 1:10){
    data$time_class[data$b_time>=(i-1)*3 & data$b_time<i*3] <- i
    tmp <- subset(data,time_class == i)
    t <- tapply(tmp$used_percent,tmp$ip,mean)
    ftr[,1+(i-1)*2+1] <- t[match(ftr$ip,names(t))]
    t <- tapply(tmp$used_percent,tmp$ip,var)
    ftr[,1+(i-1)*2+2] <- t[match(ftr$ip,names(t))]
  }  
  return(ftr)
}



#################################################################################################
#@@@ feature A
data_featureA <- function(data.du,data.ip){
  # feature
  ftr <- data.ip
  # percentage
  t <- tapply(data.du$used_percent,data.du$ip,max)
  ftr$max_perc <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used_percent,data.du$ip,max)
  ftr$max_perc <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used_percent,data.du$ip,min)
  ftr$min_perc <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used_percent,data.du$ip,mean)
  ftr$avg_perc <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used_percent,data.du$ip,var)
  ftr$var_perc <- t[match(ftr$ip,names(t))]
  
  # used
  t <- tapply(data.du$used,data.du$ip,max)
  ftr$max_used <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used,data.du$ip,min)
  ftr$min_used <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used,data.du$ip,mean)
  ftr$avg_used <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used,data.du$ip,var)
  ftr$var_used <- t[match(ftr$ip,names(t))]
  
  ftr$maxdiff_used <- ftr$max_used - ftr$min_used
  
  # change of two days
  t <- tapply(data.du$used,data.du$ip,
              function(x){max(x[2:length(x)] - x[1:(length(x)-1)])})
  ftr$max_chg <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used,data.du$ip,
              function(x){mean(x[2:length(x)] - x[1:(length(x)-1)])})
  ftr$avg_chg <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used,data.du$ip,
              function(x){var(x[2:length(x)] - x[1:(length(x)-1)])})
  ftr$var_chg <- t[match(ftr$ip,names(t))]
  # other
  t <- tapply(data.du$daycount,data.du$ip,mean)
  ftr$avg_daycount <- t[match(ftr$ip,names(t))]
  
  t <- tapply(data.du$used,data.du$ip,length)
  ftr$data_count <- t[match(ftr$ip,names(t))]
  
  return(ftr)
}
#################################################################################################
#@@@ main function
#parameters
dir_data <- 'D:/Data/0427_xiaosong/'
flag_class <- 0 #1 for bad, 0 for good
in_name.all <- 'alldata_delfactor.Rda'
out_name.pre <- 'disk_usage_preprocess.Rda'
out_name.ftr <- 'disk_usage_feture.Rda'
days_preserve <- 30

#load and preprocess
# load(paste(dir_data,in_name.all,sep=''))
# data_preprocess(data_all,data_good,data_bad,out_name.pre)

# trancate time
load(paste(dir_data,out_name.pre,sep=''))
r <- time_truncate(data.du,data.ip,days_preserve)
data.du <- r[[1]]
data.ip <- r[[2]]

#load and extract feature
ftrA <- data_featureA(data.du,data.ip)
ftrB <- data_featureB(data.du,data.ip)
ftr <- cbind(ftrA,ftrB[match(ftrA$ip,ftrB$ip),2:ncol(ftrB)])

# seperate good and bad
ftr.good <- subset(ftr,class == 0)
ftr.bad <- subset(ftr,class >= 7)
save(ftr.good,ftr.bad,file = paste(dir_data,out_name.ftr,sep=''))
# quantile of each ftr


























# idx.bg <- match(data_badgood$ip,data.ip$ip)
# idx.bg <- idx.bg[!is.na(idx.bg)]
# idx.ip <- match(data.ip$ip,data_badgood$ip)
# idx.ip <- idx.ip[!is.na(idx.ip)]
# data.ip$f_time <- as.POSIXct('2013-10-15',tz = 'UTC')
# data.ip$f_time[idx.bg] <- 
#   data_badgood$f_time[idx.ip]
# data.ip$class[idx.bg] <- 
#   data_badgood$class[idx.ip]
# data.ip$fcount[idx.bg] <- 
#   data_badgood$fcount[idx.ip]
# data.ip$f_time <- as.POSIXct('1970-01-01')
# data.ip$class <- -1
# # good
# idx.goodip <- match(data_good$ip,data.ip$ip)
# idx.gooddata <- match(data.ip$ip,data_good$ip)
# data.ip$f_time[idx.goodip[!is.na(idx.goodip)]] <- as.POSIXct('2013-10-15')
# data.ip$class[idx.goodip[!is.na(idx.goodip)]] <- 0
# # bad
# idx.badip <- match(data_bad$ip,data.ip$ip)
# idx.baddata <- match(data.ip$ip,data_bad$ip)
# data.ip$f_time[idx.badip[!is.na(idx.badip)]] <- data_bad$f_time[idx.baddata[!is.na(idx.baddata)]]
# data.ip$class[idx.badip[!is.na(idx.badip)]] <- 1
# a <- subset(deldata.zero, ip %in% data_bad$ip)
# b <- subset(deldata.sto_chg, ip %in% data_bad$ip)
# c <- subset(deldata.dup, ip %in% data_bad$ip)
# d <- subset(deldata.tdil, ip %in% data_bad$ip)
# a$ip <- factor(a$ip)
# b$ip <- factor(b$ip)
# c$ip <- factor(c$ip)
# d$ip <- factor(d$ip)
# sta.total <- hist(log2(data_all$total),breaks = 0:25)
# feature_extract(data.du,data.ip)
# data.ip$f_time <- as.POSIXct(data.ip$f_time)
# data.good <- subset(data_all,ip %in% data_good$ip)
# data.bad <- subset(data_all, ip %in% data_bad%ip)
# # f_time add
# if (flag_class == 1){
# data.ip$f_time <- data.ip_all[match(data.ip$ip,data.ip_all$ip),'f_time']
# } else if (flag_class == 0){
# data.ip$f_time <- as.POSIXct('2013-10-15')
# }
# data.ip$f_time <- as.POSIXct(data.ip$f_time)

#========================================================================================================
#feature extraction (mean,std of used during 3ds,7ds,15ds before and after time of failure)
# extra_ftr <- function(cur_ip,f_time,cur_du) {
#   if (unique(cur_du$ip) != cur_ip){
#     print('EXTRA_FTR: wrong ip')
#     return
#   }
#   cur.data.du <- cur_du
#   itv <- c(-30,-27,-24,-21,-18,-15,-12,-9,-6,-3,0)*24*60*60
#   time_point <- as.POSIXct(as.Date(f_time + itv))
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
# data_featureB <- function(data.du,data.ip){
#   col_name <- c('ip','f_time',
#                 'b10_mean','b10_std',
#                 'b9_mean','b9_std',
#                 'b8_mean','b8_std',
#                 'b7_mean','b7_std',
#                 'b6_mean','b6_std',
#                 'b5_mean','b5_std',
#                 'b4_mean','b4_std',
#                 'b3_mean','b3_std',
#                 'b2_mean','b2_std',
#                 'b1_mean','b1_std')
#   ftr <- data.frame(matrix(,nrow = nrow(data.ip),ncol = length(col_name)))
#   names(ftr) <- col_name
#   ftr$ip <- data.ip$ip
#   ftr$f_time <- data.ip$f_time
#   
#   # add a f_time column for data.du
#   idx_ft <- match(data.du$ip,data.ip$ip)
#   idx_list <- tapply(1:length(idx_ft),idx_ft,function(x)return(x))
#   
#   for (i in 1:nrow(data.ip)){
#     print(i)
#     cur_du <- data.du[idx_list[[i]],]
#     ftr[i,3:ncol(ftr)] <- extra_ftr(ftr$ip[i],ftr$f_time[i],cur_du)
#   }
#   save(data.ip,data.du,ftr,file = paste(dir_data,out_name1,sep=''))
#   return(ftr)
# }
