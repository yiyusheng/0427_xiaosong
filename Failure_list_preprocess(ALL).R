#@@@ 合并故障单,计算上架时间
rm(list = ls())
dir_data <- 'D:/Data/Disk Number/'

# 1. 读取uwork数据,因为helper中是有use_time的,所以把uwork也加一个use_time
load('D:/Data/Disk Number/flist(uwork[2012-2014]).Rda')
data.flist_uwork <- data.flist
data.flist_uwork$fType <- paste(data.flist_uwork$ftype,'uwork',sep = '_')
data.flist_uwork <- data.flist_uwork[data.flist_uwork$class>6,c('ip','svr_id','f_time','class','fType')]
data.flist_uwork$use_time <- as.POSIXct('2013-12-01',tz = 'UTC')
data.flist_uwork$from <- 'uwork'
# 2. 读取helper数据
load('D:/Data/Disk Number/flist(helper[2008-2013]).Rda')
data.flist_helper <- data.flist
data.flist_helper$fType <- paste(data.flist_helper$type,'helper',sep = '_')
data.flist_helper <- data.flist_helper[data.flist_helper$class>6,c('ip','svr_id','f_time','class','use_time','fType')]
data.flist_helper$f_time <- as.POSIXct(data.flist_helper$f_time,tz = 'UTC')
data.flist_helper$use_time <- as.POSIXct(data.flist_helper$use_time,tz = 'UTC')
data.flist_helper$from <- 'helper'
# 3. 合并数据
data.flist <- rbind(data.flist_helper,data.flist_uwork)
data.flist$ip <- factor(data.flist$ip)
data.flist$svr_id <- factor(data.flist$svr_id)
# 4. ip检验
# regexp.ip <- "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$"
# sum(grep(regexp.ip,data.flist$ip,value = T) == as.character(data.flist$ip)) == nrow(data.flist)
# 5. 去重(3天的去重时间)
dayDup <- 3
data.flist <- data.flist[order(data.flist$ip,data.flist$f_time),]
data.flist$svr_id <- as.character(data.flist$svr_id)
delset <- numeric()
pSvrid <- data.flist$svr_id[1]
pFtime <- data.flist$f_time[1]
for (i in 2:nrow(data.flist)){
  curSvrid <- data.flist$svr_id[i]
  curFtime <- data.flist$f_time[i]
  if (curSvrid == pSvrid & (curFtime - pFtime) > dayDup){
    pFtime <- curFtime
    next
  } else if(curSvrid == pSvrid & (curFtime - pFtime) <= dayDup){
    delset <- c(delset,i)
  } else if(curSvrid != pSvrid){
    pFtime <- curFtime
    pSvrid <- curSvrid
    next
  }
}
data.flist <- data.flist[-delset,]
data.flist$svr_id <- factor(data.flist$svr_id)
# 6.存储
save(data.flist,file = file.path(dir_data,'flist.Rda'))
