#@@@ 合并故障单,计算上架时间
rm(list = ls())
dir_data <- 'D:/Data/Disk Number/'

# 1. 读取uwork数据
load('D:/Data/Disk Number/flist(uwork[2012-2014]).Rda')
data.flist_uwork <- data.flist
data.flist_uwork <- data.flist_uwork[data.flist_uwork$class>6,c('ip','svr_id','f_time','class')]
data.flist_uwork$use_time <- as.POSIXct('2013-12-01',tz = 'UTC')
# 2. 读取helper数据
load('D:/Data/Disk Number/flist(helper[2008-2013]).Rda')
data.flist_helper <- data.flist
data.flist_helper <- data.flist_helper[data.flist_helper$class>6,c('ip','svr_id','f_time','class','use_time')]
data.flist_helper$f_time <- as.POSIXct(data.flist_helper$f_time,tz = 'UTC')
data.flist_helper$use_time <- as.POSIXct(data.flist_helper$use_time,tz = 'UTC')
data.flist <- rbind(data.flist_helper,data.flist_uwork)
# 3. 合并数据
data.flist$ip <- factor(data.flist$ip)
data.flist$svr_id <- factor(data.flist$svr_id)
# 4. ip检验
# regexp.ip <- "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$"
# sum(grep(regexp.ip,data.flist$ip,value = T) == as.character(data.flist$ip)) == nrow(data.flist)
save(data.flist,file = file.path(dir_data,'flist.Rda'))
