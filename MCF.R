#@@@ MCF算法实现与应用
rm(list = ls())
dir_data <- 'D:/Data/Disk Number'
load(file.path(dir_data,'disk_number_label.Rda'))
end_time <- as.POSIXct('2013-12-01',tz = 'UTC')

# 1. 删除所有13年12月1日以后的故障
data.flist <- subset(data.flist,f_time <= end_time)
data.flist$ip <- factor(data.flist$ip)
data.flist$svr_id <- factor(data.flist$svr_id)
# 2. 删除有多个svr_id的ip
data.flist$ip_svr <- paste(data.flist$ip,data.flist$svr_id,sep = '_')
tmp <- data.flist[!duplicated(data.flist$ip_svr),]
tmp1 <- table(tmp$ip)
tmp2 <- table(tmp$svr_id)
tmp3 <- names(tmp1)[tmp1!=1]
tmp4 <- names(tmp2)[tmp2!=1]
