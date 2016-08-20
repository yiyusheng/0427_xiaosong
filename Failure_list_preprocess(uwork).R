# failure list duplication, statistic failure times, extract first failure time
rm(list = ls())

# library and parameter
# library(RMySQL)
dir_data <- 'D:/Data/Disk Number/'
in_name <- 'uwork_20120101-20131210.csv'
in_name1 <- '故障单精简_06-09.csv'
out_name <- 'bad_pre(0401_1231).csv'
in_path <- paste(dir_data,in_name,sep='')
in_path1 <- paste(dir_data,in_name1,sep='')
out_path <- paste(dir_data,out_name,sep='')

# 1. read data
data.fl <- read.csv(in_path)
data.fl1 <- read.csv(in_path1)
names(data.fl) <- c('id','svr_id','ip','ftype','ori_ftype',
                    'f_time','ftype_d1','ftype_d2','ftype_d')
names(data.fl1) <- c('id','svr_id','ip','ftype','idc','dev_class_name','dev_class_id',
                     'dept_id','f_time','f_desc','ftype_d1','ftype_d2')
col_need <- c('svr_id','ip','ftype','f_time','ftype_d1','ftype_d2')
data.fl <- data.fl[,col_need]
data.fl1 <- data.fl1[,col_need]
data.fl <- rbind(data.fl,data.fl1)
data.fl$f_time <- as.POSIXct(data.fl$f_time,tz = 'UTC')

# 2. del space
data.fl$ftype[data.fl$ftype == '硬盘故障（有冗余） '] <- '硬盘故障（有冗余）'
data.fl$ftype <- factor(data.fl$ftype)

# 3. add class
data.fl$class <- -1
# data.replace <- subset(data.fl,ftype_d1 == '硬盘故障;' | ftype_d2 == '硬盘故障;')
ftlist <- c('硬盘故障（有冗余）','硬盘故障（有冗余，槽位未知）',
            '硬盘故障（无冗余）','硬盘故障（无冗余，在线换盘）',
            '硬盘即将故障（有冗余）','操作系统硬盘故障（无冗余）')


# for (i in 1:length(ftlist)){
#   data.fl$class[data.fl$ftype == ftlist[i]] <- i
#   data.fl$class[data.fl$ftype == ftlist[i] & 
#                   (data.fl$ftype_d1 == '硬盘故障;' | data.fl$ftype_d2 == '硬盘故障;')] <- i+6
# }
data.fl$class[(data.fl$ftype_d1 == '硬盘故障;' | data.fl$ftype_d2 == '硬盘故障;')] <- 7


# 4. delete no ip
data.fl_order <- data.fl[with(data.fl,order(ip,f_time)),]
data.fl_order <- data.fl_order[data.fl_order$ip!='',]           # delete no ip
data.fl_order$ip <- factor(data.fl_order$ip)                    # reconstruct factor of ip

# 5. filter ip
regexp.ip <- "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$"
idx.ip_reg <- grepl(regexp.ip,data.fl_order$ip)
data.fl_order <- data.fl_order[idx.ip_reg,]

# 6. duplication: ip
# fcount <- tapply(data.fl_order$ip,data.fl_order$ip,length)
# data.flist <- data.fl_order[!duplicated(data.fl_order$ip),]  #preserve one failure
# data.flist$fcount[match(names(fcount),data.flist$ip)] <- fcount
data.flist <- data.fl_order
rownames(data.flist) <- NULL
write.csv(file = out_path, x = data.flist, row.names=FALSE)
data.bad <- subset(data.flist,class!=-1)
save(data.flist,data.bad,file = paste(dir_data,'flist(uwork[2012-2014]).Rda',sep = ''))

#duplication: same ip & more than one f_time in three days (preserve the first one).
# data.fl_order_dup <- data.frame('ip' = rep(data.fl_order$ip[1],length(unique(data.fl_order$ip))),
#                                 'f_time' = data.fl_order$f_time[1],
#                                 'f_count' = 1)
# pre_ip <- data.fl_order$ip[1]
# pre_f_time <- data.fl_order$f_time[1]
# count <- 1
# for (i in 2:nrwo(data.fl_order)) {
#   cur_ip <- data.fl_order$ip[i]
#   cur_f_time <- data.fl_order$f_time[i]
#   if (cur_ip != pre_ip) {
#     data.fl_order_dup[count,] <- 
#   }
# }
#duplication: same ip & more than one f_time in three days (preserve the first one).
# idx.ip <- tapply(1:nrow(data.fl_order),data.fl_order$ip,function(x)return(x))
# len.ip <- as.numeric(tapply(1:nrow(data.fl_order),data.fl_order$ip,length))
# data.fl_order_dup <- data.frame('ip' = levels(data.fl_order$ip), 'f_time'=0, 'f_count'=1)
# for (i in 1:length(idx.ip)){
#   print(i)
#   cur.idx <- idx.ip[[i]]
#   if (length(cur.idx) == 1 & 
#         data.fl_order$ip[cur.idx[1]] == data.fl_order_dup$ip[i]) {
#     data.fl_order_dup$f_time[i] <- data.fl_order$f_time
#   } else {
#     cur.f_time <- data.fl_order$f_time[cur.idx]
#     len <- length(cur.f_time)
#     diff.f_time <- cur.f_time[2:len]-cur.f_time[1:(len-1)]
#     data.fl_order_dup$f_time[i] <- cur.f_time[1]
#     data.fl_order_dup$f_count[i] <- sum(diff.f_time>3*24*60*60)
#   }
# }
