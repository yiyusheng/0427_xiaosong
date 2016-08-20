#@@@ 读取helper数据并进行简要分析
rm(list = ls())
dir_data <- 'D:/Data/Disk Number/'
require(ggplot2)

# read data and save
# data <- rbind(read.csv(file.path(dir_data,'2009.csv')),
#               read.csv(file.path(dir_data,'2010.csv')),
#               read.csv(file.path(dir_data,'2011.csv')),
#               read.csv(file.path(dir_data,'2012.csv')),
#               read.csv(file.path(dir_data,'2013.csv')))
# save(data,file = file.path(dir_data,'helper[09-13].Rda'))
load(file.path(dir_data,'helper[09-13].Rda'))

# 1. extract some column and filter replica list
col_need <- c('创建时间','故障原因','当前状态','故障发生部门','部门','服务恢复时间','结单时间',
              '解决方法','固资编号','故障机固资号','主机IP','告警级别','设备型号','设备类型',
              'SN','上架时间','服务恢复耗时.小时.','事件类型','硬盘故障类型','硬盘故障数量',
              '硬盘容量','硬盘生产厂商','硬盘品牌厂商','备机固资号')
data <- data[,col_need]
names(data) <- c('f_time','reason','state','fail_dept','dept','recover_time','close_time',
                 'solution','svr_id','svr_id_failure','ip','level','model_name','dev_class_id',
                 'SN','use_time','recover_interval','type','disk_failure_type','disk_failure_count',
                 'disk_capacity','disk_vendor','disk_band_vendor','svr_id_backup')
data <- subset(data,as.numeric(state) != 1 & as.numeric(state) != 4)

# 2. 测试部门号,svr_id是否一致
# sum(as.character(data$dept) == as.character(data$fail_dept))
# #删除部门列
# data$dept <- NULL
# sum(as.character(data$svr_id) == as.character(data$svr_id_failure))
# #删除故障机固资号
# data$svr_id_failure <- NULL

# 3. 选取判断机器是否换盘字段(solution,type,disk_failure_type)
# ggplot(data,aes(x = disk_failure_type,fill = type)) + geom_histogram()
# table(data[as.numeric(data$disk_failure_count) != 1,'type'])
# # solution
# idx.solution <- sapply(data$solution,function(x) grepl('更换硬盘',x))
# data.solution <- data
# data.solution$solbool <- 0
# data.solution$solbool[idx.solution] <- 1
# # type & disk_faulure_type
# table(data.solution[data.solution$solbool == 1,'disk_failure_type'])
#结论: 使用disk_failure_type来标识换了的盘.

# 4.过滤IP
data.filter <- data[with(data,order(ip,f_time)),]
#replace wrong string
data.filter$ip <- gsub("无", "", data.filter$ip)
data.filter$ip <- gsub("\n", "", data.filter$ip)
#delete item without ip
data.filter <- data.filter[data.filter$ip!='',]           # delete no ip
#remove item with more than one ip
idx.ip_res <- nchar(as.character(data.filter$ip)) <= 15
data.filter <- data.filter[idx.ip_res,]
#remove item with ip blocked by regexp
regexp.ip <- "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$"
idx.ip_reg <- grepl(regexp.ip,data.filter$ip)
data.filter <- data.filter[idx.ip_reg,]
# sum(grep(regexp.ip,data.filter$ip,value = T) == data.filter$ip) == nrow(data.filter)
#factorize
data.filter$ip <- factor(data.filter$ip)                    # reconstruct factor of ip

# 5. 过滤f_time,use_time,close_time
# table(nchar(as.character(data.filter$f_time)))
# table(nchar(as.character(data.filter$use_time)))
# table(nchar(as.character(data.filter$close_time)))
data.filter$use_time <- gsub("无", "", data.filter$use_time)
data.filter$use_time <- gsub("\n", "", data.filter$use_time)
data.filter$use_time <- as.POSIXct(data.filter$use_time,tz = 'UTC')
data.filter$f_time <- as.POSIXct(data.filter$f_time,tz = 'UTC')
# data.filter$close_time <- as.POSIXct(data.filter$close_time,tz = 'UTC')

# 6.add class ((非硬盘故障的类型为-1,硬盘故障的类型为13(单硬盘故障),14(多硬盘故障))
data.filter$class <- -1
data.filter$class[as.character(data.filter$disk_failure_type) == '单硬盘故障'] <- 13
data.filter$class[as.character(data.filter$disk_failure_type) == '多硬盘故障'] <- 14

# 6. duplication: ip
# fcount <- tapply(data.filter$ip,data.filter$ip,length)
# data.filter.flist <- data.filter[!duplicated(data.filter$ip),]  #preserve one failure
# data.filter.flist$fcount[match(names(fcount),data.filter.flist$ip)] <- fcount
# rownames(data.filter.flist) <- NULL

# 7.save
data.flist <- data.filter
data.bad <- subset(data.flist,class!=-1)
save(data.flist,data.bad,file = paste(dir_data,'flist(helper[2008-2013]).Rda',sep = ''))
