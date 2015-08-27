# IO与SMART的机型业务特征
rm(list = ls())
source('D:/Git/R_Function/Rfun.R')
require('ggplot2')
require('xlsx')

#@@@ PARAMETERS
dir_dataA <- 'D:/Data/Disk Number'
dir_data <- 'D:/Data/attrid'

#@@@ LOAD DATA
# 1. cmdb数据
load(file.path(dir_dataA,'disk_number_label.Rda'))
load(file.path(dir_dataA,'mcf_all_age_rsv2014.Rda'))
dev_need <- c('TS4','TS5','TS6',
              'C1')
data.config$bs1 <- cmdb_dev$bs1[match(data.config$ip,cmdb_dev$ip)]
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
# data.config <- subset(data.config,dev_class_id %in% dev_need)
data.config$dev_class_id <- factor(data.config$dev_class_id)

# 2. smart数据统计
d.smart <- read.csv(file.path(dir_data,'smart_Tencent_sta.csv'))
reg_ip <- "((\\d+\\.){3}\\d+)(.*)"
d.smart <- subset(d.smart,grepl(reg_ip,ip))
d.smart <- factorX(d.smart)
d.smart$min_time <- as.POSIXct(d.smart$min_time,tz = 'UTC')
d.smart$max_time <- as.POSIXct(d.smart$max_time,tz = 'UTC')
# 2.1 取每个机器的特征
d.smarts <- data.frame(ip = levels(d.smart$ip),
                       disk_c = as.numeric(tapply(d.smart$sn,d.smart$ip,length)),
                       min_time = as.numeric(tapply(d.smart$min_time,d.smart$ip,max)),
                       max_time = as.numeric(tapply(d.smart$max_time,d.smart$ip,min)),
                       count = as.numeric(tapply(d.smart$count,d.smart$ip,sum)),
                       mean = as.numeric(tapply(d.smart$count,d.smart$ip,mean)))
col_need <- c('svr_asset_id','ip','dev_class_id','bs1','use_time','raid')
d.smarts <- merge(d.smarts,cmdb[,col_need],by.x = 'ip',by.y = 'ip')
d.smarts <- factorX(d.smarts)
# 2.2 机型统计
dev.cmdb <- tableX(cmdb$dev_class_id)
dev.smarts <- tableX(d.smarts$dev_class_id)
dev.smarts$cmdb <- dev.cmdb$count[match(dev.smarts$item,dev.cmdb$item)]
dev.smarts$cmdb_rate <- dev.smarts$count/dev.smarts$cmdb
# 2.3 业务统计
bs.cmdb <- tableX(cmdb$bs1)
bs.smarts <- tableX(d.smarts$bs1)
bs.smarts$cmdb <- bs.cmdb$count[match(bs.smarts$item,bs.cmdb$item)]
bs.smarts$cmdb_rate <- bs.smarts$count/bs.smarts$cmdb 
# 存储
save(d.smart,d.smarts,dev.smarts,bs.smarts,file = file.path(dir_data,'sta_smart.Rda'))
write.xlsx(bs.smarts[,c('item','count','cmdb','cmdb_rate')],
           file.path(dir_data,'sta_iosm.xlsx'),sheetName = 'bs_smarts',row.names = F,append = T)
write.xlsx(dev.smarts[,c('item','count','cmdb','cmdb_rate')],
           file.path(dir_data,'sta_iosm.xlsx'),sheetName = 'dev_smarts',row.names = F,append = T)

# 3. 902数据统计
d.902 <- read.csv(file.path(dir_data,'attr9020.csv'))
col_need <- c('svr_asset_id','ip','dev_class_id','bs1','use_time','raid')
d.902 <- merge(d.902,cmdb[,col_need],by.x = 'svrid',by.y = 'svr_asset_id')
d.902 <- factorX(d.902)
d.902$min_time <- as.POSIXct(as.character(d.902$min_time),format = '%Y%m%d',tz = 'UTC')
d.902$max_time <- as.POSIXct(as.character(d.902$max_time),format = '%Y%m%d',tz = 'UTC')
 
# 3.1 机型统计
dev.cmdb <- tableX(cmdb$dev_class_id)
dev.902 <- tableX(d.902$dev_class_id)
dev.902$cmdb <- dev.cmdb$count[match(dev.902$item,dev.cmdb$item)]
dev.902$cmdb_rate <- dev.902$count/dev.902$cmdb
# 3.2 业务统计
bs.cmdb <- tableX(cmdb$bs1)
bs.902 <- tableX(d.902$bs1)
bs.902$cmdb <- bs.cmdb$count[match(bs.902$item,bs.cmdb$item)]
bs.902$cmdb_rate <- bs.902$count/bs.902$cmdb 

# 3.3 机型+业务统计
d.902$db <- paste(d.902$dev_class_id,d.902$bs1,sep='_')
db.cmdb <- tableX(paste(cmdb$dev_class_id,cmdb$bs1,sep='_'))
db.902 <- tableX(d.902$db)
db.902$cmdb <- db.cmdb$count[match(db.902$item,db.cmdb$item)]
db.902$cmdb_rate <- db.902$count/db.902$cmdb

# 存储
save(d.902,dev.902,bs.902,db.902,file = file.path(dir_data,'sta_io.Rda'))
write.xlsx(bs.902[,c('item','count','cmdb','cmdb_rate')],
           file.path(dir_data,'sta_iosm.xlsx'),sheetName = 'bs_902',row.names = F,append = T)
write.xlsx(dev.902[,c('item','count','cmdb','cmdb_rate')],
           file.path(dir_data,'sta_iosm.xlsx'),sheetName = 'dev_902',row.names = F,append = T)
write.xlsx(db.902[,c('item','count','cmdb','cmdb_rate')],
           file.path(dir_data,'sta_iosm.xlsx'),sheetName = 'db_902',row.names = F,append = T)

# 4. 每个机型+业务取100台机器的数据进行处理.
load(file.path(dir_data,'sta_io.Rda'))
# d.902all <- subset(d.902)
d.902all <- subset(d.902,count == 17280)
# 4.1 收集要处理的机型+业务
tmp <- subset(d.902,dev_class_id %in% c('TS1','TS3','TS4','TS5','TS6'),db)
table.dbA <- tableX(tmp$db)
dbA <- as.character(table.dbA$item[table.dbA$count > 1000])
tmp <- subset(d.902,dev_class_id == 'C1')
table.dbB <- tableX(tmp$db)
dbB <- as.character(table.dbB$item[table.dbB$count > 4000])

# 4.2 将C1机器的2u4n和twins机器分出,添加刀架列
tmp1 <- grepl('-',as.character(d.902all$svrid))
blade_flag <- strsplit(as.character(d.902all$svrid[tmp1]),'-')
blade_flag <- data.frame(matrix(unlist(blade_flag),nrow = length(blade_flag),byrow = T))
d.902all$bladesvrid <- 'no'
d.902all$bladepos <- '0'
d.902all$bladesvrid[tmp1] <- as.character(blade_flag[,1])
d.902all$bladepos[tmp1] <- as.character(blade_flag[,2])

# 4.3 dbA,dbB分开处理,C1的2u4n,twins,server各100吧
# TS类
k131_svrid <- character(0)
for(i in 1:length(dbA)){
  tmp <- subset(d.902all,db == dbA[i])
  idx <- sample(1:nrow(tmp),100)
  k131_svrid <- c(k131_svrid,as.character(tmp$svrid[idx]))
}
# C1类
d.902C <- subset(d.902all,dev_class_id == 'C1' & bladepos %in% c('1','2','3','4','L','R','0'))
for (i in 1:length(dbB)){
  tmp <- subset(d.902C,db == dbB[i] & bladepos == '0')
  tmp1 <- sample(1:nrow(tmp),100)
}