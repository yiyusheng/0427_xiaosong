###@ 处理新的杂乱的想法
rm(list = ls())
require(ggplot2)
source('MCF_function.R')
source('plot_2yaxis.R')

#@@@ PARAMETERS
dir_data <- 'D:/Data/Disk Number'
ti <- 30
time_need <- 40
frac_max <- 0.1

#@@@ LOAD DATA
load(file.path(dir_data,'disk_number_label.Rda'))
load(file.path(dir_data,'mcf_all_age_rsv2014.Rda'))
dev_need <- c('TS4','TS5','TS6',
              'C1')
#               'A1','A5',
#               'B1','B5','B6')
# cmdb <- subset(cmdb,use_time > as.POSIXct('2010-01-01'))
# cmdb <- subset(cmdb, dev_class_id %in% dev_need)
data.config$bs1 <- cmdb_dev$bs1[match(data.config$ip,cmdb_dev$ip)]
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
data.config <- subset(data.config,dev_class_id %in% dev_need)
data.config$dev_class_id <- factor(data.config$dev_class_id)

# # 1. 500G单盘机的接口,model与故障关系分析(Q3&Q4)
# tmp1 <- subset(data.config,disk_model == 'ST3500514NS' & disk_c == 1)
# tmp2 <- subset(data.config,disk_model == 'ST500NM0011' & disk_c == 1)
# tmp1 <- merge(tmp1,cmdb[c('ip','dev_class_name','model_name','bs1')],by = 'ip',all.x = T)
# tmp2 <- merge(tmp2,cmdb[c('ip','dev_class_name','model_name','bs1')],by = 'ip',all.x = T)
# summary(tmp1[,13:15])
# summary(tmp2[,13:15])
# #加入model_name作图
# data.config_model <- merge(subset(data.config,!is.na(total) & disk_c == 1 & total == 500),
#                            cmdb[c('ip','model_name')],
#                            by = 'ip',all.x = T)
# data.config_model$mm <- paste(data.config_model$disk_model,
#                               data.config_model$model_name,
#                               sep = '_')
# item <- 'mm'
# class_suffix <- ''
# title <- 'Q234_500G_single_disk_mm'
# stand_class <- 'baseline'
# config_item <- subset(data.config_model,mm %in% c('ST500NM0011_LENOVO SD210X4',
#                                                   'ST3500514NS_DELL DCS5100',
#                                                   'ST3500514NS_LENOVO SD210X4',
#                                                   'ST3500514NS_HUAWEI XH310',
#                                                   'ST500NM0011_HUAWEI XH310'))
# 
# item_need <- unique(config_item$mm)
# mcf_item_age <- mcf_all_age
# mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
#                           config_item,ti,class_suffix,title)
# mcf_item_age_Q234 <- mcf_sc(mcf_item_age,stand_class)
# eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q234,time_need,title,frac_max,title)',item)))
# 
# # 2. 各机型与故障关系(Q1)
# item <- 'dev_class'
# class_suffix <- ''
# title <- 'Q1_dev'
# # title <- 'Server Type and MCF'
# config_item <- subset(data.config)
# config_item$dev_class <- 'TS'
# config_item$dev_class[config_item$dev_class_id %in% c('A1','A5')] <- 'A'
# config_item$dev_class[config_item$dev_class_id %in% c('B1','B5','B6')] <- 'B'
# config_item$dev_class[config_item$dev_class_id == 'C1'] <- 'C'
# stand_class <- 'baseline'
# item_need <- unique(config_item$dev_class)
# mcf_item_age <- mcf_all_age
# mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
#                           config_item,ti,class_suffix,title)
# mcf_item_age_Q1 <- mcf_sc(mcf_item_age,stand_class)
# source('MCF_function.R')
# eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q1,time_need,title,frac_max,title)',item)))
# 
# # 2.1 各机型中配置单与smart数据提取到的数据不同的占比有多少
# tmp <- data.config[data.config$dup == T,]
# tmp1<- subset(tmp,disk_c == disk_cNew & 
#                 total == totalNew)
# tmp2 <- table(tmp$dev_class_id)
# tmp3 <- table(tmp1$dev_class_id)
# tmp4 <- tapply(tmp$disk_c,tmp$dev_class_id,function(x)sum(!is.na(x)))
# dev_wrong <- data.frame(dev_class_id = names(tmp2),
#                         all = as.numeric(tmp2),
#                         disk_info = as.numeric(tmp4),
#                         right = as.numeric(tmp3))
# 
# # 2.2 A5和B6的服务器型号
# dev_A5 <- subset(cmdb,dev_class_id == 'A5')
# dev_B6 <- subset(cmdb,dev_class_id == 'B6')
# 
# # 2.3 TS5和C1的总容量与时间和服务器型号的关系
# dev <- subset(data.config,dev_class_id %in% c('TS6'))
# dev$total[is.na(dev$total)] <- -1
# tmp <- table(factor(dev$total))
# tmp <- tmp[tmp>1000 & names(tmp) != -1]
# dev <- subset(dev, total %in% names(tmp) & disk_cache %in% c(32,64))
# dev$year <- format(dev$use_time,'%Y')
# 
# ggplot(dev,aes(x = use_time, fill = factor(disk_inter))) + geom_histogram()
# 
# 
# cmdb.dev <- merge(subset(cmdb,dev_class_id == 'C1'),dev[,c('ip','disk_inter','total')])
# cmdb.devA <- subset(cmdb.dev,svr_version == '3.0.0')
# cmdb.devB <- subset(cmdb.dev,svr_version == '3.2.0')
# 
# ggplot(cmdb.dev,aes(x = svr_version, fill = factor(total))) + geom_histogram()
# 
# # 2.4 C1, TS5, TS3有大量与配置单不符的机器配置,原因是什么
# col_need <- c('svr_asset_id','dev_class_id','type_name','model_name','ip','dept_id','bs1',
#               'raid','use_time','svr_version','operator')
# wrongConf <- subset(data.config,disk_c != disk_cNew | total != totalNew)
# 
# wC1 <- subset(wrongConf,dev_class_id == 'C1','ip')
# wC1 <- subset(cmdb,ip %in% wC1$ip,col_need)
# rC1 <- subset(cmdb,!(ip %in% wC1$ip) & dev_class_id == 'C1',col_need)
# 
# wTS3 <- subset(wrongConf,dev_class_id == 'TS3','ip')
# wTS3 <- subset(cmdb,ip %in% wTS3$ip,col_need)
# rTS3 <- subset(cmdb,!(ip %in% wTS3$ip) & dev_class_id == 'TS3',col_need)
# 
# wTS5 <- subset(wrongConf,dev_class_id == 'TS5','ip')
# wTS5 <- subset(cmdb,ip %in% wTS5$ip,col_need)
# rTS5 <- subset(cmdb,!(ip %in% wTS5$ip) & dev_class_id == 'TS5',col_need)
# 
# # 3. 检测单盘ST1000NM0011故障数与在线数差异原因
# tmp1 <- subset(data.config,disk_model == 'ST1000NM0011' & 
#                  disk_c == 1 & 
#                  ol_time_fail != -1,'ip')
# tmp2 <- subset(data.config,disk_model == 'ST1000NM0011' & 
#                  disk_c == 1 & 
#                  ol_time_fail == -1,'ip')
# tmp1 <- as.factor(unique(tmp1$ip))
# tmp2 <- as.factor(unique(tmp2$ip))
# col_need <- c('type_name','model_name','dept_id','bs1','idc_parent_name','use_time','svr_version')
# tmp1 <- subset(cmdb,ip %in% tmp1,col_need)
# tmp2 <- subset(cmdb,ip %in% tmp2,col_need)
# 
# # 4.1 作机型,故障次数的图(for 年会ppt)
# data.plot <- data.config
# data.plot$class <- 'normal'
# data.plot$class[data.plot$ol_time_fail != -1] <- 'fail'
# ggplot(data.plot,aes(x = dev_class_id,fill = class)) + geom_histogram()
# 
# # 4.2 作图,故障时间,服役时间,数量
# data.plot<- subset(data.config,ol_time_fail != -1,c('ol_time_fail','ol_time'))
# data.plot$ol_time_fail <- ceiling(data.plot$ol_time_fail/30)
# data.plot$ol_time <- ceiling(data.plot$ol_time/30)
# data.plot$paste <- paste(data.plot$ol_time_fail,data.plot$ol_time,sep = '_') 
# a <- table(data.plot$paste)
# data.plot <- data.frame(paste = names(a),
#                         count = as.numeric(a))
# a <- strsplit(as.character(data.plot$paste),split = '_')
# b <- data.frame(matrix(unlist(a),nrow = length(a),byrow = T))
# data.plot$ftime <- as.numeric(levels(b[,1])[b[,1]])
# data.plot$usetime <- as.numeric(levels(b[,2])[b[,1]])
# ggplot(data.plot,aes(x = usetime,y = ftime,size = count)) + geom_point()

# # 5. 各机型硬盘升级情况
# # 5.1 C1
# model_need <- c('ST31000524NS','ST3250310NS','ST1000NM0011','ST500NM0011','ST3500514NS')
# tmp <- subset(data.config,dev_class_id == 'C1' & disk_model %in% model_need)
# ggplot(tmp1,aes(x = use_time,fill = disk_model)) + geom_histogram()

# # 6. 测试同机型,同业务,同model,在不同时间上架的硬盘的故障情况是否相符[Q0]
# 
# # 6.1 C1的ST3500514NS和ST1000NM0011进行测试[Q0A]
# tmp1 <- subset(data.config,dev_class_id == 'C1' & 
#                  disk_model == 'ST3500514NS' & 
#                  use_time < as.POSIXct('2011-01-01') &
#                  use_time >= as.POSIXct('2010-07-01'))
# 
# tmp2 <- subset(data.config,dev_class_id == 'C1' & 
#                  disk_model == 'ST3500514NS' & 
#                  use_time < as.POSIXct('2012-01-01') & 
#                  use_time >= as.POSIXct('2011-07-01'))
# 
# tmp1$class <- 'C12011b'
# tmp2$class <- 'C12012b'
# 
# item <- 'class'
# class_suffix <- ''
# title <- 'Q0_time'
# stand_class <- 'baseline'
# config_item <- rbind(tmp1,tmp2)
# item_need <- unique(config_item$class)
# mcf_item_age <- mcf_all_age
# mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
#                           config_item,ti,class_suffix,title)
# mcf_item_age_Q0 <- mcf_sc(mcf_item_age,stand_class)
# eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q0,time_need,title,frac_max,title)',item)))
# 
# # 6.2 对四类机器+业务机器进行测试和分时段求MCF[Q0B]
# data.config$bs1 <- cmdb_dev$bs1[match(data.config$ip,cmdb_dev$ip)]
# data.config$dev_bs <- paste(data.config$dev_class_id,data.config$bs1,sep='_')
# # a <- data.frame(sort(table(data.config$dev_bs),decreasing = T))
# # tmp1 <- subset(data.config,dev_class_id == 'TS4' & bs1 == '[SNG][QQ相册]' & disk_model %in% c('ST1000NM0011','ST31000524NS'))
# m <- c('ST31000524NS','ST1000NM0011','ST31000524NS','ST1000NM0011',
#        'ST2000NM0011','ST32000645NS','ST2000NM0011','ST32000645NS')
# d <- c('TS4','TS4','TS4','TS4','TS5','TS5','TS6','TS6')
# b <- c('[SNG][QQ相册]','[SNG][QQ相册]','[TEG][大文件FTN]','[TEG][大文件FTN]',
#        '数据仓库','数据仓库','[SNG][QQ相册]','[SNG][QQ相册]')
# time <- list(c(as.POSIXct('2010-07-01'),as.POSIXct('2011-01-01'),as.POSIXct('2011-07-01'),as.POSIXct('2012-01-01')),
#           c(as.POSIXct('2011-07-01'),as.POSIXct('2012-01-01'),as.POSIXct('2012-08-01')),
#           c(as.POSIXct('2010-07-01'),as.POSIXct('2011-01-01'),as.POSIXct('2011-07-01'),as.POSIXct('2012-01-01')),
#           c(as.POSIXct('2011-07-01'),as.POSIXct('2012-01-01'),as.POSIXct('2012-08-01')),
#           c(as.POSIXct('2012-09-01'),as.POSIXct('2013-01-01'),as.POSIXct('2013-05-01')),
#           c(as.POSIXct('2012-10-01'),as.POSIXct('2013-04-01'),as.POSIXct('2013-08-01')),
#           c(as.POSIXct('2012-07-01'),as.POSIXct('2012-12-01'),as.POSIXct('2013-04-01')),
#           c(as.POSIXct('2012-10-01'),as.POSIXct('2013-04-01'),as.POSIXct('2013-08-01'))
#           )
# 
# for (i in 1:length(d)){
#   title <- paste('Q0B_time',d[i],b[i],m[i],sep='_')
#   tmp <- subset(data.config,dev_class_id == d[i] & bs1 == b[i] & disk_model == m[i])
# #   p <- ggplot(tmp,aes(x = use_time,fill = disk_model)) + geom_histogram() +ggtitle(paste(t,'[',nrow(tmp),']',sep = ''))
# #   ggsave(plot = p, file = file.path(dir_data,'output','mcf',paste('Q0B_',t,'.png',sep='')), width = 12, height = 9, dpi = 100)
#   t <- time[[i]]
#   tmp1 <- subset(tmp,use_time >= t[1] & use_time <t[2])
#   tmp1$class <- 'T1'
#   tmp2 <- subset(tmp,use_time >= t[2] & use_time <t[3])
#   tmp2$class <- 'T2'
#   tmpa <- rbind(tmp1,tmp2)
#   if(length(t) == 4){
#     tmp3 <- subset(tmp,use_time >= t[3] & use_time <t[4])
#     tmp3$class <- 'T3'
#     tmpa <- rbind(tmpa,tmp3)
#   }
# 
#   item <- 'class'
#   stand_class <- 'baseline'
#   class_suffix <- ''
#   config_item <- tmpa
#   item_need <- unique(tmpa$class)
#   mcf_item_age <- mcf_all_age
#   mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
#                             config_item,ti,class_suffix,title)
#   mcf_item_age_Q0B <- mcf_sc(mcf_item_age,stand_class)
#   eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q0B,time_need,title,frac_max,title)',item))) 
# }

# # 7. 分析[SNG][QQ相册]在不同机型(C1,TS4,TS6)中的MCF以描述不同的disk model对多盘机的影响
# data.config$dev_bs <- paste(data.config$dev_class_id,data.config$bs1,sep='_')
# tmp0 <- subset(data.config,bs1 == '[SNG][QQ相册]' & dup == T)
# tmp <- subset(data.config,bs1 == '[SNG][QQ相册]' & dev_class_id %in% c('TS4','TS6','C1'))
# tmp$dev_model <- paste(tmp$dev_class_id,tmp$disk_model,sep = '_')
# # dm_need <- c('TS4_ST31000524NS','TS6_ST2000NM0011',
# #              'TS4_ST1000NM0011','C1_ST3500514NS','TS6_ST32000645NS')
# dm_need <- c('TS4_ST31000524NS','TS6_ST2000NM0011',
#              'TS4_ST1000NM0011','C1_ST3500514NS')
# 
# item <- 'dev_model'
# stand_class <- 'baseline'
# title <- 'Q3A_[SNG][QQ相册]'
# class_suffix <- ''
# config_item <- tmp
# item_need <- dm_need
# mcf_item_age <- mcf_all_age
# mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
#                           config_item,ti,class_suffix,title)
# mcf_item_age_Q3A <- mcf_sc(mcf_item_age,stand_class)
# eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q3A,time_need,title,frac_max,title)',item))) 

# # 8. 分析C1,TS4,TS5,TS6这4个机型的disk model,业务来确定要固件的对象
# tmp <- subset(data.config,!is.na(disk_c))
# tmp$dbm <- paste(tmp$dev_class_id,tmp$bs1,tmp$disk_model,sep='_')
# tmp$dm <- paste(tmp$dev_class_id,tmp$disk_model,sep='_')
# # table.dbm <- data.frame(sort(table(tmp$dbm),decreasing = T))
# table.dbmA <- tapply(tmp$dbm,tmp$dbm,length)
# table.dbmB <- tapply(tmp$disk_c,tmp$dbm,sum)
# table.dbmC <- tapply(tmp$ol_time,tmp$dbm,mean)
# tmp1 <- subset(tmp,f_time >= as.POSIXct('2014-06-01') & f_time <= as.POSIXct('2014-08-01'))
# table.dbmD <- tapply(tmp1$dbm,tmp1$dbm,length)
# table.dbm <- data.frame(type = names(table.dbmA),
#                         count = as.numeric(table.dbmA),
#                         disk_c = as.numeric(table.dbmB),
#                         ol_time_mean = as.numeric(table.dbmC))
# table.dbm$fcount <- 0
# table.dbm$fcount[match(names(table.dbmD),table.dbm$type)] <- as.numeric(table.dbmD)
# table.dbm$frate <- table.dbm$fcount/table.dbm$disk_c
# # table.dbm <- subset(table.dbm,disk_c >= 500)
# table.dbm <- table.dbm[order(table.dbm$fcount,decreasing = T),]
# 
# 
# 
# dm_need <- c('C1_ST3500514NS','C1_ST500NM0011','C1_ST1000NM0011',
#              'TS4_ST31000524NS','TS4_ST1000NM0011','TS4_ST1000NM0011_ST31000524NS',
#              'TS5_ST2000NM0011','TS5_ST32000645NS','TS5_ST31000524NS',
#              'TS6_ST2000NM0011','TS6_ST32000645NS')
# table.dm <- data.frame(sort(table(tmp$dm),decreasing = T))
# tmp1 <- subset(tmp,dm %in% dm_need)
# 
# for (i in 1:length(dm_need)){
#   cur.dm <- dm_need[i]
#   tmp2 <- subset(tmp1,dm == cur.dm)
#   b <- sort(table(tmp2$bs1),decreasing = T)
#   b <- names(b)[1:5]
#   item <- 'bs1'
#   stand_class <- 'baseline'
#   title <- paste('Q3B',cur.dm,sep='_')
#   class_suffix <- ''
#   config_item <- tmp2
#   item_need <- b
#   mcf_item_age <- mcf_all_age
#   mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
#                             config_item,ti,class_suffix,title)
#   mcf_item_age_Q3B <- mcf_sc(mcf_item_age,stand_class)
#   eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q3B,time_need,title,frac_max,title)',item))) 
# }

# 9. 提取14年6月7月4机型的故障
d <- c('C1','C1','C1','TS4','TS4','TS4','TS4','TS4','TS5','TS6','TS6')
b <- c('手机QQ','浏览器','[N][Qzone]','[MIG][QQ电脑管家]','[OMG][腾讯视频]','[SNG][QQ相册]','[TEG][大文件FTN]','[CDG][QQ邮件]',
       '数据仓库','[OMG][腾讯视频]','[SNG][QQ相册]')
# 9.1 取各机型+业务100台机器数据(从14年67月的7000台机器中提取)
# 9.1.1 读取7000台机器的svrid数据
svrid_db <- read.csv('D:/Data/attrid/attr9020.csv')
svrid_db <- merge(svrid_db,cmdb[,c('svr_asset_id','dev_class_id','bs1')],
                  by.x = 'svrid',by.y = 'svr_asset_id')
svrid_db <- subset(svrid_db,count == 17280)
tmp <- subset(svrid_db,dev_class_id == d[1] & bs1 == b[1])
idx <- ceiling(runif(min(100,nrow(tmp)))*nrow(tmp))
svrid_need <- tmp[idx,]
for (i in 2:length(d)){
  tmp <- subset(svrid_db,dev_class_id == d[i] & bs1 == b[i])
  idx <- ceiling(runif(min(100,nrow(tmp)))*nrow(tmp))
  tmp <- tmp[idx,]
  print(nrow(tmp))
  svrid_need <- rbind(svrid_need,tmp)
}
write.csv(svrid_need['svrid'],file = 'D:/Data/attrid/130Ksvrid_need.csv',row.names = F)

# 9.2 故障数据
bad_io <- subset(data.config,dev_class_id == d[1] & bs1 == b[1] &
                   dup == T & ol_time_fail != -1 & 
                   f_time >= as.POSIXct('2014-06-01') & f_time <= as.POSIXct('2014-08-01'))
for (i in 2:length(d)){
  tmp <- subset(data.config,dev_class_id == d[i] & bs1 == b[i] &
                  dup == T & ol_time_fail != -1 &
                  f_time >= as.POSIXct('2014-06-01') & f_time <= as.POSIXct('2014-08-01'))
  print(nrow(tmp))
  bad_io <- rbind(bad_io,tmp)
}
