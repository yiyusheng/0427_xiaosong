# 对Server配置相关问题的处理.
rm(list = ls())
require(ggplot2)
source('MCF_function.R')
source('plot_2yaxis.R')

#@@@ PARAMETERS
dir_data <- 'D:/Data/Disk Number'
ti <- 30
time_need <- 4*round(365/ti)
frac_max <- 0.1

#@@@ LOAD DATA
load(file.path(dir_data,'disk_number_label.Rda'))
load(file.path(dir_data,'mcf_all_age.Rda'))
dev_need <- c('TS4','TS6',
              'C1','X2',
              'A1','A5',
              'B5','B6')
cmdb <- subset(cmdb,use_time > as.POSIXct('2010-01-01'))
# cmdb <- subset(cmdb, dev_class_id %in% dev_need)
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
# data.config <- subset(data.config,dev_class_id %in% dev_need)

# 1. 盘数对故障的影响(Q1, Q6)
item <- 'disk_cNew'
class_suffix <- ''
title <- 'Q6_Disk_Number'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s))',item)))
item_need <- c(1,2,8,12)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q6 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q6,time_need,title,frac_max)',item)))

# 2. 容量对单盘存储系统故障的影响(Q2)
item <- 'totalNew'
class_suffix <- 'GB'
title <- 'Q2_capacity_single_disk'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew == 1)',item)))
item_need <- c(500,1000)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q2 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q2,time_need,title,frac_max)',item)))

# 3. 容量对多盘存储系统故障的影响(Q5)
item <- 'totalNew'
class_suffix <- 'GB'
title <- 'Q5_capacity_multi_disk'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew != 1)',item)))
item_need <- c(292,600,1168,1752,12000,24000)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q5 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q5,time_need,title,frac_max)',item)))

# 4. Disk Model对单盘存储系统故障的影响(Q3)
item <- 'disk_model'
class_suffix <- ''
title <- 'Q3_disk_model_singleA'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew == 1)',item)))
item_need <- c('ST31000524NS','ST1000NM0011',
               'ST500NM0011','ST3500514NS',
               'ST3250310NS')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q3 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q3,time_need,title,frac_max)',item)))

# 5. Disk Model对多盘存储系统(单一model)故障的影响(Q9)
item <- 'disk_model'
class_suffix <- ''
title <- 'Q9_disk_model_singleB'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew != 1 & disk_model_c1 == 1)',item)))
item_need <- c('ST31000524NS','ST2000NM0011','ST1000NM0011')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q9 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q9,time_need,title,frac_max)',item)))

# 6. Disk Model对多盘存储系统(多model)故障的影响(Q10)
item <- 'disk_model'
class_suffix <- ''
title <- 'Q10_disk_model_multi'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew != 1 & disk_model_c1 != 1)',item)))
item_need <- c('ST1000NM0011_ST31000524NS')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q10 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q10,time_need,title,frac_max)',item)))

# 7. RAID(单一)对多盘存储系统故障的影响(Q7)
item <- 'raid'
class_suffix <- ''
title <- 'Q7_raid_single'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew != 1)',item)))
item_need <- c('RAID1+0','RAID1','RAID5')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q7 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q7,time_need,title,frac_max)',item)))

# 8. RAID(混合)对多盘存储系统故障的影响(Q8)
item <- 'raid'
class_suffix <- ''
title <- 'Q8_raid_multiple'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew != 1)',item)))
item_need <- c('2RAID1+10RAID10','2RAID1+5RAID5+5RAID5',
               '4RAID5+8RAID10','2RAID1+6RAID10','2RAID1+10RAID5')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q8 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q8,time_need,title,frac_max)',item)))

# 9. 接口对单盘存储系统故障的影响(Q4)
item <- 'disk_inter_all'
class_suffix <- ''
title <- 'Q4_interface_simple'
config_item <- subset(data.config, disk_inter_all != '0NOINTER' & disk_cNew == 1)
stand_class <- 'baseline'
item_need <- c('1SATA3','1SATA2')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q4 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q4,time_need,title,frac_max)',item)))

# 10. 接口对多盘存储系统故障的影响(Q11)
item <- 'disk_inter_all'
class_suffix <- ''
title <- 'Q11_interface_multi'
config_item <- subset(data.config, disk_inter_all != '0NOINTER' & disk_cNew != 1)
stand_class <- 'baseline'
item_need <- c('11SATA2+1SATA3','12SATA2','12SATA3')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q11 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q11,time_need,title,frac_max)',item)))

# # 11. 500G单盘机的接口,model与故障关系分析(Q3&Q4)
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
# eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q234,time_need,title,frac_max)',item)))

# 12. 各机型与故障关系(Q1)
item <- 'dev_class_id'
class_suffix <- ''
title <- 'Q1_dev'
config_item <- subset(data.config)
stand_class <- 'baseline'
item_need <- unique(data.config$dev_class_id)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q1 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q1,time_need,title,frac_max)',item)))

# 12.1 各机型中配置单与smart数据提取到的数据不同的占比有多少
tmp <- data.config[data.config$dup == T,]
tmp1<- subset(tmp,disk_c == disk_cNew & 
                      total == totalNew)
tmp2 <- table(tmp$dev_class_id)
tmp3 <- table(tmp1$dev_class_id)
tmp4 <- tapply(tmp$disk_c,tmp$dev_class_id,function(x)sum(!is.na(x)))
dev_wrong <- data.frame(dev_class_id = names(tmp2),
                        all = as.numeric(tmp2),
                        disk_info = as.numeric(tmp4),
                        right = as.numeric(tmp3))

# 12.2 A5和B6的服务器型号
dev_A5 <- subset(cmdb,dev_class_id == 'A5')
dev_B6 <- subset(cmdb,dev_class_id == 'B6')

# 12.3 TS5和C1的总容量与时间和服务器型号的关系
dev <- subset(data.config,dev_class_id %in% c('TS6'))
dev$total[is.na(dev$total)] <- -1
tmp <- table(factor(dev$total))
tmp <- tmp[tmp>1000 & names(tmp) != -1]
dev <- subset(dev, total %in% names(tmp) & disk_cache %in% c(32,64))
dev$year <- format(dev$use_time,'%Y')

ggplot(dev,aes(x = use_time, fill = factor(disk_inter))) + geom_histogram()


cmdb.dev <- merge(subset(cmdb,dev_class_id == 'C1'),dev[,c('ip','disk_inter','total')])
cmdb.devA <- subset(cmdb.dev,svr_version == '3.0.0')
cmdb.devB <- subset(cmdb.dev,svr_version == '3.2.0')

ggplot(cmdb.dev,aes(x = svr_version, fill = factor(total))) + geom_histogram()
# 12.4 C1, TS5, TS3有大量与配置单不符的机器配置,原因是什么
col_need <- c('svr_asset_id','dev_class_id','type_name','model_name','ip','dept_id','bs1',
              'raid','use_time','svr_version','operator')
wrongConf <- subset(data.config,disk_c != disk_cNew | total != totalNew)

wC1 <- subset(wrongConf,dev_class_id == 'C1','ip')
wC1 <- subset(cmdb,ip %in% wC1$ip,col_need)
rC1 <- subset(cmdb,!(ip %in% wC1$ip) & dev_class_id == 'C1',col_need)

wTS3 <- subset(wrongConf,dev_class_id == 'TS3','ip')
wTS3 <- subset(cmdb,ip %in% wTS3$ip,col_need)
rTS3 <- subset(cmdb,!(ip %in% wTS3$ip) & dev_class_id == 'TS3',col_need)

wTS5 <- subset(wrongConf,dev_class_id == 'TS5','ip')
wTS5 <- subset(cmdb,ip %in% wTS5$ip,col_need)
rTS5 <- subset(cmdb,!(ip %in% wTS5$ip) & dev_class_id == 'TS5',col_need)

# # 12. 检测单盘ST1000NM0011故障数与在线数差异原因
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


