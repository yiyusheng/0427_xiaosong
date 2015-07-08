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
cmdb <- subset(cmdb,use_time > as.POSIXct('2010-01-01'))


# 1. 盘数对故障的影响(Q1, Q6)
item <- 'disk_c'
class_suffix <- ''
title <- 'Q6_Disk_Number'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s))',item)))
item_need <- c(1,4,12)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q6 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q6,time_need,title,frac_max)',item)))

# 2. 容量对单盘存储系统故障的影响(Q2)
item <- 'total'
class_suffix <- 'GB'
title <- 'Q2_capacity_single_disk'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_c == 1)',item)))
item_need <- c(500,1000)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q2 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q2,time_need,title,frac_max)',item)))

# 3. 容量对多盘存储系统故障的影响(Q5)
item <- 'total'
class_suffix <- 'GB'
title <- 'Q5_capacity_multi_disk'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_c != 1)',item)))
item_need <- c(2000,12000,24000)
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
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_c == 1)',item)))
item_need <- c('ST31000524NS','ST1000NM0011',
               'ST500NM0011','ST3500514NS')
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
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_c != 1)',item)))
item_need <- c('ST31000524NS','ST2000NM0011','ST1000NM0011',
               'ST32000645NS','ST3500514NS')
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
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_c != 1)',item)))
item_need <- c('ST1000NM0011_ST31000524NS_ST3750330NS_ST3750640NS',
               'ST1000NM0011_ST31000524NS',
               'ST1000NM0011_ST31000524NS_ST3750330NS')
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
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s))',item)))
item_need <- c('RAID0','RAID1+0','RAID1','RAID5')
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
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s))',item)))
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
config_item <- subset(data.config, disk_inter_all != '0NOINTER' & disk_c == 1)
stand_class <- 'baseline'
item_need <- c('1SATA3','1SATA2')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q4 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q4,time_need,title,frac_max)',item)))

# 10. 接口对单盘存储系统故障的影响(Q11)
item <- 'disk_inter_all'
class_suffix <- ''
title <- 'Q11_interface_multi'
config_item <- subset(data.config, disk_inter_all != '0NOINTER' & disk_c != 1)
stand_class <- 'baseline'
item_need <- c('10SATA2+2SATA3','4SATA2','11SATA2+1SATA3','12SATA2','12SATA3')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q11 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q11,time_need,title,frac_max)',item)))

# 11. 500G单盘机的接口,model与故障关系分析(Q3&Q4)
tmp1 <- subset(data.config,disk_model == 'ST3500514NS' & disk_c == 1)
tmp2 <- subset(data.config,disk_model == 'ST500NM0011' & disk_c == 1)
tmp1 <- merge(tmp1,cmdb[c('ip','dev_class_name','model_name','bs1')],by = 'ip',all.x = T)
tmp2 <- merge(tmp2,cmdb[c('ip','dev_class_name','model_name','bs1')],by = 'ip',all.x = T)
summary(tmp1[,13:15])
summary(tmp2[,13:15])
#加入model_name作图
data.config_model <- merge(subset(data.config,!is.na(total) & disk_c == 1 & total == 500),
                           cmdb[c('ip','model_name')],
                           by = 'ip',all.x = T)
data.config_model$mm <- paste(data.config_model$disk_model,
                              data.config_model$model_name,
                              sep = '_')
item <- 'mm'
class_suffix <- ''
title <- 'Q234_500G_single_disk_mm'
stand_class <- 'baseline'
config_item <- subset(data.config_model,mm %in% c('ST500NM0011_LENOVO SD210X4',
                                                  'ST3500514NS_DELL DCS5100',
                                                  'ST3500514NS_LENOVO SD210X4',
                                                  'ST3500514NS_HUAWEI XH310',
                                                  'ST500NM0011_HUAWEI XH310'))

item_need <- unique(config_item$mm)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q234 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q234,time_need,title,frac_max)',item)))

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


