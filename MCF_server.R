# 对Server配置相关问题的处理.
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
load(file.path(dir_data,'mcf_all_age.Rda'))
dev_need <- c('TS4','TS5','TS6',
              'C1',
              'A1','A5',
              'B1','B5','B6')
cmdb <- subset(cmdb,use_time > as.POSIXct('2010-01-01'))
cmdb <- subset(cmdb, dev_class_id %in% dev_need)
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
data.config <- subset(data.config,dev_class_id %in% dev_need)

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
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q6,time_need,title,frac_max,title)',item)))

# 1.1. A1机型有8个盘的机器在RAID5和2RAID1+6RAID10时的MCF
item <- 'raid'
title <- 'Q6A_A1_RAID'
class_suffix <- ''
stand_class <- 'baseline'
config_item <- subset(data.config,dev_class_id == 'A1' & raid %in% c('RAID5','2RAID1+6RAID10'))
item_need <- c('RAID5','2RAID1+6RAID10')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q6A <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q6A,time_need,title,frac_max,title)',item)))

# 1.2. RAID5机器不同机型的的MCF
item <- 'dev_class_id'
title <- 'Q6B_RAID5'
stand_class <- 'baseline'
class_suffix <- ''
config_item <- subset(data.config,raid == 'RAID5')
item_need <- c('A5','A1')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q6B <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q6B,time_need,title,frac_max,title)',item)))

# 1.3. 机型,盘数,RAID结合讨论
data.config$ddr <- paste(data.config$dev_class_id,data.config$disk_cNew,data.config$raid,sep='_')
item <- 'ddr'
title <- 'Q6C_DDR'
stand_class <- 'baseline'
class_suffix <- ''
# table.ddr <- sort(table(data.config$ddr))
table.ddr <- sort(tapply(data.config$disk_cNew,data.config$ddr,sum))
config_item <- subset(data.config,ddr %in% names(table.ddr)[table.ddr>1000])
item_need <- unique(config_item$ddr)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q6C <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q6C,time_need,title,frac_max,title)',item)))

mcf_item_age_Q6C$class <- factor(mcf_item_age_Q6C$class)
mean_sm <- data.frame(item = levels(mcf_item_age_Q6C$class),
                      mean_sm = as.numeric(tapply(mcf_item_age_Q6C$stand_mcf,mcf_item_age_Q6C$class,mean)),
                      count = as.numeric(tapply(mcf_item_age_Q6C$atrisk,mcf_item_age_Q6C$class,max)))
mean_sm <- mean_sm[order(mean_sm$mean_sm),]
row.names(mean_sm) <- NULL

# 1.4. 机型与raid,机型与model联立,进行过滤
data.config$dr <- paste(data.config$dev_class_id,data.config$raid,sep='_')
data.config$dm <- paste(data.config$dev_class_id,data.config$disk_model,sep = '_')
sta_dr <- sort(tapply(data.config$disk_cNew,data.config$dr,sum))
sta_dm <- sort(tapply(data.config$disk_cNew,data.config$dm,sum))
sta_dr <- data.frame(item = names(sta_dr),value = as.numeric(sta_dr))
sta_dm <- data.frame(item = names(sta_dm),value = as.numeric(sta_dm))
# 1.5. RAID与NORAID的MCF比较
data.config$israid <- 'RAID'
data.config$israid[data.config$raid == 'NORAID'] <- 'NORAID'
item <- 'israid'
title <- 'Q6D_RAID'
stand_class <- 'baseline'
class_suffix <- ''
config_item <- subset(data.config)
item_need <- unique(data.config$israid)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q6D <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q6D,time_need,title,frac_max,title)',item)))

# 1.6. B5,B6,B1RAID1机器的MCF比较
item <- 'dev_class_id'
title <- 'Q6E_RAID'
stand_class <- 'baseline'
class_suffix <- ''
config_item <- subset(data.config, dev_class_id %in% c('B1','B5','B6') & raid == 'RAID1' & !(ip %in% cmdb$ip[cmdb$bs1 == 'CC_LOL']))
item_need <- unique(data.config$dev_class_id)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q6E <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q6E,time_need,title,frac_max,title)',item)))

col_need <- c('svr_asset_id','dev_class_id','type_name','model_name','ip','dept_id','bs1',
              'raid','use_time','svr_version','operator','idc_parent_name')
cmdb_Q6EA <- subset(cmdb,ip %in% data.config$ip[data.config$ol_time_fail != -1] & dev_class_id == 'B6',col_need)
cmdb_Q6EB <- subset(cmdb,ip %in% data.config$ip[data.config$ol_time_fail == -1] & dev_class_id == 'B6',col_need)

# 2. 容量对单盘存储系统故障的影响(Q2)
item <- 'totalNew'
class_suffix <- 'GB'
title <- 'Q2_capacity_single_disk'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew == 1)',item)))
item_need <- c(250,500,1000)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q2 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q2,time_need,title,frac_max,title)',item)))

# 3. 容量对多盘存储系统故障的影响(Q5)
item <- 'totalNew'
class_suffix <- 'GB'
title <- 'Q5_capacity_multi_disk'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew != 1)',item)))
item_need <- c(292,600,1168,1752,3600,12000,24000)
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q5 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q5,time_need,title,frac_max,title)',item)))

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
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q3,time_need,title,frac_max,title)',item)))

# 5. Disk Model对多盘存储系统(单一model)故障的影响(Q9)
item <- 'disk_model'
class_suffix <- ''
title <- 'Q9_disk_model_singleB'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew != 1 & disk_model_c1 == 1)',item)))
item_need <- c('ST31000524NS','ST2000NM0011','ST1000NM0011','ST32000645NS')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q9 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q9,time_need,title,frac_max,title)',item)))

# 6. Disk Model对多盘存储系统(多model)故障的影响(Q10)
item <- 'disk_model'
class_suffix <- ''
title <- 'Q10_disk_model_multi'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew != 1 & disk_model_c1 != 1)',item)))
item_need <- c('ST1000NM0011_ST31000524NS','ST2000NM0011_ST32000645NS')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q10 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q10,time_need,title,frac_max,title)',item)))

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
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q7,time_need,title,frac_max,title)',item)))

# 8. RAID(混合)对多盘存储系统故障的影响(Q8)
item <- 'raid'
class_suffix <- ''
title <- 'Q8_raid_multiple'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config,!is.na(%s) & disk_cNew != 1)',item)))
item_need <- c('2RAID1+10RAID10','2RAID1+5RAID5+5RAID5',
               '4RAID5+8RAID10','2RAID1+10RAID5')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q8 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q8,time_need,title,frac_max,title)',item)))

# 9. 接口对单盘存储系统故障的影响(Q4)
item <- 'itfNew'
class_suffix <- ''
title <- 'Q4_interface_simple'
config_item <- subset(data.config, itfNew != '0NOINTER' & disk_cNew == 1)
stand_class <- 'baseline'
item_need <- c('SATA3','SATA2','SAS')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q4 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q4,time_need,title,frac_max,title)',item)))

# 10. 接口对多盘存储系统故障的影响(Q11)
item <- 'itfNew'
class_suffix <- ''
title <- 'Q11_interface_multi'
config_item <- subset(data.config, itfNew != '0NOINTER' & disk_cNew != 1)
stand_class <- 'baseline'
item_need <- c('SAS','SATA2','SATA3')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q11 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q11,time_need,title,frac_max,title)',item)))


