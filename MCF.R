#@@@ MCF算法实现与数据计算
rm(list = ls())
require(ggplot2)
source('MCF_function.R')
source('plot_2yaxis.R')

#@@@ PARAMETERS
dir_data <- 'D:/Data/Disk Number'
load(file.path(dir_data,'data_mcf.Rda'))
load(file.path(dir_data,'disk_number_label.Rda'))
# 读入nomodel数据并与disk_ip merge. nomodel的数据也是disk相关数据,是我们通过规则推测出来的
load(file.path(dir_data,'cmdb_nomodel_add.Rda'))
# 过滤2010-01-01前上架的机器,因为此前上面的机器下架数量很多
cmdb <- subset(cmdb,use_time > as.POSIXct('2010-01-01'))
# 读入不同disk model的信息
model_info <- read.csv(file.path(dir_data,'num_model.csv'))
model_info_clear <- model_info[!duplicated(model_info$Model_clear),]
model_info_clear <- model_info_clear[order(model_info_clear$capacity),]
end_time <- as.POSIXct('2013-12-01',tz = 'UTC')
data.mcf <- data.mcf[order(data.mcf$ip,data.mcf$ol_time_fail),]
ti <- 30
time_need <- 4*round(365/ti)
frac_max <- 0.1

#@@@ MAIN part
# 0. 处理no model数据并与disk_ip合并生成disk_ip_cn
cmdb_nomodel <- subset(cmdb_nomodel, ip != '')
cmdb_nomodel$disc_c <- cmdb_nomodel$disk_c*2
cmdb_nomodel$head_c <- cmdb_nomodel$disc_c*2
cmdb_nomodel$disk_model <- 'NOMODEL'
cmdb_nomodel$disk_model_c <- '0'
cmdb_nomodel$disk_model_c1 <- 1
cmdb_nomodel$disk_inter <- 'NOINTER'
disk_ip_cn <- rbind(disk_ip,cmdb_nomodel)
disk_ip_cn$ip <- factor(disk_ip_cn$ip)

# 1 生成数据集&baseline
cmdb <- cmdb[!duplicated(cmdb$ip),]
# 1.1 data.mcf_disk_ip为data.mcf与disk_ip的交集.为有硬盘相关信息的机器的MCF数据
# data.mcf_disk_ip <- merge(data.mcf,disk_ip_cn,by = 'ip')
# 1.2data.mcf_RAID 为data.mcf与cmdb的交集,为data.mcf添加raid信息
# data.mcf_RAID <- merge(data.mcf,cmdb[,c('ip','raid')],by = 'ip')
# 1.3结合disk_ip,cmdb,data.mcf生成包含MCDINR六个维度数据的表
data.mcf$ol_time <- data.mcf$end - data.mcf$start
data.config <- merge(data.mcf[,c('ip','start','f_time','end','ol_time','ol_time_fail')],
                     cmdb[,c('ip','raid')],by = 'ip')
data.config <- merge(data.config,
                     disk_ip[,c('ip','total','disk_c','disk_model','disk_inter','disk_model_c','disk_model_c1')],
                     by = 'ip',
                     all.x = T)
# 生成新的sata数据
data.config$disk_model_c <- as.character(data.config$disk_model_c)
data.config$disk_inter <- as.character(data.config$disk_inter)
data.config$disk_inter[is.na(data.config$disk_inter)] <- 'NOINTER'
data.config$disk_model_c[is.na(data.config$disk_model_c)] <- '0'

tmp <- mapply(function(x,y)tapply(as.numeric(unlist(strsplit(x,'_'))),unlist(strsplit(y,'_')),sum),
              data.config$disk_model_c,data.config$disk_inter)
tmp1 <- sapply(tmp,function(x){
  if(length(x) == 1)paste(x[1],names(x)[1],sep='')
  else paste(x[1],names(x)[1],'+',x[2],names(x)[2],sep='')
})
row.names(tmp1) <- NULL
data.config$disk_inter_all <- tmp1
data.config$disk_inter <- factor(data.config$disk_inter)
data.config$disk_model_c <- factor(data.config$disk_model_c)
data.config$disk_inter_all <- factor(data.config$disk_inter_all)
save(data.config,file = file.path(dir_data,'mcf_dataConfig.Rda'))
# 1.4 生成mcf
load(file.path(dir_data,'mcf_dataConfig.Rda'))
mcf_all_age <- mcf_age(data.config,ti)
mcf_all_age$class <- 'baseline'
mcf_all_age$classNew <- paste('baseline[',length(unique(data.config$ip)),
                           '/',nrow(subset(data.config,ol_time_fail!= -1)),']',sep='')
png(file = file.path(dir_data,'output','mcf','fail_atrisk',paste('Baseline','fails_atrisk.png',sep = '_')),width = 1200,height = 900,units = 'px')
g_plot <- line_plot_2yaxis(subset(mcf_all_age,,c('time','fails','atrisk')))
grid.draw(g_plot)
dev.off()
mcf_all_age <- mcf_sc(mcf_all_age,'baseline')
pset1 <- mcf_plot(mcf_all_age,time_need,'all_age',frac_max)
save(mcf_all_age,data.config,file = file.path(dir_data,'mcf_all_age.Rda'))
# 1.5 用rr把数据分为三段.
p6 <- pset1[[5]] + geom_line(data = mcf_all_age[c(2,31,43,70),],mapping = aes(time,rr),size = 1,color = 'blue') 
ggsave(file=file.path(dir_data,'output','mcf',paste('all_rr_3phase.png',sep='_')), plot=p6, width = 12, height = 9, dpi = 100)



# # 2. capacity
# capa_need <- c(250,500,1000,12000,24000)
# mcf_capa_age <- mcf_all_age
# for (i in 1:length(capa_need)) {
#   tmp <- mcf_age(subset(data.config,total == capa_need[i]),ti)
#   tmp$classNew <- paste(capa_need[i],'GB',sep='')
#   mcf_capa_age <- rbind(mcf_capa_age,tmp)
# }
# p_capa <- mcf_plot(mcf_capa_age,time_need,'Capacity')
# 
# # 3. 同样的硬盘型号在单盘机和多盘机上的故障表现差别
# model_name <- c('ST1000NM0011','ST31000524NS')
# mcf_sm_age <- mcf_all_age
# time_interval <- ti
# 
# tmp <- mcf_age(subset(data.config,disk_model == model_name[1] & disk_model_c == '1'),time_interval)
# tmp$classNew <- paste(model_name[1],'*1',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# tmp <- mcf_age(subset(data.config,disk_model == model_name[1] & disk_model_c == '12'),time_interval)
# tmp$classNew <- paste(model_name[1],'*12',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# tmp <- mcf_age(subset(data.config,disk_model == model_name[2] & disk_model_c == '1'),time_interval)
# tmp$classNew <- paste(model_name[2],'*1',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# tmp <- mcf_age(subset(data.config,disk_model == model_name[2] & disk_model_c == '12'),time_interval)
# tmp$classNew <- paste(model_name[2],'*12',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# p_sm <- mcf_plot(mcf_sm_age,1500,'Multiple_Single_disk')
# 
# # 4. 同容量,不同disk model的机器的故障表现
# model_name <- c('ST1000NM0011','ST31000524NS','ST32000645NS','ST2000NM0011',
#                 'ST500NM0011','ST3500514NS')
# mcf_diffmodel_age <- mcf_all_age
# 
# for (i in 1:length(model_name)){
#   tmp <- mcf_age(subset(data.config,disk_model == model_name[i]),ti)
#   tmp$classNew <- model_name[i]
#   mcf_diffmodel_age <- rbind(mcf_diffmodel_age,tmp)
# }
# p_diffmodel <- mcf_plot(mcf_diffmodel_age,-1,'Different_disk_model')
# 
# # 5. 1000GB单盘机
# model_name <- c('ST1000NM0011','ST31000524NS')
# mcf_sm_age <- mcf_all_age
# 
# tmp <- mcf_age(subset(data.config,disk_model == model_name[1] & disk_model_c == '1'),ti)
# tmp$classNew <- paste(model_name[1],'*1',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# tmp <- mcf_age(subset(data.config,disk_model == model_name[2] & disk_model_c == '1'),ti)
# tmp$classNew <- paste(model_name[2],'*1',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# p_sd <- mcf_plot(mcf_sm_age,-1,'1000_Single_disk')
# 
# # 6. 1T容量的服务器是用500G*2还是用1T*1好.
# mcf_capa1000_age <- mcf_all_age
# tmp <- mcf_age(subset(data.config,total == 1000 & disk_c == 1),ti)
# tmp$classNew <- 1
# mcf_capa1000_age <- rbind(mcf_capa1000_age,tmp)
# tmp <- mcf_age(subset(data.config,total == 1000 & disk_c == 2),ti)
# tmp$classNew <- 2
# mcf_capa1000_age <- rbind(mcf_capa1000_age,tmp)
# p_capa1000 <- mcf_plot(mcf_capa1000_age,-1,'capa1000')
# 
# # 7. 单盘MCF boost现象,看所有单盘的boost现象,以及开始boost的时间
# model_name <- c('ST1000NM0011','ST31000524NS',
#                 'ST500NM0011','ST3500514NS','ST3250310NS')
# mcf_singledisk_age <- mcf_all_age
# 
# for (i in 1:length(model_name)){
#   tmp0 <- subset(data.config,disk_model == model_name[i] & disk_c == 1)
#   if (nrow(tmp0) <= 10) next
#   else {
#     tmp <- mcf_age(tmp0,ti)
#     tmp$classNew <- model_name[i]
#     mcf_singledisk_age <- rbind(mcf_singledisk_age,tmp)
#   }
# }
# p_diffmodel <- mcf_plot(subset(mcf_singledisk_age,class %in% model_name[1:2]),
#                         60,'Single_disk_MCF_boost')
# 
# # 8. 不同机型的1000G单盘的boost现象是否相同.
# mcf_1Tboostdev_age <- mcf_all_age
# tmp <- subset(data.config_raid_dev_class,total == 1000 & disk_c == 1)
# uni_dev_class <- sort(table(factor(tmp$dev_class_id)))
# dev_class_need <- names(uni_dev_class)[uni_dev_class > 500]
# 
# for (i in 1:length(dev_class_need)){
#   tmp1 <- subset(tmp,dev_class_id == dev_class_need[i])
#   tmp2 <- mcf_age(tmp1,ti)
#   tmp2$class <- dev_class_need[i]
#   mcf_1Tboostdev_age <- rbind(mcf_1Tboostdev_age,tmp2)
# }
# p_1Tboostdev <- mcf_plot(subset(mcf_1Tboostdev_age),-1,'1T_boost_dev')
# 
# # 9. capacity for single disk server
# capa_need <- c(250,500,1000)
# mcf_capasingle_age <- mcf_all_age
# for (i in 1:length(capa_need)) {
#   tmp <- mcf_age(subset(data.config,total == capa_need[i]),ti)
#   tmp$classNew <- paste(capa_need[i],'GB',sep='')
#   mcf_capasingle_age <- rbind(mcf_capasingle_age,tmp)
# }
# p_capa <- mcf_plot(subset(mcf_capasingle_age,class != 'baseline'),50,'Capacity_single')
# 
# # 10. 查看1000GB盘Boost原因
# 
# model_name <- c('ST1000NM0011','ST31000524NS')
# data.config_1Ts <- subset(data.config,disk_model %in% model_name & disk_c == 1)
# data.config_1Ts$ol_time_fail <- round(data.config_1Ts$ol_time_fail/30)
# cmdb.1Ts <- merge(data.config_1Ts,
#                   cmdb[,c('ip','model_name','dept_id','dev_class_name')],
#                   by = 'ip')
# cmdb.1Ts <- cmdb.1Ts[order(cmdb.1Ts$ol_time_fail),]
# cmdb.1Ts$dept_id <- factor(as.character(cmdb.1Ts$dept_id))
# cmdb.1TsA <- subset(cmdb.1Ts,ol_time_fail < 36 & ol_time_fail != 0)
# cmdb.1TsB <- subset(cmdb.1Ts,ol_time_fail >= 36 & ol_time_fail != 0)
# cmdb.1TsC <- subset(cmdb.1Ts,ol_time_fail == 0)
# cmdb.1Ts_model <- subset(cmdb.1Ts,model_name == 'DELL DS24-ML')
# cmdb.model <- subset(cmdb,model_name == 'DELL DS24-ML')
# 
# # 删除DELL DS24-ML之后的MCF
# mcf_1Tdelmodel_age <- mcf_all_age
# data.config_1Ts <- subset(data.config,disk_model %in% model_name & disk_c == 1)
# cmdb.1Ts <- merge(data.config_1Ts,
#                   cmdb[,c('ip','model_name','dept_id','dev_class_name')],
#                   by = 'ip')
# tmp0 <- subset(cmdb.1Ts, model_name != 'DELL DS24-ML',names(data.mcf))
# tmp <- mcf_age(tmp0,ti)
# tmp$classNew <- '1T_noDELL_DS24-ML'
# mcf_1Tdelmodel_age <- rbind(mcf_1Tdelmodel_age,tmp)
# p_capa <- mcf_plot(subset(mcf_1Tdelmodel_age),50,'1T_delmodel')
# 
# # 11. 不同RAID的MCF
# raid_need <- sort(table(data.config$raid))
# raid_need <- names(raid_need)
# raid_need_mix <- raid_need[c(12,13)]
# raid_need_single <- raid_need[c(14,7,15)]
# 
# # mix raid
# mcf_raid_mix_age <- mcf_all_age
# for (i in 1:length(raid_need_mix)) {
#   tmp <- mcf_age(subset(data.config,raid == raid_need_mix[i]),ti)
#   tmp$classNew <- raid_need_mix[i]
#   tmp <- subset(tmp,atrisk > 50)
#   mcf_raid_mix_age <- rbind(mcf_raid_mix_age,tmp)
# }
# p_raid_mix <- mcf_plot(subset(mcf_raid_mix_age),-1,'raid_mix')
# 
# # single raid
# mcf_raid_single_age <- mcf_all_age
# for (i in 1:length(raid_need_single)) {
#   tmp <- mcf_age(subset(data.config,raid == raid_need_single[i]),ti)
#   tmp$classNew <- raid_need_single[i]
#   tmp <- subset(tmp,atrisk > 50)
#   mcf_raid_single_age <- rbind(mcf_raid_single_age,tmp)
#   if (raid_need_single[i] == 'RAID1'){
#     tmp$mcf <- tmp$mcf*5
#   }
# }
# p_raid_single <- mcf_plot(subset(mcf_raid_single_age),-1,'raid_single')
# 
# # 12. 各种RAID类型的硬盘数量统计
# # data.config_diskc 为data.config与disk_ip的diskc的交集,为data.config添加diskc信息
# data.config_diskc <- merge(data.config,disk_ip_cn[,c('ip','total','disk_c')],by = 'ip')
# data.config_diskc <- data.config_diskc[!duplicated(data.config_diskc$ip),]
# raid_need <- sort(table(data.config_diskc$raid))
# raid_need <- raid_need[raid_need > 0]
# sort(table(data.config_diskc$disk_c[data.config_diskc$raid == 'RAID5']))
# 
# # 补全12个盘的raid1和12个盘的raid5
# merge.cmdb_disk <- merge(disk_ip,cmdb[,c('ip','raid','dev_class_id',
#                                          'model_name','dept_id','bs1',
#                                          'idc_parent_id','idc_id','use_time',
#                                          'operator')],by = 'ip')
# data.raid1 <- subset(merge.cmdb_disk,raid == 'RAID1' & disk_c == 12)
# data.raid5 <- subset(merge.cmdb_disk,raid == 'RAID5' & disk_c == 12)
# cmdb.raid1 <- subset(cmdb,raid == 'RAID1')
# cmdb.raid5 <- subset(cmdb,raid == 'RAID5')
# 
# # 13. 查看RAID1的容量是否与真实容量一致
# load(file.path(dir_data,'disk_usage_good.Rda'))
# data.disk_usage <- data.du
# load(file.path(dir_data,'disk_usage_bad.Rda'))
# data.disk_usage <- rbind(data.disk_usage,data.du)
# data.disk_usage <- data.disk_usage[!duplicated(data.disk_usage$ip),]
# 
# merge.cmdb_usage <- merge(cmdb[,c('ip','raid','dev_class_id',
#                                   'model_name','dept_id','bs1',
#                                   'idc_parent_id','idc_id','use_time',
#                                   'operator')],
#                           data.disk_usage[,c('ip','total','usage_percent')],by = 'ip')
# merge.cmdb_usage$total <- merge.cmdb_usage$total/1000
# 
# merge.cmdb_disk_usage <- merge(merge.cmdb_disk,
#                                data.disk_usage[,c('ip','total','usage_percent')],by = 'ip')
# merge.cmdb_disk_usage$total.y <-  merge.cmdb_disk_usage$total.y/1000
# merge.cmdb_disk_usage <- merge.cmdb_disk_usage[,c('ip','total.x','total.y','raid','model_name','dept_id',
#                                                   'idc_parent_id')]
# 
# # 14. 查看机型与Raid的关系
# sort(table(factor(cmdb$raid[cmdb$dev_class_name == 'A5'])))
# sort(table(factor(cmdb$dev_class_id[cmdb$raid == 'RAID0'])))
# 
# # 15. 查看A5机器各类RAID的容量关系
# A5_usage <- subset(merge.cmdb_usage,dev_class_id == 'A5')
# sort(table(factor(A5_usage$raid)))
# A5_usage$ip <- factor(A5_usage$ip)
# A5_usage$total3 <- round(A5_usage$total/10)*10
# A5_total <- tapply(A5_usage$total3,A5_usage$total3,length)
# A5_usage <- A5_usage[!duplicated(A5_usage[c('raid','total3')]),
#                      c('ip','raid','total3','model_name','dept_id',
#                        'bs1','idc_parent_id','idc_id')]
# A5_usage <- A5_usage[order(A5_usage$raid,A5_usage$total3),]
# 
# # 16. 取一些机器的数据
# ip_need <- c('10.157.13.15','10.179.10.232','172.27.180.200',
#              '10.180.1.45','10.194.1.10')
# data.ip_need <- subset(disk_ip_cn,ip %in% ip_need,c('ip','total','disk_model','disk_model_c'))
# data.ip_need1 <- subset(data.disk_usage,ip %in% ip_need)
# data.ip_need2 <- merge(data.ip_need1,cmdb[,c('ip','raid')])


