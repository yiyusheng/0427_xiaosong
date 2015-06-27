#@@@ MCF算法实现与数据计算
rm(list = ls())
dir_data <- 'D:/Data/Disk Number'
load(file.path(dir_data,'data_mcf.Rda'))
end_time <- as.POSIXct('2013-12-01',tz = 'UTC')
require(ggplot2)
data.mcf <- data.mcf[order(data.mcf$ip,data.mcf$ol_time_fail),]

##########################################
#@@@ 函数
# 1. MCF算法(AGE).
mcf_age <- function(data.mcf,time_interval){
  # 原始的time_interval是1天
  data.mcf$ol_time_fail <- round(data.mcf$ol_time_fail/time_interval)
  data.mcf$start <- round(data.mcf$start/time_interval)
  data.mcf$end <- round(data.mcf$end/time_interval)
  table.ol_time_fail <- table(data.mcf$ol_time_fail)[-c(1,2)]
  tmp <- names(table.ol_time_fail)
  mcf <- data.frame(time = as.numeric(names(table.ol_time_fail)),
                    fails = as.numeric(table.ol_time_fail))
  mcf$atrisk <- sapply(mcf$time,function(x){
    #   nrow(subset(data.mcf,start < x & end > x))
    sum(data.mcf$start < x & data.mcf$end > x)
  })
  mcf$errorrate_pertime <- mcf$fails/mcf$atrisk
  mcf$mcf <- sapply(1:nrow(mcf),function(x){
    sum(mcf$errorrate_pertime[1:x])
  })
  len <- nrow(mcf)
  diff <- (mcf$mcf[2:len] - mcf$mcf[1:(len-1)])/(mcf$time[2:len] - mcf$time[1:(len-1)])
  diff <- c(0,diff)
  mcf$rr <- diff
  #如果余下的磁盘不足最大值时的1%,则停止
  mcf <- subset(mcf,atrisk > max(atrisk)*0.01)
  return(mcf)
}

# 2. MCF算法(CALENDAR)
mcf_cal <- function(data.mcf,time_interval) {
  # 原始的time_interval是1天
  data.mcf$ol_time_fail_c <- round(data.mcf$ol_time_fail_c/time_interval)
  data.mcf$start_c <- round(data.mcf$start_c/time_interval)
  data.mcf$end_c <- round(data.mcf$end_c/time_interval)
  table.ol_time_fail <- table(data.mcf$ol_time_fail_c)[-c(1,2)]
  tmp <- names(table.ol_time_fail)
  mcf <- data.frame(time = as.numeric(names(table.ol_time_fail)),
                    fails = as.numeric(table.ol_time_fail))
  mcf$atrisk <- sapply(mcf$time,function(x){
    #   nrow(subset(data.mcf,start < x & end > x))
    sum(data.mcf$start_c < x & data.mcf$end_c > x)
  })
  mcf$errorrate_pertime <- mcf$fails/mcf$atrisk
  mcf$mcf <- sapply(1:nrow(mcf),function(x){
    sum(mcf$errorrate_pertime[1:x])
  })
  len <- nrow(mcf)
  diff <- (mcf$mcf[2:len] - mcf$mcf[1:(len-1)])/(mcf$time[2:len] - mcf$time[1:(len-1)])
  diff <- c(0,diff)
  mcf$rr <- diff
  return(mcf)
}

# 3. 作图并保存
mcf_plot <- function(mcf,time_limit,prefix) {
  # 属性
  line_size = 2
  if (time_limit == -1){
    time_limit <- max(mcf$time)
  }
  mcf_p <- subset(mcf,time <= time_limit & atrisk > 50)
  geo_aes <- aes(x = time,y = mcf,group = class, color = class)
  p1 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size) + ggtitle('mcf')

  geo_aes <- aes(x = time,y = errorrate_pertime,group = class, color = class)
  p2 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size) + ggtitle('rate')
  
  geo_aes <- aes(x = time,y = fails,group = class, color = class)
  p3 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size) + ggtitle('fails')
  
  geo_aes <- aes(x = time,y = atrisk,group = class, color = class)
  p4 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size) + ggtitle('at_risk')
  
  geo_aes <- aes(x = time,y = rr,group = class, color = class)
  p5 <- ggplot(subset(mcf_p,rr != 0,time < time_limit),geo_aes) + geom_line(size = line_size) + ggtitle('Recurrence_rate')

  ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'mcf.png',sep='_')), plot=p1, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'rate.png',sep='_')), plot=p2, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'fails.png',sep='_')), plot=p3, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'atrisk.png',sep='_')), plot=p4, width = 12, height = 9, dpi = 100)
  ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'rr.png',sep='_')), plot=p5, width = 12, height = 9, dpi = 100)

  return(list(p1=p1,p2=p2,p3=p3,p4=p4,p5=p5))
}

##########################################
# #@@@ MAIN part
load(file.path(dir_data,'disk_number_label.Rda'))
# 读入nomodel数据并与disk_ip merge. nomodel的数据也是disk相关数据,是我们通过规则推测出来的
load(file.path(dir_data,'cmdb_nomodel_add.Rda'))
cmdb_nomodel <- subset(cmdb_nomodel, ip != '')
cmdb_nomodel$disc_c <- cmdb_nomodel$disk_c*2
cmdb_nomodel$head_c <- cmdb_nomodel$disc_c*2
cmdb_nomodel$disk_model <- 'NOMODEL'
cmdb_nomodel$disk_model_c <- '0'
cmdb_nomodel$disk_model_c1 <- 1
disk_ip_cn <- rbind(disk_ip,cmdb_nomodel)
disk_ip_cn$ip <- factor(disk_ip_cn$ip)
# data.mcf_disk_ip为data.mcf与disk_ip的交集.为有硬盘相关信息的机器的MCF数据
data.mcf_disk_ip <- merge(data.mcf,disk_ip_cn,by = 'ip')
# data.mcf_RAID 为data.mcf与cmdb的交集,为data.mcf添加raid信息
data.mcf_RAID <- merge(data.mcf,cmdb[,c('ip','raid')],by = 'ip')


ti <- 30
# # 1. baseline
mcf_all_age <- mcf_age(data.mcf,ti)
mcf_all_age$class <- 'baseline'
pset1 <- mcf_plot(mcf_all_age,72,'all_age')
# 用rr把数据分为三段.
p6 <- pset1[[5]] + geom_line(data = mcf_all_age[c(2,31,43,70),],mapping = aes(time,rr),size = 1,color = 'blue') 
ggsave(file=file.path(dir_data,'output','mcf',paste('all_rr_3phase.png',sep='_')), plot=p6, width = 12, height = 9, dpi = 100)


# # 2. capacity
capa_need <- c(250,500,1000,12000,24000)
mcf_capa_age <- mcf_all_age
for (i in 1:length(capa_need)) {
  tmp <- mcf_age(subset(data.mcf_disk_ip,total == capa_need[i]),ti)
  tmp$class <- paste(capa_need[i],'GB',sep='')
  mcf_capa_age <- rbind(mcf_capa_age,tmp)
}
p_capa <- mcf_plot(mcf_capa_age,72,'Capacity')
# 
# # 3. 同样的硬盘型号在单盘机和多盘机上的故障表现差别
# model_name <- c('ST1000NM0011','ST31000524NS')
# mcf_sm_age <- mcf_all_age
# time_interval <- ti
# 
# tmp <- mcf_age(subset(data.mcf_disk_ip,disk_model == model_name[1] & disk_model_c == '1'),time_interval)
# tmp$class <- paste(model_name[1],'*1',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# tmp <- mcf_age(subset(data.mcf_disk_ip,disk_model == model_name[1] & disk_model_c == '12'),time_interval)
# tmp$class <- paste(model_name[1],'*12',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# tmp <- mcf_age(subset(data.mcf_disk_ip,disk_model == model_name[2] & disk_model_c == '1'),time_interval)
# tmp$class <- paste(model_name[2],'*1',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# tmp <- mcf_age(subset(data.mcf_disk_ip,disk_model == model_name[2] & disk_model_c == '12'),time_interval)
# tmp$class <- paste(model_name[2],'*12',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# p_sm <- mcf_plot(mcf_sm_age,1500,'Multiple_Single_disk')
# 
# # 4. 同容量,不同disk model的机器的故障表现
# model_name <- c('ST1000NM0011','ST31000524NS','ST32000645NS','ST2000NM0011',
#                 'ST500NM0011','ST3500514NS')
# mcf_diffmodel_age <- mcf_all_age
# 
# for (i in 1:length(model_name)){
#   tmp <- mcf_age(subset(data.mcf_disk_ip,disk_model == model_name[i]),ti)
#   tmp$class <- model_name[i]
#   mcf_diffmodel_age <- rbind(mcf_diffmodel_age,tmp)
# }
# p_diffmodel <- mcf_plot(mcf_diffmodel_age,-1,'Different_disk_model')
# 
# # 5. 1000GB单盘机
# model_name <- c('ST1000NM0011','ST31000524NS')
# mcf_sm_age <- mcf_all_age
# 
# tmp <- mcf_age(subset(data.mcf_disk_ip,disk_model == model_name[1] & disk_model_c == '1'),ti)
# tmp$class <- paste(model_name[1],'*1',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# tmp <- mcf_age(subset(data.mcf_disk_ip,disk_model == model_name[2] & disk_model_c == '1'),ti)
# tmp$class <- paste(model_name[2],'*1',sep='')
# mcf_sm_age <- rbind(mcf_sm_age,tmp)
# p_sd <- mcf_plot(mcf_sm_age,-1,'1000_Single_disk')
# 
# # 6. 1T容量的服务器是用500G*2还是用1T*1好.
# mcf_capa1000_age <- mcf_all_age
# tmp <- mcf_age(subset(data.mcf_disk_ip,total == 1000 & disk_c == 1),ti)
# tmp$class <- 1
# mcf_capa1000_age <- rbind(mcf_capa1000_age,tmp)
# tmp <- mcf_age(subset(data.mcf_disk_ip,total == 1000 & disk_c == 2),ti)
# tmp$class <- 2
# mcf_capa1000_age <- rbind(mcf_capa1000_age,tmp)
# p_capa1000 <- mcf_plot(mcf_capa1000_age,-1,'capa1000')
# 
# # 7. 单盘MCF boost现象,看所有单盘的boost现象,以及开始boost的时间
# model_name <- c('ST1000NM0011','ST31000524NS',
#                 'ST500NM0011','ST3500514NS','ST3250310NS')
# mcf_singledisk_age <- mcf_all_age
# 
# for (i in 1:length(model_name)){
#   tmp0 <- subset(data.mcf_disk_ip,disk_model == model_name[i] & disk_c == 1)
#   if (nrow(tmp0) <= 10) next
#   else {
#     tmp <- mcf_age(tmp0,ti)
#     tmp$class <- model_name[i]
#     mcf_singledisk_age <- rbind(mcf_singledisk_age,tmp)
#   }
# }
# p_diffmodel <- mcf_plot(subset(mcf_singledisk_age,class %in% model_name[1:2]),
#                         60,'Single_disk_MCF_boost')
# 
# # 8. 不同机型的1000G单盘的boost现象是否相同.
# mcf_1Tboostdev_age <- mcf_all_age
# tmp <- subset(data.mcf_disk_ip_raid_dev_class,total == 1000 & disk_c == 1)
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
#   tmp <- mcf_age(subset(data.mcf_disk_ip,total == capa_need[i]),ti)
#   tmp$class <- paste(capa_need[i],'GB',sep='')
#   mcf_capasingle_age <- rbind(mcf_capasingle_age,tmp)
# }
# p_capa <- mcf_plot(subset(mcf_capasingle_age,class != 'baseline'),50,'Capacity_single')

# # 10. 查看1000GB盘Boost原因
# 
# model_name <- c('ST1000NM0011','ST31000524NS')
# data.mcf_disk_ip_1Ts <- subset(data.mcf_disk_ip,disk_model %in% model_name & disk_c == 1)
# data.mcf_disk_ip_1Ts$ol_time_fail <- round(data.mcf_disk_ip_1Ts$ol_time_fail/30)
# cmdb.1Ts <- merge(data.mcf_disk_ip_1Ts,
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
# data.mcf_disk_ip_1Ts <- subset(data.mcf_disk_ip,disk_model %in% model_name & disk_c == 1)
# cmdb.1Ts <- merge(data.mcf_disk_ip_1Ts,
#                   cmdb[,c('ip','model_name','dept_id','dev_class_name')],
#                   by = 'ip')
# tmp0 <- subset(cmdb.1Ts, model_name != 'DELL DS24-ML',names(data.mcf))
# tmp <- mcf_age(tmp0,ti)
# tmp$class <- '1T_noDELL_DS24-ML'
# mcf_1Tdelmodel_age <- rbind(mcf_1Tdelmodel_age,tmp)
# p_capa <- mcf_plot(subset(mcf_1Tdelmodel_age),50,'1T_delmodel')
# 
# # 11. 不同RAID的MCF
# raid_need <- sort(table(data.mcf_RAID$raid))
# raid_need <- names(raid_need)
# raid_need_mix <- raid_need[c(12,13)]
# raid_need_single <- raid_need[c(14,7,15)]
# 
# # mix raid
# mcf_raid_mix_age <- mcf_all_age
# for (i in 1:length(raid_need_mix)) {
#   tmp <- mcf_age(subset(data.mcf_RAID,raid == raid_need_mix[i]),ti)
#   tmp$class <- raid_need_mix[i]
#   tmp <- subset(tmp,atrisk > 50)
#   mcf_raid_mix_age <- rbind(mcf_raid_mix_age,tmp)
# }
# p_raid_mix <- mcf_plot(subset(mcf_raid_mix_age),-1,'raid_mix')
# 
# # single raid
# mcf_raid_single_age <- mcf_all_age
# for (i in 1:length(raid_need_single)) {
#   tmp <- mcf_age(subset(data.mcf_RAID,raid == raid_need_single[i]),ti)
#   tmp$class <- raid_need_single[i]
#   tmp <- subset(tmp,atrisk > 50)
#   mcf_raid_single_age <- rbind(mcf_raid_single_age,tmp)
#   if (raid_need_single[i] == 'RAID1'){
#     tmp$mcf <- tmp$mcf*5
#   }
# }
# p_raid_single <- mcf_plot(subset(mcf_raid_single_age),-1,'raid_single')
# 
# # 12. 各种RAID类型的硬盘数量统计
# # data.mcf_RAID_diskc 为data.mcf_RAID与disk_ip的diskc的交集,为data.mcf_raid添加diskc信息
# data.mcf_RAID_diskc <- merge(data.mcf_RAID,disk_ip_cn[,c('ip','total','disk_c')],by = 'ip')
# data.mcf_RAID_diskc <- data.mcf_RAID_diskc[!duplicated(data.mcf_RAID_diskc$ip),]
# raid_need <- sort(table(data.mcf_RAID_diskc$raid))
# raid_need <- raid_need[raid_need > 0]
# sort(table(data.mcf_RAID_diskc$disk_c[data.mcf_RAID_diskc$raid == 'RAID5']))
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
# # 查看RAID1的容量是否与真实容量一致
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
# # 查看机型与Raid的关系
# sort(table(factor(cmdb$raid[cmdb$dev_class_name == 'A5'])))
# sort(table(factor(cmdb$dev_class_id[cmdb$raid == 'RAID0'])))
# 
# # 查看A5机器各类RAID的容量关系
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
# # 取一些机器的数据
# ip_need <- c('10.157.13.15','10.179.10.232','172.27.180.200',
#              '10.180.1.45','10.194.1.10')
# data.ip_need <- subset(disk_ip_cn,ip %in% ip_need,c('ip','total','disk_model','disk_model_c'))
# data.ip_need1 <- subset(data.disk_usage,ip %in% ip_need)
# data.ip_need2 <- merge(data.ip_need1,cmdb[,c('ip','raid')])
