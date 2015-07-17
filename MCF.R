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
# cmdb <- subset(cmdb,use_time > as.POSIXct('2010-01-01'))
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
# cmdb_nomodel <- subset(cmdb_nomodel, ip != '')
# cmdb_nomodel$disc_c <- cmdb_nomodel$disk_c*2
# cmdb_nomodel$head_c <- cmdb_nomodel$disc_c*2
# cmdb_nomodel$disk_model <- 'NOMODEL'
# cmdb_nomodel$disk_model_c <- '0'
# cmdb_nomodel$disk_model_c1 <- 1
# cmdb_nomodel$disk_inter <- 'NOINTER'
# disk_ip_cn <- rbind(disk_ip,cmdb_nomodel)
# disk_ip_cn$ip <- factor(disk_ip_cn$ip)
# B1 B5 B6的数据应该是错的,所以不再用这部分数据
disk_ip_cn <- disk_ip

# 1 生成数据集&baseline
cmdb <- cmdb[!duplicated(cmdb$ip),]

# 1.1. 确定处理机型
dev_config <- read.csv(file.path(dir_data,'各机型配置.csv'))
names(dev_config) <- c('dev_class_id','cpu_core','memory','total',
                       'disk_c','capacity','interface','raid','ssd_c','capacity_sdd')
dev_config <- subset(dev_config,,c('dev_class_id','disk_c','total','capacity','interface'))
dev_need <- c('TS4','TS5','TS6',
              'C1',
              'A1','A5',
              'B1','B5','B6')
dev_info <- subset(dev_config, 
                   dev_class_id %in% dev_need & 
                     !(dev_class_id == 'TS6' & capacity == 300))

# 1.2.1. 给确定机型添加硬盘数
cmdb_dev <- subset(cmdb,dev_class_id %in% dev_need)
cmdb_dev <- cmdb
cmdb_dev$disk_cNew <- dev_info$disk_c[match(cmdb_dev$dev_class_id,dev_info$dev_class_id)]
cmdb_dev$totalNew <- dev_info$total[match(cmdb_dev$dev_class_id,dev_info$dev_class_id)]
cmdb_dev$itfNew <- as.character(dev_info$interface[match(cmdb_dev$dev_class_id,dev_info$dev_class_id)])

# 1.2.2. 给某几个机型的数据根据时间进行修正.
cmdb_dev$itfNew[cmdb_dev$dev_class_id %in% dev_need[1:4]] <- 'SATA2'
cmdb_dev$itfNew[cmdb_dev$dev_class_id == 'C1' & 
                  cmdb_dev$use_time > as.POSIXct('2012-02-01')] <- 'SATA3'
cmdb_dev$itfNew[cmdb_dev$dev_class_id == 'TS4']<- 'SATA3'
cmdb_dev$itfNew[cmdb_dev$dev_class_id == 'TS4' & cmdb_dev$svr_version == '3.0.0' &
                  cmdb_dev$use_time > as.POSIXct('2010-08-01')] <- 'SATA2'
cmdb_dev$itfNew[cmdb_dev$dev_class_id == 'TS5' & 
                  cmdb_dev$use_time > as.POSIXct('2012-09-15')] <- 'SATA3'
cmdb_dev$itfNew[cmdb_dev$dev_class_id == 'TS6' & 
                  cmdb_dev$use_time > as.POSIXct('2012-01-01')] <- 'SATA3'
cmdb_dev$itfNew[cmdb_dev$dev_class_id == 'TS5' & 
                  cmdb_dev$use_time > as.POSIXct('2012-09-15')] <- 'SATA3'
cmdb_dev$itfNew[cmdb_dev$dev_class_id == 'TS5' & 
                  cmdb_dev$use_time > as.POSIXct('2012-09-15')] <- 'SATA3'
cmdb_dev$itfNew[cmdb_dev$dev_class_id == 'TS5' & 
                  cmdb_dev$use_time > as.POSIXct('2012-09-15')] <- 'SATA3'
cmdb_dev$itfNew <- factor(cmdb_dev$itfNew)

cmdb_dev$totalNew[cmdb_dev$dev_class_id == 'A5' & 
                    cmdb_dev$use_time > as.POSIXct('2011-12-03')] <- 300*12
cmdb_dev$totalNew[cmdb_dev$dev_class_id == 'TS5' & 
                    cmdb_dev$use_time > as.POSIXct('2011-09-15')] <- 2000*12
cmdb_dev$totalNew[cmdb_dev$dev_class_id == 'C1' & 
                    (cmdb_dev$svr_version == '3.0.1' | cmdb_dev$svr_version == '4.2.0')] <- 1000
cmdb_dev$totalNew[cmdb_dev$dev_class_id == 'C1' & 
                    cmdb_dev$svr_version == '2.0.0'] <- 250
                  
# 1.3. 结合disk_ip,cmdb_dev,data.mcf生成包含MCDINR六个维度数据的表
data.mcf$ol_time <- data.mcf$end - data.mcf$start
data.config <- merge(data.mcf[,c('ip','start','f_time','end','ol_time','ol_time_fail')],
                     cmdb_dev[,c('ip','use_time','raid','dev_class_id','disk_cNew',
                                 'totalNew','itfNew')],
                     by = 'ip')
data.config <- merge(data.config,
                     disk_ip[,c('ip','total','disk_c','disk_model','disk_cache',
                                'disk_inter','disk_model_c','disk_model_c1')],
                     by = 'ip',
                     all.x = T)

# 1.4. 生成新的接口数据
data.config$disk_model_c <- as.character(data.config$disk_model_c)
data.config$disk_inter <- as.character(data.config$disk_inter)
data.config$disk_inter[is.na(data.config$disk_inter)] <- 'NOINTER'
data.config$disk_model_c[is.na(data.config$disk_model_c)] <- '0'

# 添加分类的接口数据
# tmp <- mapply(function(x,y)tapply(as.numeric(unlist(strsplit(x,'_'))),unlist(strsplit(y,'_')),sum),
#               data.config$disk_model_c,data.config$disk_inter)
# tmp1 <- sapply(tmp,function(x){
#   if(length(x) == 1)paste(x[1],names(x)[1],sep='')
#   else paste(x[1],names(x)[1],'+',x[2],names(x)[2],sep='')
# })
# row.names(tmp1) <- NULL
# data.config$disk_inter_all <- tmp1
# data.config$disk_inter_all <- factor(data.config$disk_inter_all)

data.config$disk_inter <- factor(data.config$disk_inter)
data.config$disk_model_c <- factor(data.config$disk_model_c)
data.config$dup <- !duplicated(data.config$ip)
# save(data.config,dev_info,cmdb_dev,file = file.path(dir_data,'mcf_dataConfig.Rda'))

# 1.5. 生成MCF of all servers
# load(file.path(dir_data,'mcf_dataConfig.Rda'))
mcf_all_age <- mcf_age(data.config,ti)
mcf_all_age$class <- 'baseline'
tmp1 <- subset(data.config,dup == T)
mcf_all_age$classNew <- paste('baseline[',sum(tmp1$disk_cNew),
                              '/',nrow(subset(data.config,ol_time_fail!= -1)),']',sep='')
# png(file = file.path(dir_data,'output','mcf','fail_atrisk',paste('Baseline','fails_atrisk.png',sep = '_')),width = 1200,height = 900,units = 'px')
# g_plot <- line_plot_2yaxis(subset(mcf_all_age,,c('time','fails','atrisk')))
# grid.draw(g_plot)
# dev.off()
mcf_all_age <- mcf_sc(mcf_all_age,'baseline')
pset1 <- mcf_plot(mcf_all_age,time_need,'all_age',frac_max)
save(mcf_all_age,data.config,dev_info,cmdb_dev,file = file.path(dir_data,'mcf_all_age.Rda'))

# 1.5 用rr把数据分为三段.
# p6 <- pset1[[5]] + geom_line(data = mcf_all_age[c(2,31,43,70),],mapping = aes(time,rr),size = 1,color = 'blue') 
# ggsave(file=file.path(dir_data,'output','mcf',paste('all_rr_3phase.png',sep='_')), plot=p6, width = 12, height = 9, dpi = 100)
