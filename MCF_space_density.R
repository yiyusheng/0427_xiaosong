# 空间与服务器密度分析
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

# 1. 给config添加f_time,城市标记,机房标记,机架标记,刀架标记
# 1.1. 城市解析
# col_need <- c('svr_id','svr_asset_id','svr_sn','dev_class_id','type_name','model_name','ip',
#               'dept_id','bs1','bs2','idc_parent_name','idc_name','rack_name','pos_id','pos_name',
#               'use_time','last_update','raid','svr_version','operator')
# cmdb_sd <- cmdb[,col_need]
# cmdb_sd$idcp_type <- factor(gsub('[^A-Z]','',cmdb_sd$idc_parent_name))
# cmdb_sd$idcp_city <- factor(gsub('[A-Z]','',cmdb_sd$idc_parent_name,))
# cmdb_sd$idcp_name <- sapply(as.character(cmdb_sd$idcp_city),
#                             function(x){
#                               substring(x,3,nchar(x))
#                             })
# cmdb_sd$idcp_name <- factor(cmdb_sd$idcp_name)
# cmdb_sd$idcp_city <- factor(substr(cmdb_sd$idcp_city,1,2))
# 
# # 1.2. 刀架标记
# tmp <- strsplit(as.character(cmdb_sd$svr_asset_id),'-')
# cmdb_sd$blade_name <- factor(sapply(tmp,function(x)x[1]))
# cmdb_sd$blade_pos <- factor(sapply(tmp,function(x)x[2]))
# 
# # 1.3. 因为不同的机房可能有同样的rack_name,所以要把idc_name和rack_name结合作为rack_name
# cmdb_sd$rack_name <- factor(paste(cmdb_sd$idc_name,cmdb_sd$rack_name,sep='_'))
# 
# # 1.4. 添加标记
# data.config_sd <- merge(data.config,cmdb_sd[c('ip','model_name','dept_id','bs1',
#                                               'svr_version','idcp_city','idc_name','rack_name',
#                                               'blade_name','blade_pos','type_name')],
#                         'ip',all.x = T)
# save(cmdb_sd,data.config_sd,file = file.path(dir_data,'MCF_sd.Rda'))
load(file.path(dir_data,'MCF_sd.Rda'))

# 1.5. 处理之前对时间进行过滤
dev_need <- c('TS4','TS6',
              'C1','X2',
              'A1','A5',
              'B5','B6')
cmdb_sd <- subset(cmdb_sd,use_time > as.POSIXct('2010-01-01') & dev_class_id %in% dev_need)
data.config_sd <- subset(data.config_sd,use_time > as.POSIXct('2010-01-01') & dev_class_id %in% dev_need)


# 2. 城市对存储系统故障的影响(Q12A)
item <- 'idcp_city'
class_suffix <- ''
title <- 'Q12_City'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config_sd,!is.na(%s))',item)))
# item_need <- c('深圳','上海','天津','广州','东莞','成都',
#                '杭州','西安','济南','南京','汕头','香港')
item_need <- c('深圳','上海','天津','广州','成都',
               '西安')

mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q12 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q12,time_need,title,frac_max)',item)))

# 3. 不同type_name对存储系统故障的影响(Q13)
item <- 'type_name'
class_suffix <- ''
title <- 'Q13_box'
config_item <- subset(data.config_sd,!is.na(type_name))
stand_class <- 'baseline'
item_need <- c('TwinsChild','Server','2U4SChild')
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_Q13<- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_Q13,time_need,title,frac_max)',item)))

# 4. 验证各机型配置的硬盘数与smart对应的硬盘数的差异.并取部分主要机型进行处理
dev_config <- read.csv(file.path(dir_data,'各机型配置.csv'))
names(dev_config) <- c('dev_class_id','cpu_core','memory','total',
                       'disk_c','capacity','interface','raid','ssd_c','capacity_sdd')
dev_config <- subset(dev_config,,c('dev_class_id','disk_c','total','capacity'))
t <- subset(data.config_sd,dup == T)
t$dev_class_id <- factor(t$dev_class_id)
t1 <- subset(t,!is.na(t$disk_c))

# 4.1. 每一项分别为: 机型,机型机器总数,机型有硬盘信息总数,有硬盘信息的硬盘数中占比最高的硬盘数,
# 该硬盘数的机器数量
dev_sta <- data.frame(dev_class_id = levels(t$dev_class_id),
                      count = tapply(t$disk_c,t$dev_class_id,length),
                      disk_info_c = tapply(t$disk_c,t$dev_class_id,function(x)sum(!is.na(x))),
                      max_cat_c = tapply(t1$disk_c,t1$dev_class_id,function(x){
                        y <- sort(table(factor(x)),decreasing = T)
                        as.numeric(y[1])
                      }),
                      category = tapply(t1$disk_c,t1$dev_class_id,function(x)sum(!duplicated(x))),
                      disk_c = tapply(t1$disk_c,t1$dev_class_id,function(x){
                        y <- sort(table(factor(x)),decreasing = T)
                        as.numeric(names(y)[1])
                      })
)
dev_sta$cover_rate <- dev_sta$max_cat_c/dev_sta$count
dev_sta <- dev_sta[order(dev_sta$cover_rate),]
dev_sta <- subset(dev_sta,count >= 100)
dev_merge <- merge(dev_sta,dev_config,by = 'dev_class_id',all.x = T)
dev_merge <- dev_merge[order(dev_merge$cover_rate),]
# 4.2. 确定处理机型
dev_need <- c('TS3','TS4','TS5','TS6',
              'C1','X2',
              'A1','A5',
              'B1','B5','B6')
dev_info <- subset(dev_config, 
                   dev_class_id %in% dev_need & 
                     !(dev_class_id == 'TS6' & capacity == 300))
# 4.3. 给确定机型添加硬盘数
data.config_sd_dev <- subset(data.config_sd,dev_class_id %in% dev_need)
data.config_sd_dev$disk_cNew <- dev_info$disk_c[match(data.config_sd_dev$dev_class_id,dev_info$dev_class_id)]
data.config_sd_dev$totalNew <- dev_info$total[match(data.config_sd_dev$dev_class_id,dev_info$dev_class_id)]


# 5. 机柜数据分析 
ori <- '1970-01-01'
tmp <- data.config_sd_dev
tmp$ol_time[duplicated(tmp$ip)] <- -1
names(tmp)[names(tmp) == 'rack_name'] <- 't'
tmp$t <- factor(tmp$t)

# 5.1 每个机柜数据汇总
tmp1 <- data.frame(name = levels(tmp$t),
                   max = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,max)),origin = ori),
                   min = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,min)),origin = ori),
                   mean = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,mean)),origin = ori),
                   var = as.numeric(tapply(tmp$use_time,tmp$t,var)),
                   count = as.numeric(tapply(tmp$ip,tmp$t,function(x)sum(!duplicated(x)))),
                   fcount = as.numeric(tapply(tmp$ol_time_fail,tmp$t,function(x)sum(x!=-1))),
                   mean_ol_time = round(as.numeric(tapply(tmp$ol_time,tmp$t,function(x)mean(subset(x,x != -1))))),
                   mean_ol_timef = round(as.numeric(tapply(tmp$ol_time_fail,tmp$t,function(x)mean(subset(x,x != -1))))),
                   
                   disk_c = as.numeric(tapply(tmp$disk_cNew,tmp$t,sum)),
                   total = as.numeric(tapply(tmp$totalNew,tmp$t,sum)),
                   devc = as.numeric(tapply(tmp$dev_class_id,tmp$t,function(x)length(unique(x)))),
                   modelc = as.numeric(tapply(tmp$model_name,tmp$t,function(x)length(unique(x)))),
                   svrvc = as.numeric(tapply(tmp$svr_version,tmp$t,function(x)length(unique(x)))),
                   raidc = as.numeric(tapply(tmp$raid,tmp$t,function(x)length(unique(x)))),
                   bsc = as.numeric(tapply(tmp$bs1,tmp$t,function(x)length(unique(x)))),
                   
                   dev = tapply(tmp$dev_class_id,tmp$t,function(x)paste(names(table(factor(x))),collapse = '_')),
                   dev_rate = tapply(tmp$dev_class_id,tmp$t,function(x)paste(as.numeric(table(factor(x))),collapse = '_'))
)
tmp1$afr <- tmp1$fcount/(tmp1$count*tmp1$mean_ol_time/365)
tmp1$count <- factor(tmp1$count)
tmp1$disk_c <- factor(tmp1$disk_c)
row.names(tmp1) <- NULL
rack_sta <- tmp1

ggplot(subset(rack_sta,fcount>0)) + geom_point(aes(x =mean_ol_time ,y = disk_C, 
                                                   alpha = fcount, size = fcount))

# 5.2 不同机器数的机柜数据汇总
rack_densitym <- data.frame(density = as.numeric(levels(tmp1$count)),
                            count = as.numeric(tapply(tmp1$count,tmp1$count,length)),
                            fcount = as.numeric(tapply(tmp1$fcount,tmp1$count,sum)),
                            frackc = as.numeric(tapply(tmp1$fcount,tmp1$count,function(x)sum(x!=0))),
                            mean_afr = as.numeric(tapply(tmp1$afr,tmp1$count,mean)),
                            mean_ol_time = as.numeric(tapply(tmp1$mean_ol_time,tmp1$count,mean)),
                            mean_ol_timef = as.numeric(tapply(tmp1$mean_ol_timef,tmp1$count,
                                                              function(x)mean(subset(x,x != -1)))),
                            sem_afr = as.numeric(tapply(tmp1$afr,tmp1$count,function(x){
                              sqrt(var(x)/length(x))
                            }))
)
rack_densitym$up <- rack_densitym$mean_afr + rack_densitym$sem_afr*qnorm(0.025)
rack_densitym$down <- rack_densitym$mean_afr + rack_densitym$sem_afr*qnorm(0.975)
rack_densitym <- subset(rack_densitym,count >= 7)
row.names(rack_densitym) <- NULL

limits <- aes(ymax = up, ymin = down)
p <- ggplot(rack_densitym, aes(x = density, y = mean_afr))
p + geom_bar(stat = 'identity') + geom_errorbar(limits,width=0.25)

# 5.3 不同硬盘数的机柜数据汇总
rack_densityd <- data.frame(density = as.numeric(levels(tmp1$disk_c)),
                            count = as.numeric(tapply(tmp1$disk_c,tmp1$disk_c,length)),
                            fcount = as.numeric(tapply(tmp1$fcount,tmp1$disk_c,sum)),
                            frackc = as.numeric(tapply(tmp1$fcount,tmp1$disk_c,function(x)sum(x!=0))),
                            mean_afr = as.numeric(tapply(tmp1$afr,tmp1$disk_c,mean)),
                            mean_ol_time = as.numeric(tapply(tmp1$mean_ol_time,tmp1$disk_c,mean)),
                            mean_ol_timef = as.numeric(tapply(tmp1$mean_ol_timef,tmp1$disk_c,
                                                              function(x)mean(subset(x,x != -1)))),
                            sem_afr = as.numeric(tapply(tmp1$afr,tmp1$disk_c,function(x){
                              sqrt(var(x)/length(x))
                            }))
)
rack_densityd$up <- rack_densityd$mean_afr + rack_densityd$sem_afr*qnorm(0.025)
rack_densityd$down <- rack_densityd$mean_afr + rack_densityd$sem_afr*qnorm(0.975)
# rack_densityd <- subset(rack_densityd,disk_c >= 7)
row.names(rack_densityd) <- NULL

limits <- aes(ymax = up, ymin = down)
p <- ggplot(rack_densityd, aes(x = density, y = mean_afr)) + geom_bar(stat = 'identity')
p + geom_errorbar(limits,width=0.25)

# 5.4 分析机器密度为12,20,40三个类型机柜的故障率差异
rd_need <- subset(rack_densitym, density %in% c(12,20,40))
rackA <- subset(rack_sta,count == 12)
rackB <- subset(rack_sta,count == 20)
rackC <- subset(rack_sta,count == 40)

# 5.5 硬盘密度与机器密度分布图
tmp <- data.frame(count = as.numeric(levels(rack_sta$count)[rack_sta$count]),
                  disk_c = as.numeric(rack_sta$disk_c))
tmp$cdc <- paste(tmp$count,tmp$disk_c,sep='_')

tmp1 <- table(tmp$cdc)
tmp1 <- data.frame(type = names(tmp1),c = as.numeric(tmp1))
tmp1$type <- as.character(tmp1$type)
names(tmp1) <- c('type','c')
tmp2 <- data.frame(matrix(unlist(strsplit(tmp1$type,'_')),nrow = nrow(tmp1),byrow = T))
tmp1$count <- as.numeric(levels(tmp2$X1)[tmp2$X1])
tmp1$disk_c <- as.numeric(levels(tmp2$X2)[tmp2$X2])

ggplot(tmp1,aes(x = count,y = disk_c,size = c)) + geom_point()

# 6. 业务在不同的位置的机器数
item_need <- c('深圳','上海','天津','广州','东莞','成都',
               '杭州','西安','济南','南京','汕头','香港')
tmp <- subset(cmdb_sd,idcp_city %in% item_need)
tmp$idcp_city <- factor(tmp$idcp_city)
tmp$bs1 <- factor(tmp$bs1)
tmp1 <- tapply(tmp$idcp_city,tmp$bs1,function(x)as.numeric(table(x)))
tmp2 <- data.frame(matrix(unlist(tmp1),nrow=length(tmp1),byrow = T))
row.names(tmp2) <- levels(tmp$bs1)
names(tmp2) <- sort(item_need)
tmp2$sum <- rowSums(tmp2)
tmp2 <- tmp2[order(tmp2$sum,decreasing = T),]
tmp2$not0 <- sapply(1:nrow(tmp2),function(x){
  sum(tmp2[x,1:12] != 0)
})
tmp3 <- subset(tmp2,not0 >= 8)

# 7. 各城市空气质量与afr对比
date_seq <- seq.Date(as.Date('2010-01-01'),as.Date('2013-01-01'),'4 years')
item_need <- c('深圳','上海','天津','广州','成都','杭州','西安','济南')
require('reshape2')

# # 7.1. 各城市每个季节的空气质量数据
# tmp <- read.csv(file.path(dir_data,'air','aqi.csv'),header = F,encoding = 'UTF-8',sep = ',')
# names(tmp) <- c('id','zip','city','value','attr','time')
# aqi <- subset(tmp,city %in% item_need)
# aqi$city <- factor(aqi$city)
# aqi$attr <- factor(aqi$attr)
# aqi$time <- as.Date(aqi$time)
# 
# air_city <- sapply(item_need,function(x){
#   tmp <- 0
#   for (i in 1:(length(date_seq)-1)){
#     tmp[i] <- mean(aqi$value[aqi$city == x & 
#                             aqi$time >= date_seq[i] & 
#                             aqi$time <date_seq[i+1]])
#   }
#   print(x)
#   tmp
# })
# air_city <- data.frame(matrix(unlist(air_city),nrow = nrow(air_city),byrow = T))
# names(air_city) <- item_need
# air_city$time <- date_seq[1:(length(date_seq)-1)]
# air_city <- setNames(melt(air_city,id.vars = 'time'),c('time','city','value'))
# 
# # 7.2. 求每个城市每季度的故障率.
# afr_city <- sapply(item_need,function(x){
#   d <- sapply(date_seq,function(y){
#     list(subset(data.config_sd,idcp_city == x & 
#                   as.Date(use_time) <= y,c('ip','use_time','f_time','dup')))
#   })
#   f <- 0
#   t <- 0
#   for (i in 1:(length(date_seq)-1)){
#     f[i] <- nrow(subset(d[[i]],as.Date(f_time) >= date_seq[i] & 
#                           as.Date(f_time) <date_seq[i+1]))
#     t[i] <- sum(d[[i]]$dup)
#   }
#   print(x)
#   ret <- f/t
# })
# afr_city <- data.frame(matrix(unlist(afr_city),nrow = nrow(afr_city),byrow = T))
# names(afr_city) <- item_need
# afr_city$time <- date_seq[1:(length(date_seq)-1)]
# afr_city <- setNames(melt(afr_city,id.vars = 'time'),c('time','city','value'))
# 
# # 7.3 merge and plot
# airCity <- air_city
# airCity$afr <- afr_city$value
# airCity <- airCity[order(airCity$city,airCity$value),]
# ggplot(airCity,aes(x = afr, y = value, color = city)) + geom_point()
# ggplot(subset(airCity,city == '深圳'),aes(x = afr, y = value)) + geom_line()

# 7.4 另一份空气质量图 (Q12B)
pm10 <- read.csv(file.path(dir_data,'air','pm10.csv'))
item_need <- c('深圳','上海','天津','广州','成都','西安')
# pm10$Mean <- pm10$Mean_3year
pm10 <- within(pm10,city <- factor(city,levels = city[order(Mean_3year)]))
p_Q12 <- ggplot(subset(pm10,city %in% item_need),aes(x = city, y = Mean)) + 
  geom_bar(stat = 'identity') + 
  ylab('Mean_API')
ggsave(file=file.path(dir_data,'output','mcf',paste('Q12B','mcf.png',sep='_')), plot=p_Q12, width = 12, height = 9, dpi = 100)

