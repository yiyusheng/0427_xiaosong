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
cmdb <- subset(cmdb,use_time > as.POSIXct('2010-01-01'))

# 1. 给config添加f_time,城市标记,机房标记,机架标记,刀架标记
# 1.1. 城市解析
col_need <- c('svr_id','svr_asset_id','svr_sn','dev_class_name','type_name','model_name','ip',
              'dept_id','bs1','bs2','idc_parent_name','idc_name','rack_name','pos_id','pos_name',
              'use_time','last_update','raid','svr_version','operator')
cmdb_sd <- cmdb[,col_need]
cmdb_sd$idcp_type <- factor(gsub('[^A-Z]','',cmdb_sd$idc_parent_name))
cmdb_sd$idcp_city <- factor(gsub('[A-Z]','',cmdb_sd$idc_parent_name,))
cmdb_sd$idcp_name <- sapply(as.character(cmdb_sd$idcp_city),
                             function(x){
                               substring(x,3,nchar(x))
                             })
cmdb_sd$idcp_name <- factor(cmdb_sd$idcp_name)
cmdb_sd$idcp_city <- factor(substr(cmdb_sd$idcp_city,1,2))

# 1.2. 刀架标记
tmp <- strsplit(as.character(cmdb_sd$svr_asset_id),'-')
cmdb_sd$blade_name <- factor(sapply(tmp,function(x)x[1]))
cmdb_sd$blade_pos <- factor(sapply(tmp,function(x)x[2]))

# 1.3. 因为不同的机房可能有同样的rack_name,所以要把idc_name和rack_name结合作为rack_name
cmdb_sd$rack_name <- factor(paste(cmdb_sd$idc_name,cmdb_sd$rack_name,sep='_'))

# 1.4. 添加标记
data.config_sd <- merge(data.config,cmdb_sd[c('ip','dev_class_name','model_name','idcp_city','idc_name','rack_name',
                                              'blade_name','blade_pos','type_name','use_time')],
                        'ip',all.x = T)

# 2. 城市对存储系统故障的影响(Q12)
item <- 'idcp_city'
class_suffix <- ''
title <- 'Q12_City'
stand_class <- 'baseline'
eval(parse(text = sprintf('config_item <- subset(data.config_sd,!is.na(%s))',item)))
item_need <- c('西安','广州','香港','天津','成都','上海','深圳')

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

# 4. 机房,机架,刀架机器上架时间统计(最大值,最小值,平均值,方差,数量,故障次数)
data.config_sd_ip <- data.config_sd[!duplicated(data.config_sd$ip),]
data.config_sd_ip$idc_name <- factor(data.config_sd_ip$idc_name)
data.config_sd_ip$rack_name <- factor(data.config_sd_ip$rack_name)
data.config_sd_ip$blade_name <- factor(data.config_sd_ip$blade_name)
ori <- '1970-01-01'

# #机房 
# tmp <- data.config_sd_ip
# names(tmp)[names(tmp) == 'idc_name'] <- 't'
# tmp$t <- factor(tmp$t)
# tmp1 <- data.frame(name = levels(tmp$t),
#                    max = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,max)),origin = ori),
#                    min = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,min)),origin = ori),
#                    mean = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,mean)),origin = ori),
#                    var = as.numeric(tapply(tmp$use_time,tmp$t,var)),
#                    count = as.numeric(tapply(tmp$use_time,tmp$t,length)),
# #                    diskc = as.numeric(tapply(tmp$disk_c,tmp$t,sum)),
#                    fcount = as.numeric(tapply(tmp$ol_time_fail,tmp$t,function(x)sum(x!=-1,rm.na = T))),
#                    mean_ol_time = as.numeric(tapply(tmp$ol_time,tmp$t,mean)),
#                    mean_ol_timef = as.numeric(tapply(tmp$ol_time_fail,tmp$t,function(x)mean(subset(x,x != -1)))),
#                    devc = as.numeric(tapply(tmp$dev_class_name,tmp$t,function(x)length(unique(x)))),
#                    modelc = as.numeric(tapply(tmp$model_name,tmp$t,function(x)length(unique(x)))))
# tmp1 <- tmp1[order(tmp1$count,decreasing = T),]
# row.names(tmp1) <- NULL
# idc_sta <- tmp1
# 
# ggplot(idc_sta) + geom_point(aes(x = count,y = mean_ol_time,size = fcount/count,colour = fcount/count))

#机柜 
tmp <- data.config_sd
tmp$ol_time[duplicated(tmp$ip)] <- -1
names(tmp)[names(tmp) == 'rack_name'] <- 't'
tmp$t <- factor(tmp$t)
tmp1 <- data.frame(name = levels(tmp$t),
                   max = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,max)),origin = ori),
                   min = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,min)),origin = ori),
                   mean = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,mean)),origin = ori),
                   var = as.numeric(tapply(tmp$use_time,tmp$t,var)),
                   count = as.numeric(tapply(tmp$ip,tmp$t,function(x)sum(!duplicated(x)))),
                   fcount = as.numeric(tapply(tmp$ol_time_fail,tmp$t,function(x)sum(x!=-1))),
                   mean_ol_time = round(as.numeric(tapply(tmp$ol_time,tmp$t,function(x)mean(subset(x,x != -1))))),
                   mean_ol_timef = round(as.numeric(tapply(tmp$ol_time_fail,tmp$t,function(x)mean(subset(x,x != -1))))),
                   devc = as.numeric(tapply(tmp$dev_class_name,tmp$t,function(x)length(unique(x)))),
                   modelc = as.numeric(tapply(tmp$model_name,tmp$t,function(x)length(unique(x)))))
tmp1$afr <- tmp1$fcount/(tmp1$count*tmp1$mean_ol_time/365)
tmp1 <- tmp1[order(tmp1$count,decreasing = T),]
row.names(tmp1) <- NULL
tmp1$count <- factor(tmp1$count)
# for each rack
rack_density <- data.frame(density = levels(tmp1$count),
                           count = as.numeric(tapply(tmp1$count,tmp1$count,length)),
                           mean_afr = as.numeric(tapply(tmp1$afr,tmp1$count,mean)),
                           sem_afr = as.numeric(tapply(tmp1$afr,tmp1$count,function(x){
                             sqrt(var(x)/length(x))
                             }))
                           )
rack_density$up <- rack_density$mean + rack_density$sem_afr*qnorm(0.025)
rack_density$down <- rack_density$mean + rack_density$sem_afr*qnorm(0.975)
rack_density <- subset(rack_density,count >= 10)
row.names(rack_density) <- NULL

ggplot(rack_density) + geom_bar(aes(x = density, y = mean_afr), stat = 'identity') + 
  geom_errorbar(aes(ymax = up, ymin = down))


rack_sta <- tmp1
ggplot(subset(rack_sta,fcount>0)) + geom_point(aes(x =mean_ol_time ,y =count, 
                                                   alpha = fcount, size = fcount))

# #刀箱 
# tmp <- data.config_sd_ip
# names(tmp)[names(tmp) == 'type_name'] <- 't'
# tmp$t <- factor(tmp$t)
# tmp1 <- data.frame(name = levels(tmp$t),
#                    max = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,max)),origin = ori),
#                    min = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,min)),origin = ori),
#                    mean = as.POSIXct(as.numeric(tapply(tmp$use_time,tmp$t,mean)),origin = ori),
#                    var = as.numeric(tapply(tmp$use_time,tmp$t,var)),
#                    count = as.numeric(tapply(tmp$use_time,tmp$t,length)),
#                    #                    diskc = as.numeric(tapply(tmp$disk_c,tmp$t,sum)),
#                    fcount = as.numeric(tapply(tmp$ol_time_fail,tmp$t,function(x)sum(x!=-1,rm.na = T))),
#                    mean_ol_time = as.numeric(tapply(tmp$ol_time,tmp$t,mean)),
#                    mean_ol_timef = as.numeric(tapply(tmp$ol_time_fail,tmp$t,function(x)mean(subset(x,x != -1)))),
#                    devc = as.numeric(tapply(tmp$dev_class_name,tmp$t,function(x)length(unique(x)))),
#                    modelc = as.numeric(tapply(tmp$model_name,tmp$t,function(x)length(unique(x)))))
# tmp1 <- tmp1[order(tmp1$count,decreasing = T),]
# row.names(tmp1) <- NULL
# blade_sta <- tmp1
# 
# ggplot(blade_sta) + geom_point(aes(x = fcount,y = mean_ol_time,size = count,colour = count))