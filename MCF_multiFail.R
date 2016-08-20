# 多次故障之间的关联性分析
rm(list = ls())
require(ggplot2)
source('MCF_function.R')

#@@@ PARAMETERS
dir_data <- 'D:/Data/Disk Number'

#@@@ LOAD DATA
# load(file.path(dir_data,'disk_number_label.Rda'))
# load(file.path(dir_data,'mcf_all_age.Rda'))
load(file = file.path(dir_data,'MCF_sd.Rda'))

#1. 求机器,刀架,机柜上多次故障的分布,要有第一次故障时间,MTBF
# a <- tapply(tmp$ip,tmp$ip,length)
# b <- tapply(tmp$rack_name,tmp$rack_name,length)
# c <- tapply(tmp$blade_name,tmp$blade_name,length)
tmp <- subset(data.config_sd,ol_time_fail != -1)
server.ftime <- tapply(tmp$f_time,factor(tmp$ip),function(x)sort(x))
server.ftime <- data.frame(name = names(server.ftime),
                           f_time = matrix(server.ftime,
                                           nrow = length(server.ftime)))
server.ftime$ol_time <- data.config_sd$ol_time[match(server.ftime$name,data.config_sd$ip)]
server.ftime$use_time <- tmp$use_time[match(server.ftime$name,tmp$ip)]
#1.1. 生成服役时间
len <- nrow(server.ftime)
server.ftime$ftime_ol <- sapply(1:nrow(server.ftime),function(x){
  as.numeric(server.ftime$f_time[[x]] - server.ftime$use_time[x])
  })
#1.2. 生成MTBF
server.ftime$mtbf <- sapply(1:nrow(server.ftime),function(x){
  tmp1 <- server.ftime$ftime_ol[[x]]
  tmp1[2:length(tmp1)] - tmp1[1:(length(tmp1)-1)]
})
#1.3. 判断是否有多次故障
server.ftime$mtbfc <- sapply(1:nrow(server.ftime),function(x){
  tmp2 <- server.ftime$mtbf[[x]]
  !is.na(tmp2[1])
})
# 1.4. 第一次故障时服役时间
server.ftime$first_fol <- sapply(1:len,function(x){
  server.ftime$ftime_ol[[x]][1]
})
# 1.5. 故障次数
server.ftime$count <- sapply(1:len,function(x){
  length(server.ftime$ftime[[x]])
})

# 2.1. 提取多次故障机器的数据
server.ftimem <- subset(server.ftime,mtbfc == T)
lenm <- nrow(server.ftimem)
# 2.2. 故障次数
server.ftimem$count <- sapply(1:lenm,function(x){
  length(server.ftimem$mtbf[[x]]) + 1
})
# 2.3. 平均mtbf
server.ftimem$mtbf_mean <- server.ftimem$mtbf
server.ftimem$mtbf_mean <- sapply(1:lenm,function(x){
  mean(server.ftimem$mtbf[[x]])
})
# 2.4. mtbf方差
server.ftimem$mtbf_var <- sapply(1:lenm,function(x){
  var(server.ftimem$mtbf[[x]])
})
server.ftimem$mtbf_var[is.na(server.ftimem$mtbf_var)] <- 0

#3. 机型标记
data.config_sd$class[data.config_sd$dev_class_id %in% c('TS4','TS6')] <- 'TS'
data.config_sd$class[data.config_sd$dev_class_id %in% c('B5','B6')] <- 'B'
data.config_sd$class[data.config_sd$dev_class_id %in% c('A1','A5')] <- 'A'
data.config_sd$class[data.config_sd$dev_class_id %in% c('C1','X2')] <- 'C'
server.ftimem$class <- data.config_sd$class[match(server.ftimem$name,
                                                  data.config_sd$ip)]
# 4. 作图

# 作图: 故障次数与平均mtbf
ggplot(server.ftimem,aes(x = count,y = mtbf_mean,color = class)) + geom_point()
# 作图: 第一次故障时服役时间与平均mtbf
ggplot(server.ftimem,aes(x = first_fol,y = mtbf_mean,color = class)) + geom_point()
# 作图: 第一次故障时服役时间与故障次数
ggplot(server.ftimem,aes(x = first_fol,y = count,color = class)) + geom_point()
# 作图: 各机型故障次数
ggplot(data.config_sd,aes(x = factor(dev_class_id),fill = (ol_time_fail != -1))) + geom_histogram()
# 作图: 第一次故障时服役时间分布
ggplot(server.ftime,aes(x = round(first_fol))) + geom_histogram(binwidth = 30)
# 作图: 多次故障机器的换盘次数,换盘时平均服役时间,换盘时服役时间方差
ggplot(server.ftimem,aes(x = mtbf_mean, y = mtbf_var, color = count)) + geom_point()
