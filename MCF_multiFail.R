# ��ι���֮��Ĺ����Է���
rm(list = ls())
require(ggplot2)
source('MCF_function.R')

#@@@ PARAMETERS
dir_data <- 'D:/Data/Disk Number'

#@@@ LOAD DATA
# load(file.path(dir_data,'disk_number_label.Rda'))
# load(file.path(dir_data,'mcf_all_age.Rda'))
load(file = file.path(dir_data,'MCF_sd.Rda'))

#1. �����,����,�����϶�ι��ϵķֲ�,Ҫ�е�һ�ι���ʱ��,MTBF
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
#1.1. ���ɷ���ʱ��
len <- nrow(server.ftime)
server.ftime$ftime_ol <- sapply(1:nrow(server.ftime),function(x){
  as.numeric(server.ftime$f_time[[x]] - server.ftime$use_time[x])
  })
#1.2. ����MTBF
server.ftime$mtbf <- sapply(1:nrow(server.ftime),function(x){
  tmp1 <- server.ftime$ftime_ol[[x]]
  tmp1[2:length(tmp1)] - tmp1[1:(length(tmp1)-1)]
})
#1.3. �ж��Ƿ��ж�ι���
server.ftime$mtbfc <- sapply(1:nrow(server.ftime),function(x){
  tmp2 <- server.ftime$mtbf[[x]]
  !is.na(tmp2[1])
})
# 1.4. ��һ�ι���ʱ����ʱ��
server.ftime$first_fol <- sapply(1:len,function(x){
  server.ftime$ftime_ol[[x]][1]
})
# 1.5. ���ϴ���
server.ftime$count <- sapply(1:len,function(x){
  length(server.ftime$ftime[[x]])
})

# 2.1. ��ȡ��ι��ϻ���������
server.ftimem <- subset(server.ftime,mtbfc == T)
lenm <- nrow(server.ftimem)
# 2.2. ���ϴ���
server.ftimem$count <- sapply(1:lenm,function(x){
  length(server.ftimem$mtbf[[x]]) + 1
})
# 2.3. ƽ��mtbf
server.ftimem$mtbf_mean <- server.ftimem$mtbf
server.ftimem$mtbf_mean <- sapply(1:lenm,function(x){
  mean(server.ftimem$mtbf[[x]])
})
# 2.4. mtbf����
server.ftimem$mtbf_var <- sapply(1:lenm,function(x){
  var(server.ftimem$mtbf[[x]])
})
server.ftimem$mtbf_var[is.na(server.ftimem$mtbf_var)] <- 0

#3. ���ͱ��
data.config_sd$class[data.config_sd$dev_class_id %in% c('TS4','TS6')] <- 'TS'
data.config_sd$class[data.config_sd$dev_class_id %in% c('B5','B6')] <- 'B'
data.config_sd$class[data.config_sd$dev_class_id %in% c('A1','A5')] <- 'A'
data.config_sd$class[data.config_sd$dev_class_id %in% c('C1','X2')] <- 'C'
server.ftimem$class <- data.config_sd$class[match(server.ftimem$name,
                                                  data.config_sd$ip)]
# 4. ��ͼ

# ��ͼ: ���ϴ�����ƽ��mtbf
ggplot(server.ftimem,aes(x = count,y = mtbf_mean,color = class)) + geom_point()
# ��ͼ: ��һ�ι���ʱ����ʱ����ƽ��mtbf
ggplot(server.ftimem,aes(x = first_fol,y = mtbf_mean,color = class)) + geom_point()
# ��ͼ: ��һ�ι���ʱ����ʱ������ϴ���
ggplot(server.ftimem,aes(x = first_fol,y = count,color = class)) + geom_point()
# ��ͼ: �����͹��ϴ���
ggplot(data.config_sd,aes(x = factor(dev_class_id),fill = (ol_time_fail != -1))) + geom_histogram()
# ��ͼ: ��һ�ι���ʱ����ʱ��ֲ�
ggplot(server.ftime,aes(x = round(first_fol))) + geom_histogram(binwidth = 30)
# ��ͼ: ��ι��ϻ����Ļ��̴���,����ʱƽ������ʱ��,����ʱ����ʱ�䷽��
ggplot(server.ftimem,aes(x = mtbf_mean, y = mtbf_var, color = count)) + geom_point()