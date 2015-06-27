#@@@ �¼ܻ�������ͳ��,ͳ��ĳ��ʱ�����ϼܵĻ������¼ܻ����ı���.���ڹ����¼ܻ�������
rm(list = ls())
library(ggplot2)
library(scales)
dir_data <- 'D:/Data/Disk Number'
load(file.path(dir_data,'disk_number_label.Rda'))
data.offline <- read.csv(file.path(dir_data,'���ۻ�������.csv'))
names(data.offline) <- c('svr_asset_id','info','dev_class_name','hardware','use_time','model_name')

# 1. ����ʱ��,ɾ��252̨����
data.offline <- subset(data.offline,use_time != '1900/1/1 0:00:00' & use_time !='')
data.offline$use_time <- as.POSIXct(data.offline$use_time,tz = 'UTC')
data.offline <- data.offline[order(data.offline$use_time),]

# 2. ɾ��svr_id�ظ���,ɾ��133̨����
data.offline <- data.offline[!duplicated(data.offline$svr_asset_id),]
cmdb <- cmdb[!duplicated(cmdb$svr_asset_id),]

# 3. �ϲ�����
col_need <- c('use_time','svr_asset_id','state','dev_class_name','model_name')
cmdb$state <- 'online'
data.offline$state <- 'offline'
dup.svr_id <- intersect(cmdb$svr_asset_id,data.offline$svr_asset_id)
data.offline <- subset(data.offline,!(svr_asset_id %in% dup.svr_id))
data.plot <- rbind(cmdb[,col_need],data.offline[,col_need])

# 4. ��ͼ
data.plot$use_time_month <- 
num.online <- tapply(data.plot$state,data.plot$use_time,function(x)sum(x == 'online'))
num.offline <- tapply(data.plot$state,data.plot$use_time,function(x)sum(x == 'offline'))
data.plot1 <- data.frame(use_time = as.POSIXct(names(num.online),tz = 'UTC'),
                         online = num.online, 
                         offline = num.offline)
row.names(data.plot1) <- NULL
data.plot1$rate <- data.plot1$offline/(data.plot1$online + data.plot1$offline)

ggplot(data.plot1,aes(x = as.Date(use_time),y = rate)) + geom_line() +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month")

ggplot(data.plot,aes(x = as.Date(use_time),fill = state)) + geom_bar() +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month")

ggplot(data.plot1,aes(x = use_time, y = online)) + geom_line(color = 'red') +
  geom_line(data = data.plot1,aes(x = use_time, y = offline),color = 'blue')