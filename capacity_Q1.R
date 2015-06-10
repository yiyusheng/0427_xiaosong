#@@@�ش���������:
# 1. ͬ����Ӳ���ͺ�,�ڵ��̻��ϺͶ��̻��ϵĹ��ϱ����кβ��.
# 2. ���̻�����ͻ����ͷǻ���ͻ����Ĺ��ϱ������.
# 3. RAID��NORAID�Թ��ϵ�Ӱ��.������Բ��ù�Ӳ������.
# 4. ��ͬ���������ʵĻ����Ĺ����ʱ���.
# 5. ��ͬ�����Ļ����Ĺ����ʱ���.(250,500,1000,12000,24000)
# 6. Ҫȷ������ͻ���������Ϊ���ϲų��ֵ�.
rm(list = ls())
dir_data <- 'D:/Data/Disk Number'
load(file.path(dir_data,'disk_error_1m_1m.Rda'))
require(ggplot2)

#���ɷ���ʱ��
end_time <- as.POSIXct('2013-12-01',tz = 'UTC')
# 1. ��end_timeʱ�ķ���ʱ��
tmp <- end_time - disk_error$use_time
units(tmp) <- 'days'
disk_error$ol_time <- tmp
# 2. ����ʱ����ʱ��
disk_error$f_time[disk_error$f_time == as.POSIXct('1970-01-01 00:00:00',tz = 'UTC')] <- end_time
tmp <- disk_error$f_time - disk_error$use_time  
units(tmp) <- 'days'
disk_error$ol_time_fail <- tmp
# 3. ɾ��ol_time_failС��0�Ĺ��ϻ�
disk_error <- subset(disk_error,ol_time_fail >= 0)
disk_error$ol_time <- as.numeric(disk_error$ol_time)
disk_error$ol_time_fail <- as.numeric(disk_error$ol_time_fail)
# 4. �������ַ���ʱ���Ӧ���ºͼ���
disk_error$ol_time_month <- disk_error$ol_time/30
disk_error$ol_time_quanter <- disk_error$ol_time/(30*3)
disk_error$ol_time_fail_month <- disk_error$ol_time_fail/30
disk_error$ol_time_fail_quanter <- disk_error$ol_time_fail/(30*3)
# 5. �����޹��ϻ��Ĺ���ʱ����ʱ��Ϊ-1
disk_error$ol_time_fail_quanter[disk_error$biclass == 0] <- -1
disk_error$ol_time_fail_month[disk_error$biclass == 0] <- -1
# 6. ɾ��end_time��Ĺ���.��Ϊ�м��жϲ�,ֻ��ɾ��.
disk_error$biclass[disk_error$f_time > end_time] <- 0

# ѡ��disk model
model_need <- c('ST3250310NS',
                'ST3500514NS','ST500NM0011',
                'ST31000524NS','ST1000NM0011',
                'ST32000645NS','ST2000NM0011')
data.modelA1 <- subset(disk_error,disk_model == model_need[4] & disk_model_c == 1) 
data.modelA2 <- subset(disk_error,disk_model == model_need[4] & disk_model_c == 12)
data.modelB1 <- subset(disk_error,disk_model == model_need[5] & disk_model_c == 1)
data.modelB2 <- subset(disk_error,disk_model == model_need[5] & disk_model_c == 12)

################################################################################################
# ����ͳ��

ggplot(subset(disk_error,biclass == 1),aes(qtrf)) + geom_histogram()
ggplot(subset(data.modelA1,biclass == 1),aes(qtrf)) + geom_histogram()
ggplot(subset(data.modelA2,biclass == 1),aes(qtrf)) + geom_histogram()
ggplot(subset(data.modelB1,biclass == 1),aes(qtrf)) + geom_histogram()
ggplot(subset(data.modelB2,biclass == 1),aes(qtrf)) + geom_histogram()

# ����ͳ��
tableA1 <- table(factor(data.modelA1$disk_model_c))
tableA2 <- table(factor(data.modelA2$disk_model_c))
tableB1 <- table(factor(data.modelB1$disk_model_c))
tableB2 <- table(factor(data.modelB2$disk_model_c))

# �ϼ�ʱ��ͳ��
onlineA1 <- data.frame(table(factor(data.modelA1$qtr)))
onlineA2 <- data.frame(table(factor(data.modelA2$qtr)))
onlineB1 <- data.frame(table(factor(data.modelB1$qtr)))
onlineB2 <- data.frame(table(factor(data.modelB2$qtr)))
onlineA1$model <- model_need[4]
onlineA2$model <- model_need[4]
onlineB1$model <- model_need[5]
onlineB2$model <- model_need[5]
online <- rbind(onlineA1,onlineA2,onlineB1,onlineB2)
names(online) <- c('time','count','model')
online <- online[order(as.numeric(online$time)),]

# ���ϼ�����ͳ�Ʋ���ͼ
title <- 'Q1_���ϼ�����ͳ��'
geo_aes <- aes(x = time,y = count,group = model,color = model)
p <- ggplot(online,geo_aes) + geom_line(size = 1.5) + 
      geom_point( size = 4, shape = 21, fill = 'white') + 
      ggtitle(title) + xlab('time') + ylab('Count') +
  theme(text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=1)) 
ggsave(file=file.path(dir_data,'output','capacity_Q1',paste(title,'.png',sep = '')), 
                      plot=p, width = 12, height = 9, dpi = 100)

# ��model���̻��뵥�̻��ϼ�����ͳ�Ʋ���ͼ
geo_aes <- aes(x = qtr,fill = disk_model_c)
#A
title <- paste('Q1',model_need[4],'���̻��뵥�̻��ϼ�����ͳ��',sep = '_')
p <- ggplot(rbind(data.modelA1,data.modelA2),geo_aes) + geom_histogram() +
  ggtitle(title) + xlab('time') + ylab('Count') +
  theme(text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=1)) 
ggsave(file=file.path(dir_data,'output','capacity_Q1',paste(title,'.png',sep = '')), 
       plot=p, width = 12, height = 9, dpi = 100)
#B
title <- paste('Q1',model_need[5],'���̻��뵥�̻��ϼ�����ͳ��',sep = '_')
p <- ggplot(rbind(data.modelB1,data.modelB2),geo_aes) + geom_histogram() +
  ggtitle(title) + xlab('time') + ylab('Count') +
  theme(text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=1)) 
ggsave(file=file.path(dir_data,'output','capacity_Q1',paste(title,'.png',sep = '')), 
       plot=p, width = 12, height = 9, dpi = 100)

################################################################################################
#����
#@@@ online_time_plot: ���ڼ���ͬһʱ���ϼܵĻ�������ĳһ�ֶεĹ�������ʱ��ı仯���.
online_time_plot <- function(data,
                             attr,qtr,qtrf,class,
                             curr.qtr,count_attr,file_path,prefix) {
  # all data(ѡ���ϼ�ʱ��)
  if (curr.qtr == -1) {
    plot.data <- data
  }else {
    eval(parse(text = sprintf('plot.data <- subset(data,qtr == %s)',curr.qtr)))
  }
  eval(parse(text = sprintf('table.attr_all <- sort(table(plot.data$%s),decreasing = T)',attr)))
  # bad data
  eval(parse(text = sprintf('plot.data <- subset(plot.data,%s == 1)',class)))
  eval(parse(text = sprintf('table.attr <- sort(table(plot.data$%s),decreasing = T)',attr)))
  # select data
  sum.all <- sum(table.attr_all)
  sum.bad <- sum(table.attr)
  count_attr <- min(count_attr,length(table.attr))
  per <- sum(table.attr[1:count_attr])/sum(table.attr)
  per <- round(per*1000)/1000       # �������ݱ������İٷֱ�
  attr_need <- names(table.attr)[1:count_attr]
  eval(parse(text = sprintf('plot.data <- subset(plot.data, %s %%in%% names(table.attr)[1:%i])',
                            attr,count_attr)))
  eval(parse(text = sprintf('plot.data$%s <- factor(plot.data$%s)',qtrf,qtrf)))
  # calculate rate
  plot.line <- data.frame(id = character(),
                          value = numeric(),
                          color = character())
  for (k in 1:count_attr){
    # ĳ�ֶ�ĳֵ�����ʼ���(idΪʱ��,valueΪ������,colorΪĳ�ֶ�ֵ)
    eval(parse(text = sprintf('table.attr_each <- table(subset(plot.data, %s == names(table.attr)[k],%s))',attr,qtrf)))
    table.attr_each <- table.attr_each/table.attr_all[names(table.attr_all) == names(table.attr)[k]]
    len <- length(table.attr_each)
    plot.line <- rbind(plot.line,data.frame(id = names(table.attr_each),
                                            value = as.numeric(table.attr_each),
                                            color = rep(names(table.attr)[k],len)))
  }
  plot.line$id <- as.numeric(levels(plot.line$id)[plot.line$id])
  plot.line <- plot.line[order(plot.line$id),]
  
  # plot
  geo_aes <- aes(x = factor(id), y = value, group = factor(color), colour = factor(color))
  p <- ggplot(plot.line,geo_aes) + 
    geom_line(size = 1.5) +
    geom_point( size = 4, shape = 21, fill = 'white') +
    ggtitle(paste(prefix,attr,curr.qtr,sum.all,sum.bad,per,sep='_')) + 
    xlab('Time (Quanters)') + ylab('Percent')
  if (curr.qtr == -1){
    out_name <- paste(paste(prefix,attr,sum.all,sum.bad,per,sep='_'),'.png')
  } else {
    out_name <- paste(paste(prefix,attr,curr.qtr,sum.all,sum.bad,per,sep='_'),'.png')
  }
  ggsave(file=file.path(file_path,out_name), plot=p, width = 12, height = 9, dpi = 100)
  return(p)
}

#@@@ power_on_hours: ���ڼ���ͬһʱ���ϼܵĻ�������ĳһ�ֶε�AFR��ʱ��ı仯���.
power_on_hours <- function(data,
                             attr,qtr,qtrf,class,
                             curr.qtr,count_attr,file_path,prefix) {
  if (curr.qtr == -1) {
    plot.data <- data
  }else {
    eval(parse(text = sprintf('plot.data <- subset(data,qtr == %s)',curr.qtr)))
  }
  # seperate bad and good data
  eval(parse(text = sprintf('plot.data$olt_ceiling <- ceiling(plot.data$%s)',qtrf)))
  eval(parse(text = sprintf('data.bad <- subset(plot.data,%s == 1)',class)))
  eval(parse(text = sprintf('data.good <- subset(plot.data,%s == 0)',class)))
  # time to calculate
  uni.time <- data.frame(table(ceiling(data.bad$olt_ceiling)))
  names(uni.time) <- c('time','count')
  uni.time$time <- as.numeric(levels(uni.time$time)[uni.time$time])
  
  # calculate MTBF for bad
  mtbf.bad <- mapply(function(x){
    eval(parse(text = sprintf('sum(data.bad$%s[data.bad$olt_ceiling == x] - 
                              floor(data.bad$%s[data.bad$olt_ceiling == x]))',qtrf,qtrf)))
  },uni.time$time)
  
  count.bad <- mapply(function(x){
    eval(parse(text = sprintf('length(data.bad$%s[data.bad$olt_ceiling == x])',qtrf)))
  },uni.time$time)
  
  # calculate MTBF for good. only count is need because poh of each good disk is 1
  count.good <- mapply(function(x){
    eval(parse(text = sprintf('length(data.good$%s[data.good$olt_ceiling >= x] + 
                              length(data.bad$%s[data.bad$olt_ceiling >= x]))',qtrf,qtrf)))
  },uni.time$time)
  
  mtbf <- data.frame(time = uni.time$time,
                     mtbf_bad = mtbf.bad,
                     mtbf_good = count.good,
                     count_bad = count.bad,
                     count_good = count.good)
  mtbf$afr <- ((mtbf$mtbf_bad + mtbf$mtbf_good)*24*30*12/(mtbf$count_bad + mtbf$count_good))/1200000
  
  # plot
  ggplot(subset(mtbf,count_good > 100),aes(x = time,y = afr)) + geom_line()
}
  
#@@@ afr: ���������л�����AFR
afr_quanter <- function(data,range){
  if (length(range) == 1)  AFR <- data.frame(time = unique(ceiling(data$ol_time_fail_quanter)))
  else  AFR <- data.frame(time = range)
  AFR <- subset(AFR,time != -1)
  
  AFR$f <- mapply(function(x){
    sum(ceiling(data$ol_time_fail_quanter[data$biclass == 1]) == x)
  },AFR$time)
  AFR$n <- mapply(function(x){
    sum(ceiling(data$ol_time_quanter) >= x)
  },AFR$time)
  AFR$afr <- AFR$f/AFR$n 
  AFR <- AFR[order(AFR$time),]
  return(AFR)
}

afr_month <- function(data,range){
  if (length(range) == 1)  AFR <- data.frame(time = unique(ceiling(data$ol_time_fail_month)))
  else  AFR <- data.frame(time = range)
  AFR <- subset(AFR,time != -1)
  
  AFR$f <- mapply(function(x){
    sum(ceiling(data$ol_time_fail_month[data$biclass == 1]) == x)
  },AFR$time)
  AFR$n <- mapply(function(x){
    sum(ceiling(data$ol_time_month) >= x)
  },AFR$time)
  AFR$afr <- AFR$f/AFR$n 
  AFR <- AFR[order(AFR$time),]
  return(AFR)
}
                           
 
################################################################################################  
#MAIN

# ��modelͬһʱ���ϼܻ�������ʱ����ʱ��ͳ�Ʋ���ͼ,�������̻�����̻�����ʱ����ʱ��ֲ�
file_path <- file.path(dir_data,'output','capacity_Q1')
# power_on_hours(subset(disk_error,disk_model == 'ST31000524NS' | disk_model == 'ST1000NM0011'),
#                'disk_model','qtr','ol_time_month','biclass',
#                -1,5,file_path,'test')
# # model A
# online_time_plot(subset(data.modelA1,use_time > as.POSIXct('2012-06-01') & use_time < as.POSIXct('2012-08-01')),
#                  'disk_c','qtr','ol_time_month','biclass',-1,5,file_path,'Q1_A1_t1')
# online_time_plot(subset(data.modelA2,use_time > as.POSIXct('2010-08-01') & use_time < as.POSIXct('2012-01-01')),
#                  'disk_c','qtr','ol_time_month','biclass',-1,5,file_path,'Q1_A2_t1')
# # model B
# online_time_plot(subset(data.modelB1,use_time > as.POSIXct('2012-05-01') & use_time < as.POSIXct('2012-10-01')),
#                  'disk_c','qtr','ol_time_month','biclass',-1,5,file_path,'Q1_B1_t1')
# online_time_plot(subset(data.modelB2,use_time > as.POSIXct('2011-09-01') & use_time < as.POSIXct('2012-08-01')),
#                  'disk_c','qtr','ol_time_month','biclass',-1,5,file_path,'Q1_B2_t1')
# # model A and model B with single disk
# online_time_plot(subset(rbind(data.modelB1,data.modelA1),use_time > as.POSIXct('2012-05-01') & use_time < as.POSIXct('2012-10-01')),
#                  'disk_c','qtr','ol_time_month','biclass',-1,5,file_path,'Q1_AB1_t1')
# 
# 
# table(data.modelA1$ol_time_month)
# table(data.modelB1$ol_time_month)
# tmp1 <- subset(disk_error,total == 1000 & disk_c == 1)
# tmp2 <- subset(disk_error,total == 12000 & disk_c == 12)
# ggplot(tmp1,aes(x = qtr,fill = disk_model)) + geom_histogram() +
#   theme(text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=1)) 
# ggplot(tmp2,aes(x = qtr,fill = disk_model)) + geom_histogram() +
#   theme(text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=1)) 

# ��AFR����
m <- 1    #�����½��м�����Ϊ3,�Լ��Ƚ��м�����Ϊ1
qu <- 20
inter_time <- as.POSIXct('2011-12-01',tz = 'UTC')
if (m == 3) {
  afr_fun <- afr_month
} else {
  afr_fun <- afr_quanter
}
  
AFR <- afr_fun(subset(disk_error, use_time <= inter_time),1)
p <- ggplot(subset(AFR,n > 100 & time <= qu*m),aes(x = time,y = afr*(12/m))) + 
  geom_bar(stat = 'identity')
# ��modelAFR����
AFRA1 <- afr_fun(subset(data.modelA1, use_time <= inter_time),1)
p1 <- ggplot(subset(AFRA1,n > 100 & time <= qu*m),aes(x = time,y = afr*(12/m))) + 
  geom_bar(stat = 'identity')

AFRA2 <- afr_fun(subset(data.modelA2, use_time <= inter_time),1)
p2 <- ggplot(subset(AFRA2,n > 100 & time <= qu*m),aes(x = time,y = afr*(12/m))) + 
  geom_bar(stat = 'identity')

AFRB1 <- afr_fun(subset(data.modelB1, use_time <= inter_time),1)
p3 <- ggplot(subset(AFRB1,n > 100 & time <= qu*m),aes(x = time,y = afr*(12/m))) + 
  geom_bar(stat = 'identity')

AFRB2 <- afr_fun(subset(data.modelB2, use_time <= inter_time),1)
p4 <- ggplot(subset(AFRB2,n > 100 & time <= qu*m),aes(x = time,y = afr*(12/m))) + 
  geom_bar(stat = 'identity')


ggsave(file=file.path(file_path,'Q1_AFR_All.png'), plot=p, width = 12, height = 9, dpi = 100)
ggsave(file=file.path(file_path,'Q1_AFR_A1.png'), plot=p1, width = 12, height = 9, dpi = 100)
ggsave(file=file.path(file_path,'Q1_AFR_A2.png'), plot=p2, width = 12, height = 9, dpi = 100)
ggsave(file=file.path(file_path,'Q1_AFR_B1.png'), plot=p3, width = 12, height = 9, dpi = 100)
ggsave(file=file.path(file_path,'Q1_AFR_B2.png'), plot=p4, width = 12, height = 9, dpi = 100)