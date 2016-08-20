#@@@ capacity_error.R: 分析容量,硬盘数,硬盘类型,服务类型在不同的时间段与故障率之间的关系.
rm(list = ls())
dir_data <- 'D:/Data/Disk Number'
load(file.path(dir_data,'disk_ip.Rda'))

# 选取行
col_need <- c('ip','total','disk_c','disc_c','head_c',
              'disk_model','disk_model_c','disk_model_c1',
              'dev_class_name','model_name','bs1','raid',
              'class','fcount','biclass','use_time','f_time')
disk_error <- disk_ip[,col_need]

# 生成服役时间
disk_ip$f_time[disk_ip$f_time == as.POSIXct('1970-01-01',tz = 'UTC')] <- as.POSIXct('2015-01-01',tz = 'UTC')
tmp <- disk_ip$f_time - disk_ip$use_time
units(tmp) <- 'days'
disk_error$ol_time <- tmp
# 处理ol_time小于0的
disk_error$class[disk_error$ol_time < 0] <- 0
disk_error$biclass[disk_error$ol_time < 0] <- 0
disk_error$ol_time[disk_error$ol_time < 0] <- -1
disk_error$ol_time <- as.numeric(disk_error$ol_time)

# 非单一model机器
nosingle <- subset(disk_error,disk_model_c1 != 1)
single <- subset(disk_error,disk_model_c1 == 1)

# 好机坏机
disk_error.good <- subset(disk_error,biclass == 0)
disk_error.bad <- subset(disk_error,biclass == 1)

#生成数据
disk_error$qtr <- paste(format(disk_error$use_time, "%y"), 
                        sprintf("%02i", (as.POSIXlt(disk_error$use_time)$mon) %/% 1L + 1L), 
                        sep="")
disk_error$qtrf <- paste(format(disk_error$f_time, "%y"), 
                         sprintf("%02i", (as.POSIXlt(disk_error$f_time)$mon) %/% 1L + 1L), 
                         sep="")
save(cmdb,disk_error,disk_model,file = file.path(dir_data,'disk_error_1m_1m.Rda'))
##########################################################################################################
# 作图: 使用某一上架时间范围内的机器的故障时间进行作图
require(ggplot2)
# fig.1: 统计图表
#各units上架机器数以及故障占比.
eval(parse(text = sprintf('geo_aes <- aes(x = qtr,fill = as.character(biclass))',attr)))
title <- '各月上架机器数与故障'
p <- ggplot(disk_error) + geom_histogram(geo_aes) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=1)) +
  ggtitle(title) + xlab('months')
ggsave(file=file.path(dir_data,'output','capacity_error',paste('STA_',title,'.png',sep = '')), 
                             plot=p, width = 12, height = 9, dpi = 100)
#容量hist图
attrset <- c('disk_c','disc_c','head_c',
             'disk_model','disk_model_c','disk_model_c1',
             'dev_class_name','model_name','raid','bs1')
plot.total <- subset(disk_error,total %in% c(250,500,1000,12000,24000))
eval(parse(text = sprintf('geo_aes <- aes(x = as.character(total),fill = factor(as.character(%s)))','biclass')))
title <- '各容量硬盘与故障'
p <- ggplot(plot.total) + geom_histogram(geo_aes) +
  theme(text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=1)) +
  ggtitle(title) + xlab('capacity')
ggsave(file=file.path(dir_data,'output','capacity_error',paste('STA_',title,'.png',sep = '')), 
       plot=p, width = 12, height = 9, dpi = 100)


for (i in 1:length(attrset)) {
  attr <- attrset[i]
  eval(parse(text = sprintf('geo_aes <- aes(x = as.character(total),fill = factor(as.character(%s)))',attr)))
  p <- ggplot(plot.total) + geom_histogram(geo_aes) + 
    ggtitle(paste('total',attr,sep='_')) + 
    xlab('Capacity(MB)') + ylab('Count')
  out_name <- paste(paste('total',attr,sep='_'),'.png')
  ggsave(file=file.path(dir_data,'output','capacity_error',out_name), plot=p, width = 12, height = 9, dpi = 100)
}
# a <- subset(plot.total,total == 500)
# b <- subset(plot.total,total == 1000)
# c <- subset(plot.total,total == 12000)
# d <- subset(plot.total,total == 24000)
##########################################################################################################
# fig.2*14: 各季度上架机器故障时的服役时间关于容量,盘数,机型的分布
attrset <- c('total','disk_c','disc_c','head_c',
             'disk_model','disk_model_c','disk_model_c1',
             'dev_class_name','model_name','raid')
quanter_need <- sort(unique(disk_error$qtr))[12:38]
quanter_need <- quanter_need[quanter_need != '1201']

for (i in 1:length(attrset)) {
  for (j in 1:length(quanter_need)) {
    curr.quanter <- quanter_need[j]
    curr.attr <- attrset[i]
    # all data
    plot.data <- subset(disk_error,qtr == curr.quanter)
    eval(parse(text = sprintf('table.attr_all <- sort(table(plot.data$%s),decreasing = T)',curr.attr)))
    # bad data
    plot.data <- subset(plot.data,biclass == 1)
    eval(parse(text = sprintf('table.attr <- sort(table(plot.data$%s),decreasing = T)',curr.attr)))
    # select data
    sum.all <- sum(table.attr_all)
    sum.bad <- sum(table.attr)
    if(sum.bad/sum.all < 0.1 & sum.bad < 200) next
#     table.attr <- table.attr[table.attr>100]
    count_attr <- min(5,length(table.attr))
    per <- sum(table.attr[1:count_attr])/sum(table.attr)
    per <- round(per*1000)/1000
    attr_need <- names(table.attr)[1:count_attr]
    eval(parse(text = sprintf('plot.data <- subset(plot.data, %s %%in%% names(table.attr)[1:%i])',
                              curr.attr,count_attr)))
    plot.data$qtrf <- factor(plot.data$qtrf)
    # calculate rate
    plot.line <- data.frame(id = character(),
                            value = numeric(),
                            color = character())
    for (k in 1:count_attr){
      # 某字段某值故障率计算(id为时间,value为故障率,color为某字段值)
      eval(parse(text = sprintf('table.attr_each <- table(subset(plot.data, %s == names(table.attr)[k],%s))',curr.attr,'qtrf')))
      table.attr_each <- table.attr_each/table.attr_all[names(table.attr_all) == names(table.attr)[k]]
      len <- length(table.attr_each)
      plot.line <- rbind(plot.line,data.frame(id = names(table.attr_each),
                                              value = as.numeric(table.attr_each),
                                              color = rep(names(table.attr)[k],len)))
    }

    # plot
    geo_aes <- aes(x = id, y = value, group = factor(color), colour = factor(color))
    p <- ggplot(plot.line,geo_aes) + 
            geom_line(size = 1.5) +
            geom_point(size = 4, shape = 21, fill = 'white') +
            ggtitle(paste(curr.attr,curr.quanter,sum.all,sum.bad,per,sep='_')) + 
            xlab('Time (Quanters)') + ylab('Percent')
    out_name <- paste(paste(curr.attr,curr.quanter,sum.all,sum.bad,per,sep='_'),'.png')
    ggsave(file=file.path(dir_data,'output','capacity_error',out_name), plot=p, width = 12, height = 9, dpi = 100)
  }
}
##########################################################################################################

# # 作图A: 基于故障时已服役时间进行分类作图
# require(ggplot2)
# plotdata.bad <- subset(disk_error.bad,ol_time < 365*5 - 26)
# plotdata.bad$ol_week <- ceiling(plotdata.bad$ol_time/(30))
# 
# # plot for different attributes
# for (i in 2:12) {
#   attr <- names(plotdata.bad)[i]
#   eval(parse(text = sprintf('fill_cont <- 
#                             sort(table(factor(plotdata.bad$%s)),decreasing = T)',attr)))
#   if (length(fill_cont > 8)) {
#     per <- sum(fill_cont[1:8])/sum(fill_cont)
#     per <- round(per*1000)/1000
#     fill_cont <- names(fill_cont)[1:8]
#   }
#   per <- 1
#   eval(parse(text = sprintf('tmp <- subset(plotdata.bad,%s %%in%% fill_cont)',attr)))
#   tmp[,i] <- factor(tmp[,i])
#   eval(parse(text = sprintf('geo_aes <- aes(x = ol_week,fill = %s)',attr)))
#   
#   p <- ggplot(tmp)+
#         geom_histogram(geo_aes,position = 'fill')+
#         ggtitle(paste(attr,per,sep='_')) + xlab('Online Time (Months)') + ylab('Percentage')
#   out_name <- paste(attr,'.png')
#   ggsave(file=file.path(dir_data,'output','Online_time',out_name), plot=p, width = 12, height = 9, dpi = 100)
# }

  
