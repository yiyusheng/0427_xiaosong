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
tmp <- disk_ip$f_time - disk_ip$use_time
units(tmp) <- 'days'
disk_error$ol_time <- tmp
disk_error$class[disk_error$ol_time < 0] <- 0
disk_error$biclass[disk_error$ol_time < 0] <- 0
disk_error$ol_time[disk_error$ol_time < 0] <- -1
disk_error$ol_time <- as.numeric(disk_error$ol_time)

# 非单一model机器
nosingle <- subset(disk_error,disk_model_c1 != 1)
single <- subset(disk_error,disk_model_c1 == 1)
disk_error.good <- subset(disk_error,biclass == 0)
disk_error.bad <- subset(disk_error,biclass == 1)


# 作图
require(ggplot2)
plotdata.bad <- subset(disk_error.bad,ol_time < 365*5 - 26)
plotdata.bad$ol_week <- ceiling(plotdata.bad$ol_time/(30))

# plot for different attributes
for (i in 2:12) {
  attr <- names(plotdata.bad)[i]
  eval(parse(text = sprintf('fill_cont <- 
                            sort(table(factor(plotdata.bad$%s)),decreasing = T)',attr)))
  if (length(fill_cont > 8)) {
    per <- sum(fill_cont[1:8])/sum(fill_cont)
    per <- round(per*1000)/1000
    fill_cont <- names(fill_cont)[1:8]
  }
  per <- 1
  eval(parse(text = sprintf('tmp <- subset(plotdata.bad,%s %%in%% fill_cont)',attr)))
  tmp[,i] <- factor(tmp[,i])
  eval(parse(text = sprintf('geo_aes <- aes(x = ol_week,fill = %s)',attr)))
  
  p <- ggplot(tmp)+
        geom_histogram(geo_aes,position = 'fill')+
        ggtitle(paste(attr,per,sep='_')) + xlab('Online Time (Months)') + ylab('Percentage')
  out_name <- paste(attr,'.png')
  ggsave(file=file.path(dir_data,'output','Online_time',out_name), plot=p, width = 12, height = 9, dpi = 100)
}

  
