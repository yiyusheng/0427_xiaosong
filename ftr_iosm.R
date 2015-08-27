# IO特征与业务之间的对应关系
rm(list = ls())
source('D:/Git/R_Function/Rfun.R')
require('ggplot2')
require('scales')
require('xlsx')

#@@@ PARAMETERS
dir_dataA <- 'D:/Data/Disk Number'
dir_data <- 'D:/Data/attrid'

#@@@ LOAD DATA
# 1. cmdb数据
load(file.path(dir_dataA,'disk_number_label.Rda'))
load(file.path(dir_dataA,'mcf_all_age_rsv2014.Rda'))
dev_need <- c('TS4','TS5','TS6',
              'C1')
data.config$bs1 <- cmdb_dev$bs1[match(data.config$ip,cmdb_dev$ip)]
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
# data.config <- subset(data.config,dev_class_id %in% dev_need)
data.config$dev_class_id <- factor(data.config$dev_class_id)

# # 2. IO特征数据
data <- read.csv(file.path(dir_data,'k130_902'))
data$date <- as.POSIXct(data$date)
data$dev_class_id <- cmdb$dev_class_id[match(data$svrid,cmdb$svr_asset_id)]
data$bs1 <- cmdb$bs1[match(data$svrid,cmdb$svr_asset_id)]
data$db <- paste(data$bs1,data$dev_class_id,sep='_')
data$dbd <- paste(data$db,data$date,sep='_')
# data <- data[sort(data$dbd),]

# 3. 作图
table.db <- table(data$db)
for (i in 1:length(dev_need)){
  d <- dev_need[i]
  tmp <- subset(data,dev_class_id == d)
  l <- length(unique(tmp$svrid))
  t <- paste('read',d,sep='_')
  title <- paste(t,'[',l,']',sep='')
  p <- ggplot(tmp,aes(x = date,y = mean,group = svrid,color = db)) + geom_line() +
  xlab('Time') + ylab('read_mean') + 
    ggtitle(title) +
    scale_color_discrete(name = 'Class') +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  ggsave(file=file.path(dir_data,'io',paste(t,'.png',sep='')), 
         plot=p, width = 12, height = 9, dpi = 100)
}