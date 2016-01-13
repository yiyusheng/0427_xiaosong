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
data.config$bs1 <- cmdb_dev$bs1[match(data.config$ip,cmdb_dev$ip)]
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
dev_need <- c('TS3','TS4','TS5','TS6','C1')
# data.config <- subset(data.config,dev_class_id %in% dev_need)
data.config$dev_class_id <- factor(data.config$dev_class_id)


# 2. 样本及IO与SMART统计数据
k131_svrid <- read.csv(file.path(dir_data,'k131_svrid_old'),header = F)
names(k131_svrid) <- 'svrid'
col_need <- c('svr_asset_id','ip','dev_class_id','bs1','type_name','pos_id')
k131_svrid <- merge(k131_svrid,cmdb[,col_need],by.x = 'svrid',by.y = 'svr_asset_id')
k131_svrid$db <- paste(k131_svrid$dev_class_id,k131_svrid$bs1,sep='_')
k131_svrid$dbt <- paste(k131_svrid$db,k131_svrid$type_name,sep='_')
k131_svrid <- factorX(k131_svrid)
load(file.path(dir_data,'sta_smart.Rda'))
load(file.path(dir_data,'sta_io.Rda'))

# 3. IO特征数据
data <- read.csv(file.path(dir_data,'k131_902'))
data$date <- as.POSIXct(data$date)
col_need <- c('svr_asset_id','ip','dev_class_id','bs1','type_name','pos_id')
data <- merge(data,cmdb[,col_need],by.x = 'svrid',by.y = 'svr_asset_id')
data$db <- paste(data$dev_class_id,data$bs1,sep='_')
data$dbt <- paste(data$db,data$type_name,sep='_')
data <- factorX(data)

# 4. 作图
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