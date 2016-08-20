# 柳永康的实验结果作图,数据来自其毕业论文
rm(list = ls())
require(ggplot2)
library(scales)
dir_data <- 'D:/Data/Disk Number'

# disk failure prediction
x <- c(0.9967,0.9877,0.9961,0.9887,0.9947,0.9754,0.9865,0.9897,0.9942,0.9947)
y <- c(0,0.0001,0.0006,0.0007,0.0002,0.0009,0.0008,0.0003,0.0004,0.0003)
failure_predict <- data.frame(precision = x[order(x)],recall = y[order(y)])
p1 <- ggplot(failure_predict,aes(x = recall,y = precision)) + 
  geom_line(size = 1.5) +
  geom_point(size = 4, shape = 21, fill = 'white') +
  ggtitle('硬盘故障预测') + 
  scale_y_continuous(labels = percent) +scale_x_continuous(labels = percent) +
  xlab('FAR') + ylab('FDR') +
  theme(axis.text.x = element_text(colour="grey20",size=20,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=.5,face="plain"),
        title = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=1,face="plain"))

ggsave(file=file.path(dir_data,'disk failure prediction','硬盘故障预测.png'), 
       plot=p1, width = 12, height = 9, dpi = 100)

# time prediction
x <- c(0.8501,0.7813,0.7013,0.5670,0.4191)
y <- c(0.2751,0.1745,0.0906,0.0236,0.0159)
failure_predict <- data.frame(precision = x,recall = y)
p2 <- ggplot(failure_predict,aes(x = recall,y = precision)) + 
  geom_line(size = 1.5) +
  geom_point(size = 4, shape = 21, fill = 'white') +
  ggtitle('故障时间预测(5天)') + 
  scale_y_continuous(labels = percent) +scale_x_continuous(labels = percent) +
  xlab('FAR') + ylab('FDR') +
  theme(axis.text.x = element_text(colour="grey20",size=20,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=20,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=.5,face="plain"),
        title = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=1,face="plain"))


ggsave(file=file.path(dir_data,'disk failure prediction','故障时间预测(5天).png'), 
       plot=p2, width = 12, height = 9, dpi = 100)