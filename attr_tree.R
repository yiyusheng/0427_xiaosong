# 求机型,业务,RAID,盘数,容量,接口和cache之间的相关关系,并决定树型fix图的结构

# 1. 参数与导入数据
rm(list = ls())
dir_data <- 'D:/Data/Disk Number'
dir_code <- 'D:/Git/0427_xiaosong'
dir_lib <- 'D:/Git/R_Function'
# load(file.path(dir_data,'disk_number_label.Rda'))
load(file.path(dir_data,'mcf_all_age.Rda'))
source(file.path(dir_lib,'entropy.R'))

# 2. 求各字段之间的条件熵
col_need <- c('dev_class_id','bs1','raid','disk_cNew','totalNew','itfNew','cacheNew')
cmdb_dev <- cmdb_dev[,col_need]
for (i in 1:length(col_need)){
  cmdb_dev[[col_need[i]]] <- factor(cmdb_dev[[col_need[i]]])
}


etrp <- matrix(0,nrow = length(col_need),ncol = length(col_need))
for (i in 1:length(col_need)){
  for (j in 1:length(col_need)){
    A <- cmdb_dev[[col_need[i]]]
    B <- cmdb_dev[[col_need[j]]]
    tmp <- entropy(A,B)
    etrp[i,j] <- tmp[[1]]
  }
}
etrp <- data.frame(etrp)
names(etrp) <- col_need
row.names(etrp) <- col_need
