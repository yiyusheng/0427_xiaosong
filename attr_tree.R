# �����,ҵ��,RAID,����,����,�ӿں�cache֮�����ع�ϵ,����������fixͼ�Ľṹ

# 1. �����뵼������
rm(list = ls())
dir_data <- 'D:/Data/Disk Number'
dir_code <- 'D:/Git/0427_xiaosong'
dir_lib <- 'D:/Git/R_Function'
# load(file.path(dir_data,'disk_number_label.Rda'))
load(file.path(dir_data,'mcf_all_age.Rda'))
source(file.path(dir_lib,'entropy.R'))

# 2. ����ֶ�֮���������
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