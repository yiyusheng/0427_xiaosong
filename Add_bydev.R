#@@@ 添加新数据
rm(list = ls())
dir_data <- 'D:/Data/Disk Number'
load(file.path(dir_data,'disk_number_label.Rda'))

# 1. 查看大量缺失数据的机型
cmdb_nomodel <- subset(cmdb,!(ip %in% disk_ip$ip))
sort(table(cmdb_nomodel$dev_class_id))
cmdb_nomodel$total <- 0
cmdb_nomodel$disk_c <- 0

# 2. 查看有model的机器的数据(B6,A5,B5,M1,TS8,B1,A1)
cmdb_model <- merge(disk_ip,cmdb,by = 'ip')
dev_need <- c('A1','A5','B1','B5','B6','TS8','M1')
col_need <- c('ip','total','disk_c','dev_class_id','model_name','dept_id','bs1',
              'idc_parent_id','idc_id','use_time','operator','raid')
cmdb_model_dev <- subset(cmdb_model,dev_class_id %in% dev_need,col_need)
cmdb_model_dev <- cmdb_model_dev[order(cmdb_model_dev$dev_class_id),]

# 3. 处理34707台B6
mnA <- c('DELL DCS6230','HP DL2000')
mnB <- c('HUAWEI XH320v2','DELL DCSC6100')
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'B6' &
                     cmdb_nomodel$model_name %in% mnA] <- 1000
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'B6' &
                     cmdb_nomodel$model_name %in% mnB] <- 500
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'B6' &
                     cmdb_nomodel$model_name == 'DELL DCSC6130' & 
                     cmdb_nomodel$idc_parent_id == 159] <- 500
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'B6' &
                     cmdb_nomodel$model_name == 'DELL DCSC6130' & 
                     cmdb_nomodel$idc_parent_id == 180] <- 1000
cmdb_nomodel$disk_c[cmdb_nomodel$dev_class_id == 'B6' &
                      cmdb_nomodel$total != 0] <- 1

# 4. 处理14478台A5
A5_model <- subset(cmdb_model_dev,dev_class_id == 'A5')
A5_model <- A5_model[order(A5_model$model_name),]
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'A5' &
                     cmdb_nomodel$model_name == 'DELL DCS2210'] <- 24000
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'A5' &
                     cmdb_nomodel$model_name == 'IBM X3650M3'] <- 12000                   
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'A5' &
                     cmdb_nomodel$model_name == 'HP DL380G7'] <- 500  
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'A5' &
                     cmdb_nomodel$model_name == 'DELL FS12-TY' & 
                     cmdb_nomodel$idc_id == 701] <- 12000
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'A5' &
                     cmdb_nomodel$model_name == 'DELL FS12-TY' & 
                     cmdb_nomodel$idc_id == 826] <- 1000
cmdb_nomodel$disk_c[cmdb_nomodel$dev_class_id == 'A5' &
                      cmdb_nomodel$total >= 500] <- 1
cmdb_nomodel$disk_c[cmdb_nomodel$dev_class_id == 'A5' &
                      cmdb_nomodel$total >= 1200] <- 12

# 5. 处理5179台B5
B5_model <- subset(cmdb_model_dev,dev_class_id == 'B5')
B5_model <- B5_model[order(B5_model$model_name),]
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'B5' &
                     cmdb_nomodel$model_name == 'LENOVO SD220X4']<- 1000
cmdb_nomodel$disk_c[cmdb_nomodel$dev_class_id == 'B5' &
                      cmdb_nomodel$total >= 500] <- 1

# 6. 处理8739台M1
M1_model <- subset(cmdb_model_dev,dev_class_id == 'M1')
M1_model <- M1_model[order(M1_model$model_name),]
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'M1' &
                     cmdb_nomodel$model_name == 'HP DL360pG8']<- 24000
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'M1' &
                     cmdb_nomodel$model_name == 'DELL R620']<- 24000
cmdb_nomodel$disk_c[cmdb_nomodel$dev_class_id == 'M1' &
                      cmdb_nomodel$total >= 500] <- 12

# 7. 处理5656台TS8
TS8_model <- subset(cmdb_model_dev,dev_class_id == 'TS8')
TS8_model <- TS8_model[order(TS8_model$model_name),]
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'TS8' &
                     cmdb_nomodel$model_name == 'HP DL380eG8' &
                     cmdb_nomodel$idc_parent_id == 159]<- 24000
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'TS8' &
                     cmdb_nomodel$model_name == 'HP DL380eG8' &
                     cmdb_nomodel$idc_parent_id == 167]<- 1000
cmdb_nomodel$total[cmdb_nomodel$dev_class_id == 'TS8' &
                     cmdb_nomodel$model_name == 'HUAWEI RH2285v2']<- 24000
cmdb_nomodel$disk_c[cmdb_nomodel$dev_class_id == 'TS8' &
                      cmdb_nomodel$total >= 500] <- 1
cmdb_nomodel$disk_c[cmdb_nomodel$dev_class_id == 'TS8' &
                      cmdb_nomodel$total >= 24000] <- 12

cmdb_nomodel <- subset(cmdb_nomodel,total > 0,c('ip','total','disk_c'))
save(cmdb_nomodel,file = file.path(dir_data,'cmdb_nomodel_add.Rda'))