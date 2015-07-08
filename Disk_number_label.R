#@@@ label each ip with diskA number, diskA capacity, diskA model.
rm(list = ls())
dir_data <- 'D:/Data/Disk Number'
# #@@@ 计算A列中每种类型中B占比的熵,再以A中各类的占比作为权值计算平均熵,即B关于A的条件熵
entropy <- function(A,B) {
  A <- factor(A)
  B <- factor(B)
  t1 <- table(A)
  t2 <- table(B)
  t1 <- data.frame(name = names(t1),perc = as.numeric(t1)/sum(t1))
  t2 <- data.frame(name = names(t2),perc = as.numeric(t2)/sum(t2))
  t1$entro <- 0
  tmp <- tapply(B,A,function(x){
    s <- length(x)
    per <- as.numeric(table(x))/s
    per <- per[per!=0]
    return(-1*sum(per*log2(per)))         #entropy of each class of A
  })
  entro_A <- sum(t1$perc*as.numeric(tmp))      #conditional entropy
  return(list(entro = entro_A,entro_each = tmp))
}
###################################################################################
# 1. read cmdb and generate disk information
# cmdb <- read.csv(file.path(dir_data,'cmdb1104_allattr.csv'))
# load(file.path(dir_data,'cmdb.Rda'))
# select.col <- c('svr_id','ip','svr_asset_id','dev_class_id',
#                 'model_name','cpu','memory','dept_id','use_time','raid')
# cmdb$use_time <- as.POSIXct(cmdb$use_time,tz = 'UTC')
# 
# # 2. read ip and disk number in d_smart_1021.csv
# diskA_ip <- read.csv(file.path(dir_data,'d_smart_1021.csv'),head = F)
# diskA <- diskA_ip[,1:2]
# names(diskA) <- c('ip','device')
# diskA <- diskA[!duplicated(diskA[c('ip','device')]),]
# disknum <- table(diskA$ip)
# diskA <- data.frame('ip' = names(disknum))
# diskA$disknum <- as.numeric(disknum)
# 
# # 3. read ip and disk number and diskA model in smart_Tencent_disk.csv
# diskB_ip <- read.csv(file.path(dir_data,'0303_smart_Tencent_disk.csv'),head = F)
# diskBModel <- diskB_ip[,1:4]
# names(diskBModel) <- c('sn','ip','device','model')
# diskBModel <- diskBModel[!duplicated(diskBModel[c('ip','sn')]),]
# diskBModel$model <- as.character(diskBModel$model)
# diskBModel$model[diskBModel$model==''] <- 'NOMODEL'
# diskBModel$model <- as.factor(diskBModel$model)
# 
# # 4. merge diskBmodel and num_info
# info.model <- read.csv(file.path(dir_data,'num_model.csv'))
# diskBModel <- merge(diskBModel,info.model[,1:2],by.x = 'model',by.y = 'Model_ori',all.x = T)
# diskBModel$Model_clear <- as.character(diskBModel$Model_clear)
# diskBModel$Model_clear[is.na(diskBModel$Model_clear)] <- 'NOMODEL'
# diskBModel$Model_clear <- factor(diskBModel$Model_clear)
# 
# # 5. output disk model to search capacity of them
# diskBModel$ipm <- paste(diskBModel$ip,diskBModel$Model_clear,sep='_')
# table.ipm <- table(diskBModel$ipm)
# disk_model <- strsplit(as.character(names(table.ipm)),'_')
# disk_model <- data.frame(t(sapply(disk_model,c)))
# disk_model$count <- as.numeric(table.ipm)
# names(disk_model) <- c('ip','model','number')
# 
# # 6. read model info
# disk_model$model_ori <- disk_model$model
# disk_model$model <- info.model$Model_clear[match(disk_model$model_ori,info.model$Model_ori)]
# disk_model$model <- as.character(disk_model$model)
# disk_model$model[!is.element(disk_model$model,info.model$Model_clear)] <- 'NOMODEL'
# disk_model$model <- as.factor(disk_model$model)
# 
# #number
# disknum <- table(diskBModel$ip)
# diskB <- data.frame('ip' = names(disknum))
# diskB$disknum <- as.numeric(disknum)
# 
# # merge two dataset
# disk <- rbind(diskA,diskB)
# disk <- disk[!duplicated(disk['ip']),]
# save(cmdb,disk,diskBModel,disk_model,file = file.path(dir_data,'disk_cmdb.Rda'))
# disk是一个ip一行,disk_model是一个(ip,model)一行.
###################################################################################
# #@@@ plot attribute and disk num
# load(file.path(dir_data,'output','disk_number_label.Rda'))
# require(ggplot2)
# cmdb.simple <- cmdb[,c('ip','dev_class_id','model_name','raid')]
# cmdb.simple <- merge(cmdb.simple,disk,by = 'ip',all.y = T)
# # cmdb.simple$disknum <- as.character(cmdb.simple$disknum)
# 
# # a <- entropy(cmdb.simple$dev_class_id,cmdb.simple$raid)
# 
# for (i in 2:(ncol(cmdb.simple) - 1)){
#   if (is.factor(cmdb.simple[,i]))
#     cmdb.simple[,i] <- factor(cmdb.simple[,i])
#   #plot and save
#   attr <- names(cmdb.simple)[i]
#   eval(parse(text = sprintf('geo_aes <- aes(x = disknum,fill = %s)',attr)))
#   p <- 
#     ggplot(cmdb.simple) +
#     geom_histogram(geo_aes,position = 'fill') +
#     ggtitle(attr)
#   out_name <- paste(attr,'rate.jpg',sep='_')
#   ggsave(file=file.path(dir_data,'output','figure',out_name), plot=p)
# }
####################################################################################
#@@@ dev_class_name - disk num
# load(file.path(dir_data,'output','disk_number_label.Rda'))
# require(ggplot2)
# cmdb.simple <- cmdb[,c('ip','dev_class_id','model_name','cpu','memory','raid')]
# cmdb.simple <- merge(cmdb.simple,disk,by = 'ip',all.x = T)
# cmdb.simple$disknum[is.na(cmdb.simple$disknum)] <- 0
# 
# #作图: 不同机型的硬盘数
# table.dev <- sort(table(cmdb.simple$dev_class_id),decreasing = T)
# reserve.dev <- names(table.dev)[table.dev > 10000]
# cmdb.dev <- cmdb.simple[is.element(cmdb.simple$dev_class_id,reserve.dev),]
# cmdb.dev$disknum <- as.factor(cmdb.dev$disknum)
# table1.dev <- tapply(cmdb.dev$disknum,factor(cmdb.dev$dev_class_id),table)
# 
# attr <- 'dev_class_id'
# eval(parse(text = sprintf('geo_aes <- aes(x = %s,fill = disknum)',attr)))
# ggplot(cmdb.dev) +
# geom_histogram(geo_aes,position = 'fill') +
# ggtitle(attr)
# 
# #盘数少于12的TS4/TS6/TS8是否发生过故障.
# cmdb.TS <- cmdb.dev[is.element(cmdb.dev$dev_class_id,c('TS4','TS6','TS8')) 
#                     & as.numeric(as.character(cmdb.dev$disknum)) < 12,]
# cmdb.allTS <- cmdb[is.element(cmdb$dev_class_id,c('TS4','TS6','TS8')),]
# load(file.path('D:/Data/0427_xiaosong/','alldata_delfactor.Rda'))
# data_bad.TS <- data_bad[is.element(data_bad$ip,cmdb.allTS$ip),]
# inter.TS <- intersect(data_bad.TS$ip,cmdb.TS$ip)
# 
# #每个机型中有标记和无标记的占比统计
# labeled.dev <- sort(tapply(cmdb.simple$disknum,
#                       factor(cmdb.simple$dev_class_id),function(x)sum(x == 0)/length(x)),decreasing = F)
# tmp <- data.frame(x = names(labeled.dev), y = as.numeric(labeled.dev))
# ggplot(tmp) + geom_bar(aes(x = y))
# 
# #SMART中有SSD机型的disk model统计
# load(file.path(dir_data,'output','disk_number_label.Rda'))
# tmp <- sort(table(diskBModel$model),decreasing = T)
# table.model <- data.frame(name = names(tmp),count = as.numeric(tmp))
# table.model_intel <- subset(table.model,grepl('INTEL',name) | grepl('SSD',name))
# sum.ssd <- sum(table.model_intel$count)

#联合dev_class_name,RAID,model_name对disk num进行分析.
#求出每一个类型中有多少种盘数以及最高那一种盘数的占比
# cmdb <- read.csv(file.path(dir_data,'cmdb1104_allattr.csv'))
# cmdb.disk <- merge(cmdb,disk,by = 'ip',all.y = T)
# cmdb.disk$drm <- factor(paste(cmdb.disk$dev_class_id,cmdb.disk$raid,cmdb.disk$model_name))
# cmdb.disk$dr <- factor(paste(cmdb.disk$dev_class_id,cmdb.disk$raid))
# cmdb.disk$dm <- factor(paste(cmdb.disk$dev_class_id,cmdb.disk$model_name))
# cmdb.disk$rm <- factor(paste(cmdb.disk$raid,cmdb.disk$model_name))
# cmdb.disk <- cmdb.disk[!is.na(cmdb.disk$raid),]
# 
# drm <- data.frame(table = tapply(cmdb.disk$disknum,factor(cmdb.disk$drm),table))
# drm$max_rate <- sapply(drm$table,function(x) max(x)/sum(x))
# drm$sum <- sapply(drm$table,sum)
# drm <- drm[order(drm$sum,decreasing = T),]
# drm <- drm[drm$sum>100,]
# 
# dr <- data.frame(table = tapply(cmdb.disk$disknum,factor(cmdb.disk$dr),table))
# dr$max_rate <- sapply(dr$table,function(x) max(x)/sum(x))
# dr$sum <- sapply(dr$table,sum)
# dr <- dr[order(dr$sum,decreasing = T),]
# dr <- dr[dr$sum>100,]
# 
# dm <- data.frame(table = tapply(cmdb.disk$disknum,factor(cmdb.disk$dm),table))
# dm$max_rate <- sapply(dm$table,function(x) max(x)/sum(x))
# dm$sum <- sapply(dm$table,sum)
# dm <- dm[order(dm$sum,decreasing = T),]
# dm <- dm[dm$sum>100,]
# 
# rm <- data.frame(table = tapply(cmdb.disk$disknum,factor(cmdb.disk$rm),table))
# rm$max_rate <- sapply(rm$table,function(x) max(x)/sum(x))
# rm$sum <- sapply(rm$table,sum)
# rm <- rm[order(rm$sum,decreasing = T),]
# rm <- rm[rm$sum>100,]
# 
# d <- data.frame(table = tapply(cmdb.disk$disknum,factor(cmdb.disk$dev_class_id),table))
# d$max_rate <- sapply(d$table,function(x) max(x)/sum(x))
# d$sum <- sapply(d$table,sum)
# d <- d[order(d$sum,decreasing = T),]
# d <- d[d$sum>100,]
# 
# r <- data.frame(table = tapply(cmdb.disk$disknum,factor(cmdb.disk$raid),table))
# r$max_rate <- sapply(r$table,function(x) max(x)/sum(x))
# r$sum <- sapply(r$table,sum)
# r <- r[order(r$sum,decreasing = T),]
# r <- r[r$sum>100,]
# 
# m <- data.frame(table = tapply(cmdb.disk$disknum,factor(cmdb.disk$model_name),table))
# m$max_rate <- sapply(m$table,function(x) max(x)/sum(x))
# m$sum <- sapply(m$table,sum)
# m <- m[order(m$sum,decreasing = T),]
# m <- m[m$sum>100,]
# 
# 
# cmdb.someTS6A <- subset(cmdb.disk,dev_class_id == 'TS6' & model_name == 'HUAWEI RH2285v2' & raid == 'NORAID' & disknum == 12)
# cmdb.someTS6B <- subset(cmdb.disk,dev_class_id == 'TS6' & model_name == 'HUAWEI RH2285v2' & raid == 'NORAID' & disknum == 13)
# for (i in 1:ncol(cmdb.someTS6A)) {
#   if(is.factor(cmdb.someTS6A[,i])){
#     cmdb.someTS6A[,i] <- factor(cmdb.someTS6A[,i])
#     cmdb.someTS6B[,i] <- factor(cmdb.someTS6B[,i])
#   }
# }
# 
# cmdb.TS4 <- subset(cmdb.disk,dev_class_id == 'TS4' & model_name == 'HUAWEI RH2285' & raid == 'NORAID' & disknum != 12)
# # write.csv(cmdb.TS4,file = file.path(dir_data,'cmdb_TS4.csv'))
# 
# #找出哪些机型在diskBModel中有数据,哪些机型完全没有数据
# table.dev <- table(cmdb$dev_class_id)
# cmdb.model <- merge(cmdb,disk,by = 'ip',all.x = T)
# cmdb.model$disknum[!is.na(cmdb.model$disknum)] <- 1
# cmdb.model$disknum[is.na(cmdb.model$disknum)] <- 0
# cmdb.model$disknum <- as.character(cmdb.model$disknum)
# require(ggplot2)
# ggplot(cmdb.model) + geom_bar(aes(x = dev_class_id, fill = disknum))
# 
# #查看哪些机型有容量数据
# load(file.path(dir_data,'alldata.Rda'))
# cmdb <- read.csv(file.path(dir_data,'cmdb.Rda'))
# data_capacity <- data_all[!duplicated(data_all$ip),]
# cmdb.capacity <- merge(cmdb,data_capacity,by = 'ip',all.x = T)
# cmdb.capacity$existcapa <- 1
# cmdb.capacity$existcapa[is.na(cmdb.capacity$total)] <- 0
# ggplot(cmdb.capacity) + geom_bar(aes(x = dev_class_id, fill = existcapa))
###################################################################################
#@@@ Try to use RAID to clarify disk number and disk model 
# load(file.path(dir_data,'output','disk_number_label.Rda'))
# require(ggplot2)
# cmdb.simple <- cmdb[,c('ip','dev_class_id','model_name','raid')]
# cmdb.simple <- merge(cmdb.simple,disk,by = 'ip',all.x = T)
# cmdb.simple$disknum[is.na(cmdb.simple$disknum)] <- 0

# RAID and model
# diskRaid <- merge(diskBModel,cmdb.simple,by = 'ip',all.x = T)
# a <- tapply(diskBModel$ip,diskBModel$ip,length)
# a <- tapply(diskBModel$model,diskBModel$ip,length(unique))
###################################################################################
#@@@ 给CMDB数据添加DISK MODEL信息
# 1. 给disk model添加容量,数量等信息
load(file.path(dir_data,'disk_cmdb.Rda'))
# model_info <- read.csv(file.path(dir_data,'num_model.csv'))
# model_info <- model_info[!duplicated(model_info$Model_clear),]
disk_model <- subset(disk_model,model!='NOMODEL')
# disk_model <- merge(disk_model,model_info,by.x = 'model',by.y = 'Model_clear',all.x = T)
disk_model$total <- disk_model$capacity*disk_model$number
disk_model$Count <- NULL
disk_model$Model_ori <- NULL
disk_model$Discs <- disk_model$Discs*disk_model$number
disk_model$Heads <- disk_model$Heads*disk_model$number

# 2. 为有disk model的ip建立的表
disk_model$interface <- as.character(disk_model$interface)
disk_model$interface[disk_model$interface == 'SATA 3Gb/s'] <- 'SATA2'
disk_model$interface[disk_model$interface == 'SATA 6Gb/s'] <- 'SATA3'
disk_ip <- data.frame(ip = levels(disk_model$ip),
                      total = as.numeric(tapply(disk_model$total,disk_model$ip,sum)),
                      disk_c = as.numeric(tapply(disk_model$number,disk_model$ip,sum)),
                      disc_c = as.numeric(tapply(disk_model$Discs,disk_model$ip,sum)),
                      head_c = as.numeric(tapply(disk_model$Heads,disk_model$ip,sum)),
                      disk_model = factor(tapply(as.character(disk_model$model),disk_model$ip,
                                          function(x)paste(x,collapse='_'))),
                      disk_inter = factor(tapply(as.character(disk_model$interface),disk_model$ip,
                                                 function(x)paste(x,collapse='_'))),
                      disk_model_c = factor(tapply(disk_model$number,disk_model$ip,
                                            function(x)paste(x,collapse='_'))),
                      disk_model_c1 = as.numeric(tapply(disk_model$number,disk_model$ip,length)))      
disk_ip <- disk_ip[!is.na(disk_ip$total),]
row.names(disk_ip) <- NULL


# 3. 导入故障数据并存储
load(file.path(dir_data,'flist.Rda'))
save(disk_ip,data.flist,cmdb,file = file.path(dir_data,'disk_number_label.Rda'))
# 3. load failure list from uwork and flist (老版本,只把第一次故障作为故障,过滤后面所有故障)
# load('D:/Data/Disk Number/flist(uwork[2012-2014]).Rda')
# data.flist_uwork <- data.flist
# data.flist_uwork <- data.flist_uwork[data.flist_uwork$class>6,c('ip','svr_id','f_time','class','fcount')]
# load('D:/Data/Disk Number/flist(helper[2008-2013]).Rda')
# data.flist_helper <- data.flist
# data.flist_helper <- data.flist_helper[data.flist_helper$class>6,c('ip','svr_id','f_time','class','fcount')]
# data.flist_helper$f_time <- as.POSIXct(data.flist_helper$f_time,tz = 'UTC')
# data.flist <- rbind(data.flist_helper,data.flist_uwork)
# data.flist$ip <- factor(data.flist$ip)
# data.flist$svr_id <- factor(data.flist$svr_id)
# # recalculate fcount
# data.flist <- data.flist[with(data.flist,order(f_time,ip)),]
# fcount <- tapply(data.flist$fcount,factor(data.flist$ip),function(x) {
#   if(length(x)>1) return(sum(x))
#   else return(-1)
#   })
# fcount <- fcount[as.numeric(fcount) != -1]
# # duplication: ip
# data.flist <- data.flist[!duplicated(data.flist$ip),]  #preserve one failure
# data.flist$fcount[match(names(fcount),data.flist$ip)] <- fcount
# data.flist <- data.flist[!duplicated(data.flist$svr_id),]   #preserve one svr_id
# rownames(data.flist) <- NULL
################################################################################################
# # merge failure list and disk_ip
# disk_ip <- merge(disk_ip,cmdb,by = 'ip')
# disk_ip <- merge(disk_ip,data.flist,by = 'ip',all.x=T)
# # fix some na and add a bi-class
# disk_ip$f_time[is.na(disk_ip$f_time)] <- as.POSIXct('1970-01-01',tz = 'UTC')
# disk_ip$class[is.na(disk_ip$class)] <- 0
# disk_ip$fcount[is.na(disk_ip$fcount)] <- 0
# disk_ip$biclass <- 0
# disk_ip$biclass[disk_ip$class>0] <- 1
# res_col <- c('total','disk_c','disc_c','head_c','disk_model','disk_model_c','disk_model_c1',
#              'dev_class_id','dev_class_name','type_name','model_name',
#              'cpu','memory','os_kernal','dept_id','bs1',
#              'idc_parent_name','idc_name','pos_name',
#              'status_name','raid','os_version','svr_version',
#              'class','fcount','biclass',
#              'ip','use_time','f_time','bs2','bs3','rack_name')
# disk_ip <- disk_ip[,res_col]
# 
# # conditional entropy
# condi_entropy <- data.frame(matrix(0,nrow = (ncol(disk_ip)-6),ncol = (ncol(disk_ip)-6)))
# names(condi_entropy) <- res_col[1:(length(res_col)-6)]
# row.names(condi_entropy) <- res_col[1:(length(res_col)-6)]
# condi_entropy_limit <- condi_entropy
# condi_entropy_list <- list()
# 
# for (i in 1:nrow(condi_entropy)){
#   for (j in 1:ncol(condi_entropy)){
#     tmp <- entropy(disk_ip[,i],disk_ip[,j])
#     condi_entropy[i,j] <- tmp[[1]]
#     condi_entropy_list[[(i-1)*ncol(condi_entropy)+j]] <- tmp[[2]]
#   }
# }
# # entropy divide by max entropy log2(n)
# # entropy_order <- data.frame(matrix(nrow = nrow(condi_entropy),ncol = 10))
# # row.names(entropy_order) <- names(condi_entropy)
# for (j in 1:ncol(condi_entropy)){
#   col_name <- names(condi_entropy_limit)[j]
#   condi_entropy_limit[,col_name] <- condi_entropy[,col_name]/log2(length(unique(disk_ip[,col_name])))
# #   tmp <- condi_entropy_limit[order(condi_entropy_limit[,j]),]
# #   tmp <- data.frame(name = row.names(tmp),value = tmp[,j])
# #   tmp <- tmp[tmp$name!=col_name,]
# #   entropy_order[j,1:5] <- as.character(tmp$name[1:5])
# #   entropy_order[j,6:10] <- as.character(tmp$value[1:5])
# }
# # entropy_order <- entropy_order[order(entropy_order$X6),]
# #找出entropy小于0.1的A,B对
# bool <- condi_entropy_limit <= 0.1
# len.bool <- sum(bool)
# entropy_info <- data.frame()
# for (i in 1:nrow(condi_entropy_limit)) {
#   len.bool_col <- sum(bool[i,])
#   tmp <- condi_entropy_limit[i,bool[i,]]
#   if (length(tmp)<=1) next
#   entropy_info <- rbind(entropy_info,data.frame(as.character(rep(row.names(tmp),len.bool_col)),
#                                                as.character(names(tmp)),as.numeric(tmp)))
# }
# names(entropy_info) <- c('A','B','entropy')
# entropy_info <- entropy_info[entropy_info$A != entropy_info$B,]
# entropy_info <- entropy_info[with(entropy_info,order(B,entropy)),]
# row.names(entropy_info) <- NULL
# entropy_info_need <- subset(entropy_info,A %in% c('total','disk_c','disc_c','disk_model_c','head_c','f_count','class') 
#                             & B %in% c('dev_class_id','raid','type_name'))
# 
# 
# save(disk_ip,condi_entropy,condi_entropy_list,
#      condi_entropy_limit,entropy_info,entropy_info_need,
#      cmdb,data.flist,diskBModel,disk_model,model_info,
#      file = file.path(dir_data,'disk_ip.Rda'))
