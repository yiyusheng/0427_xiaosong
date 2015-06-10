#@@@ ��ȡhelper���ݲ����м�Ҫ����
rm(list = ls())
dir_data <- 'D:/Data/Disk Number/'
require(ggplot2)

# read data and save
# data <- rbind(read.csv(file.path(dir_data,'2009.csv')),
#               read.csv(file.path(dir_data,'2010.csv')),
#               read.csv(file.path(dir_data,'2011.csv')),
#               read.csv(file.path(dir_data,'2012.csv')),
#               read.csv(file.path(dir_data,'2013.csv')))
# save(data,file = file.path(dir_data,'helper[09-13].Rda'))
load(file.path(dir_data,'helper[09-13].Rda'))

# 1. extract some column and filter replica list
col_need <- c('����ʱ��','����ԭ��','��ǰ״̬','���Ϸ�������','����','����ָ�ʱ��','�ᵥʱ��',
              '�������','���ʱ��','���ϻ����ʺ�','����IP','�澯����','�豸�ͺ�','�豸����',
              'SN','�ϼ�ʱ��','����ָ���ʱ.Сʱ.','�¼�����','Ӳ�̹�������','Ӳ�̹�������',
              'Ӳ������','Ӳ����������','Ӳ��Ʒ�Ƴ���','�������ʺ�')
data <- data[,col_need]
names(data) <- c('f_time','reason','state','fail_dept','dept','recover_time','close_time',
                 'solution','svr_id','svr_id_failure','ip','level','model_name','dev_class_id',
                 'SN','use_time','recover_interval','type','disk_failure_type','disk_failure_count',
                 'disk_capacity','disk_vendor','disk_band_vendor','svr_id_backup')
data <- subset(data,as.numeric(state) != 1 & as.numeric(state) != 4)

# 2. ���Բ��ź�,svr_id�Ƿ�һ��
# sum(as.character(data$dept) == as.character(data$fail_dept))
# #ɾ��������
# data$dept <- NULL
# sum(as.character(data$svr_id) == as.character(data$svr_id_failure))
# #ɾ�����ϻ����ʺ�
# data$svr_id_failure <- NULL

# 3. ѡȡ�жϻ����Ƿ����ֶ�(solution,type,disk_failure_type)
# ggplot(data,aes(x = disk_failure_type,fill = type)) + geom_histogram()
# table(data[as.numeric(data$disk_failure_count) != 1,'type'])
# # solution
# idx.solution <- sapply(data$solution,function(x) grepl('����Ӳ��',x))
# data.solution <- data
# data.solution$solbool <- 0
# data.solution$solbool[idx.solution] <- 1
# # type & disk_faulure_type
# table(data.solution[data.solution$solbool == 1,'disk_failure_type'])
#����: ʹ��disk_failure_type����ʶ���˵���.

# 4.����IP
data.filter <- data[with(data,order(ip,f_time)),]
#replace wrong string
data.filter$ip <- gsub("��", "", data.filter$ip)
data.filter$ip <- gsub("\n", "", data.filter$ip)
#delete no ip
data.filter <- data.filter[data.filter$ip!='',]           # delete no ip
#remove item with more than one ip
idx.ip_res <- nchar(as.character(data.filter$ip)) <= 15
data.filter <- data.filter[idx.ip_res,]
#remove item with ip blocked by regexp
regexp.ip <- "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$"
idx.ip_reg <- grepl(regexp.ip,data.filter$ip)
data.filter <- data.filter[idx.ip_reg,]
# sum(grep(regexp.ip,data.filter$ip,value = T) == data.filter$ip) == nrow(data.filter)
#factorize
data.filter$ip <- factor(data.filter$ip)                    # reconstruct factor of ip

# 5. ����f_time,use_time,close_time
# table(nchar(as.character(data.filter$f_time)))
# table(nchar(as.character(data.filter$use_time)))
# table(nchar(as.character(data.filter$close_time)))
data.filter$use_time <- gsub("��", "", data.filter$use_time)
data.filter$use_time <- gsub("\n", "", data.filter$use_time)
data.filter$use_time <- as.POSIXct(data.filter$use_time,tz = 'UTC')
data.filter$f_time <- as.POSIXct(data.filter$f_time,tz = 'UTC')
# data.filter$close_time <- as.POSIXct(data.filter$close_time,tz = 'UTC')

# 6.add class ((��Ӳ�̹��ϵ�����Ϊ-1,Ӳ�̹��ϵ�����Ϊ13(��Ӳ�̹���),14(��Ӳ�̹���))
data.filter$class <- -1
data.filter$class[as.character(data.filter$disk_failure_type) == '��Ӳ�̹���'] <- 13
data.filter$class[as.character(data.filter$disk_failure_type) == '��Ӳ�̹���'] <- 14

# 6. duplication: ip
# fcount <- tapply(data.filter$ip,data.filter$ip,length)
# data.filter.flist <- data.filter[!duplicated(data.filter$ip),]  #preserve one failure
# data.filter.flist$fcount[match(names(fcount),data.filter.flist$ip)] <- fcount
# rownames(data.filter.flist) <- NULL

# 7.save
data.flist <- data.filter
data.bad <- subset(data.flist,class!=-1)
save(data.flist,data.bad,file = paste(dir_data,'flist(helper[2008-2013]).Rda',sep = ''))