#@@@ MCF ����׼��
rm(list = ls())
dir_data <- 'D:/Data/Disk Number'
load(file.path(dir_data,'disk_number_label.Rda'))
end_time <- as.POSIXct('2013-12-01',tz = 'UTC')
require(ggplot2)
#####################################################
#@@@ data.flist����
# 1. [ɾ��9877]����13��12��1���Ժ�Ĺ���
data.flist <- subset(data.flist,f_time <= end_time)

# 2. [ɾ��566]�ж��svr_id��ip,���ж��ip��svr_id.
data.flist$ip_svr <- paste(data.flist$ip,data.flist$svr_id,sep = '_')
tmp <- data.flist[!duplicated(data.flist$ip_svr),]
tmp1 <- table(tmp$ip)
tmp2 <- table(tmp$svr_id)
tmp3 <- names(tmp1)[tmp1!=1]
tmp4 <- names(tmp2)[tmp2!=1]
data.flist <- subset(data.flist,!(ip %in% tmp3) & !(svr_id %in% tmp4))
data.flist$ip_svr <- NULL

# 3. [ɾ��1949]������flist,��ȫuse_time in data.flist by cmdb (helper��use_time�Ѿ���������,uworkû��use_time)
data.flist <- merge(data.flist,cmdb[,c('ip','use_time')],by = 'ip', all.x = T)
data.flist <- subset(data.flist, !is.na(use_time.y) | use_time.x != end_time)

# 4. �����2013-12-01֮ǰ�¼ܵĻ���
data.flist$is_online <- 1
data.flist$is_online[data.flist$class > 7 & is.na(data.flist$use_time.y)] <- 0

# 5. [ɾ��186]helper���ϵ���use_time��cmdb�ж�Ӧ��use_time��һ�µ�ip
tmp <- subset(data.flist,use_time.x != end_time & !is.na(use_time.y))
tmp1 <- tmp[tmp$use_time.x != tmp$use_time.y,]
data.flist <- subset(data.flist, !(ip %in% tmp1$ip))
data.flist$use_time.y[is.na(data.flist$use_time.y)] <- data.flist$use_time.x[is.na(data.flist$use_time.y)]
data.flist$use_time <- data.flist$use_time.y
data.flist$use_time.x <- NULL
data.flist$use_time.y <- NULL

# 6. [ɾ��3]use_time Ϊ1900���
data.flist <- subset(data.flist,use_time != as.POSIXct('1900-01-01',tz = 'UTC'))

# 7. [ɾ��767]����ʱ��С���ϼ�ʱ���
data.flist <- subset(data.flist,use_time < f_time)

# 8. [ɾ��4253]���ϵ�ȥ��
days_rep <- 7
#[ɾ��164]��ȫ��ͬ����ʱ��ȥ��
tmp <- paste(data.flist$ip,data.flist$f_time,sep='_')
data.flist <- data.flist[!duplicated(tmp),]
#[ɾ��4089]days_repʱ���ڹ��ϵ�ȥ��
data.flist$ip <- factor(data.flist$ip)
data.flist <- data.flist[order(data.flist$ip,data.flist$f_time),]
tmp <- tapply(data.flist$f_time,data.flist$ip,function(x) {
  if (length(x) == 1){
    return(as.numeric(-1))
  }else {
    x1 <- c(x[1],x[1:(length(x)-1)])
    itv <- x - x1
    units(itv) <- 'days'
    itv[1] <- -1
    return(as.numeric(itv))
  }
})
idx.rep <- unlist(tmp)
data.flist <- data.flist[idx.rep == -1 | idx.rep > days_rep,]
#####################################################
#@@@ cmdb,disk_ip����
# 1. ip regexp examine
regexp.ip <- "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$"
cmdb <- cmdb[grepl(regexp.ip,cmdb$ip),]
disk_ip <- disk_ip[grepl(regexp.ip,disk_ip$ip),]

# 2. [ɾ��]cmdb,svr_id��ip��Ӧ����
cmdb$svr_ip <- paste(cmdb$ip,cmdb$svr_asset_id,sep = '_')
tmp <- cmdb[!duplicated(cmdb$svr_ip),]
tmp1 <- table(tmp$ip)
tmp2 <- table(tmp$svr_id)
tmp3 <- names(tmp1)[tmp1!=1]
tmp4 <- names(tmp2)[tmp2!=1]
cmdb <- subset(cmdb,!(ip %in% tmp3) & !(svr_id %in% tmp4))
cmdb$svr_ip <- NULL

# 3. factorize ip and svr_id
data.flist$ip <- factor(data.flist$ip)
data.flist$svr_id <- factor(data.flist$svr_id)
cmdb$ip <- factor(cmdb$ip)
disk_ip$ip <- factor(disk_ip$ip)

#####################################################
#@@@ MCF���׼��,�ϲ�����
# 1. ��flist���ӹ���ʱ����ʱ��
data.flist$ol_time_fail <- data.flist$f_time - data.flist$use_time
units(data.flist$ol_time_fail) <- 'days'

# 2. ��flist���ӿ�ʼ����ʱ��ͽ���ʱ��.
first_failure_time <- as.POSIXct('2009-01-01',tz = 'UTC')
last_failure_time <- as.POSIXct('2013-12-01',tz = 'UTC')
data.flist$start <- difftime(first_failure_time,first_failure_time,units = 'days')
idx.young <- data.flist$use_time < first_failure_time
data.flist$start[idx.young] <- difftime(first_failure_time,data.flist$use_time[idx.young],units = 'days')
data.flist$end <- difftime(last_failure_time,data.flist$use_time,units = 'days')

# 3. ��flist����calendar�Ŀ�ʼ����ʱ��ͽ���ʱ��.
first_failure_time_c <- as.POSIXct('2003-01-01',tz = 'UTC')
last_failure_time_c <- as.POSIXct('2013-12-01',tz = 'UTC')
data.flist$start_c <- difftime(data.flist$use_time,first_failure_time_c,units = 'days')
data.flist$end_c <- difftime(last_failure_time_c,first_failure_time_c,units = 'days')
data.flist$ol_time_fail_c <- difftime(data.flist$f_time,first_failure_time_c,units = 'days')
#����ǰ�¼ܻ�����end_cҪ���д���.

# 3. [ɾ��]������ǰ�¼ܵĻ���,�����ʱ������2013-12-01 ��ʱ�Ȳ�����.
data.flist <- subset(data.flist,is_online == 1)
# day_add <- difftime('2013-10-08','2013-10-01',units = 'days')
# tmp <- subset(data.flist,is_online == 0)
# tmp1 <- tapply(tmp$f_time,factor(tmp$ip),function(x) {
#   return(max(x))})
# tmp1 <- as.POSIXct(tmp1, origin="1970-01-01", tz = 'UTC') + day_add
# 
# ia <- match(data.flist$ip,names(tmp1))
# ib <- match(names(tmp1),data.flist$ip)
# data.flist$end[ia[!is.na(ia)]] <- tmp1[ib[!is.na(ib)]] - data.flist$use_time[ia[!is.na(ia)]]
# 
# sapply(tmp1,function(x){
#   tmp$end[tmp$ip == names(x)] <- x - tmp$use_time
# })
# data.flist <- subset(data.flist,is_online == 1)
# data.flist <- rbind(data.flist,tmp1)
# data.flist <- data.flist[order(data.flist$ip,data.flist$f_time),]


# 4. ��cmdb�еķǹ��ϻ����ӿ�ʼ����ʱ��ͽ���ʱ��
data.cmdblist <- subset(cmdb,!(ip %in% data.flist$ip),c('ip','svr_asset_id','use_time'))
names(data.cmdblist) <- c('ip','svr_id','use_time')
#AGE
data.cmdblist$start <- difftime(first_failure_time,first_failure_time,units = 'days')
idx.young <- data.cmdblist$use_time < first_failure_time
data.cmdblist$start[idx.young] <- difftime(first_failure_time,data.cmdblist$use_time[idx.young],units = 'days')
data.cmdblist$end <- difftime(last_failure_time,data.cmdblist$use_time,units = 'days')
#CALENLAR
data.cmdblist$start_c <- difftime(data.cmdblist$use_time,first_failure_time_c,units = 'days')
data.cmdblist$end_c <- difftime(last_failure_time_c,first_failure_time_c,units = 'days')
#factorize
data.cmdblist$ol_time_fail <- -1
data.cmdblist$ol_time_fail_c <- -1
data.cmdblist$ip <- factor(data.cmdblist$ip)
data.cmdblist$svr_id <- factor(data.cmdblist$svr_id)
data.cmdblist$f_time <- as.POSIXct('2015-01-01',tz = 'UTC')
# 5. �ϲ�data.cmdblist��data.flist
col_need <- c('ip','svr_id',
              'use_time','f_time',
              'start','ol_time_fail','end',
              'start_c','ol_time_fail_c','end_c')
data.mcf <- rbind(data.flist[,col_need],data.cmdblist[,col_need])

# 6. [ɾ��142]����ip,svr_id�����
data.mcf$svr_ip <- paste(data.mcf$ip,data.mcf$svr_id,sep = '_')
tmp <- data.mcf[!duplicated(data.mcf$svr_ip),]
tmp1 <- table(tmp$ip)
tmp2 <- table(tmp$svr_id)
tmp3 <- names(tmp1)[tmp1!=1]
tmp4 <- names(tmp2)[tmp2!=1]
data.mcf <- subset(data.mcf,!(ip %in% tmp3) & !(svr_id %in% tmp4))
data.mcf$svr_ip <- NULL

# 7. factorize and numeric
data.mcf$ip <- factor(data.mcf$ip)
data.mcf$svr_id <- factor(data.mcf$svr_id)
data.mcf$ol_time_fail <- round(as.numeric(data.mcf$ol_time_fail))
data.mcf$start <- round(as.numeric(data.mcf$start))
data.mcf$end <- round(as.numeric(data.mcf$end))
data.mcf$ol_time_fail_c <- round(as.numeric(data.mcf$ol_time_fail_c))
data.mcf$start_c <- round(as.numeric(data.mcf$start_c))
data.mcf$end_c <- round(as.numeric(data.mcf$end_c))
save(data.mcf,file = file.path(dir_data,'data_mcf.Rda'))
#####################################################
#@@@ ����ʱ����ʱ��ͳ��(AGE)
tmp <- subset(data.mcf,ol_time_fail >= 0)
sta.ol_time <- data.frame(table(round(tmp$ol_time_fail/30)))
names(sta.ol_time) <- c('v','f')
sta.ol_time$v <- as.numeric(levels(sta.ol_time$v)[sta.ol_time$v])
p <- ggplot(sta.ol_time,aes(x = v,y = f)) + geom_line() + 
  ggtitle('����ʱ����ʱ��ͳ��(AGE)') + xlab('ʱ��(��)') + ylab('����')
ggsave(file=file.path(dir_data,'output','mcf','����ʱ����ʱ��ͳ��(AGE).png'), plot=p, width = 12, height = 9, dpi = 100)

#@@@ ����ʱ����ʱ��ͳ��(CALENLAR)
tmp <- subset(data.mcf,ol_time_fail_c >= 0)
sta.ol_time <- data.frame(table(round(tmp$ol_time_fail_c/30)))
names(sta.ol_time) <- c('v','f')
sta.ol_time$v <- as.numeric(levels(sta.ol_time$v)[sta.ol_time$v])
p <- ggplot(sta.ol_time,aes(x = v,y = f)) + geom_line() + 
  ggtitle('����ʱ����ʱ��ͳ��(CALENLAR)') + xlab('ʱ��(��)') + ylab('����')
ggsave(file=file.path(dir_data,'output','mcf','����ʱ����ʱ��ͳ��(CALENLAR).png'), plot=p, width = 12, height = 9, dpi = 100)