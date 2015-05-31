# failure list duplication, statistic failure times, extract first failure time
rm(list = ls())

# library and parameter
# library(RMySQL)
dir_data <- 'D:/Data/0427_xiaosong/'
in_name <- 'uwork_20120101-20131210.csv'
in_name1 <- '¹ÊÕÏµ¥¾«¼ò_06-09.csv'
out_name <- 'bad_pre(0401_1231).csv'
in_path <- paste(dir_data,in_name,sep='')
in_path1 <- paste(dir_data,in_name1,sep='')
out_path <- paste(dir_data,out_name,sep='')

#read data
data.fl <- read.csv(in_path)
data.fl1 <- read.csv(in_path1)
names(data.fl) <- c('id','svr_id','ip','ftype','ori_ftype',
                    'f_time','ftype_d1','ftype_d2','ftype_d')
names(data.fl1) <- c('id','svr_id','ip','ftype','idc','dev_class_name','dev_class_id',
                     'dept_id','f_time','f_desc','ftype_d1','ftype_d2')
col_need <- c('svr_id','ip','ftype','f_time','ftype_d1','ftype_d2')
data.fl <- data.fl[,col_need]
data.fl1 <- data.fl1[,col_need]
data.fl <- rbind(data.fl,data.fl1)
data.fl$f_time <- as.POSIXct(data.fl$f_time,tz = 'UTC')

#del space
data.fl$ftype[data.fl$ftype == 'Ó²ÅÌ¹ÊÕÏ£¨ÓÐÈßÓà£© '] <- 'Ó²ÅÌ¹ÊÕÏ£¨ÓÐÈßÓà£©'
data.fl$ftype <- factor(data.fl$ftype)

#add class
data.fl$class <- -1
# data.replace <- subset(data.fl,ftype_d1 == 'Ó²ÅÌ¹ÊÕÏ;' | ftype_d2 == 'Ó²ÅÌ¹ÊÕÏ;')
ftlist <- c('Ó²ÅÌ¹ÊÕÏ£¨ÓÐÈßÓà£©','Ó²ÅÌ¹ÊÕÏ£¨ÓÐÈßÓà£¬²ÛÎ»Î´Öª£©',
            'Ó²ÅÌ¹ÊÕÏ£¨ÎÞÈßÓà£©','Ó²ÅÌ¹ÊÕÏ£¨ÎÞÈßÓà£¬ÔÚÏß»»ÅÌ£©',
            'Ó²ÅÌ¼´½«¹ÊÕÏ£¨ÓÐÈßÓà£©','²Ù×÷ÏµÍ³Ó²ÅÌ¹ÊÕÏ£¨ÎÞÈßÓà£©')


for (i in 1:length(ftlist)){
  data.fl$class[data.fl$ftype == ftlist[i]] <- i
  data.fl$class[data.fl$ftype == ftlist[i] & 
                  (data.fl$ftype_d1 == 'Ó²ÅÌ¹ÊÕÏ;' | data.fl$ftype_d2 == 'Ó²ÅÌ¹ÊÕÏ;')] <- i+6
}



#delete no ip | 
data.fl_order <- data.fl[with(data.fl,order(ip,f_time)),]
data.fl_order <- data.fl_order[data.fl_order$ip!='',]           # delete no ip
data.fl_order$ip <- factor(data.fl_order$ip)                    # reconstruct factor of ip

#duplication: ip
fcount <- tapply(data.fl_order$ip,data.fl_order$ip,length)
data.flist <- data.fl_order[!duplicated(data.fl_order$ip),]  #preserve one failure
data.flist$fcount[match(names(fcount),data.flist$ip)] <- fcount
rownames(data.flist) <- NULL
write.csv(file = out_path, x = data.flist, row.names=FALSE)
data.bad <- subset(data.flist,class!=-1)
save(data.flist,data.bad,file = paste(dir_data,'flist(0401-1231).Rda',sep = ''))

#duplication: same ip & more than one f_time in three days (preserve the first one).
# data.fl_order_dup <- data.frame('ip' = rep(data.fl_order$ip[1],length(unique(data.fl_order$ip))),
#                                 'f_time' = data.fl_order$f_time[1],
#                                 'f_count' = 1)
# pre_ip <- data.fl_order$ip[1]
# pre_f_time <- data.fl_order$f_time[1]
# count <- 1
# for (i in 2:nrwo(data.fl_order)) {
#   cur_ip <- data.fl_order$ip[i]
#   cur_f_time <- data.fl_order$f_time[i]
#   if (cur_ip != pre_ip) {
#     data.fl_order_dup[count,] <- 
#   }
# }
#duplication: same ip & more than one f_time in three days (preserve the first one).
# idx.ip <- tapply(1:nrow(data.fl_order),data.fl_order$ip,function(x)return(x))
# len.ip <- as.numeric(tapply(1:nrow(data.fl_order),data.fl_order$ip,length))
# data.fl_order_dup <- data.frame('ip' = levels(data.fl_order$ip), 'f_time'=0, 'f_count'=1)
# for (i in 1:length(idx.ip)){
#   print(i)
#   cur.idx <- idx.ip[[i]]
#   if (length(cur.idx) == 1 & 
#         data.fl_order$ip[cur.idx[1]] == data.fl_order_dup$ip[i]) {
#     data.fl_order_dup$f_time[i] <- data.fl_order$f_time
#   } else {
#     cur.f_time <- data.fl_order$f_time[cur.idx]
#     len <- length(cur.f_time)
#     diff.f_time <- cur.f_time[2:len]-cur.f_time[1:(len-1)]
#     data.fl_order_dup$f_time[i] <- cur.f_time[1]
#     data.fl_order_dup$f_count[i] <- sum(diff.f_time>3*24*60*60)
#   }
# }
