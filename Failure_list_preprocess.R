# failure list duplication, statistic failure times, extract first failure time
rm(list = ls())

# library and parameter
# library(RMySQL)
dir_data <- 'D:/Data/0427_xiaosong/'
in_name <- 'bad_ori(0401_1231).csv'
out_name <- 'bad_pre(0401_1231).csv'
in_path <- paste(dir_data,in_name,sep='')
out_path <- paste(dir_data,out_name,sep='')

#read data
data.ip <- read.csv(in_path)
names(data.ip) <- c('sn','ip','f_time')
data.ip$f_time <- as.POSIXct(data.ip$f_time)

#delete no ip | 
data.ip_order <- data.ip[with(data.ip,order(ip,f_time)),]
data.ip_order <- data.ip_order[data.ip_order$ip!='',]           # delete no ip
data.ip_order$ip <- factor(data.ip_order$ip)                    # reconstruct factor of ip

#duplication: ip
data.ip_order_dup <- data.ip_order[!duplicated(data.ip_order$ip),]
rownames(data.ip_order_dup) <- NULL
data.ip_order_dup$ip <- as.character(data.ip_order_dup$ip)
data.ip_order_dup$sn <- NULL
write.csv(file = out_path, x = data.ip_order_dup, row.names=FALSE)










#duplication: same ip & more than one f_time in three days (preserve the first one).
# data.ip_order_dup <- data.frame('ip' = rep(data.ip_order$ip[1],length(unique(data.ip_order$ip))),
#                                 'f_time' = data.ip_order$f_time[1],
#                                 'f_count' = 1)
# pre_ip <- data.ip_order$ip[1]
# pre_f_time <- data.ip_order$f_time[1]
# count <- 1
# for (i in 2:nrwo(data.ip_order)) {
#   cur_ip <- data.ip_order$ip[i]
#   cur_f_time <- data.ip_order$f_time[i]
#   if (cur_ip != pre_ip) {
#     data.ip_order_dup[count,] <- 
#   }
# }












#duplication: same ip & more than one f_time in three days (preserve the first one).
# idx.ip <- tapply(1:nrow(data.ip_order),data.ip_order$ip,function(x)return(x))
# len.ip <- as.numeric(tapply(1:nrow(data.ip_order),data.ip_order$ip,length))
# data.ip_order_dup <- data.frame('ip' = levels(data.ip_order$ip), 'f_time'=0, 'f_count'=1)
# for (i in 1:length(idx.ip)){
#   print(i)
#   cur.idx <- idx.ip[[i]]
#   if (length(cur.idx) == 1 & 
#         data.ip_order$ip[cur.idx[1]] == data.ip_order_dup$ip[i]) {
#     data.ip_order_dup$f_time[i] <- data.ip_order$f_time
#   } else {
#     cur.f_time <- data.ip_order$f_time[cur.idx]
#     len <- length(cur.f_time)
#     diff.f_time <- cur.f_time[2:len]-cur.f_time[1:(len-1)]
#     data.ip_order_dup$f_time[i] <- cur.f_time[1]
#     data.ip_order_dup$f_count[i] <- sum(diff.f_time>3*24*60*60)
#   }
# }
