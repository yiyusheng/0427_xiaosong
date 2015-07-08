# MCF function

##########################################
#@@@ ����
# 1. MCF�㷨(AGE).
mcf_age <- function(data.mcf,time_interval){
  # ԭʼ��time_interval��1��
  data.mcf$ol_time_fail <- round(data.mcf$ol_time_fail/time_interval)
  data.mcf$start <- round(data.mcf$start/time_interval)
  data.mcf$end <- round(data.mcf$end/time_interval)
  table.ol_time_fail <- table(data.mcf$ol_time_fail)[-c(1,2)]
  tmp <- names(table.ol_time_fail)
  # 1. ʱ������ϻ�����
  mcf <- data.frame(time = as.numeric(names(table.ol_time_fail)),
                    fails = as.numeric(table.ol_time_fail))
  # 2. ���߻�����
  mcf$atrisk <- sapply(mcf$time,function(x){
    #   nrow(subset(data.mcf,start < x & end > x))
    sum(data.mcf$start < x & data.mcf$end > x)
  })
  # 3. ����ʱ��Ĺ�����
  mcf$errorrate_pertime <- mcf$fails/mcf$atrisk
  # 4. MCF����
  mcf$mcf <- sapply(1:nrow(mcf),function(x){
    sum(mcf$errorrate_pertime[1:x])
  })
  # 5. RR ����
  len <- nrow(mcf)
  diff <- (mcf$mcf[2:len] - mcf$mcf[1:(len-1)])/(mcf$time[2:len] - mcf$time[1:(len-1)])
  diff <- c(0,diff)
  mcf$rr <- diff
  # 6. �������߻�����ռ������߻������ı������ڹ���
  mcf$frac_max <- mcf$atrisk/max(mcf$atrisk)
  # 7. ����stand_mcf��,Ϊ�˿���rbind
  mcf$stand_mcf <- 0
  return(mcf)
}

# 2. MCF�㷨(CALENDAR)
mcf_cal <- function(data.mcf,time_interval) {
  # ԭʼ��time_interval��1��
  data.mcf$ol_time_fail_c <- round(data.mcf$ol_time_fail_c/time_interval)
  data.mcf$start_c <- round(data.mcf$start_c/time_interval)
  data.mcf$end_c <- round(data.mcf$end_c/time_interval)
  table.ol_time_fail <- table(data.mcf$ol_time_fail_c)[-c(1,2)]
  tmp <- names(table.ol_time_fail)
  mcf <- data.frame(time = as.numeric(names(table.ol_time_fail)),
                    fails = as.numeric(table.ol_time_fail))
  mcf$atrisk <- sapply(mcf$time,function(x){
    #   nrow(subset(data.mcf,start < x & end > x))
    sum(data.mcf$start_c < x & data.mcf$end_c > x)
  })
  mcf$errorrate_pertime <- mcf$fails/mcf$atrisk
  mcf$mcf <- sapply(1:nrow(mcf),function(x){
    sum(mcf$errorrate_pertime[1:x])
  })
  len <- nrow(mcf)
  diff <- (mcf$mcf[2:len] - mcf$mcf[1:(len-1)])/(mcf$time[2:len] - mcf$time[1:(len-1)])
  diff <- c(0,diff)
  mcf$rr <- diff
  return(mcf)
}

# 3. ��ͼ������.������Ա�������
mcf_plot <- function(mcf,time_limit,prefix,frac_m) {
  line_size = 2
  # 1. ʱ�����&����������
  # ʱ�䴦��
  if (time_limit == -1){
    time_limit <- max(mcf$time)
  }
  mcf_p <- subset(mcf,time <= time_limit & 
                    atrisk > 50 & 
                    frac_max > frac_m)

  # 2. ��ͼ
  # 2.1 MCF
  geo_aes <- aes(x = time,y = mcf,group = classNew, color = classNew)
  p1 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size)
  # 2.2 error rate
  geo_aes <- aes(x = time,y = errorrate_pertime,group = classNew, color = classNew)
  p2 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size)
  # 2.3 fails count
  geo_aes <- aes(x = time,y = fails,group = classNew, color = classNew)
  p3 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size)
  # 2.4 atrisks count
  geo_aes <- aes(x = time,y = atrisk,group = classNew, color = classNew)
  p4 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size)
  # 2.5 recurrence rate
  geo_aes <- aes(x = time,y = rr,group = classNew, color = classNew)
  p5 <- ggplot(subset(mcf_p,rr != 0,time < time_limit),geo_aes) + geom_line(size = line_size)
  # 2.6 stand_mcf
  geo_aes <- aes(x = time,y = stand_mcf,group = classNew, color = classNew)
  p6 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size)
  
  # 3. ����
  ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'mcf.png',sep='_')), plot=p1, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'rate.png',sep='_')), plot=p2, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'fails.png',sep='_')), plot=p3, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'atrisk.png',sep='_')), plot=p4, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'rr.png',sep='_')), plot=p5, width = 12, height = 9, dpi = 100)
  ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'smcf.png',sep='_')), plot=p6, width = 12, height = 9, dpi = 100)
  return(list(p_mcf=p1,p_er=p2,p_fail=p3,p_risk=p4,p_rr=p5,p_sm=p6))
}

# 4. ��Ա�������: ����classȷ��һ����Ϊ��׼��,���������߹��������ߵı���
mcf_sc <- function(mcf,stand_class){
  tmp <- subset(mcf,class = stand_class)
  mcf$stand_mcf <- tmp$mcf[match(mcf$time,tmp$time)]
  mcf$stand_mcf <- mcf$mcf/mcf$stand_mcf
  return(mcf)
}

# 5. ���㲢�ϳ���������MCF
mcf_merge <- function(mcf_item_age,item,item_need,
                      config_item,ti,class_suffix,title){
  for (i in 1:length(item_need)) {
    eval(parse(text = sprintf('config_item_sub <- subset(config_item,%s == item_need[i])',item)))
    if (nrow(subset(config_item_sub,ol_time_fail!= -1)) <= 20) next
    tmp <- mcf_age(subset(config_item_sub),ti)
    tmp$class <- paste(item_need[i],class_suffix,sep='')
    tmp$classNew <- paste(item_need[i],class_suffix,
                          '[',length(unique(config_item_sub$ip)),'/',
                          nrow(subset(config_item_sub,ol_time_fail!= -1)),']',sep='')
    mcf_item_age <- rbind(mcf_item_age,tmp)
    # fails and atrisk plot
    png(file = file.path(dir_data,'output','mcf','fail_atrisk',paste(title,item_need[i],'fa.png',sep = '_')),width = 1200,height = 900,units = 'px')
    g_plot <- line_plot_2yaxis(subset(tmp,,c('time','fails','atrisk')))
    grid.draw(g_plot)
    dev.off()
  }
  return(mcf_item_age)
}

##########################################