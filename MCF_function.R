# MCF function
require(scales)
##########################################
#@@@ 函数
# 1. MCF算法(AGE).
mcf_age <- function(data.mcf,time_interval){
  # 原始的time_interval是1天
  data.mcf$ol_time_fail <- round(data.mcf$ol_time_fail/time_interval)
  data.mcf$start <- round(data.mcf$start/time_interval)
  data.mcf$end <- round(data.mcf$end/time_interval)
  table.ol_time_fail <- table(data.mcf$ol_time_fail)
  table.ol_time_fail <- table.ol_time_fail[2:length(table.ol_time_fail)]
  tmp <- names(table.ol_time_fail)
  # 1. 时间与故障机器数
  mcf <- data.frame(time = as.numeric(names(table.ol_time_fail)),
                    fails = as.numeric(table.ol_time_fail))
  # 2. 在线机器数(因为故障机器是有重复的,要把这部分重复去掉)
  tmp <- subset(data.mcf,dup == T)
  mcf$atrisk <- sapply(mcf$time,function(x){
    tmp1 <- subset(tmp,start < x & end >= x)
    sum(tmp1$disk_cNew)
#     nrow(tmp1)
  })
  # 3. 关于时间的故障率
  mcf$errorrate_pertime <- mcf$fails/mcf$atrisk
  # 4. MCF计算
  mcf$mcf <- sapply(1:nrow(mcf),function(x){
    sum(mcf$errorrate_pertime[1:x])
  })
  # 5. RR 计算
  len <- nrow(mcf)
  diff <- (mcf$mcf[2:len] - mcf$mcf[1:(len-1)])/(mcf$time[2:len] - mcf$time[1:(len-1)])
  diff <- c(0,diff)
  mcf$rr <- diff
  # 6. 计算在线机器数占最大在线机器数的比例用于过滤
  mcf$frac_max <- mcf$atrisk/max(mcf$atrisk)
  # 7. 添加stand_mcf列,为了可以rbind
  mcf$stand_mcf <- 0
  return(mcf)
}

# 2. MCF算法(CALENDAR)
mcf_cal <- function(data.mcf,time_interval) {
  # 原始的time_interval是1天
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

# 3. 作图并保存.加入相对比例计算
mcf_plot <- function(mcf,time_limit,prefix,frac_m,title) {
  line_size = 2
  # 1. 时间过滤&按比例过滤
  # 时间处理
  if (time_limit == -1){
    time_limit <- max(mcf$time)
  }
  mcf_p <- subset(mcf,time <= time_limit & 
                    atrisk > 50 & 
                    frac_max > frac_m)
  # 2. 作图
  # 2.1 MCF
  geo_aes <- aes(x = time,y = mcf,group = classNew, color = classNew)
  p1 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size) + 
    xlab('Time(month)') + ylab('MCF') + 
    ggtitle(title) + 
    scale_color_discrete(name = 'Class') +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  
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
  p6 <- ggplot(mcf_p,geo_aes) + geom_line(size = line_size) + 
    xlab('Time(month)') + ylab('Stand_MCF') + 
    ggtitle(title) +
    scale_color_discrete(name = 'Class') +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  
  # 3. 保存
  ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'mcf.png',sep='_')), plot=p1, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'rate.png',sep='_')), plot=p2, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'fails.png',sep='_')), plot=p3, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'atrisk.png',sep='_')), plot=p4, width = 12, height = 9, dpi = 100)
  #   ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'rr.png',sep='_')), plot=p5, width = 12, height = 9, dpi = 100)
  ggsave(file=file.path(dir_data,'output','mcf',paste(prefix,'smcf.png',sep='_')), plot=p6, width = 12, height = 9, dpi = 100)
  return(list(p_mcf=p1,p_er=p2,p_fail=p3,p_risk=p4,p_rr=p5,p_sm=p6))
}

# 4. 相对比例计算: 根据class确定一条线为标准线,计算其它线关于这条线的比例
mcf_sc <- function(mcf,stand_class){
  tmp <- subset(mcf,class = stand_class)
  mcf$stand_mcf <- tmp$mcf[match(mcf$time,tmp$time)]
  mcf$stand_mcf <- mcf$mcf/mcf$stand_mcf
  return(mcf)
}

# 5. 计算并合成所有类别的MCF
mcf_merge <- function(mcf_item_age,item,item_need,
                      config_item,ti,class_suffix,title){
  for (i in 1:length(item_need)) {
    eval(parse(text = sprintf('config_item_sub <- subset(config_item,%s == item_need[i])',item)))
    if (nrow(subset(config_item_sub,ol_time_fail!= -1)) < 10) next
    tmp <- mcf_age(subset(config_item_sub),ti)
    tmp1 <- subset(config_item_sub,dup == T)
    tmp$class <- paste(item_need[i],class_suffix,sep='')
    tmp$classNew <- paste(item_need[i],class_suffix,
                          '[',sum(tmp1$disk_cNew),'/',
                          nrow(subset(config_item_sub,ol_time_fail!= -1)),']',sep='')
    mcf_item_age <- rbind(mcf_item_age,tmp)
    # fails and atrisk plot
#     png(file = file.path(dir_data,'output','mcf','fail_atrisk',paste(title,item_need[i],'fa.png',sep = '_')),width = 1200,height = 900,units = 'px')
#     g_plot <- line_plot_2yaxis(subset(tmp,,c('time','fails','atrisk')))
#     grid.draw(g_plot)
#     dev.off()
  }
  return(mcf_item_age)
}

##########################################