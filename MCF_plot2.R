# MCF plot together
rm(list = ls())
library(ggplot2)
library(gtable)
library(grid)
dir_data <- 'D:/Data/Disk Number'
load(file.path(dir_data,'data_mcf.Rda'))

mcf_age <- function(data.mcf,time_interval){
  # 原始的time_interval是1天
  data.mcf$ol_time_fail <- round(data.mcf$ol_time_fail/time_interval)
  data.mcf$start <- round(data.mcf$start/time_interval)
  data.mcf$end <- round(data.mcf$end/time_interval)
  table.ol_time_fail <- table(data.mcf$ol_time_fail)[-c(1,2)]
  tmp <- names(table.ol_time_fail)
  mcf <- data.frame(time = as.numeric(names(table.ol_time_fail)),
                    fails = as.numeric(table.ol_time_fail))
  mcf$atrisk <- sapply(mcf$time,function(x){
    #   nrow(subset(data.mcf,start < x & end > x))
    sum(data.mcf$start < x & data.mcf$end > x)
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

mcf <- mcf_age(data.mcf,30)
mcf$class <- 'baseline'
mcf <- subset(mcf,time <= 72)

#@ plot two line together#############################################
grid.newpage()

# two plots
#   p1 <- ggplot(mcf, aes(time, fails)) + geom_bar(colour = "blue",stat = 'identity') + theme_bw()
p1 <- ggplot(mcf, aes(time, mcf)) + geom_line(colour = "blue",size = 1) + xlab('time') + ylab('fails') +theme_bw()
p2 <- ggplot(mcf, aes(time, errorrate_pertime)) + geom_line(colour = "red",size = 1) + xlab('time') + ylab('servers') +theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

#@@@plot bar and line#############################################
p1 <- ggplot(mcf , aes(x=time, y=fails)) + 
  geom_bar( stat="identity",colour = "blue") +
  scale_fill_grey(start =.1, end = .7 ) +
  xlab("time") + 
  ylab("Number of Fails") + 
  theme(legend.position="top")
g1 <- ggplotGrob(p1)

# Lineplot ------------------------------------------------
p2 <- ggplot(mcf , aes(x=time, y=rr, group=1)) + geom_line(size=2,colour = "red")  + 
  scale_y_continuous(limits=c(0,0.05)) + 
  ylab("Recurrence Rate") +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
g2 <- ggplotGrob(p2)


# Add plots together
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)


# Add second axis for accuracy
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)


# Add second y-axis title 
ia <- which(g2$layout$name == "ylab")
ax <- g2$grobs[[ia]]
# str(ax) # you can change features (size, colour etc for these - 
# change rotation below 
ax$rot <- 270
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

grid.draw(g)