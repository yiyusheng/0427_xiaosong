# ���ڻ��ͺ�ҵ��������������Ĵ���.
rm(list = ls())
require(ggplot2)
source('MCF_function.R')
source('plot_2yaxis.R')

#@@@ PARAMETERS
dir_data <- 'D:/Data/Disk Number'
ti <- 30
time_need <- 4*round(365/ti)
frac_max <- 0.1

#@@@ LOAD DATA
load(file.path(dir_data,'disk_number_label.Rda'))
load(file.path(dir_data,'mcf_all_age.Rda'))
dev_need <- c('TS1','TS4','TS5','TS6','TS8',
              'C1','M1',
              'A1','A5',
              'B6','B5')
bs1_need <- c('CC_LOL','΢��','[��Ѷ��]','CC_��Խ����','[N][Qzone]',
              'matrix���⻯��Ӫ֧��','[SNG][QQ���]','�ֻ�QQ','���ݲֿ�',
              '[TEG][�ƴ洢]')
data.config$bs1 <- cmdb$bs1[match(data.config$ip,cmdb$ip)]
data.config$bs3 <- cmdb$bs3[match(data.config$ip,cmdb$ip)]
cmdb <- subset(cmdb,use_time > as.POSIXct('2010-01-01'))
# cmdb <- subset(cmdb,dev_class_id %in% dev_need & bs1 %in% bs1_need)
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
# data.config <- subset(data.config,dev_class_id %in% dev_need  & bs1 %in% bs1_need)

tmp1 <- paste(data.config$dev_class_id,data.config$bs1,sep='_')
tmp2 <- sort(table(tmp1))
tmp2 <- tmp2[tmp2>500]

# 1. [SNG][QQ���]��C1,B6,TS1,TS4,TS6,TS8,A5�е�MCF
tmp_dev <- c('C1','B6','A5',
             'TS4','TS6','TS8')
tmp_bs1 <- '[SNG][QQ���]'

item <- 'dev_class_id'
class_suffix <- ''
title <- 'P1_bs1_QQ���'
stand_class <- 'baseline'
config_item <- subset(data.config,bs1 == '[SNG][QQ���]')
item_need <- tmp_dev
mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_P1 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_P1,time_need,title,frac_max)',item)))

# 2. C1��SATA2,SATA3��B6��SAS�ӿڵĶԱ�.ʹ��C1+��Ѷ�ƴ���sata2,C1+΢�Ŵ���sata3,B6+��Ѷ��/΢��/CC_LOL����sas
item <- 'interface'
class_suffix <- ''
title <- 'P2_inter'
stand_class <- 'baseline'
bs_need <- c('CC_LOL','΢��','[��Ѷ��]')
config_itemA1 <- subset(data.config,dev_class_id == 'C1' & disk_inter == 'SATA2')
config_itemA2 <- subset(data.config,dev_class_id == 'C1' & disk_inter == 'SATA3')
config_itemB <- subset(data.config,dev_class_id == 'B6' & bs1 %in% bs_need)
config_itemA1$interface <- 'SATA2'
config_itemA2$interface <- 'SATA3'
config_itemB$interface <- 'SAS'
config_item <- rbind(config_itemA1,config_itemA2,config_itemB)
item_need <- unique(config_item$interface)

mcf_item_age <- mcf_all_age
mcf_item_age <- mcf_merge(mcf_item_age,item,item_need,
                          config_item,ti,class_suffix,title)
mcf_item_age_P2 <- mcf_sc(mcf_item_age,stand_class)
eval(parse(text = sprintf('p_%s <- mcf_plot(mcf_item_age_P2,time_need,title,frac_max)',item)))

# 3. �Զ��̻���sata2��sata3���бȽ�