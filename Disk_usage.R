# select disk usage information based on a iplist
rm(list = ls())

# library
library(RMySQL)
dir_data <- 'D:/Data/0427_xiaosong/'

# exec -> function(DB,Table) {
  DB <- 'pmdbe'
  Table <- 'd_disk_usage_sum_5'
  # open a connection to remote Mysql server
  drv<-dbDriver("MySQL")
  con <- dbConnect(drv, DB,
                   groups = "Mysql_conn", 
                   default.file = "D:/Git/Config/Mysql_conn")
  
  
  # read ip list
  csv_file <- 'ip_0401_1231.csv'
  csv_path <- paste(dir_data,csv_file,sep='')
  ip_list <- read.csv(csv_path)
  
  # create table of ip list to join with disk usage tables
  name.iptable = '0427_ip'
  if (!dbExistsTable(con,name.iptable)) {
    str.sql <- paste('CREATE TABLE ',name.iptable,' (ip varchar(128));',sep='')
    dbSendQuery(con, str.sql)
    dbWriteTable(con, name.iptable, ip_list,
                 row.names = F, overwrite = T)
  }
  
  
  
  
  # tables for selection'
  str.sql <- sprintf('SELECT a.* from %s a, %s b
                     where a.ip = b.ip;', Table, name.iptable)
#   r <- dbGetQuery(con,str.sql)
  
  
  # on exit
#   on.exit(dbRemoveTable(con,name.iptable))
  on.exit(dbDisconnect(con))