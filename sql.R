# SQL
rm(list = ls())

# library and parameter
library(RMySQL)
# open a connection to remote Mysql server
drv<-dbDriver("MySQL")
DB <- 'Data'
con <- dbConnect(drv, DB,
                 groups = "Mysql_conn", 
                 default.file = "D:/Git/Config/Mysql_conn")

# create table of ip list to join with disk usage tables
name.iptable = '0427_ip'
if (!dbExistsTable(con,name.iptable)) {
  str.sql <- paste('CREATE TABLE ',name.iptable,' (ip varchar(128));',sep='')
  dbSendQuery(con, str.sql)
  dbWriteTable(con, name.iptable, ip_list,
               row.names = F, overwrite = T) 


  
# on exit
  on.exit(dbRemoveTable(con,name.iptable))
  on.exit(dbDisconnect(con))

