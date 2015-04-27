rm(list = ls())

# library
library(methods)
library(DBI)
library(RMySQL)

# test
# rmysql.settingsfile<-"D:/Git/0427_xiaosong/my.cnf"
# rmysql.db<-"pmdbe"
# drv<-dbDriver("MySQL")
# con<-dbConnect(drv, user = 'root', password = 'qwer1234',
#                default.file=rmysql.settingsfile,group=rmysql.db) 

# test 1
drv<-dbDriver("MySQL")
DB_name <- 'pmdbe'
con <- dbConnect(drv, DB_name,
                 groups = "Mysql_conn", 
                 default.file = "D:/Git/Config/Mysql_conn")


# DB configure
# DB_name <- "pmdbe"
# con <- dbConnect(dbDriver('MySQL'),dbname=DB_name,
#                  user = 'root', password = 'qwer1234',
#                  host = '202.114.10.172',port = 13306)