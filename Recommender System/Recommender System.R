install.packages("recommenderlab")
library("recommenderlab")
install.packages("RPostgreSQL")
require("RPostgreSQL")
install.packages("RMySQL")
library("RMySQL")

# create a connection to postgres DB
connectPostgres <-function(){
  pw <- {"Password"}
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "Data Integration",
                   host = "localhost", port = 5432,
                   user = "postgres", password = pw)
  rm(pw)
  return (con)
}

#create a connection to MySQL DB (not tested yet)
connectMySQL <-function(){
  pw <- {"PASSWORD"}
  con <- dbConnect(MySQL(), user= "user", password= pw, 
                   dbname="database_name", host="localhost")
  rm(pw)
  return (con)
}

#for testing purpose
con = connectPostgres()
dbExistsTable(con, "album")
dbGetQuery(con, "SELECT * from album")