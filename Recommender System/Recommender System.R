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

#for testing purpose database
con = connectPostgres()
dbExistsTable(con, "album")
dbGetQuery(con, "SELECT * from album")


#for testing purpose recommender system
A = matrix(c(1,NA,3,NA,NA,5,NA,NA,5,NA,4,NA,
        NA,NA,5,4,NA,NA,4,NA,NA,2,1,3,
        2,4,NA,1,2,NA,3,NA,4,3,5,NA,
        NA,2,4,NA,5,NA,NA,4,NA,NA,2,NA,
        NA,NA,4,3,4,2,NA,NA,NA,NA,2,5,
        1,NA,3,NA,3,NA,NA,2,NA,NA,4,NA),nrow=6,ncol=12,byrow = TRUE)
rownames(A) <- paste('movie', 1:6)
colnames(A) <- paste('user', 1:12)
#A <- t(A)
r <- as(A, "realRatingMatrix") #creates rating matrix
getRatingMatrix(r) #for testing
r_m <- normalize(r) #normalization
getRatingMatrix(r_m) #for testing

Rec.model<-Recommender(r, method = "UBCF")
re <- predict(Rec.model, r, n=6)
as(re, "list")
m <- predict(Rec.model, r, type="ratings")
as(m, "list")







