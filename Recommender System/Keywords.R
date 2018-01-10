install.packages("RPostgreSQL")
require("RPostgreSQL")

connectPostgres <-function(){
  pw <- {"pw"}
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "4PlayDB",
                   host = "localhost", port = 5432,
                   user = "postgres", password = pw)
  rm(pw)
  return (con)
}

con = connectPostgres()
dbExistsTable(con, "album_content")
dbGetQuery(con, "Insert into 'keyword' values (1,'hi')")

#Keywords for songs
keywords = c("love", "hate", "betrayal", "friendship", "relationship", "breakup",
             "drums", "bass", "piano", "guitar", "horn", "saxophone", "electronic guitar",
             "speed")