# Recommendersystem based on the 4Play Database

# ----------------------- HowTo ----------------------- #
# 1. Mark all code and run it (maybe uncomment the install.packages statements in the packages section)

# 2. set up a db-connection using db = connectToDB(db_driver, db_name, host,  username, pw, [port])

# 2a. As our database only has about 40 likes (User likes a Song), we need to add some more entries here
# using (e.g.) createAdditionalRelations(db, 5000, 'User', 'Song', 'User_favourited_song')

# 3. call createRecommendations(db, user_table_name, other_table_name, relation_table_name, 
# user_id_col_name, other_id_col_name, user_firstname_col_name,
# user_lastname_col_name, other_printed_col_name, binary=FALSE) 
# (e.g. createRecommendations(db, 'User', 'Song', 'User_favourited_song', 'userID', 'songID', 'firstname', 'lastname', 'title')) 
# to get a list of Users with their most recommended items


# ----------------------- packages ----------------------- #
# (uncomment install.packeges if you don't have them installed)
# install.packages("recommenderlab")
library("recommenderlab")
# install.packages("RPostgreSQL")
require("RPostgreSQL")
# install.packages("RMySQL")
library("RMySQL")



# ----------------------- code ----------------------- #

# -------- establish Database connection ------ #
# create a database connection
# port is optional as sometimes you need to pass a port and sometimes not
connectToDB = function(db_driver, db_name, host,  username, pw, port=0) {
  if (port > 0){
    connection = dbConnect(db_driver, host = host, port = port, dbname = db_name, 
                           user = username, password = pw)
  }
  else {
    connection = dbConnect(db_driver, host = host, dbname = db_name, 
                           user = username, password = pw)
  }
  rm(pw)
  return(connection)
}


# -------- main execution function ------ #
# function that creates a list of lists with users as keys and a list of top 10 recommended 
# songs as value. Params:
# db: database connetion, 
# user_table_name: db-table-name of the users table, 
# other_table_name: db-table-name of the item table, 
# relation_table_name: db-table-name of the user-item-relation, 
# user_id_col_name: db-column-name of the user-ID, 
# other_id_col_name: db-column-name of the item-ID,  
# user_firstname_col_name: db-column-name of the user's firstname, 
# user_lastname_col_name: db-column-name of the user's lastname, 
# other_printed_col_name: db-column-name of the item's attribute that is supposed to be printed, 
# binary=FALSE: if true, the ranking matrix only consists of 0 and 1, otherwise of rankings with values 1-5
createRecommendations = function(db, user_table_name, other_table_name, relation_table_name, 
                                 user_id_col_name, other_id_col_name, user_firstname_col_name,
                                 user_lastname_col_name, other_printed_col_name, binary=FALSE){
  # create a ranking matrix using DB-entries
  ranking_matrix = createRanking(db, user_table_name, other_table_name, relation_table_name, binary=binary)
  
  # use that matrix to get a recommendation matrix
  rec_matrix = CFRecommender(ranking_matrix)
  
  # create a list of users with lists of most recommended items
  recommendations = list()
  
  # create dataframes out of the user, song, and user_favourited_song tables
  user_query = paste0('select * from ', user_table_name)
  user_table = dbGetQuery(db, user_query)
  
  other_query = paste0('select * from ', other_table_name)
  other_table = dbGetQuery(db, other_query)
  
  # for each user, create a list with most recommended items (at least ranking of 4)
  # and add an entry to the recommendations-list
  for(user_id in row.names(rec_matrix)){
    sorted_by_rec = sort(rec_matrix[user_id,], decreasing = TRUE)
    song_ids = names(which(sorted_by_rec >= 4))[1:10]
    
    # write the user's name instead of the ID
    user_name = paste(user_table[which(user_table[,user_id_col_name] == user_id), user_firstname_col_name], 
                      " ",
                      user_table[which(user_table[,user_id_col_name] == user_id), user_lastname_col_name],
                      " [ID: ",user_id ,"]",
                      sep="")
    
    # same applies for songs
    song_names = c()
    for (song_id in song_ids){
      if (is.na(song_id)){}
      else{
        name = paste(other_table[which(other_table[,other_id_col_name] == song_id), other_printed_col_name], 
                     " [ID: ",song_id ,"]",
                     sep="")
        song_names = c(song_names, name)
      }
      
    }
    
    recommendations[[user_name]] = song_names
  }
  print(recommendations)
}


# -------- Pre-function ------ #
# generate additional database entries for the x_favourited_y table
# where x and y are tables on their own and the x_favourited_y tables
# consists of FK-pairs pointing to the respective IDs
createAdditionalRelations = function(db_connection, number_of_new_entries, user_table_name, other_table_name, relation_table_name){
  user_query = paste0('select * from ', user_table_name)
  user_table = dbGetQuery(db_connection, user_query)
  
  other_query = paste0('select * from ', other_table_name)
  other_table = dbGetQuery(db_connection, other_query)
  
  like_query = paste0('select * from ', relation_table_name)
  like_table = dbGetQuery(db_connection, like_query)
  
  new_users = c()
  new_songs = c()
  for(i in 1:number_of_new_entries){
    rUser = floor(runif(n = 1, min = 2, max = NROW(user_table)))
    rSong = floor(runif(n = 1, min = 1, max = NROW(other_table)))
    
    new_users = append(new_users, user_table[rUser,1])
    new_songs = append(new_songs, other_table[rSong,1])
  }
  
  # get the names of the like_table columns (db-column-names)
  relation_col_names = colnames(like_table)
  
  # create a new dataframe with the respective col_names
  new_relations = data.frame(new_users, new_songs)
  colnames(new_relations) = c(relation_col_names[1], relation_col_names[2])

  # delete duplicates
  all_relations_with_potential_duplicates = rbind(like_table[1:2], new_relations)
  consistent_relations = unique(all_relations_with_potential_duplicates)

  # add the new entries to the DB
  for(j in (NROW(like_table)+1):NROW(consistent_relations)){
    #creates a random date between 01-01-2000 and 01-01-2018 (needed for the 3rd column of our user-song relation)
    randomdate = sample(seq(as.Date('2000/01/01'), as.Date('2018/01/01'), by="day"), 1)
    query = paste0("INSERT INTO ", relation_table_name," (",relation_col_names[1],",",relation_col_names[2],",",relation_col_names[3],") VALUES (",consistent_relations[j, 1],",",consistent_relations[j, 2],",","\'", randomdate, "\')")
    dbSendQuery(db_connection, query)
  }

  return(paste0(NROW(consistent_relations)-NROW(like_table), " new relations were created and written to the database!"))
}


# -------- Helper for main execution ------ #
# create User-Song-Ranking-Matrix
# as parameter give a database connection, the table names and say if the matrix should be filled with 1 only
# (which is the original DB value) or with a random value between 1 and 5
createRanking = function(db_connection, user_table_name, other_table_name, relation_table_name, binary=TRUE){
  # create dataframes out of the user, song, and user_favourited_song tables
  user_query = paste0('select * from ', user_table_name)
  user_table = dbGetQuery(db_connection, user_query)
  
  other_query = paste0('select * from ', other_table_name)
  other_table = dbGetQuery(db_connection, other_query)
  
  like_query = paste0('select * from ', relation_table_name)
  like_table = dbGetQuery(db_connection, like_query)
  
  # create the initial dataframe (Rank-Matrix)
  m = data.frame(matrix(NA, nrow = nrow(user_table), ncol = nrow(other_table)))

  # as the matrix needs to have the Users as rows and the Others (e.g. Song) as columns,
  # we need to redefine the column- / row-names here
  user_id = colnames(user_table)[1]
  row_names = sort(user_table[,user_id])
  
  other_id = colnames(other_table)[1]
  col_names = sort(other_table[,other_id])

  colnames(m) = col_names
  row.names(m) = row_names
  
  # get the ID names of the relation table
  rel_first_id = colnames(like_table)[1]
  rel_second_id = colnames(like_table)[2]
  
  # for every entry in the user_favourited_song table, set the respective matrix entry
  for (i in 1:nrow(like_table)){
    
    if (binary){
      value = 1
    }
    else {
      value = sample(c(1:5),1,replace = TRUE)
    }
    
    m[paste0(like_table[i,rel_first_id]), paste0(like_table[i,rel_second_id])] = value
  }

  # return the ranking matrix
  return(as.matrix(m))
}


# -------- Helper for main execution ------ #
# calculates the preferences matrix for CF
# input: Matrix M with ratings (rows:Users,columns:Items)
CFRecommender = function(M){
  r <- as(M, "realRatingMatrix") #creates rating matrix
  Rec.model<-Recommender(r, method = "IBCF")
  rp <- predict(Rec.model, r, type="ratings")
  return(as(rp,"matrix"))
}







# ------------------ addtional code --------------------- #
# calculates the preference matrix for CB
# input: Matrix U with ratings (rows:Users,columns:Items) and Matrix F with features (rows:Features,columns:Items)
# As we don't have useful features for our songs, we cannot apply this to our usecase. Nevertheless,
# if you want to test it simply call the function with a ratings-matrix (e.g. output of createRanking) and
# a feature matrix
CBRecommender = function(ratings_matrix,feature_matrix){
  recommendation_matrix <- matrix(0,nrow(ratings_matrix),ncol(feature_matrix)) #empty matrix
  rownames(recommendation_matrix)=rownames(ratings_matrix)
  colnames(recommendation_matrix)=colnames(feature_matrix)
  ratings_matrix[is.na(ratings_matrix)] <- 0 #replace NA with 0
  feature_matrix[is.na(feature_matrix)] <- 0
  
  #calculates the weigthed profiles
  y <- apply(ratings_matrix,1,function(x){
    y = 0
    for (i in 1:length(x)){
      y = y +((x[i]*feature_matrix[,i])/sum(x!=0))
    }
    return(y)
  })
  
  #calculates the cos-difference between each weigthed profile and item profile
  for(i in 1:nrow(recommendation_matrix)){
    for(j in 1:ncol(recommendation_matrix)){
      recommendation_matrix[i,j] = sum(feature_matrix[,j]*y[,i])/(sqrt(sum(feature_matrix[,j]^2))*sqrt(sum(y[,i]^2)))
    }
  }
  return(recommendation_matrix)
}
