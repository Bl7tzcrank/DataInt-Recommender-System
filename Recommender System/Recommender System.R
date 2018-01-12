# Recommendersystem based on the 4Play Database

# ----------------------- HowTo ----------------------- #
# 1. Mark all code before the testing part and run it
# 2. set up a db-connection using connectToDB()
# 3. create the ranking-martix m using createUserSongRanking()
# 4. create the recommenation using CFRecommender(m)

# ----------------------- packages ----------------------- #
# (uncomment install.packeges if you don't have them installed)
# install.packages("recommenderlab")
library("recommenderlab")
# install.packages("RPostgreSQL")
require("RPostgreSQL")
# install.packages("RMySQL")
# library("RMySQL")



# ----------------------- code ----------------------- #

# create a database connection
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

# generates additional user-song-relations for the user_favourited_song table
createUserSongRelations = function(connection, amountofnewdata){
  number_of_new_relations = amountofnewdata
  users = dbGetQuery(connection, "SELECT * from users")
  songs = dbGetQuery(connection, "SELECT * from song")
  user_favourited_song = dbGetQuery(connection, "SELECT * from user_favourited_song")
  
  new_users = c()
  new_songs = c()
  for(i in 1:number_of_new_relations){
    rUser = floor(runif(n = 1, min = 2, max = NROW(users)))
    rSong = floor(runif(n = 1, min = 1, max = NROW(songs)))
    
    new_users = append(new_users, users[rUser,1])
    new_songs = append(new_songs, songs[rSong,1])
  }
  new_relations = data.frame('userid' = new_users, 'songid' = new_songs)
  all_relations_with_potential_duplicates = rbind(user_favourited_song[1:2], new_relations)
  
  consistent_relations = unique(all_relations_with_potential_duplicates)
  
  
  for(j in (NROW(user_favourited_song)+1):NROW(consistent_relations)){
    #creates a random date between 01-01-2000 and 01-01-2018 (needed for the 3rd column of our user-song relation)
    randomdate = sample(seq(as.Date('2000/01/01'), as.Date('2018/01/01'), by="day"), 1)
    query = paste0("INSERT INTO user_favourited_song (userid, songid, date) VALUES (",consistent_relations[j, 1],",",consistent_relations[j, 2],",","\'", randomdate, "\')")
    dbSendQuery(connection, query)
  }
  
  return(paste0(NROW(consistent_relations)-NROW(user_favourited_song), " new user-song relations were created and written to the database!"))
}

# generates additional user-album-relations for the user_favourited_album table
createUserAlbumRelations = function(connection, amountofnewdata){
  number_of_new_relations = amountofnewdata
  users = dbGetQuery(connection, "SELECT * from users")
  albums = dbGetQuery(connection, "SELECT * from album")
  user_favourited_album = dbGetQuery(connection, "SELECT * from user_favourited_album")
  
  new_users = c()
  new_albums = c()
  for(i in 1:number_of_new_relations){
    rUser = floor(runif(n = 1, min = 2, max = NROW(users)))
    rAlbum = floor(runif(n = 1, min = 1, max = NROW(albums)))
    
    new_users = append(new_users, users[rUser,1])
    new_albums = append(new_albums, albums[rAlbum,1])
  }
  new_relations = data.frame('userid' = new_users, 'albumid' = new_albums)
  all_relations_with_potential_duplicates = rbind(user_favourited_album[1:2], new_relations)
  
  consistent_relations = unique(all_relations_with_potential_duplicates)
  
  
  for(j in (NROW(user_favourited_album)+1):NROW(consistent_relations)){
    #creates a random date between 01-01-2000 and 01-01-2018 (needed for the 3rd column of our user-album relation)
    randomdate = sample(seq(as.Date('2000/01/01'), as.Date('2018/01/01'), by="day"), 1)
    query = paste0("INSERT INTO user_favourited_album (userid, albumid, date) VALUES (",consistent_relations[j, 1],",",consistent_relations[j, 2],",","\'", randomdate, "\')")
    dbSendQuery(connection, query)
  }
  
  return(paste0(NROW(consistent_relations)-NROW(user_favourited_album), " new user-album relations were created and written to the database!"))
}

# generates additional user-playlist-relations for the user_favourited_playlist table
createUserPlaylistRelations = function(connection, amountofnewdata){
  number_of_new_relations = amountofnewdata
  users = dbGetQuery(connection, "SELECT * from users")
  playlists = dbGetQuery(connection, "SELECT * from playlist")
  user_favourited_playlist = dbGetQuery(connection, "SELECT * from user_favourited_playlist")
  
  new_users = c()
  new_playlists = c()
  for(i in 1:number_of_new_relations){
    rUser = floor(runif(n = 1, min = 2, max = NROW(users)))
    rPlaylist = floor(runif(n = 1, min = 1, max = NROW(playlists)))
    
    new_users = append(new_users, users[rUser,1])
    new_playlists = append(new_playlists, playlists[rPlaylist,1])
  }
  new_relations = data.frame('userid' = new_users, 'playlistid' = new_playlists)
  all_relations_with_potential_duplicates = rbind(user_favourited_playlist[1:2], new_relations)
  
  consistent_relations = unique(all_relations_with_potential_duplicates)
  
  
  for(j in (NROW(user_favourited_playlist)+1):NROW(consistent_relations)){
    #creates a random date between 01-01-2000 and 01-01-2018 (needed for the 3rd column of our user-playlist relation)
    randomdate = sample(seq(as.Date('2000/01/01'), as.Date('2018/01/01'), by="day"), 1)
    query = paste0("INSERT INTO user_favourited_playlist (userid, playlistid, date) VALUES (",consistent_relations[j, 1],",",consistent_relations[j, 2],",","\'", randomdate, "\')")
    dbSendQuery(connection, query)
  }
  
  return(paste0(NROW(consistent_relations)-NROW(user_favourited_playlist), " new user-playlist relations were created and written to the database!"))
}

# create User-Song-Ranking-Matrix
# as parameter give a database connection and say if the matrix should be filled with 1 only
# (which is the original DB value) or with a random value between 1 and 5
createUserSongRanking = function(db_connection, user_table_name, song_table_name, relation_name, binary=TRUE){
  # create dataframes out of the user, song, and user_favourited_song tables
  user_query = paste('select * from', user_table_name, sep=' ')
  user_table = dbSendQuery(db_connection, user_query)
  user_df = fetch(user_table)
  
  song_query = paste('select * from', song_table_name, sep=' ')
  song_table = dbSendQuery(db_connection, song_query)
  song_df = fetch(song_table)
  
  like_query = paste('select * from', relation_name, sep=' ')
  like_table = dbSendQuery(db_connection, like_query)
  like_df = fetch(like_table)
  
  # create the initial dataframe (Rank-Matrix)
  m = data.frame(matrix(NA, nrow = nrow(user_df), ncol = nrow(song_df)))
  colnames(m) = song_df$songID
  row.names(m) = user_df$userID
  
  # for every entry in the user_favourited_song table, set the respective matrix entry
  for (i in 1:nrow(like_df)){
    if (binary){
      value = 1
    }
    else {
      value = sample(c(1:5),1,replace = TRUE)
    }
    m[like_df[i,]$userID, like_df[i,]$songID] = value
  }

  # return the ranking matrix
  return(as.matrix(m))
}

#calculates the preferences matrix for CF
#input: Matrix M with ratings (rows:Users,columns:Items)
CFRecommender = function(M){
  r <- as(M, "realRatingMatrix") #creates rating matrix
  Rec.model<-Recommender(r, method = "IBCF")
  rp <- predict(Rec.model, r, type="ratings")
  return(as(rp,"matrix"))
}

#calculates the preference matrix for CB
#input: Matrix U with ratings (rows:Users,columns:Items) and Matrix F with features (rows:Features,columns:Items)
CBRecommender = function(U,F){
  WU <- matrix(0,nrow(U),ncol(F)) #empty matrix
  rownames(WU)=rownames(U)
  colnames(WU)=colnames(F)
  U[is.na(U)] <- 0 #replace NA with 0
  F[is.na(F)] <- 0
  
  #calculates the weigthed profiles
  y <- apply(U,1,function(x){
    y = 0
    for (i in 1:length(x)){
      y = y +((x[i]*F[,i])/sum(x!=0))
    }
    return(y)
  })
  
  #calculates the cos-difference between each weigthed profile and item profile
  for(i in 1:nrow(WU)){
    for(j in 1:ncol(WU)){
      WU[i,j] = sum(F[,j]*y[,i])/(sqrt(sum(F[,j]^2))*sqrt(sum(y[,i]^2)))
    }
  }
  return(WU)
}
