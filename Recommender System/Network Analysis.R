# Network analysis based on the 4Play Database

# ------------------------------------ HowTo ------------------------------------ #
# Let's see ...

# ------------------------------------ Package installations ------------------------------------ #
install.packages("igraph")
library("igraph")
install.packages("RPostgreSQL")
require("RPostgreSQL")
install.packages("RMySQL")
require("RMySQL")

# ------------------------------------ Database connection ------------------------------------ #
# choose your settings and set up a the respective connection (simply run the lines that fit)
driver = dbDriver("PostgreSQL")
driver = MySQL()

dbname = 'DataIntegration'
dbname = '4Play'

host = '127.0.0.1'

port = 5432
port = 3006

user = 'postgres'
user = 'root'

password = 'pw'
password = ''

con = dbConnect(driver, dbname = dbname,
                  host = host, port = port,user = user, password = password)

con = dbConnect(driver, dbname = dbname,
                host = host, user = user, password = password)


 
# ------------------------------------ Helper Function declaration ------------------------------------ #

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

# Takes a dataframe (for example user_favourited songs) and calculates the edgelist of the first
# two columns where as the first column is the entity for vertices and if there is the same column2 entry
# for a different column1 entry the two corresponding column1 entries will be c1 and c2 for the edgelist.
# For example: userid 1 favoures songid 2 and userid 10 favours songid 2, one row of the upcoming edgelist would be
# 1 - 10 - weight. The weight is determined by the number of same favourited songs. If userid 1 and 10 would only
# have this one songid (2) in common the weight would be 1. 
createWeightedGraph = function(data){
  c1 = c()
  c2 = c()
  weight = c()
  comparison = list()
  comparison[[1]] = as.numeric(c(0,0,0)) #dummy entry for list comparison in the first step
  insertat = 1
  adjusted = FALSE
  cname1 = paste0(colnames(data)[1], "1")
  cname2 = paste0(colnames(data)[1], "2")
  for(i in 1:(nrow(data)-1)){
   for(j in (i+1):nrow(data)){
     if(data[i,2] == data[j,2] && data[i,1] != data[j,1]){
       for(k in 1:length(comparison)){
         if(!adjusted && is.element(data[i,1], comparison[[k]][1:2]) && is.element(data[j,1], comparison[[k]][1:2])){
           comparison[[k]][3] = comparison[[k]][3] + 1
           adjusted = TRUE
           break
         }
       }
       if(!adjusted){
          comparison[[insertat]] = as.numeric(c(data[i,1], data[j,1], 1))
          insertat = insertat + 1
       }
       adjusted = FALSE
     }
   }
  }
  for(i in 1:length(comparison)){
   c1 = append(c1, comparison[[i]][1])
   c2 = append(c2, comparison[[i]][2])
   weight = append(weight, comparison[[i]][3])
  }
  df = data.frame(c1, c2, weight)
  colnames(df) = c(cname1, cname2, "weight")
  
  graph = graph_from_data_frame(df[1:2], directed = FALSE)
  E(graph)$weight = as.numeric(as.vector(df[,3]))
  return(graph)
}




# ------------------------------------ Get Graph Data ------------------------------------ #

# Define db table names here to use them later in the code
# MySQL 
table_song_production = 'Song_production'
table_user_favourited_song = 'User_favourited_song'
table_user_follower = 'User_follower'
table_artist_genre = 'Artist_genre'
table_user = 'User'
table_song = 'Song'

# Postgres
table_song_production = 'song_production'
table_user_favourited_song = 'user_favourited_song'
table_user_follower = 'user_follower'
table_artist_genre = 'artist_genre'
table_user = 'user'
table_song = 'song'

#create new user-song relations as our DB is quite sparse
createAdditionalRelations(con, 100, table_user, table_song, table_user_favourited_song)

# get the data for our graph
song_production <- dbGetQuery(conn = con, paste0("SELECT * FROM ", table_song_production))
user_song <- dbGetQuery(conn = con, paste0("SELECT * FROM ", table_user_favourited_song))
user_follower <- dbGetQuery(conn = con, paste0("SELECT * FROM ", table_user_follower))
artist_genre <- dbGetQuery(conn = con, paste0("SELECT * FROM ", table_artist_genre))


# create weighted graph
user_song_weighted_graph = createWeightedGraph(user_song)
song_production_weighted_graph = createWeightedGraph(song_production)

#betweenness(song_prod_graph, directed = F, weights = NA)

#http://kateto.net/networks-r-igraph
#https://cran.r-project.org/web/packages/igraph/igraph.pdf

# ------------------------------------ Create the graph ------------------------------------ #

# Test Graph
g1 <- graph(edges = c("A","B", "B","C", "B","C", "C","A", "B","D", 
                      "D","E", "D","G", "D","F", "E","F", 
                      "F","G"), directed = FALSE)

g1 <- delete.edges(g1, E(g1)[E(g1)$weight <= 3])
plot(g1)


# choose the graph you want to plot
graph_to_plot = song_production_weighted_graph
graph_to_plot = user_song_weighted_graph
graph_to_plot = g1


# Create diffrent plots
# 1.Newman-Girvan
#e <- edge.betweenness.community(g1, directed=F)
c <- cluster_edge_betweenness(graph_to_plot) 
membership(c)
dendPlot(c, mode="hclust")
plot(c,graph_to_plot)

plot(user_follower_graph, layout=layout.fruchterman.reingold)

# 2.Label propagation
p <- cluster_label_prop(graph_to_plot)
plot(p,graph_to_plot)

# 3.Fast greedy
g <- cluster_fast_greedy(graph_to_plot)
plot(g,graph_to_plot)

# 4.Walktrap
w <- cluster_walktrap(graph_to_plot)
plot(w,graph_to_plot)

# 5.leading eigenvector
e <- cluster_leading_eigen(graph_to_plot)
plot(e,graph_to_plot)

# 6.Spinglass
s <- cluster_spinglass(graph_to_plot)
plot(s,graph_to_plot)

# 7.Infomap
i <- cluster_infomap(graph_to_plot)
plot(i,graph_to_plot)

# Furthermore
edge_betweenness(graph_to_plot)

# Layout options
plot(cnet,
     net,
vertex.color = "grey",
vertex.size = 8,
vertex.label.cex = 0.9,
vertex.label.color ="black",
vertex.label.dist=0,
vertex.shape="square",
edge.color = 'red',
edge.arrow.size=.10,
edge.arrow.width=10,
arrow.mode=0,
layout = layout.fruchterman.reingold)

