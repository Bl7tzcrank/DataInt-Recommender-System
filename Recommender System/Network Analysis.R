install.packages("igraph")
library("igraph")
install.packages("RPostgreSQL")
require("RPostgreSQL")

##configuration of the database connection ####
driver = dbDriver("PostgreSQL")
# driver = MySQL()
# dbname = 'DataIntegration'
dbname = '4Play'
host = '127.0.0.1'
# port = 5432
port = 3006
user = 'postgres'
# user = 'root'
pw = 'admin'
# password = ''
 con = dbConnect(driver, dbname = dbname,
                  host = host, port = port,
                  user = user, password = pw)
##end of database configuration ###############
 
##function declaration ########################
 
 #Takes a dataframe (for example user_favourited songs) and 
 createWeightedGraph = function(data){
   #c1 = c()
   #c2 = c()
   comparison = list()
   #comparison[[1]] = c(0,0,0) #dummy entry for list comparison in the first step
   insertat = 1
   adjusted = FALSE
   cname1 = paste0(colnames(data)[1], "1")
   cname2 = paste0(colnames(data)[1], "2")
   for(i in 1:nrow(data)){
     for(j in i:nrow(data)){
       if(data[i,2] == data[j,2] && data[i,1] != data[j,1]){
         #c1 = append(c1, data[i,1])
         #c2 = append(c2, data[j,1])
         for(k in 1:length(comparison)){
           if(!adjusted && is.element(data[i,1], comparison[[k]][1:2]) && is.element(data[j,1], comparison[[k]][1:2])){
             comparison[[k]][3] = comparison[[k]][3] + 1
             adjusted = TRUE
             break
           }
         }
         if(!adjusted){
            comparison[[insertat]] = c(data[i,1], data[j,1], 1)
            insertat = insertat + 1
         }
         adjusted = FALSE
       }
     }
   }
   #df = data.frame(c1, c2)
   #colnames(df) = c(cname1, cname2)
   return(comparison)
 }
 
 createWeightedGraph(small_user_song)
 small_user_song = user_song[1:10,]
 
 nrow(small_user_song)
 
 user_song[100,2]
 user_song[1,2]
 test = list()
 test[[1]] = c(1,4,2)
 test[[2]] = c(4,5,6)
 test[[3]] = c(4,5,6)
 test[[1]][3] = test[[1]][3] + 1
 test = append(test[[]], c(3,7,9))
 test = test[1:2]
 length(test)
 if(is.element(1, test[[1]][1:2]) && is.element(2, test[[1]][1:2])){
   print("Jawoll")
 } else{
   print("NEIN")
 }
##end of function declaration #################

#get the data for our graph

song_production <- dbGetQuery(conn = con, "SELECT * FROM song_production")
user_song <- dbGetQuery(conn = con, "SELECT * FROM user_favourited_song")
artists <- dbGetQuery(conn = con, "SELECT * FROM artist")
user_follower <- dbGetQuery(conn = con, "SELECT * FROM user_follower")
artist_genre <- dbGetQuery(conn = con, "SELECT * FROM artist_genre")

artists[1:2]

nrow(user_song)


user_song[(user_song$userid==23),]
createedgelist(user_song)


song_production_edgelist = createedgelist(song_production)
user_song_edgelist = createedgelist(user_song[1:2])
artist_genre_edgelist = createedgelist(artist_genre)
nrow(artist_genre_edgelist)

write.csv2(song_production_edgelist, "song_production_edgelist.csv")
write.csv2(user_song_edgelist, "user_song_edgelist.csv")

user_song_graph = graph_from_data_frame(user_song_edgelist, directed = FALSE)
song_production_graph = graph_from_data_frame(song_production_edgelist, directed = FALSE)
user_follower_graph = graph_from_data_frame(user_follower, directed = FALSE)
artist_genre_graph = graph_from_data_frame(artist_genre_edgelist, directed = FALSE)

E(user_follower_graph)$weight = 1/(length(V(user_follower_graph))-1)

#create new user-song relations
createAdditionalRelations(con, 100, "users", "song", "user_favourited_song")

set.seed(50)

betweenness(song_prod_graph, directed = F, weights = NA)

write.csv2(song_production, "song_production2.csv")
g2 = graph_from_data_frame(test)
#http://kateto.net/networks-r-igraph
#https://cran.r-project.org/web/packages/igraph/igraph.pdf

#Defining the graph
g1 <- graph(edges = c("A","B", "B","C", "B","C", "C","A", "B","D", 
                      "D","E", "D","G", "D","F", "E","F", 
                      "F","G"), directed = FALSE)
E(g1)$weight


g1 <- delete.edges(g1, E(g1)[E(g1)$weight <= 3])
plot(g1)
g2 <- delete.edges(g2, E(g2)[E(g2)$weight <= 1])
plot(g2) #visualization

#1.Newman-Girvan
#e <- edge.betweenness.community(g1, directed=F)
c <- cluster_edge_betweenness(artist_genre_graph) 
membership(c)
dendPlot(c, mode="hclust")
plot(c,artist_genre_graph)

plot(user_follower_graph, layout=layout.fruchterman.reingold)
#2.Label propagation
p <- cluster_label_prop(g1)
plot(p,g1)

#3.Fast greedy
g <- cluster_fast_greedy(g1)
plot(g,g1)

#4.Walktrap
w <- cluster_walktrap(g1)
plot(w,g1)

#5.leading eigenvector
e <- cluster_leading_eigen(g1)
plot(e,g1)

#6.Spinglass
s <- cluster_spinglass(g1)
plot(s,g1)

#7.Infomap
i <- cluster_infomap(g1)
plot(i,g1)

#Furthermore
edge_betweenness(g1)