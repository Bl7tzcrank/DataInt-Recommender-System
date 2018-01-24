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
pw = 'pw'
# password = ''
 con = dbConnect(driver, dbname = dbname,
                  host = host, port = port,
                  user = user, password = pw)
##end of database configuration ###############
 
##function declaration ########################
 
 #Takes a dataframe (for example user_favourited songs) and calculates the edgelist of the first
 #two columns where as the first column is the entity for vertices and if there is the same column2 entry
 #for a different column1 entry the two corresponding column1 entries will be c1 and c2 for the edgelist.
 #For example: userid 1 favoures songid 2 and userid 10 favours songid 2, one row of the upcoming edgelist would be
 # 1 - 10 - weight. The weight is determined by the number of same favourited songs. If userid 1 and 10 would only
 #have this one songid (2) in common the weight would be 1. 
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

##end of function declaration #################

#get the data for our graph

song_production <- dbGetQuery(conn = con, "SELECT * FROM song_production")
user_song <- dbGetQuery(conn = con, "SELECT * FROM user_favourited_song")
user_follower <- dbGetQuery(conn = con, "SELECT * FROM user_follower")
artist_genre <- dbGetQuery(conn = con, "SELECT * FROM artist_genre")

#create weighted graph
user_song_weighted_graph = createWeightedGraph(user_song)
song_production_weighted_graph = createWeightedGraph(song_production)
E(song_production_weighted_graph)$weight


#create new user-song relations
createAdditionalRelations(con, 100, "users", "song", "user_favourited_song")

#betweenness(song_prod_graph, directed = F, weights = NA)

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
c <- cluster_edge_betweenness(song_production_weighted_graph) 
membership(c)
dendPlot(c, mode="hclust")
plot(c,song_production_weighted_graph)

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