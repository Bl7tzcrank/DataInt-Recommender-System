# Network analysis based on the 4Play Database

# ------------------------------------ HowTo ------------------------------------ #
# 1. First of all, run the function definitions (and package installations / requirements / library calls)
# 2. create a database connection by running the respective commands in the "Database connection" part
# 3. select the database tables you need by running the respective commands in the "Get Graph Data" part
# 4. follow the steps in the "Create the graph" part to generate a network graph

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
dbname = '4PlayNetwork'

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

# for a given graph, the number of incident edges is assigned as the vertex.size 
setVertexSizes = function(graph, factor, log = FALSE, default = FALSE){
  if(default){
    V(graph)$size = 5
  }
  else {
    for(node in V(graph)){
      if(log){
        V(graph)$size[node] = log(length(incident(graph, node, 'all')) * factor)
      }
      else {
        V(graph)$size[node] = length(incident(graph, node, 'all')) * factor
      }
    }
  }
  return(V(graph)$size)
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


# get the data for our graph
song_production <- dbGetQuery(conn = con, paste0("SELECT * FROM ", table_song_production))
user_song <- dbGetQuery(conn = con, paste0("SELECT * FROM ", table_user_favourited_song))
user_follower <- dbGetQuery(conn = con, paste0("SELECT * FROM ", table_user_follower))
artist_genre <- dbGetQuery(conn = con, paste0("SELECT * FROM ", table_artist_genre))


# create weighted graph
user_song_weighted_graph = createWeightedGraph(user_song)
song_production_weighted_graph = createWeightedGraph(song_production)
artist_genre_weighted_graph = createWeightedGraph(artist_genre)

# information to the graph design
# http://kateto.net/networks-r-igraph
# https://cran.r-project.org/web/packages/igraph/igraph.pdf


# ------------------------------------ Create the graph ------------------------------------ #

# Test Graph
g1 <- graph(edges = c("A","B", "A","B", "A","B", "A","B", "A","B", "A","B", "A","B", "A","B", "A","B", "A","B", "A","B", "A","B", "B","C", "B","C", "C","A", "B","D", 
                      "D","E", "D","G", "D","F", "E","F", 
                      "F","G"), directed = FALSE)

g1 <- delete.edges(g1, E(g1)[E(g1)$weight <= 3])
plot(g1)


# choose the graph you want to plot
graph_to_plot = song_production_weighted_graph
graph_to_plot = user_song_weighted_graph
graph_to_plot = artist_genre_weighted_graph
graph_to_plot = g1


# Create diffrent network algorithms -> choose one of them to be plotted later

# 1.Newman-Girvan
newman_girvan <- cluster_edge_betweenness(graph_to_plot) 

# 2.Label propagation
label_propagation <- cluster_label_prop(graph_to_plot)

# 3.Fast greedy
fast_greedy <- cluster_fast_greedy(graph_to_plot)

# 4.Walktrap
walktrap <- cluster_walktrap(graph_to_plot)

# 5.leading eigenvector
leading_eigenvector <- cluster_leading_eigen(graph_to_plot)

# 6.Spinglass
spinglass <- cluster_spinglass(graph_to_plot)

# 7.Infomap
infomap <- cluster_infomap(graph_to_plot)

# show edge betweenness
edge_betweenness(graph_to_plot)


# set the algorithm and plot
algorithm = newman_girvan
name = 'Communities based on "Edge-Betweenness"'
algorithm = label_propagation
name = 'Communities based on "Label propagation"'
algorithm = fast_greedy
name = 'Communities based on "Fast greedy"'
algorithm = walktrap
name = 'Communities based on "Walktrap"'
algorithm = leading_eigenvector
name = 'Communities based on "Leading eigenvector"'
algorithm = spinglass
name = 'Communities based on "Spinglass"'
algorithm = infomap
name = 'Communities based on "Infomap"'


# Layout options
plot(
  algorithm,
  graph_to_plot,
  #graph_to_plot_simplified,
  vertex.color = "grey",
  vertex.size = setVertexSizes(graph_to_plot, 15, log=TRUE, default = FALSE),
  #vertex.size = setVertexSizes(graph_to_plot, .2, log=FALSE, default = FALSE),
  #vertex.size = setVertexSizes(graph_to_plot, 0.1, default = TRUE),
  vertex.label.cex = 0.5,
  vertex.label.color ="black",
  vertex.label.dist=0,
  vertex.shape="square",
  edge.width=E(graph_to_plot)$weight * .3,
  arrow.mode=1,
  layout = layout.auto,
  main = name
)




