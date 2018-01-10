install.packages("recommenderlab")
library("recommenderlab")

data("MovieLense")

MovieLense100 = MovieLense[rowCounts(MovieLense)>100,]
MovieLense100

train = MovieLense100[1:50]

rec = Recommender(train, method= "UBCF")
rec

pre = predict(rec, MovieLense100[101:102], n = 10)
pre

as(pre, "list")
