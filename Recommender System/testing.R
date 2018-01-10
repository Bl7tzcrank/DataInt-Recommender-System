# ----------------------- testing ----------------------- #
# testing - database

# Uncomment / set the respective values here
# driver = dbDriver("PostgreSQL")
# driver = MySQL()
# dbname = 'Data Integration'
# dbname = '4Play'
# host = '127.0.0.1'
# port = 5432
# port = 0
# user = 'postgres'
# user = 'root'
# password = 'pw'
# password = ''
# con = dbConnect(driver, dbname = dbname,
#                  host = host, port = port,
#                  user = user, password = pw)
# dbExistsTable(con, "album")
# dbGetQuery(con, "SELECT * from album")


#testing - Section for collaborative filtering

#for testing purpose recommender system
A = matrix(c(1,NA,3,NA,NA,5,NA,NA,5,NA,4,NA,
             NA,NA,5,4,NA,NA,4,NA,NA,2,1,3,
             2,4,NA,1,2,NA,3,NA,4,3,5,NA,
             NA,2,4,NA,5,NA,NA,4,NA,NA,2,NA,
             NA,NA,4,3,4,2,NA,NA,NA,NA,2,5,
             1,NA,3,NA,3,NA,NA,2,NA,NA,4,NA),nrow=6,ncol=12,byrow = TRUE)
rownames(A) <- paste('movie', 1:6)
colnames(A) <- paste('user', 1:12)
A <- t(A)
r <- as(A, "realRatingMatrix") #creates rating matrix
getRatingMatrix(r) #for testing
r_m <- normalize(r) #normalization
getRatingMatrix(r_m) #for testing

#model on general popularity
Rec.model<-Recommender(r, method = "popular")
rey <- predict(Rec.model, r, n=1) #n = number of recommendations to each user
as(rey, "list")
rep <- predict(Rec.model, r, type="ratings") #prediction
as(rep,"matrix")

#binary model on general popularity
b<- binarize(r, minRating=0)
Rec.model<-Recommender(b, method = "popular")
rek <- predict(Rec.model, b, n=1)
as(rek, "list")

# model on IBCF (Item based collaaborative filtering)
Rec.model<-Recommender(r, method = "IBCF")
reu <- predict(Rec.model, r, type="ratings")
rex <- predict(Rec.model, r, n=1)
as(reu,"matrix")
as(rex,"list")

# binary model on IBCF
b<- binarize(r, minRating=0)
Rec.model<-Recommender(b, method = "IBCF")
reub <- predict(Rec.model, b, type="ratings")
rexb <- predict(Rec.model, b, n=1)
as(reub,"matrix")
as(rexb,"list")

#testing - Section for content-based recommendations

#for testing purpose recommender system
F = matrix(c(NA,0.6,0.7,1.0,1.0,0.6,0.5,0.5,
             1,NA,NA,NA,NA,0.6,NA,NA,
             NA,0.8,0.6,NA,NA,0.6,0.5,0.5,
             1,0.7,0.7,NA,NA,0.5,0.5,0.5,
             0.8,0.6,0.7,0.9,1.0,0.6,0.4,0.4,
             NA,0.8,0.6,NA,NA,0.8,0.2,0.2,
             NA,NA,NA,NA,NA,0.5,NA,NA,
             0.8,1,1,NA,NA,NA,0.6,0.6,
             0.6,NA,0.1,0.7,0.8,0.5,0.6,0.6,
             1,NA,NA,1,1,NA,NA,NA,
             1,NA,NA,NA,NA,NA,0.7,0.7,
             NA,NA,NA,1,1,NA,NA,NA),nrow=12,ncol=8,byrow = TRUE)
rownames(F) <- paste('Feature', 1:12)
colnames(F) <- paste('Item', 1:8)

U = matrix(c(1.0,NA,NA,0.2,NA,NA,0.1,NA,
             NA,NA,0.8,NA,NA,0.3,0.1,NA,
             NA,NA,NA,1.0,NA,NA,0.3,NA,
             NA,0.1,NA,NA,NA,NA,NA,NA,
             NA,0.1,NA,NA,NA,NA,NA,NA),nrow=5,ncol=8,byrow = TRUE)
rownames(U) <- paste('User', 1:5)
colnames(U) <- paste('Item', 1:8)

#r <- as(U, "realRatingMatrix")
#bU <-binarize(r, minRating=0)
#U <-as(bU,"matrix")

WU <- matrix(0,nrow(U),ncol(F)) #empty matrix
rownames(WU)=rownames(U)
colnames(WU)=colnames(F)

#go through each row of U and pick those ratings which are not NA
#multiply each rating with its corresponding feature vector
#go for the average of them
#calculate the cos of this and the feature vector
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

