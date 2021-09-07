library(SLHD)

# 500 sliced maximin LHD for TRENTO 2D (3-dimensional)
# 50,100,150 design points for TRENTO 3D (7-dimensional)
# 300 design points for TRENTO 3D Test (7-dimensional)

d_2D <- 3
d_3D <- 7
n_2D <- 500
n_3D <- 150
n_test <- 300
m <- 50 # sample size in each slice

X_all <- maximinSLHD(t = (n_2D+n_test)/m, m = m, k = d_2D)$StandDesign
X_2D <- X_all[1:n_2D,]
X_test_I <- X_all[(n_2D+1):(n_2D+n_test),]
X_3Dorig <- X_2D[X_2D[,1] %in% c(1,2,3),]

# additional 4 parameters for 3D & test set
X_3Dadd_test <- maximinSLHD(t = (n_3D+n_test)/m, m = m, k = d_3D-d_2D)$StandDesign
X_3Dadd <- X_3Dadd_test[1:n_3D,]; X_3Dadd <- X_3Dadd[,-1]
X_test_II <- X_3Dadd_test[(n_3D+1):(n_3D+n_test),]; X_test_II <- X_test_II[,-1]
X_3D <- cbind(X_3Dorig,X_3Dadd)
X_test <- cbind(X_test_I,X_test_II)

X_2D <- X_2D[,-1]; X_2D <- data.frame(X_2D); names(X_2D) <- c("X1","X2","X3")
X_3D <- X_3D[,-1]; X_3D <- data.frame(X_3D); names(X_3D) <- c("X1","X2","X3","X4","X5","X6","X7")
X_test <- X_test[,-1]; X_test <- data.frame(X_test); names(X_test) <- c("X1","X2","X3","X4","X5","X6","X7")

write.csv(X_2D,"2D_500POINTS_standard.txt",row.names=F)
write.csv(X_3D,"3D_150POINTS_standard.txt",row.names=F)
write.csv(X_test,"Test_3D_300POINTS_standard.txt",row.names=F)


#### map to individual range (inaccurate)
lb <- c(9, 0.35, 0.1, 0.2, 3, -0.5, 1)
ub <- c(28, 1.40, 0.9, 1, 5, 1.5, 1.5)

D_2d <- X_2D; D_3d <- X_3D; D_test <- X_test

for(i in 1:length(lb)){
  if(i<=3){
    for(j in 1:nrow(X_2D)){
      D_2d[j,i] <- (ub[i]-lb[i])*X_2D[j,i]+lb[i] 
    }
  }
  for(j in 1:nrow(X_3D)){
    D_3d[j,i] <- (ub[i]-lb[i])*X_3D[j,i]+lb[i]
  }
  for(j in 1:nrow(X_test)){
    D_test[j,i] <- (ub[i]-lb[i])*X_test[j,i]+lb[i] 
  }
}

sum(abs(D_2d[1:150,]-D_3d[,1:3]))

write.csv(D_2d,"2D_500POINTS_rescaled.txt",row.names=F)
write.csv(D_3d,"3D_150POINTS_rescaled.txt",row.names=F)
write.csv(D_test,"Test_3D_300POINTS_rescaled.txt",row.names=F)


# visualize
X_2D <- read.csv("2D_500POINTS_standard.txt", header=T)
X_3D <- read.csv("3D_150POINTS_standard.txt", header=T)
X_test <- read.csv("Test_3D_300POINTS_standard.txt",header=T)

par(mfrow=c(2,2))
for(i in 1:3){
  hist(X_2D[,i])
}
for(i in 1:7){
  hist(X_3D[,i])
}
for(i in 1:7){
  hist(X_test[,i])
}

D_2d <- read.csv("2D_500POINTS_rescaled.txt",header=T)
D_3d <- read.csv("3D_150POINTS_rescaled.txt",header=T)
D_test <- read.csv("Test_3D_300POINTS_rescaled.txt",header=T)

par(mfrow=c(2,2))
for(i in 1:3){
  hist(D_2d[,i])
}
for(i in 1:7){
  hist(D_3d[,i])
}
for(i in 1:7){
  hist(D_test[,i])
}
