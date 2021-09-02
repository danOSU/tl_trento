library(SLHD)
library(lhs)

# 500 sliced maximin LHD for TRENTO 2D (3-dimensional)
# 50,100,150 design points for TRENTO 3D (7-dimensional)

d_2D <- 3
d_3D <- 7
n_2D <- 500
n_3D <- 150
m <- 50 # sample size in each slice

X_2D <- maximinSLHD(t = n_2D/m, m = m, k = d_2D)$StandDesign
X_3Dorig <- X_2D[X_2D[,1] %in% c(1,2,3),]

# additional 4 parameters for 3D
X_3Dadd <- maximinLHS(n = n_3D, k = d_3D-d_2D)
X_3D <- cbind(X_3Dorig,X_3Dadd)

X_2D <- X_2D[,-1]; X_2D <- data.frame(X_2D); names(X_2D) <- c("X1","X2","X3")
X_3D <- X_3D[,-1]; X_3D <- data.frame(X_3D); names(X_3D) <- c("X1","X2","X3","X4","X5","X6","X7")

write.csv(X_2D,"2D_500POINTS.txt",row.names=F)
write.csv(X_3D,"3D_150POINTS.txt",row.names=F)


#### map to individual range (inaccurate)
lb <- c(9, 0.35, 0.1, 0.2, 3, -0.5, 1)
ub <- c(28, 1.40, 0.9, 1, 5, 1.5, 1.5)

for(i in 1:length(lb)){
  if(i<=3){
    D_2d[,i] <- (ub[i]-lb[i])*X_2D[,i]+lb[i]
  }
  D_3d[,i] <- (ub[i]-lb[i])*X_3D[,i]+lb[i]
}

sum(abs(D_2d[1:150,]-D_3d[,1:3]))

write.csv(D_2d,"2D_500POINTS_rescaled.txt",row.names=F)
write.csv(D_3d,"3D_150POINTS_rescaled.txt",row.names=F)
