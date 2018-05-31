data("trees");
X = as.matrix(trees);
X = scale(X,center=TRUE,scale=FALSE);
covMatrix = matrix(0, nrow = 3, ncol = 3);
#X
#covMatrix
for (i in 1:3){
  for (j in i:3){
    for (k in 1:31){
      covMatrix[i,j] = covMatrix[i,j] + (X[k,i] - mean(X[,i]))*(X[k,j] - mean(X[,j]))/30;
    }
  }
}
for (i in 1:3){
  for (j in 1:3){
    if (covMatrix[i,j] == 0){
      covMatrix[i,j] = covMatrix[j,i];
    }
  }
}
covMatrix;
T = matrix(0,nrow=3,ncol=3);  # transformation matrix
for (i in 1:3) for (j in 1:3) {  # create rxd transformation matrix, T
  T[i,j] = j %% (i+1);
}
Y = X %*% T;
Y;
#Y = scale(Y)  # make columns 0 mean
#pairs(Y);
S = t(Y) %*% Y/30;
S
covMatrix;
cov(X)