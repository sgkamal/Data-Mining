#demonstration of principal components to remove redundant features in data matrix
data('iris');
X = as.matrix(iris); # the apparent dimension of the data ... you'll see
X
#stopp;
r = ncol(X);   # the real dimension
n = nrow(X); # num data points
X = matrix(runif(n*r),nrow=n,ncol=r); 
X
# original data matrix with r indepedent variables
#T = matrix(0,nrow=r,ncol=d);  # transformation matrix
#for (i in 1:r) for (j in 1:d) {  # create rxd transformation matrix, T
#    T[i,j] = j %% (i+1);
#}
#Y = X %*% T; # even though Y has d features, there can only be r independent ones so there
             # is a lot of redundancy in the new features  
Y = scale(X)  # make columns 0 mean
#pairs(Y);
S = t(Y) %*% Y/(n-1);  # the sample covariance matrix
svd =  svd(S);  # take the singular value decomposition S = UDU^t
d = svd$d;  # d is diag(D)  # only the first are should be different from 0.
d
length(d)
ncol(d)
U = svd$u;  # U are the "loadings" or independent directions
Z = Y %*% U;  # transform Y to a new matrix that removes the redundancy
pairs(Z)

#Positive Definite
