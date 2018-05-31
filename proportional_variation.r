# When T is sum of random sample X_1, ... , X_n observe that the proportional variation
# of T is smaller than the proportional variation in the individual samples X_1 ....
# Can think of proportional variation is the variation of the leading digit.  


n = 10;
k = 10;
X = matrix(runif(n*k),nrow=n,ncol=n);
T = rowSums(X);
