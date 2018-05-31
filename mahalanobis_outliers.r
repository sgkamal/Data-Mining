# use Mahalanobis distance to find outliers in multivariate data set

data("state")  # the state dataset gives various per-capita measurements for the 50 US states
Y = state.x77  # the numbers of interest
n = nrow(Y);   # how many obsevations?  50 of course!
X = as.matrix(Y[,2:6]);  # strip out (Income, Illiteracy, Life Expect, Murder, HS grad)
X = scale(X);		 # standardize each variable have mean 0 and variance 1
#pairs(X);                # take a look ... can only see pairwise relations
S = t(X) %*% X / n;      # the sample covariance matrix
Sinv = solve(S);         # inverse of S
m = rep(0,length = n);  # will store Mahalanobis distances here
for (i in 1:n) {  # compute M. distances ...
    m[i] = t(X[i,]) %*% Sinv  %*% X[i,];
}
plot(m,pch="");  # do a blank plot of the M distances
text(m,labels=state.name);  # add the state labels



#library("maps")
#m = m/max(m);
#map(database = "state",regions=state.name,col=rgb(m,m,m),fill=T)