# simple example demonstrating the "curse of dimensionality"
# create random points in a d-dimensional hypercube for different values of d  
# The bigger the dimension, d,  the more the mass of points is clustered around the edges

d = 50     # the dimension (number of features) in the data
n = 1000   # number of data points
X = matrix(runif(d*n),nrow=n,ncol=d);  # create a nxd random data matrix.  each coord of each observation is unif(0,1)
# pairs(X);  # pairs plot to check the data behaves as we expect
dist_to_edge = rep(0,n);  # this will hold the distance to the hypercube edge for each data point.
dist_to_edge1 = rep(0,n);
for (i in 1:n) {
   dist_to_edge[i] = .5 - max(abs(.5 - X[i,]))  # the distance to the closest edge of hypercube
   dist_to_edge1[i] = min(0.5 - abs(0.5 - X[i,]))
}
hist(dist_to_edge)
#hist(dist_to_edge1)
