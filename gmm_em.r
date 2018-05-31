
n = 50			# points for each cluster
K = 3			# number of clusters
D = 2;			# dimension of points
iterations = 100;	# iterations of em algorithm

library(mvtnorm);
N = K*n;		# total number of pts. 
cent = array(10*rnorm(K*D),c(K,D));  # cent[k,] is center of kth cluster
T = array(rnorm(K*D*D),c(K,D,D));    # T[k,,] is transformation used in generating kth cluster
X = matrix(0,nrow=D,ncol=0);	     # matrix holding the points
for (k in 1:K) {
    m = cent[k,];
    t = T[k,,];
    for (i in 1:n) X = cbind(X,t %*% rnorm(D)+m);  # each point adds a new columen
}
X = t(X);  # take transpose so the rows are the observations (as usual)
plot(X[,1],X[,2]);	       # what did we get??
Sigma = array(0,c(K,D,D));     # Sigma[k,,] will be kth covariance
for (k in 1:K) Sigma[k,,] = 10*diag(D);  # initialze to be large (non informative)
mu = array(rnorm(K*D),c(K,D));	# mu[,] is kth mean
p = rep(1/K,K);			# initialization of the class probabilities
pi = matrix(0,N,K);		# pi[i,k] is the prob of ith example being from kth class
M = rep(0,K);
cluster = rep(0,N);		# cluster[i] is the cluster (in 1 .. K) estimated for ith point
for (j in  1:iterations) {
    for (i in 1:N) {
    	for (k in 1:K) pi[i,k] = p[k]*dmvnorm(X[i,],mu[k,],Sigma[k,,])  # dmvnorm is normal density
	pi[i,] = pi[i,] / sum(pi[i,]);	# the posterior probabilities using current parameters
	cluster[i] = which.max(pi[i,]);  # assign each point to most likely cluster
    }
    for (k in 1:K) {
    	M[k] = sum(pi[,k]);	# estimated count of examples from kth cluster
	p[k] = M[k]/N;		# estimated proportion belonging to kth class
	mu[k,] = 0;		
	Sigma[k,,]=0;
	for (i in 1:N)  mu[k,] = mu[k,] + (pi[i,k]/M[k])*X[i,];  # reestimate mu[k,] and Sigma[k,,]
        for (i in 1:N) Sigma[k,,] = Sigma[k,,] + (pi[i,k]/M[k])*(X[i,] - mu[k,]) %*% t(X[i,]-mu[k,]);  
    }
    plot(X[,1],X[,2],col=cluster);  # do this for more than 3 clusters
#    points(proto,pch='x',col=1:K,cex=3);   	 # plot cluster centers as well
#      plot(X[,1],X[,2],col = rgb(pi[,1],pi[,2],pi[,3]));   # when we have 3 clusters do this
      # pi[1,] pi[,2], pi[,3] are the probs of classes 1,2,3.  Make colors that reflect the likelihood of the
      # classes.  
      # if probs are (1,0,0) use red
      # if probs are (0,1,0) use green
      # if probs are (0,0,1) use blue
      # ow mix these colors using the 3 probabilities 

    readline();   # wait for use input
}




