# Demonstration of bias-variance tradeoff.  More complex models tend to have higher variance, but lower bias

N = 100;  # number of x's    # we perform regression on a function of this many points
x = seq(0,1,length=N);       # the x values we consider
#f = cumsum(cumsum(rnorm(N)));  # the true function f
#f = cumsum(rnorm(N));  # the true function f
f = sin(12*x);  # the true (but unknown to us) function
plot(x,f,type="l");   



T = 1000;  	  # number of random repetitions with different data sets
sigma2 = 10;      # the data variance
maxorder = 10;	  # maximum order of model
i0 = N/2;         # where we will evaluate our estimates (could be any point)
y = rep(0,N);	  # will hold our observed y values which vary randomly from experiment to experiment
fhat = rep(0,T);	  # our estimate function value at i0.  
bias2 = rep(0,maxorder);  # estimated squared bias at i0
var =  rep(0,maxorder)	  # estimated variance at i0
for (order in 1:maxorder) {
    X = matrix(1,nrow=N,ncol=1);
    for (o in 1:order) X = cbind(X,x^o);  # construct polynomial fit of order o
    for (t in 1:T) {   	   		    
        y = f + rnorm(N,sd=sqrt(sigma2));  # a random data set (only y's changing)
	betahat = solve(t(X) %*% X, t(X) %*% y);      
	fhat[t] = sum(X[i0,] * betahat);     # our estimate of f evaluated at i0
     }
     bias2[o] = (mean(fhat) - f[i0])^2;	     # the component of sq error coming from choice of model
     var[o] = var(fhat);      		     # the component of error coming from overfitting
}
plot(1:maxorder,bias2,pch='b')
#plot(1:maxorder,var,pch='v')

