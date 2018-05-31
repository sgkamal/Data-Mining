
# demonstration of what happens when we vote using a collection of indpendent classifiers, only slighly better
# than random guessing.  


epsilon = .1;   # how much better than random guessing
M = 11;	  	# number of indepenent classifiers  (odd to avoid tie)
N = 1000;       # trials

C = rep(0,M)
error = 0
for (n in 1:N) {
    trueclass = sample(c(0,1),1);
    if (trueclass == 1) { p = c(.5-epsilon,.5+epsilon); }
    else { p = c(.5+epsilon,.5-epsilon); }
    C = sample(c(0,1),M,replace=T,prob=p);
    est = ifelse (sum(C) > M/2,1,0); 
    if (est != trueclass) error = error+1
}
print("error rate:")
print(error/N);

    
    

