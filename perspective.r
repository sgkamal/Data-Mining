data(volcano)

persp(volcano,phi=50,theta=90)

#stopp;

for (p in seq(0,180,by=1)) {
#persp(volcano,phi=p,theta=90)
persp(volcano,phi=p,theta=p)
}