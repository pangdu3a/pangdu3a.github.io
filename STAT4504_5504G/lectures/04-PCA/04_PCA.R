data("USArrests")
head(USArrests)
pr.out=prcomp(USArrests,scale.=TRUE)
pr.out$rotation
pr.out$x[1:5,]
biplot(pr.out,scale=0,cex=c(0.6,0.75),xlim=c(-4,4))
pr.out$rotation=-pr.out$rotation; pr.out$x=-pr.out$x
biplot(pr.out,scale=0,cex=c(1/2,1),xlim=c(-4,4))
biplot(prcomp(USArrests,scale.=FALSE),scale=0,cex=c(1/2,1))
