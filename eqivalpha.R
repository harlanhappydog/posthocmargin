n1<-n2<-50
sig<-1
mu1<-0.52
mu2<-0

smp1 <- rnorm(n1, mu1, sig)
smp2 <- rnorm(n2, mu2, sig)
sig.hat <- sqrt( ((n1-1)*var(smp1)+(n2-1)*var(smp2))/(n1+n2-2) ) 



sig.hat<-1
x2bar<-0.2
x1bar<-0


ci <- x2bar-x1bar + c(-1,1)*qt(.9, df=n1+n2-2)*sig.hat*sqrt(1/n1+1/n2) 
n<-n1+n2
  
max(abs(ci)) < Delta

Delta <- 0.5


pval_vec<-NULL
Deltavec<-seq(0.20,1,0.01)
for(jj in 1:length(Deltavec)){
Delta<-Deltavec[jj]
T1 <- (x1bar - x2bar + Delta )/(sig.hat*sqrt(1/n1+1/n2))
T2 <- (x1bar - x2bar - Delta )/(sig.hat*sqrt(1/n1+1/n2))
pval_vec[jj]<-(max(c( pt(-T1,n-2), pt(T2,n-2))))

}
op <- par(mar = c(5,7,4,2) + 0.1) 

plot(pval_vec, Deltavec, type='l', lwd=3, xlab=expression(paste(alpha, " for which we can reject ", H[0], ", for given ",Delta)), ylab="", cex.axis=1, cex.lab=1.2, axes=FALSE, xlim=c(0,0.50))
axis(1, at=seq(0,1,0.05))
axis(2, at=seq(0,1,0.10),las=2)
#lines(x=c(0.05,0.05),y=c(0,1), lty=5)
lines(x=c(0,1),y=c(x2bar, x2bar), lty=5)
lines(y=c(0,1),x=c(0.5,0.5), lty=5)
title(main=expression(paste(Delta, " for which we can reject ", H[0], ", for given ",alpha, "                                          ")))