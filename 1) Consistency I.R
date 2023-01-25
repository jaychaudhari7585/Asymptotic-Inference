#Que1 
#simulation study:
mu=2.5;
sigma=1;eps=.1
est_t1_prob=0;
est_t2_prob=0;
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  x=matrix(rcauchy(n[i]*100,mu,sigma),100,n[i])
  t1=apply(x,1,mean)
  t2=apply(x,1,median)
  est_t1_prob[i]=mean(abs(t1-mu)<eps)
  est_t2_prob[i]=mean(abs(t2-mu)<eps)
}
data.frame('sample size'=n,est_t1_prob,est_t2_prob)


#Que2
rm(list =ls(all=T) )
theta=2;
eps=0.1
est_t1_prob=0;est_t2_prob=0;est_t3_prob=0;
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  x=matrix(runif(n[i]*100,min = 0,max=theta),100,n[i])
  t1=apply(x,1,max)
  t2=apply(x,1,mean)
  t3=apply(2*x,1,mean)
  est_t1_prob[i]=mean(abs(t1-theta)<eps)
  est_t2_prob[i]=mean(abs(t2-theta)<eps)
  est_t3_prob[i]=mean(abs(t3-theta)<eps)
}
data.frame('Sample Size'=n,est_t1_prob,est_t2_prob,est_t3_prob)


#Que3
rm(list =ls(all=T) )
mu=2;
sigma=1;
eps=0.1
est_t1_prob=0;
est_t2_prob=0;
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  x=matrix(rnorm(n[i]*100,mean = mu,sd = sigma),100,n[i])
  t1=apply(x,1,mean)
  t2=apply(x,1,var)
  est_t1_prob[i]=mean(abs(t1-mu)<eps)
  est_t2_prob[i]=mean(abs(t2-sigma^2)<eps)
}
data.frame('Sample Size'=n,est_t1_prob,est_t2_prob)

#Que4
rm(list=ls(all=T))
theta=1;
T=2
n=c(20,40,80,160,500,1000,2000)
est_t1_prob=0;
f_x=1-exp(-T*theta)
for (i in 1:length(n)){
  x=matrix(rexp(n[i]*100,theta),100,n[i])
  y=x<=2;
  t1=apply(y,1,mean)
  est_t1_prob[i]=mean(abs(t1-f_x)<0.1)
}
cbind(n,est_t1_prob)

#Que5
rm(list =ls(all=T) )
lambda=2;
eps=0.1
est_t1_prob=0;est_t2_prob=0;est_t3_prob=0;est_t4_prob=0;
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  x=matrix(rpois(n[i]*100,lambda =lambda ),100,n[i])
  t1=apply(x,1,mean)
  t2=apply(x,1,var)
  t3=exp(-t1)
  t4=exp(-t2)
  est_t1_prob[i]=mean(abs(t1-lambda)<eps)
  est_t2_prob[i]=mean(abs(t2-lambda)<eps)
  est_t3_prob[i]=mean(abs(t3-exp(-lambda))<eps)
  est_t4_prob[i]=mean(abs(t4-exp(-lambda))<eps)
}
data.frame('Sample Size'=n,est_t1_prob,est_t2_prob,est_t3_prob,est_t4_prob)

#Que6
rm(list =ls(all=T) )
theta=2;
eps=0.1
est_t1_prob=0;
est_t2_prob=0;
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  y=matrix(runif(n[i]*100,0,1),100,n[i])
  x=theta-log(1-y)
  t1=apply(x-1,1,mean)
  t2=apply(x,1,min)
  est_t1_prob[i]=mean(abs(t1-theta)<eps)
  est_t2_prob[i]=mean(abs(t2-theta)<eps)
}
data.frame('Sample Size'=n,est_t1_prob,est_t2_prob)


#OR
rm(list =ls(all=T) )
theta=2;
eps=0.1
est_t1_prob=0;
est_t2_prob=0;
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  y=matrix(runif(n[i]*100,0,1),100,n[i])
  x=theta-log(1-y)
  t1=apply(x,1,mean)-1
  t2=apply(x,1,min)
  est_t1_prob[i]=mean(abs(t1-theta)<eps)
  est_t2_prob[i]=mean(abs(t2-theta)<eps)
}
data.frame('Sample Size'=n,est_t1_prob,est_t2_prob)

