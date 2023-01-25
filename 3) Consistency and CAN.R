#Que1 
#simulation study:
mu=3;
sigma=1.5;eps=.1
est_t1_prob=0;est_t2_prob=0;
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  x=matrix(rnorm(n[i]*100,mu,sigma),100,n[i])
  t1=apply(x,1,mean)
  t2=apply(x,1,var)
  est_t1_prob[i]=mean(abs(t1-mu)<eps)
  est_t2_prob[i]=mean(abs(t2-sigma^2)<eps)
}
data.frame('sample size'=n,est_t1_prob,est_t2_prob)
par(mfrow=c(1,2))
hist(t1)
hist(t2)
qqnorm(t1);qqnorm(t2);


#Que2
rm(list=ls())
#simulation study:
lambda=3;
eps=0.5
est_t1_prob=0;est_t2_prob=0;
n=c(10,50,100,500,1000,5000)
for (i in 1:length(n))
{
  x=matrix(rpois(n[i]*100,lambda = lambda),100,n[i])
  t1=apply(x,1,mean)
  t2=apply(x,1,var)
  est_t1_prob[i]=mean(abs(t1-lambda)<eps)
  est_t2_prob[i]=mean(abs(t2-lambda)<eps)
}
data.frame('sample size'=n,est_t1_prob,est_t2_prob)
par(mfrow=c(1,2))
hist(t1)
hist(t2)
qqnorm(t1);qqnorm(t2);


#Que3
rm(list=ls())
#simulation study:
sigma=2.5;mu=0.5;
eps=0.5
est_t1_prob=0;est_t2_prob=0;est_t3_prob=0;est_t4_prob=0;
n=c(10,50,100,500,1000,5000)
for (i in 1:length(n))
{
  y=matrix(runif(n[i]*100,0,1),100,n[i])
  x=ifelse(y<=0.5,mu+log(2*y)*sigma,mu-log(2-2*y)*sigma)
  t1=apply(x,1,mean)
  t2=apply(x,1,median)
  m_2=apply(x,1,var)
  t3=sqrt(m_2/2)
  t4=apply(abs(x-t1),1,mean)
  est_t1_prob[i]=mean(abs(t1-mu)<eps)
  est_t2_prob[i]=mean(abs(t2-mu)<eps)
  est_t3_prob[i]=mean(abs(t3-sigma)<eps)
  est_t4_prob[i]=mean(abs(t4-sigma)<eps)
  
}
data.frame('sample size'=n,est_t1_prob,est_t2_prob,est_t3_prob,est_t4_prob)
par(mfrow=c(2,2))
hist(t1);hist(t2);hist(t3);hist(t4)
par(mfrow=c(2,2))
qqnorm(t1);qqnorm(t2);qqnorm(t3);qqnorm(t4);



##Que.4 
rm(list=ls())
#simulation study:
sigma=2.5;mu=0.5;
eps=0.5
est_t1_prob=0;est_t2_prob=0;est_t3_prob=0;est_t4_prob=0;
n=c(10,50,100,500,1000,5000)
for (i in 1:length(n))
{
  y=matrix(runif(n[i]*100,0,1),100,n[i])
  x=mu-sigma*log(1-y)
  t1=apply(x,1,mean)
  t2=apply(x,1,min)
  t3=apply(x,1,var)
  t4=apply(x-t2,1,mean)
  est_t1_prob[i]=mean(abs(t1-mu)<eps)
  est_t2_prob[i]=mean(abs(t2-mu)<eps)
  est_t3_prob[i]=mean(abs(t3-sigma)<eps)
  est_t4_prob[i]=mean(abs(t4-sigma)<eps)
}
data.frame('sample size'=n,est_t1_prob,est_t2_prob,est_t3_prob,est_t4_prob)
par(mfrow=c(2,2))
hist(t1);hist(t2);hist(t3);hist(t4)
par(mfrow=c(2,2))
qqnorm(t1);qqnorm(t2);qqnorm(t3);qqnorm(t4)








