#Que1
rm(list =ls(all=T))
mu=1;sigma=1.2;
eps=0.1
est_t1_prob=0;est_t2_prob=0;est_t3_prob=0;
n=c(25,50,100,500,1000,1500,2000)
for (i in 1:length(n))
{
  x=matrix(rnorm(n[i]*100,mean = mu,sd = sigma),100,n[i])
  t1=apply(x,1,mean)^2
  m2=apply(x,1,var)
  t2=m2^2
  t3=t1+t2
  est_t1_prob[i]=mean(abs(t1-mu^2)<eps)
  est_t2_prob[i]=mean(abs(t2-sigma^4)<eps)
  est_t3_prob[i]=mean(abs(t3-(sigma^4+mu^2))<eps)
  
}
data.frame('Sample Size'=n,est_t1_prob,est_t2_prob,est_t3_prob)
par(mfrow=c(2,2))
hist(t1);qqnorm(t1);
hist(t2);qqnorm(t2);
hist(t3);qqnorm(t3);

#Que2
rm(list =ls(all=T))
lambda=0.5;
eps=0.1
est_t1_prob=0;est_t2_prob=0;est_t3_prob=0;
n=c(25,50,100,500,1000,1500,2000)
for (i in 1:length(n))
{
  x=matrix(rpois(n[i]*100,lambda = lambda),100,n[i])
  m1=apply(x,1,mean)
  t1=exp(-m1)
  t2=m1*exp(-m1)
  est_t1_prob[i]=mean(abs(t1-exp(-lambda))<eps)
  est_t2_prob[i]=mean(abs(t2-t1*lambda)<eps)
}
data.frame('Sample Size'=n,est_t1_prob,est_t2_prob)
par(mfrow=c(2,2))
hist(t1);qqnorm(t1);
hist(t2);qqnorm(t2);

#Que3
rm(list =ls(all=T))
m=10
p=0.6;
eps=0.1
est_t1_prob=0;est_t2_prob=0;est_t3_prob=0;
n=c(25,50,100,500,1000,1500,2000)
for (i in 1:length(n)){ 
  x=matrix(rbinom(n[i]*100,size = m,prob = p),100,n[i])
 p_hat=apply(x/m,1,mean)
   t1=(1-p_hat)^m
  t2=m*(p_hat)*(1-p_hat)^m
  t3=p_hat*(1-p_hat)
  est_t1_prob[i]=mean(abs(t1-(1-p)^m)<eps)
  est_t2_prob[i]=mean(abs(t2-m*(p)*(1-p)^m)<eps)
  est_t3_prob[i]=mean(abs(t3-p*(1-p))<eps)}
data.frame('Sample Size'=n,est_t1_prob,est_t2_prob,est_t3_prob)
par(mfrow=c(2,2))
hist(t1);qqnorm(t1);
hist(t2);qqnorm(t2)


#when m=1
m=1
p=0.6;
eps=0.1
est_t1_prob=0;est_t2_prob=0;est_t3_prob=0;
n=c(25,50,100,500,1000,1500,2000)
for (i in 1:length(n)){ 
  x=matrix(rbinom(n[i]*100,size = m,prob = p),100,n[i])
  p_hat=apply(x/m,1,mean)
  t1=(1-p_hat)^m
  t2=m*(p_hat)*(1-p_hat)^m
  t3=p_hat*(1-p_hat)
  est_t1_prob[i]=mean(abs(t1-(1-p)^m)<eps)
  est_t2_prob[i]=mean(abs(t2-m*(p)*(1-p)^m)<eps)
  est_t3_prob[i]=mean(abs(t3-p*(1-p))<eps)}
data.frame('Sample Size'=n,est_t1_prob,est_t2_prob,est_t3_prob)
par(mfrow=c(2,2))
hist(t1);qqnorm(t1);
hist(t2);qqnorm(t2)

#Que5 
theta=3;
eps=0.1;
n=c(50,100,200,500,1000,5000);
est_t1_prob=0;
for (i in 1:length(n)){
  y=matrix(runif(n[i]*100,0,1),100,n[i])
  x=ifelse(y<=0.5,theta+log(2*y),theta-log(2-2*y))
  t1=apply(x,1,median)
  est_t1_prob[i]=mean(abs(t1-theta)<eps)
}
data.frame('sample size'=n,est_t1_prob)
par(mfrow=c(1,1))
hist(t1);
qqnorm(t1)
