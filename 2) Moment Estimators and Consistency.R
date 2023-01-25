#Que1
#Simulation study:
p=0.7;
eps=0.1
est_t1_prob=0;
MSE_t1=0
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  x=matrix(rbinom(n[i]*100,prob = p,size = n),100,n[i])
  m1=apply(x,1,mean)
  t1=m1/n
  est_t1_prob[i]=mean(abs(t1-p)<eps)
  MSE_t1[i]=mean((t1-p)^2)
  }
data.frame('sample size'=n,est_t1_prob,MSE_t1)
hist(t1)

#Que2
#Simulation study:
alpha=2;beta=3;
eps=0.1
est_t1_prob=0;est_t2_prob=0;
MSE_t1=0;MSE_t2=0;
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  x=matrix(rgamma(n[i]*100,shape = alpha,scale = beta),100,n[i])
  m_1=apply(x,1,mean)
  m_2=apply(x,1,var)
  t1=m_1^2/m_2
  t2=m_2/m_1
  est_t1_prob[i]=mean(abs(t1-alpha)<eps)
  est_t2_prob[i]=mean(abs(t2-beta)<eps)
  MSE_t1[i]=mean((t1-alpha)^2)
  MSE_t2[i]=mean((t2-beta)^2)
  
}
data.frame('sample size'=n,est_t1_prob,est_t2_prob,MSE_t1,MSE_t2)
par(mfrow=c(1,2))
hist(t1)
hist(t2)


#Que3
#Simulation study:
rm(list=ls(all=T))
a=2;b=3;
eps=0.1;
est_t1_prob=0;est_t2_prob=0;
MSE_t1=0;MSE_t2=0;
n=c(10,50,100,500,1000)
for (i in 1:length(n))
{
  x=matrix(rbeta(n[i]*100,shape1 = a,shape2 = b),100,n[i])
  m_1=apply(x,1,mean)
  m_2=apply(x,1,var)
  t1=(m_1*(1-m_1)/m_2-1)*m_1
  t2=(m_1*(1-m_1)/m_2-1)*(1-m_1)
  est_t1_prob[i]=mean(abs(t1-a)<eps)
  est_t2_prob[i]=mean(abs(t2-b)<eps)
  MSE_t1[i]=mean((t1-a)^2)
  MSE_t2[i]=mean((t2-b)^2)
  
}
data.frame('sample size'=n,est_t1_prob,est_t2_prob,MSE_t1,MSE_t2)
par(mfrow=c(1,2))
hist(t1)
hist(t2)
