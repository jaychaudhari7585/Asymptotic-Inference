#Que.1
rm=list(ls(all=True));
n=20
lambda=2
x=rpois(n,lambda)
xbar=mean(x)
z=abs(qnorm(0.025,FALSE))
#Pivot method
l1=((2*xbar+z^2/n)-sqrt(z^4/n^2+4*xbar*z^2/n))/2
l2=((2*xbar+z^2/n)+sqrt(z^4/n^2+4*xbar*z^2/n))/2


#Que.3
rm(list = ls(all=T))
mu=4;sigma=1;n=25
x=rcauchy(n,mu,sigma);
u=c(median(x),rep(0,10)) # For newton raphson method
v=c(median(x),rep(0,10)) # For fisher scoring method
I=1/2
for (i in 1:10){
  z=x-u[i]
  d1=2*sum(z/(1+z^2))
  d2=2*sum((z^2-1)/(1+z^2)^2)
  u[i+1]=u[i]-d1/d2
  v[i+1]=v[i]+4*sum(z/(1+z^2))/n
}
u
v
th_hat=v[3]
Z=abs(qnorm(0.05))
ACI=c(th_hat-Z*sqrt(2/n),th_hat+Z*sqrt(2/n))
ACI


#Que.4
rm(list = ls(all=T))
n=25;
theta=3;
x=rexp(n,1/theta)
x_bar=mean(x)
z=abs(qnorm(0.01,FALSE))
#ACI using VST method
L_1=x_bar/(1+z/sqrt(n))
U_1=x_bar/(1-z/sqrt(n))
cbind(L_1,U_1)
length_1=U_1- L_1              
#ACI using Pivotal
L_2=exp(log(x_bar)-z/sqrt(n))
U_2=exp(log(x_bar)+z/sqrt(n))
cbind(L_2,U_2)
length_2=U_2- L_2

