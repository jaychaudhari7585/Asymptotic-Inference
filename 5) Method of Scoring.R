#Que.1
rm(list = ls(all=T))
mu=4;sigma=1;n=25
x=rnorm(n,mu,sigma);
u=c(median(x),rep(0,9)) # For newton Raphson method
v=c(median(x),rep(0,9)) # For fisher scoring method
I=1/sigma^2
for (i in 1:10){
  #d1=derivative of log(L)
  #d2=second order derivative of log(L)
  #u[i+1]=u[i]-d1/d2
  #v[i+1]=v[i]+d1/(n*I)
  u[i+1]=u[i]+(mean(x)-u[i])
  v[i+1]=v[i]+n*(mean(x)-v[i])/(n*I)
}
cbind(u,v)



#Que.2
rm(list = ls(all=T))
mu=0;sigma=1;n=25
x=rcauchy(n,mu,sigma);
u=c(median(x),rep(0,9)) # For newton raphson method
v=c(median(x),rep(0,9)) # For fisher scoring method
I=1/2
for (i in 1:10){
  z=x-u[i]
  d1=2*sum(z/(1+z^2))
 d2=2*sum((z^2-1)/(1+z^2)^2)
  u[i+1]=u[i]-d1/d2
  v[i+1]=v[i]+4*sum(z/(1+z^2))/n
}
cbind(u,v)


#Que.5
rm(median(x),list = ls(all=T))
theta=0.5
n1=c(1977,906,904,32)
n=n1[1]+n1[2]+n1[3]+n1[4]
p=c(0.1,0.2,0.3,0.4)
x=rmultinom(n,n1,p);
initial_0=1-4*n1[2]/n
u=c(initial_0,rep(0,10)) # For newton raphson method
v=c(initial_0,rep(0,10)) # For fisher scoring method
I=(2*theta+1)/(2*theta*(1-theta)*(2+theta))
for (i in 1:10){
  d1=2*sum(z/(1+z^2))
  d2=2*sum((z^2-1)/(1+z^2)^2)
  u[i+1]=u[i]-d1/d2
  v[i+1]=v[i]+4*sum(z/(1+z^2))/n
}
u
v