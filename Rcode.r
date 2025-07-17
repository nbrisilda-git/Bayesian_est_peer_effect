
################################################################################
### Including more information in the selection part
################################################################################
 
setwd("C:\\")
# Check and install required packages
if (!require(mvtnorm)) install.packages("mvtnorm")
library(mvtnorm)

if (!require(igraph)) install.packages("igraph")
library(igraph)

if (!require(rstan)) install.packages("rstan")
library(rstan, quietly = TRUE)

if (!require(shinystan)) install.packages("shinystan")
library(shinystan)


Probit = function(d, yy, alpha0, alpha1, alpha2)
{
  return(pnorm(alpha0+alpha1*yy+alpha2*d))
}

getProb_LSM = function(distMat,yy, alpha0,alpha1,alpha2)
{
  D = distMat
  prob = array(NA,dim=c(nrow(D),nrow(D)))
  for(i in 1:nrow(D)){
    for(j in i:nrow(D)){
      prob[i,j] = Probit(D[i,j], yy[i,j], alpha0, alpha1, alpha2)
      prob[j,i] = Probit(D[j,i], yy[j,i], alpha0, alpha1, alpha2)
    }
  }
  return(prob)
}

genDDabs1 = function(ZZ)   ## Latent  abs distance
{
  dd <- matrix(0,nrow(ZZ),nrow(ZZ))

  for(i in 1:nrow(ZZ)){
   for(j in 1:nrow(ZZ)){
   dd[i,j]<-abs(ZZ[i]-ZZ[j])
   dd[j,i]<-dd[i,j]
                       }
                     }
  diag(dd) = 0
  return(dd)
}


################################################################################
###          Set parameters to generate longitudinal networks
################################################################################

TT = 3
n = 80

## selection
alpha0 = -0.4
alpha1 = -0.45
alpha2 = -0.5

## influence
beta1 = 0.3
beta2 = 0.6
sigma = 0.3

#Z<-rmvt(n,sigma=0.1*diag(2))  # latent c_i and C_j (column 1 and 2)
Z<-rmvt(n,sigma=0.1*diag(1))

PP = list(length = TT) ### Network 0, 1
YY = list(length = TT) ### response
EE = list(length = TT) ### Exposure


Prob = getProb_LSM(distMat = genDDabs1(Z), matrix(0,n,n), alpha0, alpha1, alpha2)
YY[[1]] = rnorm(n,2,sigma)
EE[[1]] = array(0,dim=c(n,1))
PP[[1]] = array(NA,dim=c(n,n))

for(jj in 1:n){
  PP[[1]][jj,] = rbinom(n,1,Prob[jj,])
  if (sum(PP[[1]][jj,])!=0){
      EE[[1]][jj]<-(PP[[1]][jj,]%*%YY[[1]])/sum(PP[[1]][jj,])
}
   }
diag(PP[[1]]) = 0

  
for(kk in 2:TT){
  YY[[kk]] = beta1*YY[[kk-1]]+ beta2*EE[[kk-1]] + Z + rnorm(n,0,sigma)
   EE[[kk]] = array(0,dim=c(n,1))
   Prob = getProb_LSM(distMat = genDDabs1(Z), yy=PP[[kk-1]], alpha0, alpha1, alpha2)
   PP[[kk]] = array(NA,dim=c(n,n))
   for (i in 1:n)
  {
    PP[[kk]][i,] = rbinom(n,1,Prob[i,])
      if (sum(PP[[kk]][i,])!=0)
     EE[[kk]][i]<-(PP[[kk]][i,]%*%YY[[kk]])/sum(PP[[kk]][i,])
  }
  diag(PP[[kk]]) = 0
}


nj<-rep(TT-1,n)
y<-matrix(0,n*(TT-1),1)
ylag<-matrix(0,n*(TT-1),1)
latent<-matrix(0,n*(TT-1),1)
expo<-matrix(0,n*(TT-1),1)


for (j in 1:n){
a<-rep(0,TT-1)
b<-rep(0,TT-1)
cc<-rep(0,TT-1)
i<-0
for (k in 2:TT){
i<-i+1
a[i]<- YY[[k]][j]
b[i]<- YY[[k-1]][j]
cc[i]<-EE[[k-1]][j]
}
y[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]<-a
ylag[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]<-b
latent[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]<-c(Z[j],Z[j])
expo[(sum(nj[1:j-1])+1) : (sum(nj[1:j]))]<-cc
}

x<- cbind(ylag, expo)
y<-c(y)


##### STAN


n1 <- length(y)
x<- cbind(ylag, expo)
x_p<- ncol(x)

PP1<-PP[[1]]
PP2<-PP[[2]]
PP3<-PP[[3]]


initf1 <- function() {list(ZZ = rep(0,n))}  

fit2_stanAR <- stan(file='codeStan.stan', init = initf1,
                   data = list(n1=n, n=n1, x_p=x_p, y=y,x=x,PP1=PP1,PP2=PP2,PP3=PP3),
                   thin = 1000, chains = 2, iter = 10000, warmup = 1000,
                   seed = 9955)


print(fit2_stanAR,par=c("beta","alpha","prec", "ZZ", "sigmasq","tau"))






