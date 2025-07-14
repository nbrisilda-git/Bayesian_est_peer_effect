functions{
matrix genDDabs1(vector ZZ, int nrows) {
  matrix[nrows, nrows] dd;

  for(i in 1:nrows){
   for(j in 1:nrows){
   dd[i,j]<-abs(ZZ[i]-ZZ[j]);
   dd[j,i]<-abs(ZZ[i]-ZZ[j]);
                       }
  dd[i,i] = 0;
                     }  
  return dd;
}

vector dupli(vector Z, int nrows){
    int k;
	  k = 1;
	  int nrows2;
	  nrows2 = 2*nrows; 
vector[nrows2] dd;
for (i in 1: nrows){
dd[(k):(2*i)] = [Z[i], Z[i]]';
k = 2*i+1;
}
return dd;
}
}


data{
    int<lower=1> n; // number of observations x 2
    int<lower=1> n1; // number of observations
    vector[n] y; // dep_data
    int x_p;
    matrix[n,x_p] x; //covariates
    int PP1[n1,n1]; //network
    int PP2[n1,n1]; //network
    int PP3[n1,n1]; //network
     }
parameters {
    real<lower=0> prec;
    vector[x_p]  beta;
    vector[3]  alpha;
    vector[n1] ZZ;
    real<lower=0> tau;
}
transformed parameters{
    real<lower=0> sigmasq;
    matrix[n1, n1] ZZ1;
    sigmasq = 1/prec;  
    ZZ1 = genDDabs1(ZZ, n1);
     
}


model{
    vector[n] mu;
    vector[n] x1;
     x1 = dupli(ZZ, n1);
     mu = x*beta+x1;
     for (j in 1:n1){
     ZZ[j] ~ normal(0,tau); 
     //ZZ[j,2] ~ normal(0,tau);
       }
    //specify priors;
    sigmasq ~  student_t(4,0,5);//gamma(0.1, 0.1);
    tau ~  student_t(4,0,5);//gamma(0.1, 0.1);
      for (r in 1:x_p){
  beta[r] ~ normal(0, 100); //regression parameters
  }
  
      for (s in 1:3){
  alpha[s] ~ normal(0, 100); //regression parameters
  }
  
   for (i in 1:(n1-1)){
    for (j in (i+1):(n1-1)){
    //real mu1 = alpha[1] + alpha[3]*ZZ1[i,j];
    real mu2 = alpha[1] + alpha[2]*PP1[i,j] + alpha[3]*ZZ1[i,j];
    real mu3 = alpha[1] + alpha[2]*PP2[i,j] + alpha[3]*ZZ1[i,j];
    
   //PP1[i,j] ~ bernoulli(Phi(mu1));
   PP2[i,j] ~ bernoulli(Phi(mu2));
   PP3[i,j] ~ bernoulli(Phi(mu3));
   }
   } 
   
   
    for(i in 1:n) y[i] ~ normal(mu[i],sigmasq);
}
