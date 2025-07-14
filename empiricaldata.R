#empirical data
install.packages(rstan)
library(rstan, quietly = T)
library(shinystan)
library(igraph)
#install.packages('plyr')
library('plyr')
#install.packages('epiDisplay') #compare dataset
library(epiDisplay)
#install.packages("arsenal") #compare dataset
library(arsenal)
library(statnet)
#install.packages("rstanmulticore")
library(rstanmulticore) #in paralel MCMC
#install.packages("devtools")
library(devtools)
#install_github('nathanvan/rstanmulticore') #psatn 



#install.packages("statnet")
#remove.packages("rlang")
#install.packages("rlang")
#update.packages("rlang")

#install.packages("doParallel")
#library(doParallel) try to run in paralel
#detectCores()
#registerDoParallel(cores = 6)
#system.time({
# foreach(i= 1:100000, .combine = "c") %dopar%{
# rnorm(1)+rnorm(1)
#}
#})

setwd("C:\\Users\\Brisilda Ndreka\\OneDrive\\Desktop\\paper1\\Bayes_girl\\Glasgow_data")

#saveRDS(m, "model.rds") save model as
#my_model <- readRDS("model.rds")

#setwd("C:\\Users\\nbris\\Desktop\\Bayes_girl")
load("Glasgow-friendship.Rdata")
load("Glasgow-geographic.Rdata")
load("Glasgow-lifestyle.Rdata")
load("Glasgow-selections.Rdata")
load("Glasgow-substances.Rdata")
load("Glasgow-various.Rdata")
load("Glasgow-demographic.Rdata")

####################################################################
# data cleaning
####################################################################

#create temp 0 1 if data have /na and 10
temp <-  (is.na(friendship.1)|(friendship.1==10))*1+ (is.na(friendship.2)|(friendship.2==10))*1+(is.na(friendship.3)|(friendship.3==10))*1

#hist(smoke[,3])
#slect only when temp =0
N1<-friendship.1[temp[,1]==0,temp[,1]==0]
N2<-friendship.2[temp[,1]==0,temp[,1]==0]
N3<-friendship.3[temp[,1]==0,temp[,1]==0]
#
#smoke<-tobacco[rownames(tobacco)%in%rownames(N1),]
#alcoh<-alcohol[rownames(alcohol)%in% rownames(N1),]
activ1<-leisure1[rownames(leisure1)%in%rownames(N1),]
activ2<-leisure2[rownames(leisure2)%in%rownames(N1),]
activ3<-leisure3[rownames(leisure3)%in%rownames(N1),]
Noactivity<-cbind(activ1[,15],activ2[,15],activ3[,15] )
#family<-familysmoking[rownames(familysmoking)%in%rownames(N1),]
money<-money[rownames(money)%in%rownames(N1),]
drug<-cannabis[rownames(cannabis)%in%rownames(N1),]
sport<-cbind(activ1[,5],activ2[,5],activ3[,5])
#sport<-sport[rownames(sport)%in%rownames(N1),]

#religion<-cbind(leisure1[,12],leisure2[,12],leisure3[,12])
#religion<-religion[rownames(religion)%in%rownames(N1),]


#romance<-romance[rownames(romance)%in%rownames(N1),]
###%  data missing
mean(is.na(drug))*100 # 11.36%
mean(is.na(sport))*100    #0%
mean(is.na(money))*100    #0%
mean(is.na(Noactivity))*100    #0%
#mean(is.na(drug))*100     #0.25%
#mean(is.na(sport))*100 
# mean imputation
#canabis
for(i in 1:nrow(drug)) {
  drug[i, ][is.na(drug[i, ])] <- mean(as.numeric(drug[i, ]), na.rm = TRUE)
}

#
for(i in 1:nrow(money)) {
  money[i, ][is.na(money[i, ])] <- mean(as.numeric(money[i, ]), na.rm = TRUE)
}

#Noactivity
for(i in 1:nrow(Noactivity)) {
  Noactivity[i, ][is.na(Noactivity[i, ])] <- mean(as.numeric(Noactivity[i, ]), na.rm = TRUE)
}



#religion
for(i in 1:nrow(religion)) {
  religion[i, ][is.na(religion[i, ])] <- mean(as.numeric(religion[i, ]), na.rm = TRUE)
}


#romance
for(i in 1:nrow(romance)) {
  romance[i, ][is.na(romance[i, ])] <- mean(as.numeric(romance[i, ]), na.rm = TRUE)
}
## observation in alcohol after imputation NA delete rows
#religion<-na.omit(religion)
drug<-na.omit(drug)
sport<-sport[row.names(sport) %in% row.names(sport),]
money<-money[row.names(money) %in% row.names(sport),]
Noactivity<-Noactivity[row.names(Noactivity) %in% row.names(sport),]
#family[family=="2"]<-"0"
#activity<-activity[row.names(activity) %in% row.names(alcoh),]

#sport<-sport[row.names(sport) %in% row.names(alcoh),]

PP1<-N1[row.names(N1)%in% row.names(money), colnames(N1)%in% row.names(money)]
PP2<-N2[row.names(N2)%in% row.names(money), colnames(N2)%in% row.names(money)]
PP3<-N3[row.names(N3)%in% row.names(money), colnames(N3)%in% row.names(money)]
#see we have same student
identical(rownames(PP1), rownames(drug)) #check same obs
identical(colnames(PP1), colnames(PP2))

#ddd<-as.data.frame(alcoh, head=TRUE)
#ddd$t1<-as.integer(ddd$t1)
#ddd$t2<-as.integer(ddd$t2)
#str(drug)
#alcoh<-ddd
##network manipulation  close friends=2 to just friends =1
#PP1[PP1=='2']<-'1'
#PP2[PP2=='2']<-'1'
#PP3[PP3=='2']<-'1'

#N1<- N1[!(row.names(N1) %in%c("s002","s021","s023","s068","s083","s120") ),!(row.names(N1) %in%c("s002","s021","s023","s068","s083","s120"))]
##############################################################
#Data manipulation
####################################################################
write.table(PP1,file="Net1.dat",row.names=FALSE,col.names = FALSE) # drops the rownames
write.table(PP2,file="Net2.dat",row.names=FALSE,col.names = FALSE) # drops the rownames
write.table(PP3,file="Net3.dat",row.names=FALSE,col.names = FALSE) # drops the rownames

#write.table(romance,file="Romance.dat",row.names=FALSE,col.names = FALSE)
#write.table(religion,file="Religion.dat",row.names=FALSE,col.names = FALSE)
write.table(sport,file="sport.dat",row.names=FALSE,col.names = FALSE)
write.table(money,file="money.dat",row.names=FALSE,col.names = FALSE)
write.table(drug,file="drug.dat",row.names=FALSE,col.names = FALSE)
#write.table(drug,file="Drug.dat",row.names=FALSE,col.names = FALSE)
write.table(Noactivity,file="Noactivity.dat",row.names=FALSE,col.names = FALSE)
#write.table(sport,file="Sport.dat",row.names=FALSE,col.names = FALSE)

#N.1<-as.matrix(read.table("Net1.dat",header = FALSE))

#####################################################################
# prepare data influence 
####################################################################
s123D<-read.table("drug.dat",header=FALSE)
s123M<-read.table("money.dat",header=FALSE)
s123Sp<-read.table("sport.dat",header=FALSE)
s123Nac<-read.table("Noactivity.dat",header=FALSE)
#s123d<-read.table("Drug.dat",header=FALSE)
#s123sp<-read.table("Sport.dat",header=FALSE)
#s123re<-read.table("Religion.dat",header=FALSE)
#s123ro<-read.table("Romance.dat",header=FALSE)

s123.1<-as.matrix(read.table("Net1.dat",header = FALSE))
s123.2<-as.matrix(read.table("Net2.dat",header = FALSE))
s123.3<-as.matrix(read.table("Net3.dat",header = FALSE))

#latent space adjusted approach
g1<-network(s123.1,directed=TRUE)
g1%v%"d" <- s123D[,1]
g1%v%"m" <- s123M[,1]
g1%v%"sp" <- s123Sp[,1]
g1%v%"na" <- s123Nac[,1]

g2<-network(s123.2,directed=TRUE)
g2%v%"d" <- s123D[,2]
g2%v%"m" <- s123M[,2]
g2%v%"sp" <- s123Sp[,2]
g2%v%"na" <- s123Nac[,2]

g3<-network(s123.3,directed=TRUE)
g3%v%"d" <- s123D[,3]
g3%v%"m" <- s123M[,3]
g3%v%"sp" <- s123Sp[,3]
g3%v%"na" <- s123Nac[,3]


E<-matrix(0,129,3)
for (i in 1:129)
{
  if (sum(s123.1[i,])!=0)
    E[i,1]<-(s123.1[i,]%*%s123M[,1])/sum(s123.1[i,])
  if (sum(s123.2[i,])!=0)
    E[i,2]<-(s123.2[i,]%*%s123M[,2])/sum(s123.2[i,])
  if (sum(s123.3[i,])!=0)
    E[i,3]<-(s123.3[i,]%*%s123M[,3])/sum(s123.3[i,])
}

money<-c(s123M[,3],s123M[,2])
lag_money<-c(s123M[,2],s123M[,1])
expo<-c(E[,2],E[,1])
sport<-c(s123Sp[,3],s123Sp[,2])
drug<-c(s123D[,3],s123D[,2])
Nact<-c(s123Nac[,3],s123Nac[,2])

money_infl<-data.frame(cbind(money,lag_money,expo,sport,drug,Nact,rep(c(1:129),2),rep(c(1:2),each=129)))

#DATA<-data.frame(cbind(alcohol,lag_alc,expo,drug,smoke,sport))

money_data<-money_infl
money_data$ID<-c(1:129)
money_data<-money_data[order(money_data$ID),]


save(smoke_data,file= "smoke_data.Rdata")
########################################################################
setwd("C:\\Users\\Brisilda Ndreka\\OneDrive\\Desktop\\paper2\\SmokeM2")
load("smoke_infl.Rdata")
smoke_data<-smoke_infl 
smoke_data$ID<-c(1:123) # ID 
#<-infl[c(7,1:6)] #chage ID position
smoke_data<-smoke_data[order(smoke_data$ID),] #prepare for STAN

save(infl, file="staninfl.Rdata")


setwd("C:\\Users\\nbris\\Downloads")
load("Data.Rdata")

str(DATA)
#summary(comparedf(temp,df))
#all.equal(temp,df,check.attributes = TRUE)

####################################################

#STAN run
#################################################

load("staninfl.Rdata")
#infl<-infl[c(1:10),]
infl<- infl[,-c(1)]
y<-infl[,1] #dependent data
#Y<-log(y) #log of data
x<-infl[,-c(1)]
x<-cbind(1,x)
n1 <- length(y)
#n<-10
n<-123
x_p<- ncol(x)


PP1<-as.matrix(read.table("Net1.dat",header = FALSE))
PP2<-as.matrix(read.table("Net2.dat",header = FALSE))
PP3<-as.matrix(read.table("Net3.dat",header = FALSE))

#s50a<-read.table("s50-alcohol.dat",header = FALSE)
#s50d<-read.table("s50-drugs.dat",header = FALSE)
#s50s<-read.table("s50-smoke.dat",header = FALSE)
#s50sp<-read.table("s50-sport.dat",header = FALSE)

#distance<-function(vector,n){
  #dd<-matrix(,nrow = n,ncol = n)
  #for (i in 1:n){
   # for (j in 1:n){
    #  dd[i,j]<-abs(vector[i]-vector[j])
     # dd[j,i]<-abs(vector[i]-vector[j])
    #}
    #dd[i,i]=0
  #}
  #return (dd)
#}

model <- stan(file='empiricalstan.stan',
              data = list(n1=n, n=n1, x_p=x_p, y=y,x=x, PP1=PP1, PP2=PP2,PP3=PP3),
              thin = 100, chains = 3, iter = 100000, warmup = 1500,seed = 9955)

saveRDS(model, "model.rds")
##########################################################
#include cov in model
########################################################
#str(YY1)
#YY1<-distance(c(as.vector(s50a[,1])),length(as.vector(s50a[,1])))
#YY2<-distance(c(as.vector(s50a[,2])),length(as.vector(s50a[,2])))

#DD1<-distance(c(as.vector(s50d[,1])),length(as.vector(s50d[,1])))
#DD2<-distance(c(as.vector(s50d[,2])),length(as.vector(s50d[,2])))

#SS1<-distance(c(as.vector(s50s[,1])),length(as.vector(s50s[,1])))
#SS2<-distance(c(as.vector(s50s[,2])),length(as.vector(s50s[,2])))

#SP1<-distance(c(as.vector(s50sp[,1])),length(as.vector(s50sp[,1])))
#SP2<-distance(c(as.vector(s50sp[,2])),length(as.vector(s50sp[,2])))

#YY1<-as.matrix(YY1)
#YY2<-as.matrix(YY2)

#DD1<-as.matrix(DD1)
#DD2<-as.matrix(DD2)

#SS1<-as.matrix(SS1)
#SS2<-as.matrix(SS2)

#SP1<-as.matrix(SP1)
#SP2<-as.matrix(SP2)

#model <- stan(file='empiricalstan.stan',
                   # data = list(n1=n, n=n1, x_p=x_p, y=y,x=x, PP1=PP1, PP2=PP2,PP3=PP3,
                              #  YY1=YY1,YY2=YY2,
                               # DD1=DD1,DD2=DD2,
                                #SS1=SS1,SS2=SS2,
                                #SP1=SP1,SP2=SP2),
                    #thin = 10, chains = 2, iter = 5000, warmup = 100,
                    #seed = 9955)


str(YY1)
print(model)

###########################################################
#visulaization
##############################################################
#write.table(PP1,file="Netwe1.dat",row.names=FALSE,col.names = FALSE) # drops the rownames
#write.table(PP2,file="Netwe2.dat",row.names=FALSE,col.names = FALSE) # drops the rownames
#write.table(PP3,file="Netwe3.dat",row.names=FALSE,col.names = FALSE) # drops the rownames
setwd("C:\\Users\\Brisilda Ndreka\\Downloads")




N1<-as.matrix(read.table("Net1.dat",header = FALSE))
N2<-as.matrix(read.table("Net2.dat",header = FALSE))
N3<-as.matrix(read.table("Net3.dat",header = FALSE))

A<-as.data.frame(read.table("Smoke.dat",header = FALSE))


G1<-graph_from_adjacency_matrix(N1, mode="undirected", diag=FALSE,weighted=TRUE)
G2<-graph_from_adjacency_matrix(N2, mode="undirected", diag=FALSE,weighted=TRUE)
G3<-graph_from_adjacency_matrix(N3, mode="undirected", diag=FALSE,weighted=TRUE)
G1 <- set_vertex_attr(G1, "name", value=paste("T",1:123,sep=""))
G2 <- set_vertex_attr(G2, "name", value=paste("T",1:123,sep=""))
G3 <- set_vertex_attr(G3, "name", value=paste("T",1:123,sep=""))


cor1<-layout_with_fr(G1)
cor2<-layout_with_fr(G2)
cor3<-layout_with_fr(G3)

#Religion
Tenure<-A$V1
Tenure1<-A$V2
Tenure2<-A$V3


# DRUG
#Tenure<-s123d$V1
#Tenure1<-s123d$V2
#Tenure2<-s123d$V3

#SOMOke
#Tenure<-s123s$V1
#Tenure1<-s123s$V2
#Tenure2<-s123s$V3


 #TenureA<-(round(10*(Tenure)/max((Tenure))))
 colfunc <- colorRampPalette(c("green", "blue"))
 COLWB<-colfunc(3)
 
 TenureA<-Tenure
 TenureA[Tenure==1]<-COLWB[1]
 TenureA[Tenure==2]<-COLWB[2]
 TenureA[Tenure==3]<-COLWB[3]
 TenureA[Tenure==4]<-COLWB[4]
 TenureA[Tenure==5]<-COLWB[5]
 
 TenureA1<-Tenure1
 TenureA1[Tenure1==1]<-COLWB[1]
 TenureA1[Tenure1==2]<-COLWB[2]
 TenureA1[Tenure1==3]<-COLWB[3]
 TenureA1[Tenure1==4]<-COLWB[4]
 TenureA1[Tenure1==5]<-COLWB[5]
 
 TenureA2<-Tenure2
 TenureA2[Tenure2==1]<-COLWB[1]
 TenureA2[Tenure2==2]<-COLWB[2]
 TenureA2[Tenure2==3]<-COLWB[3]
 TenureA2[Tenure2==4]<-COLWB[4]
 TenureA2[Tenure2==5]<-COLWB[5]


 pdf("smoke.pdf", width =15, height = 15)

#png("heatmap.png", width = 465, height = 225, units='mm', res = 300)
par(mar=c(4,4,2,2))
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))

plot(G1, 
     layout=cor1,           # how the nodes are positioned
     vertex.size=7,         # the size of the node shapes
     #vertex.label=NA,
     edge.color="grey70",
     vertex.color=TenureA,
     vertex.label.cex=.4,           # to decrease the size of the node labels
     vertex.label.family="",        # change the font (default is Times New Roman here)
     vertex.label.color="black",    # change the color of the labels to black (default blue)
     vertex.label.dist=0.1,         # change the distance of the labels from the center of the node shape (default is 0)
     vertex.label.degree=pi/2,      # to position the labels below the node shapes
     edge.color="grey70",
     #vertex.sha
     #edge.arrow.size=.5,        # to make the color of the ties/edges 30% black and 70% white
     main="1995",
     edge.width=abs(E(G1)$weight))

plot(G2,layout=cor2,           
     vertex.size=7,         
     #vertex.label=NA,
     
     edge.color="grey70",
     vertex.color=TenureA1,
     vertex.label.cex=.4,           # to decrease the size of the node labels
     vertex.label.family="",        # change the font (default is Times New Roman here)
     vertex.label.color="black",    # change the color of the labels to black (default blue)
     vertex.label.dist=0.1,         # change the distance of the labels from the center of the node shape (default is 0)
     vertex.label.degree=pi/2,
     edge.color="grey70",
     vertex.color=TenureA1,
     edge.color="grey70",
     main="1996",
     edge.width=abs(E(G2)$weight))
par(mar=c(2,14,2,14))
plot(G3,layout=cor3,           
     vertex.size=7,
     main="1997",
     #vertex.label=NA,
     edge.color="grey70",
     vertex.color=TenureA2,
     vertex.label.cex=.4,           # to decrease the size of the node labels
     vertex.label.family="",        # change the font (default is Times New Roman here)
     vertex.label.color="black",    # change the color of the labels to black (default blue)
     vertex.label.dist=0.1,         # change the distance of the labels from the center of the node shape (default is 0)
     vertex.label.degree=pi/2,
     vertex.color=TenureA2,
     edge.color="grey70",
     edge.width=abs(E(G3)$weight))


dev.off()





table(s50a$V1)
table(s50a$V2)
table(s50a$V3)
tab1(s50a$V1, sort.group = "decreasing", cum.percent = FALSE)
tab1(s50a$V2, sort.group = "decreasing", cum.percent = FALSE)
tab1(s50a$V3, sort.group = "decreasing", cum.percent = FALSE)

#########################################################################################
#
