install.packages("combinat")
library(combinat)

#checks validity of the given partial draw

checkvalid = function(){
  valid1=c()
  valid=matrix(0,nrow=8,ncol=3)
  regcheckerrem=matrix(0,nrow=8,ncol=3)
  for(j in 1:8){
    regcheckerrem[j,1] = length(grep("^UK",initialgroups[1,,j]))
    regcheckerrem[j,2] = length(grep("^GER",initialgroups[1,,j]))
    regcheckerrem[j,3] = length(grep("^IT",initialgroups[1,,j]))
    for(l in 1:3){if(regcheckerrem[j,l]<=1){valid[j,l]=1} else{valid[j,l]=0}} 
  }
  for(k in 1:8){
    if(which(pots[1,] %in% initialgroups[1,1,k])==which(pots[2,] %in% initialgroups[1,2,k])){
      valid1[k]=0} else{valid1[k]=1}
  }
  return(prod(valid,valid1))
}

checkvalid2 = function(){
  valid1=c()
  valid=matrix(0,nrow=8,ncol=3)
  regcheckerrem=matrix(0,nrow=8,ncol=3)
  for(j in 1:8){
    regcheckerrem[j,1] = length(grep("^UK",potentialgroups[1,,j]))
    regcheckerrem[j,2] = length(grep("^GER",potentialgroups[1,,j]))
    regcheckerrem[j,3] = length(grep("^IT",potentialgroups[1,,j]))
    for(l in 1:3){if(regcheckerrem[j,l]<=1){valid[j,l]=1} else{valid[j,l]=0}} 
  }
  for(k in 1:8){
    if(which(pots[1,] %in% potentialgroups[1,1,k])==which(pots[2,] %in% potentialgroups[1,2,k])){
      valid1[k]=0} else{valid1[k]=1}
  }
  return(prod(valid,valid1))
}

#list of all possible opponents for each team

possible1 = c("PORPor","GERBay","SPARM","PORBen")    
possible2 = c("ITNap","GERBay","UKTot","UKChe","SPARM","UKMC","PORBen")
possible3 = c("PORPor","UKTot","UKChe","SPARM","UKMC","PORBen")
possible4 = c("ITNap","PORPor","UKChe","SPARM","UKMC","PORBen")
possible5=c("PORPor","GERBay","UKTot","SPARM","UKMC","PORBen")
possible6=c("ITNap","PORPor","UKTot","UKChe","UKMC","PORBen")
possible7 = c("ITNap","PORPor","UKTot","UKChe","SPARM","PORBen")
possible8 = c("ITNap","PORPor","GERBay","UKTot","UKChe","SPARM","UKMC")

pot1 = c("ITNap","PORPor","GERBay","UKTot","UKChe","SPARM","UKMC","PORBen")
pot2 = c("UKLiv","BELCB","ITIM","GERFra","ITMil","GERLei","GERDort","FRAPSG")
pots=rbind(pot1,pot2)

nsim=100000;validityindex=c();alldraws=list()
#Start algorithm
system.time(for(r in 1:nsim){

pot1 = c("ITNap","PORPor","GERBay","UKTot","UKChe","SPARM","UKMC","PORBen")
pot2 = c("UKLiv","BELCB","ITIM","GERFra","ITMil","GERLei","GERDort","FRAPSG")

initialgroups = array(rep(NA,8),dim=c(1,2,8)) 

for (i in 1:length(pot2)) {
  selected = sample(pot2, 1)
  initialgroups[1, 2, i] = selected
  pot2 = pot2[!pot2 %in% selected]
}

pot2 = c("UKLiv","BELCB","ITIM","GERFra","ITMil","GERLei","GERDort","FRAPSG")

for(i in 1:3){
  index = which(pot2 %in% initialgroups[1,2,i])
  teams = get(paste("possible",index,sep=""))
  teams = teams[which(! teams %in% initialgroups[1,1,])]
  initialgroups[1,1,i] = sample(teams,1)
}

while(length(which(is.na(initialgroups[1,1,])==TRUE))>0){ #this section only selects valid teams from the possible valid complete draws remaining
n=which(is.na(initialgroups[1,1,])==TRUE)[1] 
pot1rem = pot1[which(! pot1 %in% initialgroups[1,1,])]
combo = permn(pot1rem) 
pot2rem = initialgroups[1,2,c(n:8)]
draws=list()
for(i in 1:length(combo)){
  draws[[i]] = rbind(combo[[i]],pot2rem)
}

potentialgroups = initialgroups
validindex = c()

for(i in 1:length(draws)){
  potentialgroups[1,,c(n:8)] = draws[[i]]
  validindex[i] = checkvalid2()
}

validindex2 = which(validindex==1)
validdraws = list()
for(i in 1:length(validindex2)){
validdraws[[i]]=draws[[validindex2[i]]]
}

validteams = unique(sapply(validdraws,"[",1))

initialgroups[1,1,n] = sample(validteams,1)
assign("initialgroups",initialgroups,envir = .GlobalEnv)
}
validityindex[r] = checkvalid()
alldraws[[r]] = initialgroups
print(paste("Simulation",r))
})

#generates match up probabilities between the pots

incMatrix <- function(i, j) {
  countMatrix[i,j] <<- countMatrix[i,j]+1;
  return(0);
}

countMatrix <- matrix(0, 8, 8)
pot1 = c("ITNap","PORPor","GERBay","UKTot","UKChe","SPARM","UKMC","PORBen")
pot2 = c("UKLiv","BELCB","ITIM","GERFra","ITMil","GERLei","GERDort","FRAPSG")
row.names(countMatrix) <- pot1;
colnames(countMatrix) <- pot2;

for(i in 1:length(alldraws)){
  mapply(incMatrix,alldraws[[i]][1,1,],alldraws[[i]][1,2,])
}
countMatrix=countMatrix/length(alldraws);countMatrix

#variances

varmatrix = countMatrix * (1 - countMatrix) / length(alldraws)
round(varmatrix, 4)
confintmatrix = qnorm(0.975, 0, 1) * sqrt(varmatrix)
round(confintmatrix, 4)
lbmatrix = countMatrix - confintmatrix
round(lbmatrix, 4)
ubmatrix = countMatrix + confintmatrix
round(ubmatrix, 4)

##############
####### Normality

incMatrixgen <- function(i, j) {
  countmatrix[i, j] <<- countmatrix[i, j] + 1
  return(0)
}

set.seed(1926002)
index = sample(1:length(drawsS))

for(i in 1:100){
  low = (1000*(i-1)+1); up = 1000*i
  assign(paste("subdrawsCL",i,sep=""),drawsS[index[low:up]],envir=.GlobalEnv)
  assign(paste("countmatrixs",i,sep=""),matrix(0,nrow=8,ncol=8),envir = .GlobalEnv)
  x=get(paste("countmatrixs",i,sep=""),env=.GlobalEnv)
  rownames(x) = pot1; colnames(x) = pot2;assign(paste("countmatrixs",i,sep=""),x,envir=.GlobalEnv)
  
  #Skewness matrices
  assign(paste("countmatrixsskew",i,sep=""),matrix(0,nrow=8,ncol=8),envir = .GlobalEnv)
  x=get(paste("countmatrixsskew",i,sep=""),env=.GlobalEnv)
  rownames(x) = pot1; colnames(x) = pot2;assign(paste("countmatrixsskew",i,sep=""),x,envir=.GlobalEnv)
}

for(i in 1:100){
  for(j in 1:length(subdrawsCL1)){
    x = get(paste("subdrawsCL",i,sep=""),env=.GlobalEnv)[[j]]
    y = get(paste("countmatrixs",i,sep=""),env=.GlobalEnv)
    
    countmatrix = matrix(0,nrow=8,ncol=8);rownames(countmatrix)=pot1;colnames(countmatrix)=pot2
    mapply(incMatrixgen,x[1,1,],x[1,2,])
    assign(paste("countmatrixs",i,sep=""),y+countmatrix,envir=.GlobalEnv)
    
  }
  assign(paste("countmatrixs",i,sep=""),get(paste("countmatrixs",i,sep=""))/length(subdrawsCL1),envir=.GlobalEnv)
}


bernskewness = function(x){
  y = ((1-2*x)/sqrt(x*(1-x)))/sqrt(length(subdrawsCL1))
  y[which(y==Inf)] = NA
  return(y)
}

bernexkurt = function(x){
  y=(1-6*x*(1-x))/(x*(1-x))
  y[which(y==Inf)] = NA
  return(y)
}

for(i in 1:100){
  assign(paste("countmatrixsskew",i,sep=""),bernskewness(get(paste("countmatrixs",i,sep=""),env=.GlobalEnv)),envir=.GlobalEnv)
}

countmatrixsskewarray = array(get(paste("countmatrixsskew",1,sep=""),env=.GlobalEnv),dim=c(8,8,1))

for(i in 2:100){
  countmatrixsskewarray = array(c(countmatrixsskewarray,get(paste("countmatrixsskew",i,sep=""),env=.GlobalEnv)),dim=c(8,8,i))
}

apply(countmatrixsskewarray,c(1,2),mean)


apply(countmatrixsskewarray,c(1,2),var)


countmatrixsarray = array(get(paste("countmatrixs",1,sep=""),env=.GlobalEnv),dim=c(8,8,1))

for(i in 2:100){
  countmatrixsarray = array(c(countmatrixsarray,get(paste("countmatrixs",i,sep=""),env=.GlobalEnv)),dim=c(8,8,i))
}

apply(countmatrixsarray,c(1,2),mean)


apply(countmatrixsarray,c(1,2),var)



#tests for normality

hist(countmatrixsarray[5,2,],probability = TRUE,breaks=15)


library(tseries);library(nortest)


#for count matrices

Shap = matrix(0,nrow=8,ncol=8); JB =matrix(0,nrow=8,ncol=8); AD = matrix(0,nrow=8,ncol=8); CVM = matrix(0,nrow=8,ncol=8); Pearson = matrix(0,nrow=8,ncol=8)

countarray = countmatrixsarray
  for(j in 1:8){
    for(k in 1:8){
      if(sum(countarray[j,k,])>0){
        Shap[j,k] = shapiro.test(countarray[j,k,])$p.value
        JB[j,k] = jarque.bera.test(countarray[j,k,])$p.value
        AD[j,k] = ad.test(countarray[j,k,])$p.value
        CVM[j,k] = cvm.test(countarray[j,k,])$p.value
        Pearson[j,k] = pearson.test(countarray[j,k,])$p.value
      }
    }
  }

Shapaccept = length(which(Shap[which(Shap>0)]>=0.05)); totalShap = length(Shap[which(Shap>0)]); propShap = Shapaccept/totalShap
JBaccept = length(which(JB[which(JB>0)]>=0.05)); totalJB = length(JB[which(JB>0)]); propJB = JBaccept/totalJB
ADaccept = length(which(AD[which(AD>0)]>=0.05)); totalAD = length(AD[which(AD>0)]); propAD = ADaccept/totalAD
CVMaccept = length(which(CVM[which(CVM>0)]>=0.05)); totalCVM = length(CVM[which(CVM>0)]); propCVM = CVMaccept/totalCVM
Pearsonaccept = length(which(Pearson[which(Pearson>0)]>=0.05)); totalPearson = length(Pearson[which(Pearson>0)]); propPearson = Pearsonaccept/totalPearson

