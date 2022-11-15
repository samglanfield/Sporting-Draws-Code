install.packages("combinat")
library(combinat)

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

nsim=50000;validityindex=c();alldraws=list()
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

while(length(which(is.na(initialgroups[1,1,])==TRUE))>0){
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


varmatrix = countMatrix * (1 - countMatrix) / length(alldraws)
round(varmatrix, 4)
confintmatrix = qnorm(0.975, 0, 1) * sqrt(varmatrix)
round(confintmatrix, 4)
lbmatrix = countMatrix - confintmatrix
round(lbmatrix, 4)
ubmatrix = countMatrix + confintmatrix
round(ubmatrix, 4)




