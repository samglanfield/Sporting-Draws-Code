library(combinat)

#generate all of the pots

pot1 = c("ITNap","PORPor","GERBay","UKTot","UKChe","SPARM","UKMC","PORBen")
pot2 = c("UKLiv","BELCB","ITIM","GERFra","ITMil","GERLei","GERDort","FRAPSG")
pots=rbind(pot1,pot2)
possible1 = c("PORPor","GERBay","SPARM","PORBen")    
possible2 = c("ITNap","GERBay","UKTot","UKChe","SPARM","UKMC","PORBen")
possible3 = c("PORPor","UKTot","UKChe","SPARM","UKMC","PORBen")
possible4 = c("ITNap","PORPor","UKChe","SPARM","UKMC","PORBen")
possible5=c("PORPor","GERBay","UKTot","SPARM","UKMC","PORBen")
possible6=c("ITNap","PORPor","UKTot","UKChe","UKMC","PORBen")
possible7 = c("ITNap","PORPor","UKTot","UKChe","SPARM","PORBen")
possible8 = c("ITNap","PORPor","GERBay","UKTot","UKChe","SPARM","UKMC")

#Checks validity of current partial draw against constraints

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


powerset <- function(x) {
  sets <- lapply(1:(length(x)), function(i) combn(x, i, simplify = F))
  unlist(sets, recursive = F)
}

#generate all subsets of the pots

pot1subsets = powerset(pot1)
pot2subsets = powerset(pot2)


#begin algorithm

countU=matrix(0,nrow=255,ncol=255);countS=matrix(0,nrow=255,ncol=255)

for(i in 1:length(pot1subsets)){
  for(j in 1:length(pot2subsets)){
    for(k in 1:length(drawsU)){
      groupindex1 = which(drawsU[[k]][1,1,] %in% pot1subsets[[i]])
      groupindex2 = which(drawsU[[k]][1,2,] %in% pot2subsets[[j]])
      if(length(which(groupindex1 %in% groupindex2))>0){
        countU[i,j]=countU[i,j]+1
      }
      groupindex3 = which(drawsS[[k]][1,1,] %in% pot1subsets[[i]])
      groupindex4 = which(drawsS[[k]][1,2,] %in% pot2subsets[[j]])
      if(length(which(groupindex3 %in% groupindex4))>0){
        countS[i,j]=countS[i,j]+1
      }
      print(paste(i,j,k))
    }
  }
}

countU = countU/length(drawsU)
countS = countS/length(drawsS)

TVMatrix = abs(countS-countU)
max(TVMatrix)


