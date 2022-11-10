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

incMatrix <- function(i, j) {
  countMatrixu[i,j] <<- countMatrixu[i,j]+1;
  return(0);
}

countMatrixu <- matrix(0, 8, 8)
totalvalid=0
pot1 = c("ITNap","PORPor","GERBay","UKTot","UKChe","SPARM","UKMC","PORBen")
pot2 = c("UKLiv","BELCB","ITIM","GERFra","ITMil","GERLei","GERDort","FRAPSG")
row.names(countMatrixu) <- pot1;
colnames(countMatrixu) <- pot2;

alldraws=list();validity=c()

while(length(alldraws)<100000){
  initialgroups = array(rep(NA,8),dim=c(1,2,8)) 
  pot1 = c("ITNap","PORPor","GERBay","UKTot","UKChe","SPARM","UKMC","PORBen")
  pot2 = c("UKLiv","BELCB","ITIM","GERFra","ITMil","GERLei","GERDort","FRAPSG")
  
  initialgroups[1,1,] = sample(pot1,8)
  initialgroups[1,2,] = sample(pot2,8)
  
  validity = checkvalid()
  if(validity==1){
    alldraws[[totalvalid+1]] = initialgroups
    totalvalid=totalvalid+1
  }
}


for(i in 1:length(alldraws)){
  mapply(incMatrix,alldraws[[i]][1,1,],alldraws[[i]][1,2,])
}
countMatrixu=countMatrixu/length(alldraws);countMatrixu


varmatrixu = countMatrixu * (1 - countMatrixu) / sqrt(length(alldraws))
round(varmatrixu, 4)
confintmatrixu = qnorm(0.975, 0, 1) * varmatrixu
round(confintmatrixu, 4)
lbmatrixu = countMatrixu - confintmatrixu
round(lbmatrixu, 4)
ubmatrixu = countMatrixu + confintmatrixu
round(ubmatrixu, 4)


