set.seed(374999987)

checkvalid = function() {
  valid = matrix(0, nrow = 8, ncol = 7); validpair=c(); validrusukr=1
  regcheckerrem = matrix(0, nrow = 8, ncol = 7)
  for (j in 1:8) {
    regcheckerrem[j, 1] = length(grep("^ENG", initialgroups[1, , j]))
    regcheckerrem[j, 2] = length(grep("^FRA", initialgroups[1, , j]))
    regcheckerrem[j, 3] = length(grep("^ESP", initialgroups[1, , j]))
    regcheckerrem[j, 4] = length(grep("^GER", initialgroups[1, , j]))
    regcheckerrem[j, 5] = length(grep("^ITA", initialgroups[1, , j]))
    regcheckerrem[j, 6] = length(grep("^UKR", initialgroups[1, , j]))
    regcheckerrem[j, 7] = length(grep("^POR", initialgroups[1, , j]))
    
    for (l in 1:7) {
      if (regcheckerrem[j, l] <= 1) {
        valid[j, l] = 1
      } else{
        valid[j, l] = 0
      }
    }
    if(length(grep("UKR",initialgroups[1,,j]))>0 && length(grep("RUS",initialgroups[1,,j]))>0){
      validrusukr=0
    } else{validrusukr=1}
  }
  
  for(i in 1:11){
    pairing = get(paste("pair",i,sep=""),env=.GlobalEnv)
    firsthalf = length(which(pairing %in% initialgroups[1,,1:4])); secondhalf = length(which(pairing %in% initialgroups[1,,5:8]))
    if(max(firsthalf,secondhalf)>=2){
      validpair[i]=0
    } else{validpair[i]=1}
    
  }
  return(prod(valid,validpair,validrusukr))
}


initialgroups = array(rep(NA,8),dim=c(1,4,8)) #Gives us all groups but are empty
total=0
#drawsUniformUEFA=list()
x=length(drawsUniformUEFA)
totalvalid=0

probofvalid=0;validity=0


system.time(
  while(length(drawsUniformUEFA)<10000){
    
    pot1=c("ENG1","ESP1","GER1","ENG2","ESP2","ITA1","POR1","FRA1")
    pot2=c("ESP3","ESP4","ITA2","ENG3","FRA2","ENG4","ESP5","GER2")
    pot3=c("POR2","NED1","UKR1","GER3","AUT1","POR3","ITA3","RUS1")
    pot4=c("TUR1","UKR2","BEL1","SUI1","ITA4","SWE1","GER4","MDA1")
    
    pair1=c("ENG1","ENG2")
    pair2=c("ESP1","ESP4")
    pair3=c("ITA1","ITA2")
    pair4=c("GER1","GER2")
    pair5=c("FRA1","FRA2")
    pair6=c("ESP2","ESP3")
    pair7=c("ENG3","ENG4")
    pair8=c("POR2","POR3")
    pair9=c("UKR1","UKR2")
    pair10=c("GER3","GER4")
    pair11=c("ITA3","ITA4")
    pairedteams = c(pair1,pair2,pair3,pair4,pair5,pair6,pair7,pair8,pair9,pair10,pair11)
    nations = c("ENG","FRA","ESP","GER","ITA","UKR","POR","NED","AUT","RUS","TUR","BEL","SUI","SWE","MDA")
    
    group = rep("",3)
    
    initialgroups = array(rep(group,8),dim=c(1,4,8)) #Gives us all groups but are empty
    
    
    initialgroups[1,1,] = sample(pot1,8)
    initialgroups[1,2,] = sample(pot2,8)  
    initialgroups[1,3,] = sample(pot3,8)
    initialgroups[1,4,] = sample(pot4,8)
    
    validity = checkvalid()
    if(validity==1){ totalvalid=totalvalid+1
    drawsUniformUEFA[[x+totalvalid]]=initialgroups
    print(paste("Simulation",totalvalid))}
  } 
)
save(drawsUniformUEFA,file="UEFA_Uniform_Draws_Group_Stage.RData")
total = length(drawsUniformUEFA)

pot1=c("ENG1","ESP1","GER1","ENG2","ESP2","ITA1","POR1","FRA1")
pot2=c("ESP3","ESP4","ITA2","ENG3","FRA2","ENG4","ESP5","GER2")
pot3=c("POR2","NED1","UKR1","GER3","AUT1","POR3","ITA3","RUS1")
pot4=c("TUR1","UKR2","BEL1","SUI1","ITA4","SWE1","GER4","MDA1")


incMatrix <- function(i, j) {
  countMatrixUEFAU[i, j] <<- countMatrixUEFAU[i, j] + 1
  
  return(0)
  
}
incMatrix2 <- function(i, j) {
  countMatrix2UEFAU[i, j] <<- countMatrix2UEFAU[i, j] + 1
  
  return(0)
  
}
incMatrix3 <- function(i, j) {
  countMatrix3UEFAU[i, j] <<- countMatrix3UEFAU[i, j] + 1
  
  return(0)
  
}
incMatrix4 <- function(i, j) {
  countMatrix4UEFAU[i, j] <<- countMatrix4UEFAU[i, j] + 1
  
  return(0)
  
}
incMatrix5 <- function(i, j) {
  countMatrix5UEFAU[i, j] <<- countMatrix5UEFAU[i, j] + 1
  
  return(0)
  
}
incMatrix6 <- function(i, j) {
  countMatrix6UEFAU[i, j] <<- countMatrix6UEFAU[i, j] + 1
  
  return(0)
  
}

countMatrixUEFAU <- matrix(0, 8, 8)
row.names(countMatrixUEFAU) <- pot1
colnames(countMatrixUEFAU) <- pot2


for (i in 1:length(drawsUniformUEFA)) {
  mapply(incMatrix, drawsUniformUEFA[[i]][1, 1, ], drawsUniformUEFA[[i]][1, 2, ])
}
countMatrixUEFAU = countMatrixUEFAU / length(drawsUniformUEFA)
countMatrixUEFAU

countMatrix2UEFAU <- matrix(0, 8, 8)
row.names(countMatrix2UEFAU) <- pot1

colnames(countMatrix2UEFAU) <- pot3


for (i in 1:length(drawsUniformUEFA)) {
  mapply(incMatrix2, drawsUniformUEFA[[i]][1, 1, ], drawsUniformUEFA[[i]][1, 3, ])
}

countMatrix2UEFAU = countMatrix2UEFAU / length(drawsUniformUEFA)
countMatrix2UEFAU


countMatrix3UEFAU <- matrix(0, 8, 8)
row.names(countMatrix3UEFAU) <- pot1

colnames(countMatrix3UEFAU) <- pot4


for (i in 1:length(drawsUniformUEFA)) {
  mapply(incMatrix3, drawsUniformUEFA[[i]][1, 1, ], drawsUniformUEFA[[i]][1, 4, ])
}

countMatrix3UEFAU = countMatrix3UEFAU / length(drawsUniformUEFA)
countMatrix3UEFAU

countMatrix4UEFAU <- matrix(0, 8, 8)
row.names(countMatrix4UEFAU) <- pot2

colnames(countMatrix4UEFAU) <- pot3


for (i in 1:length(drawsUniformUEFA)) {
  mapply(incMatrix4, drawsUniformUEFA[[i]][1, 2, ], drawsUniformUEFA[[i]][1, 3, ])
}

countMatrix4UEFAU = countMatrix4UEFAU / length(drawsUniformUEFA)
countMatrix4UEFAU

countMatrix5UEFAU <- matrix(0, 8, 8)
row.names(countMatrix5UEFAU) <- pot2

colnames(countMatrix5UEFAU) <- pot4


for (i in 1:length(drawsUniformUEFA)) {
  mapply(incMatrix5, drawsUniformUEFA[[i]][1, 2, ], drawsUniformUEFA[[i]][1, 4, ])
}

countMatrix5UEFAU = countMatrix5UEFAU / length(drawsUniformUEFA)
countMatrix5UEFAU

countMatrix6UEFAU <- matrix(0, 8, 8)
row.names(countMatrix6UEFAU) <- pot3

colnames(countMatrix6UEFAU) <- pot4


for (i in 1:length(drawsUniformUEFA)) {
  mapply(incMatrix6, drawsUniformUEFA[[i]][1, 3, ], drawsUniformUEFA[[i]][1, 4, ])
}

countMatrix6UEFAU = countMatrix6UEFAU / length(drawsUniformUEFA)
countMatrix6UEFAU

###############################
### Count for large groups

countArrayU <- array(0, dim=c(8,8,8,8), dimnames=list(pot1,pot2,pot3,pot4))

incArray <- function(i, j, k, s) {
  countArrayU[i, j, k, s] <<- countArrayU[i, j, k, s] + 1
  return(0)
}

for (i in 1:length(drawsUniformUEFA)) {
  mapply(incArray, drawsUniformUEFA[[i]][1, 1, ], drawsUniformUEFA[[i]][1, 2, ], drawsUniformUEFA[[i]][1, 3, ], drawsUniformUEFA[[i]][1, 4, ])
}

countArrayU = countArrayU / length(drawsUniformUEFA)
countArrayU


##############################
## Variances


varmatrix1 = countMatrixUEFAU * (1 - countMatrixUEFAU) / length(drawsUniformUEFA)
round(varmatrix1, 4)
confintmatrix1 = qnorm(0.975, 0, 1) * sqrt(varmatrix1)
round(confintmatrix1, 4)
lbmatrix1 = countMatrixUEFAU - confintmatrix1
round(lbmatrix1, 4)
ubmatrix1 = countMatrixUEFAU + confintmatrix1
round(ubmatrix1, 4)


varmatrix2 = countMatrix2UEFAU * (1 - countMatrix2UEFAU) / length(drawsUniformUEFA)
round(varmatrix2, 4)
confintmatrix2 = qnorm(0.975, 0, 1) * sqrt(varmatrix2)
round(confintmatrix2, 4)
lbmatrix2 = countMatrix2UEFAU - confintmatrix2
round(lbmatrix2, 4)
ubmatrix2 = countMatrix2UEFAU + confintmatrix2
round(ubmatrix2, 4)


varmatrix3 = countMatrix3UEFAU * (1 - countMatrix3UEFAU) / length(drawsUniformUEFA)
round(varmatrix3, 4)
confintmatrix3 = qnorm(0.975, 0, 1) * sqrt(varmatrix3)
round(confintmatrix3, 4)
lbmatrix3 = countMatrix3UEFAU - confintmatrix3
round(lbmatrix3, 4)
ubmatrix3 = countMatrix3UEFAU + confintmatrix3
round(ubmatrix3, 4)

varmatrix4 = countMatrix4UEFAU * (1 - countMatrix4UEFAU) / length(drawsUniformUEFA)
round(varmatrix4, 4)
confintmatrix4 = qnorm(0.975, 0, 1) * sqrt(varmatrix4)
round(confintmatrix4, 4)
lbmatrix4 = countMatrix4UEFAU - confintmatrix4
round(lbmatrix4, 4)
ubmatrix4 = countMatrix4UEFAU + confintmatrix4
round(ubmatrix4, 4)


varmatrix5 = countMatrix5UEFAU * (1 - countMatrix5UEFAU) / length(drawsUniformUEFA)
round(varmatrix5, 4)
confintmatrix5 = qnorm(0.975, 0, 1) * sqrt(varmatrix5)
round(confintmatrix5, 4)
lbmatrix5 = countMatrix5UEFAU - confintmatrix5
round(lbmatrix5, 4)
ubmatrix5 = countMatrix5UEFAU + confintmatrix5
round(ubmatrix5, 4)


varmatrix6 = countMatrix6UEFAU * (1 - countMatrix6UEFAU) / length(drawsUniformUEFA)
round(varmatrix6, 4)
confintmatrix6 = qnorm(0.975, 0, 1) * sqrt(varmatrix6)
round(confintmatrix6, 4)
lbmatrix6 = countMatrix6UEFAU - confintmatrix6
round(lbmatrix6, 4)
ubmatrix6 = countMatrix6UEFAU + confintmatrix6
round(ubmatrix6, 4)




####################################
## Skewness


incMatrixgen <- function(i, j) {
  countmatrix[i, j] <<- countmatrix[i, j] + 1
  return(0)
}

for(i in 1:100){
  assign(paste("subdrawsUEFA",i,sep=""),sample(drawsUniformUEFA,size=length(drawsUniformUEFA)/100,replace = FALSE),envir=.GlobalEnv)
  assign(paste("countmatrixf",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  x=get(paste("countmatrixf",i,sep=""),env=.GlobalEnv)
  rownames(x) = pot1; colnames(x) = pot2;assign(paste("countmatrixf",i,sep=""),x,envir=.GlobalEnv)
  assign(paste("countmatrix2f",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  y=get(paste("countmatrix2f",i,sep=""),env=.GlobalEnv)
  rownames(y) = pot1; colnames(y) = pot3;assign(paste("countmatrix2f",i,sep=""),y,envir=.GlobalEnv)
  assign(paste("countmatrix3f",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  z=get(paste("countmatrix3f",i,sep=""),env=.GlobalEnv)
  rownames(z) = pot1; colnames(z) = pot4;assign(paste("countmatrix3f",i,sep=""),z,envir=.GlobalEnv)
  assign(paste("countmatrix4f",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  x=get(paste("countmatrix4f",i,sep=""),env=.GlobalEnv)
  rownames(x) = pot2; colnames(x) = pot3;assign(paste("countmatrix4f",i,sep=""),x,envir=.GlobalEnv)
  assign(paste("countmatrix5f",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  y=get(paste("countmatrix5f",i,sep=""),env=.GlobalEnv)
  rownames(y) = pot2; colnames(y) = pot4;assign(paste("countmatrix5f",i,sep=""),y,envir=.GlobalEnv)
  assign(paste("countmatrix6f",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  z=get(paste("countmatrix6f",i,sep=""),env=.GlobalEnv)
  rownames(z) = pot3; colnames(z) = pot4;assign(paste("countmatrix6f",i,sep=""),z,envir=.GlobalEnv)
  
  #Skewness matrices
  assign(paste("countmatrixfskew",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  x=get(paste("countmatrixfskew",i,sep=""),env=.GlobalEnv)
  rownames(x) = pot1; colnames(x) = pot2;assign(paste("countmatrixfskew",i,sep=""),x,envir=.GlobalEnv)
  assign(paste("countmatrix2fskew",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  y=get(paste("countmatrix2fskew",i,sep=""),env=.GlobalEnv)
  rownames(y) = pot1; colnames(y) = pot3;assign(paste("countmatrix2fskew",i,sep=""),y,envir=.GlobalEnv)
  assign(paste("countmatrix3fskew",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  z=get(paste("countmatrix3fskew",i,sep=""),env=.GlobalEnv)
  rownames(z) = pot1; colnames(z) = pot4;assign(paste("countmatrix3fskew",i,sep=""),z,envir=.GlobalEnv)
  assign(paste("countmatrix4fskew",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  x=get(paste("countmatrix4fskew",i,sep=""),env=.GlobalEnv)
  rownames(x) = pot2; colnames(x) = pot3;assign(paste("countmatrix4fskew",i,sep=""),x,envir=.GlobalEnv)
  assign(paste("countmatrix5fskew",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  y=get(paste("countmatrix5fskew",i,sep=""),env=.GlobalEnv)
  rownames(y) = pot2; colnames(y) = pot4;assign(paste("countmatrix5fskew",i,sep=""),y,envir=.GlobalEnv)
  assign(paste("countmatrix6fskew",i,sep=""),matrix(0,nrow=12,ncol=12),envir = .GlobalEnv)
  z=get(paste("countmatrix6fskew",i,sep=""),env=.GlobalEnv)
  rownames(z) = pot3; colnames(z) = pot4;assign(paste("countmatrix6fskew",i,sep=""),z,envir=.GlobalEnv)
}

for(i in 1:100){
  for(j in 1:length(subdrawsUEFA1)){
    x = get(paste("subdrawsUEFA",i,sep=""),env=.GlobalEnv)[[j]]
    y = get(paste("countmatrixf",i,sep=""),env=.GlobalEnv)
    
    countmatrix = matrix(0,nrow=12,ncol=12);rownames(countmatrix)=pot1;colnames(countmatrix)=pot2
    mapply(incMatrixgen,x[1,1,],x[1,2,])
    assign(paste("countmatrixf",i,sep=""),y+countmatrix,envir=.GlobalEnv)
    
    z = get(paste("countmatrix2f",i,sep=""),env=.GlobalEnv)
    
    countmatrix = matrix(0,nrow=12,ncol=12);rownames(countmatrix)=pot1;colnames(countmatrix)=pot3
    mapply(incMatrixgen,x[1,1,],x[1,3,])
    assign(paste("countmatrix2f",i,sep=""),z+countmatrix,envir=.GlobalEnv)
    
    w = get(paste("countmatrix3f",i,sep=""),env=.GlobalEnv)
    
    countmatrix = matrix(0,nrow=12,ncol=12);rownames(countmatrix)=pot1;colnames(countmatrix)=pot4
    mapply(incMatrixgen,x[1,1,],x[1,4,])
    assign(paste("countmatrix3f",i,sep=""),w+countmatrix,envir=.GlobalEnv)
    
    y = get(paste("countmatrix4f",i,sep=""),env=.GlobalEnv)
    
    countmatrix = matrix(0,nrow=12,ncol=12);rownames(countmatrix)=pot2;colnames(countmatrix)=pot3
    mapply(incMatrixgen,x[1,2,],x[1,3,])
    assign(paste("countmatrix4f",i,sep=""),y+countmatrix,envir=.GlobalEnv)
    
    z = get(paste("countmatrix5f",i,sep=""),env=.GlobalEnv)
    
    countmatrix = matrix(0,nrow=12,ncol=12);rownames(countmatrix)=pot2;colnames(countmatrix)=pot4
    mapply(incMatrixgen,x[1,2,],x[1,4,])
    assign(paste("countmatrix5f",i,sep=""),z+countmatrix,envir=.GlobalEnv)
    
    w = get(paste("countmatrix6f",i,sep=""),env=.GlobalEnv)
    
    countmatrix = matrix(0,nrow=12,ncol=12);rownames(countmatrix)=pot3;colnames(countmatrix)=pot4
    mapply(incMatrixgen,x[1,3,],x[1,4,])
    assign(paste("countmatrix6f",i,sep=""),w+countmatrix,envir=.GlobalEnv)
    
  }
  assign(paste("countmatrixf",i,sep=""),get(paste("countmatrixf",i,sep=""))/length(subdrawsUEFA1),envir=.GlobalEnv)
  assign(paste("countmatrix2f",i,sep=""),get(paste("countmatrix2f",i,sep=""))/length(subdrawsUEFA1),envir=.GlobalEnv)
  assign(paste("countmatrix3f",i,sep=""),get(paste("countmatrix3f",i,sep=""))/length(subdrawsUEFA1),envir=.GlobalEnv)
  assign(paste("countmatrix4f",i,sep=""),get(paste("countmatrix4f",i,sep=""))/length(subdrawsUEFA1),envir=.GlobalEnv)
  assign(paste("countmatrix5f",i,sep=""),get(paste("countmatrix5f",i,sep=""))/length(subdrawsUEFA1),envir=.GlobalEnv)
  assign(paste("countmatrix6f",i,sep=""),get(paste("countmatrix6f",i,sep=""))/length(subdrawsUEFA1),envir=.GlobalEnv)
}

bernskewness = function(x){
  y = ((1-2*x)/sqrt(x*(1-x)))/sqrt(length(subdrawsUEFA1))
  y[which(y==Inf)] = NA
  return(y)
}

bernexkurt = function(x){
  y=(1-6*x*(1-x))/(x*(1-x))
  y[which(y==Inf)] = NA
  return(y)
}

for(i in 1:100){
  assign(paste("countmatrixfskew",i,sep=""),bernskewness(get(paste("countmatrixf",i,sep=""),env=.GlobalEnv)),envir=.GlobalEnv)
  assign(paste("countmatrix2fskew",i,sep=""),bernskewness(get(paste("countmatrix2f",i,sep=""),env=.GlobalEnv)),envir=.GlobalEnv)
  assign(paste("countmatrix3fskew",i,sep=""),bernskewness(get(paste("countmatrix3f",i,sep=""),env=.GlobalEnv)),envir=.GlobalEnv)
  assign(paste("countmatrix4fskew",i,sep=""),bernskewness(get(paste("countmatrix4f",i,sep=""),env=.GlobalEnv)),envir=.GlobalEnv)
  assign(paste("countmatrix5fskew",i,sep=""),bernskewness(get(paste("countmatrix5f",i,sep=""),env=.GlobalEnv)),envir=.GlobalEnv)
  assign(paste("countmatrix6fskew",i,sep=""),bernskewness(get(paste("countmatrix6f",i,sep=""),env=.GlobalEnv)),envir=.GlobalEnv)
}

countmatrixf1skewarray = array(get(paste("countmatrixfskew",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf2skewarray = array(get(paste("countmatrix2fskew",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf3skewarray = array(get(paste("countmatrix3fskew",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf4skewarray = array(get(paste("countmatrix4fskew",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf5skewarray = array(get(paste("countmatrix5fskew",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf6skewarray = array(get(paste("countmatrix6fskew",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))

for(i in 2:100){
  countmatrixf1skewarray = array(c(countmatrixf1skewarray,get(paste("countmatrixfskew",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf2skewarray = array(c(countmatrixf2skewarray,get(paste("countmatrix2fskew",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf3skewarray = array(c(countmatrixf3skewarray,get(paste("countmatrix3fskew",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf4skewarray = array(c(countmatrixf4skewarray,get(paste("countmatrix4fskew",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf5skewarray = array(c(countmatrixf5skewarray,get(paste("countmatrix5fskew",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf6skewarray = array(c(countmatrixf6skewarray,get(paste("countmatrix6fskew",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
}

apply(countmatrixf1skewarray,c(1,2),mean)
apply(countmatrixf2skewarray,c(1,2),mean)
apply(countmatrixf3skewarray,c(1,2),mean)
apply(countmatrixf4skewarray,c(1,2),mean)
apply(countmatrixf5skewarray,c(1,2),mean)
apply(countmatrixf6skewarray,c(1,2),mean)

apply(countmatrixf1skewarray,c(1,2),var)
apply(countmatrixf2skewarray,c(1,2),var)
apply(countmatrixf3skewarray,c(1,2),var)
apply(countmatrixf4skewarray,c(1,2),var)
apply(countmatrixf5skewarray,c(1,2),var)
apply(countmatrixf6skewarray,c(1,2),var)

countmatrixf1array = array(get(paste("countmatrixf",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf2array = array(get(paste("countmatrix2f",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf3array = array(get(paste("countmatrix3f",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf4array = array(get(paste("countmatrix4f",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf5array = array(get(paste("countmatrix5f",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))
countmatrixf6array = array(get(paste("countmatrix6f",1,sep=""),env=.GlobalEnv),dim=c(12,12,1))

for(i in 2:100){
  countmatrixf1array = array(c(countmatrixf1array,get(paste("countmatrixf",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf2array = array(c(countmatrixf2array,get(paste("countmatrix2f",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf3array = array(c(countmatrixf3array,get(paste("countmatrix3f",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf4array = array(c(countmatrixf4array,get(paste("countmatrix4f",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf5array = array(c(countmatrixf5array,get(paste("countmatrix5f",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
  countmatrixf6array = array(c(countmatrixf6array,get(paste("countmatrix6f",i,sep=""),env=.GlobalEnv)),dim=c(12,12,i))
}

apply(countmatrixf1array,c(1,2),mean)
apply(countmatrixf2array,c(1,2),mean)
apply(countmatrixf3array,c(1,2),mean)
apply(countmatrixf4array,c(1,2),mean)
apply(countmatrixf5array,c(1,2),mean)
apply(countmatrixf6array,c(1,2),mean)

apply(countmatrixf1array,c(1,2),var)
apply(countmatrixf2array,c(1,2),var)
apply(countmatrixf3array,c(1,2),var)
apply(countmatrixf4array,c(1,2),var)
apply(countmatrixf5array,c(1,2),var)
apply(countmatrixf6array,c(1,2),var)

#tests for normality

hist(countmatrixf1array[1,1,],probability =TRUE,breaks = 15)

ksdata = countmatrixf1array[1,1,]

ks.test(ksdata,"pnorm")

shapiro.test(ksdata)

library(tseries);library(nortest)

jarque.bera.test(ksdata)

ad.test(ksdata)

cvm.test(ksdata)

lillie.test(ksdata)

pearson.test(ksdata)

sf.test(ksdata)

## Hellinger Distance Estimate

n=length(countMatrixUEFAU) #dont think this is right

HellingerEstimate = function(x,y,n){
  index = which(x==0)
  term1 = sqrt(y[-index]/x[-index]); term2 = sqrt(x[-index]/y[-index]); term=term1+term2
  output = 1-1/(2*n)*sum(term)
}

HE1 = HellingerEstimate(countMatrixF,countMatrixU,n);round(HE1,4)
HE2 = HellingerEstimate(countMatrix2F,countMatrix2U,n);round(HE2,4)
HE3 = HellingerEstimate(countMatrix3F,countMatrix3U,n);round(HE3,4)

LBTV = 0.5*(HE1)^2;LBTV
UBTV = HE1;UBTV

