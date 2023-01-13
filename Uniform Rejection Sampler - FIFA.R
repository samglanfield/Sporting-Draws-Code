
set.seed(1926002)
initialgroups = array(rep(NA,16),dim=c(1,3,16)) #Gives us all groups but are empty
total=0
drawsUniform=list()
totalvalid=0

probofvalid=0;validity=0

#Start of algorithm

system.time(
while(length(drawsUniform)<5){
  
  pot1 = c("CONMEBOL1","EU1","CONMEBOL2","EU2","EU3","EU4","EU5","EU6","EU7","EU8","EU9","EU10","CONCACAF1","CONMEBOL3","CONCACAF2","CONCACAF3")
  pot2 = c("EU11","CONMEBOL4","CAF1","EU12","AFC1","EU13","CAF2","CONMEBOL5","AFC2","EU14","EU15","EU16","AFC3","CONMEBOL6","CAF3","CONMEBOL7")
  pot3 = c("CAF4","CAF5","OFC1","CAF6","CAF7","CAF8","CAF9","AFC4","AFC5","CAF10","CONCACAF4","CONCACAF5","AFC6","AFC7","CONCACAF6","AFC8")
  
  group = rep("",3)
  
  initialgroups = array(rep(group,16),dim=c(1,3,16)) #Gives us all groups but are empty
  
  initialgroups[1,1,c(1,7,13)] = pot1[c(13,15,16)]
  pot1 = pot1[-c(13,15,16)]
  
  for(i in 1:length(pot1)) {
    selected = sample(pot1, 1)
    initialgroups[1, 1, which(initialgroups[1,1,]=="")[1]] = selected
    pot1 = pot1[!pot1 %in% selected]
  }
  
  # if(length(grep("^EU",selected))==1){
  #   eurplace = rep(0,16)
  #   for(i in 1:16){
  #     if(length(grep("^EU", initialgroups[1, , i]))==1){
  #       eurplace[i]=1
  #     }
  #     assign("eurplace",eurplace,envir = .GlobalEnv)
  #   }}
    
  #initialgroups[1,2,which(eurplace==0)] = sample(pot2[c(1,4,6,10,11,12)],6)
  #pot2 = pot2[-c(1,4,6,10,11,12)] #Comment this back in to auto place EU teams in pot 2 to speed up algorithm greatly
  
  initialgroups[1,2,] = sample(pot2,16)  #if using european being placed need to add in group indec which(eurplace==1) and change 16 to 10
  initialgroups[1,3,] = sample(pot3,16)
  
  
  reg = initialgroups
  
  
  valid = c()
  for(k in 1:16){ #Checks whether the sampled draw is valid
    if(length(grep("^AFC",reg[1,,k]))<=1){
      if(length(grep("^EU",reg[1,,k]))<=1){
        # if(length(grep("^EU",reg[1,,k]))>=1){
        if(length(grep("^CAF",reg[1,,k]))<=1){
          if(length(grep("^CONCACAF",reg[1,,k]))<=1){
            if(length(grep("^CONMEBOL",reg[1,,k]))<=1){
              if(length(grep("^OFC",reg[1,,k]))<=1){
                valid[k] = 1
              } else{valid[k]=0}
            } else{valid[k]=0}
          } else{valid[k]=0}
        } else{valid[k]=0}
      } else{valid[k]=0}
    } else{valid[k]=0}
    #  } else{valid[k]=0}
  } 
  
  validity = prod(valid) 
  if(validity==1){ totalvalid=totalvalid+1
    drawsUniform[[totalvalid]]=initialgroups
    print(paste("Simulation",totalvalid))}
} 
)

total = length(drawsUniform)


#Creates match up probabilities between pots

pot1 = c("CONMEBOL1","EU1","CONMEBOL2","EU2","EU3","EU4","EU5","EU6","EU7","EU8","EU9","EU10","CONCACAF1","CONMEBOL3","CONCACAF2","CONCACAF3")
pot2 = c("EU11","CONMEBOL4","CAF1","EU12","AFC1","EU13","CAF2","CONMEBOL5","AFC2","EU14","EU15","EU16","AFC3","CONMEBOL6","CAF3","CONMEBOL7")
pot3 = c("CAF4","CAF5","OFC1","CAF6","CAF7","CAF8","CAF9","AFC4","AFC5","CAF10","CONCACAF4","CONCACAF5","AFC6","AFC7","CONCACAF6","AFC8")



incMatrix <- function(i, j) {
  countMatrixU[i,j] <<- countMatrixU[i,j]+1;
  return(0);
}
incMatrix2 <- function(i, j) {
  countMatrix2U[i,j] <<- countMatrix2U[i,j]+1;
  return(0);
}
incMatrix3 <- function(i, j) {
  countMatrix3U[i,j] <<- countMatrix3U[i,j]+1;
  return(0);
}

countMatrixU <- matrix(0, 16, 16)
row.names(countMatrixU) <- pot1;
colnames(countMatrixU) <- pot2;

for(i in 1:length(drawsUniform)){
  mapply(incMatrix,drawsUniform[[i]][1,1,],drawsUniform[[i]][1,2,])
}
countMatrixU=countMatrixU/total;countMatrixU

countMatrix2U <- matrix(0, 16, 16)
row.names(countMatrix2U) <- pot1;
colnames(countMatrix2U) <- pot3;

for(i in 1:length(drawsUniform)){
  mapply(incMatrix2,drawsUniform[[i]][1,1,],drawsUniform[[i]][1,3,])
}

countMatrix2U=countMatrix2U/total;countMatrix2U


countMatrix3U <- matrix(0, 16, 16)
row.names(countMatrix3U) <- pot2;
colnames(countMatrix3U) <- pot3;

for(i in 1:length(drawsUniform)){
  mapply(incMatrix3,drawsUniform[[i]][1,2,],drawsUniform[[i]][1,3,])
}

countMatrix3U=countMatrix3U/total;countMatrix3U

#big count of all mathcups

pot1 = c("CONMEBOL1","EU1","CONMEBOL2","EU2","EU3","EU4","EU5","EU6","EU7","EU8","EU9","EU10","CONCACAF1","CONMEBOL3","CONCACAF2","CONCACAF3")
pot2 = c("EU11","CONMEBOL4","CAF1","EU12","AFC1","EU13","CAF2","CONMEBOL5","AFC2","EU14","EU15","EU16","AFC3","CONMEBOL6","CAF3","CONMEBOL7")
pot3 = c("CAF4","CAF5","OFC1","CAF6","CAF7","CAF8","CAF9","AFC4","AFC5","CAF10","CONCACAF4","CONCACAF5","AFC6","AFC7","CONCACAF6","AFC8")

countArrayU = array(0,dim=c(16,16,16),dimnames=list(pot1,pot2,pot3))

incArrayU <- function(i, j, k) {
  countArrayU[i, j, k] <<- countArrayU[i, j, k] + 1
  return(0)
}

for(i in 1:length(drawsFIFA)){
  mapply(incArrayU,drawsUniform[[i]][1,1,],drawsUniform[[i]][1,2,],drawsUniform[[i]][1,3,])
}

countArrayU = countArrayU/length(drawsUniform)

####################################
### VARIANCES


varmatrix1 = countMatrixU * (1 - countMatrixU) / total
round(varmatrix1, 4)
confintmatrix1 = qnorm(0.975, 0, 1) * sqrt(varmatrix1)
round(confintmatrix1, 4)
lbmatrix1 = countMatrixU - confintmatrix1
round(lbmatrix1, 4)
ubmatrix1 = countMatrixU + confintmatrix1
round(ubmatrix1, 4)


varmatrix2 = countMatrix2U * (1 - countMatrix2U) / total
round(varmatrix2, 4)
confintmatrix2 = qnorm(0.975, 0, 1) * sqrt(varmatrix2)
round(confintmatrix2, 4)
lbmatrix2 = countMatrix2U - confintmatrix2
round(lbmatrix2, 4)
ubmatrix2 = countMatrix2U + confintmatrix2
round(ubmatrix2, 4)


varmatrix3 = countMatrix3U * (1 - countMatrix3U) / total
round(varmatrix3, 4)
confintmatrix3 = qnorm(0.975, 0, 1) * sqrt(varmatrix3)
round(confintmatrix3, 4)
lbmatrix3 = countMatrix3U - confintmatrix3
round(lbmatrix3, 4)
ubmatrix3 = countMatrix3U + confintmatrix3
round(ubmatrix3, 4)


