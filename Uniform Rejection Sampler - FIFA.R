
set.seed(1926002)
initialgroups = array(rep(NA,16),dim=c(1,3,16)) #Gives us all groups but are empty
total=0
draws=list()
totalvalid=0

probofvalid=0;validity=0

# countUniform1=0;countUniform2=0;countUniform3=0;countUniform4=0;countUniform5=0;countUniform6=0;countUniform7=0;countUniform8=0;countUniform9=0;countUniform10=0;countUniform11=0;countUniform12=0;countUniform13=0;countUniform14=0;countUniform15=0;countUniform16=0;countUniform17=0;countUniform18=0
# countUniform19=0;countUniform20=0;countUniform21=0;countUniform22=0;countUniform23=0;countUniform24=0;countUniform25=0;countUniform26=0;countUniform27=0;countUniform28=0;countUniform29=0;countUniform30=0;countUniform31=0;countUniform32=0;countUniform33=0;countUniform34=0;countUniform35=0;countUniform36=0


while(length(draws)<1000){
  
  pot1 = c("CONMEBOL1","EU1","CONMEBOL2","EU2","EU3","EU4","EU5","EU6","EU7","EU8","EU9","EU10","CONCACAF1","CONMEBOL3","CONCACAF2","CONCACAF3")
  pot2 = c("EU11","CONMEBOL4","CAF1","EU12","AFC1","EU13","CAF2","CONMEBOL5","AFC2","EU14","EU15","EU16","AFC3","CONMEBOL6","CAF3","CONMEBOL7")
  pot3 = c("CAF4","CAF5","OFC1","CAF6","CAF7","CAF8","CAF9","AFC4","AFC5","CAF10","CONCACAF4","CONCACAF5","AFC6","AFC7","CONCACAF6","AFC8")
  
  group = rep("",3)
  
  initialgroups = array(rep(group,16),dim=c(1,3,16)) #Gives us all groups but are empty
  
  for(i in 1:length(pot1)){
    selected = sample(pot1,1)
    initialgroups[1,1,i] = selected
    pot1 = pot1[!pot1 %in% selected] 
  }
  
  for(i in 1:length(pot2)){
    selected = sample(pot2,1)
    initialgroups[1,2,i] = selected
    pot2 = pot2[!pot2 %in% selected]  
  }
  
  for(i in 1:length(pot3)){
    selected = sample(pot3,1)
    initialgroups[1,3,i] = selected
    pot3 = pot3[!pot3 %in% selected] 
  }
  
  
  reg = initialgroups
  
  
  valid = c()
  for(k in 1:16){
    if(length(grep("^AFC",reg[1,,k]))<=1){
      if(length(grep("^EU",reg[1,,k]))<=2){
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
  # if(validity==1){for(k in 1:16){if("EU1" %in% initialgroups[1,,k] && "CAF1" %in% initialgroups[1,,k]){countUniform1=countUniform1+1}}}
  # if(validity==1){for(k in 1:16){if("EU1" %in% initialgroups[1,,k] && "EU11" %in% initialgroups[1,,k]){countUniform2=countUniform2+1}}}
  # if(validity==1){for(k in 1:16){if("EU1" %in% initialgroups[1,,k] && "CONMEBOL4" %in% initialgroups[1,,k]){countUniform3=countUniform3+1}}}
  # if(validity==1){for(k in 1:16){if("EU1" %in% initialgroups[1,,k] && "AFC1" %in% initialgroups[1,,k]){countUniform4=countUniform4+1}}}
  # if(validity==1){for(k in 1:16){if("CONCACAF1" %in% initialgroups[1,,k] && "CAF1" %in% initialgroups[1,,k]){countUniform5=countUniform5+1}}}
  # if(validity==1){for(k in 1:16){if("CONCACAF1" %in% initialgroups[1,,k] && "AFC1" %in% initialgroups[1,,k]){countUniform6=countUniform6+1}}}
  # if(validity==1){for(k in 1:16){if("CONCACAF1" %in% initialgroups[1,,k] && "EU11" %in% initialgroups[1,,k]){countUniform7=countUniform7+1}}}
  # if(validity==1){for(k in 1:16){if("CONCACAF1" %in% initialgroups[1,,k] && "CONMEBOL4" %in% initialgroups[1,,k]){countUniform8=countUniform8+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL1" %in% initialgroups[1,,k] && "CAF1" %in% initialgroups[1,,k]){countUniform9=countUniform9+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL1" %in% initialgroups[1,,k] && "EU11" %in% initialgroups[1,,k]){countUniform10=countUniform10+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL1" %in% initialgroups[1,,k] && "AFC1" %in% initialgroups[1,,k]){countUniform11=countUniform11+1}}}
  # if(validity==1){for(k in 1:16){if("CONCACAF1" %in% initialgroups[1,,k] && "CAF9" %in% initialgroups[1,,k]){countUniform12=countUniform12+1}}}
  # if(validity==1){for(k in 1:16){if("CONCACAF1" %in% initialgroups[1,,k] && "AFC5" %in% initialgroups[1,,k]){countUniform13=countUniform13+1}}}
  # if(validity==1){for(k in 1:16){if("CONCACAF1" %in% initialgroups[1,,k] && "OFC1" %in% initialgroups[1,,k]){countUniform14=countUniform14+1}}}
  # if(validity==1){for(k in 1:16){if("EU1" %in% initialgroups[1,,k] && "CAF9" %in% initialgroups[1,,k]){countUniform15=countUniform15+1}}}
  # if(validity==1){for(k in 1:16){if("EU1" %in% initialgroups[1,,k] && "OFC1" %in% initialgroups[1,,k]){countUniform16=countUniform16+1}}}
  # if(validity==1){for(k in 1:16){if("EU1" %in% initialgroups[1,,k] && "AFC5" %in% initialgroups[1,,k]){countUniform17=countUniform17+1}}}
  # if(validity==1){for(k in 1:16){if("EU1" %in% initialgroups[1,,k] && "CONCACAF4" %in% initialgroups[1,,k]){countUniform18=countUniform18+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL1" %in% initialgroups[1,,k] && "CAF9" %in% initialgroups[1,,k]){countUniform19=countUniform19+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL1" %in% initialgroups[1,,k] && "OFC1" %in% initialgroups[1,,k]){countUniform20=countUniform20+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL1" %in% initialgroups[1,,k] && "AFC5" %in% initialgroups[1,,k]){countUniform21=countUniform21+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL1" %in% initialgroups[1,,k] && "CONCACAF4" %in% initialgroups[1,,k]){countUniform22=countUniform22+1}}}
  # #ABOVE IS ALL POT 1 PERMUTATIONS WITH BOTH GROUP 1 AND 2 - BELOW IS POT2 AND 3 PERMUATIONS
  # if(validity==1){for(k in 1:16){if("CONMEBOL4" %in% initialgroups[1,,k] && "CAF9" %in% initialgroups[1,,k]){countUniform23=countUniform23+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL4" %in% initialgroups[1,,k] && "OFC1" %in% initialgroups[1,,k]){countUniform24=countUniform24+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL4" %in% initialgroups[1,,k] && "AFC5" %in% initialgroups[1,,k]){countUniform25=countUniform25+1}}}
  # if(validity==1){for(k in 1:16){if("CONMEBOL4" %in% initialgroups[1,,k] && "CONCACAF4" %in% initialgroups[1,,k]){countUniform26=countUniform26+1}}}
  # if(validity==1){for(k in 1:16){if("EU11" %in% initialgroups[1,,k] && "CAF9" %in% initialgroups[1,,k]){countUniform27=countUniform27+1}}}
  # if(validity==1){for(k in 1:16){if("EU11" %in% initialgroups[1,,k] && "OFC1" %in% initialgroups[1,,k]){countUniform28=countUniform28+1}}}
  # if(validity==1){for(k in 1:16){if("EU11" %in% initialgroups[1,,k] && "AFC5" %in% initialgroups[1,,k]){countUniform29=countUniform29+1}}}
  # if(validity==1){for(k in 1:16){if("EU11" %in% initialgroups[1,,k] && "CONCACAF4" %in% initialgroups[1,,k]){countUniform30=countUniform30+1}}}
  # if(validity==1){for(k in 1:16){if("CAF1" %in% initialgroups[1,,k] && "CONCACAF4" %in% initialgroups[1,,k]){countUniform31=countUniform31+1}}}
  # if(validity==1){for(k in 1:16){if("CAF1" %in% initialgroups[1,,k] && "AFC5" %in% initialgroups[1,,k]){countUniform32=countUniform32+1}}}
  # if(validity==1){for(k in 1:16){if("CAF1" %in% initialgroups[1,,k] && "OFC1" %in% initialgroups[1,,k]){countUniform33=countUniform33+1}}}
  # if(validity==1){for(k in 1:16){if("AFC1" %in% initialgroups[1,,k] && "CONCACAF4" %in% initialgroups[1,,k]){countUniform34=countUniform34+1}}}
  # if(validity==1){for(k in 1:16){if("AFC1" %in% initialgroups[1,,k] && "CAF9" %in% initialgroups[1,,k]){countUniform35=countUniform35+1}}}
  # if(validity==1){for(k in 1:16){if("AFC1" %in% initialgroups[1,,k] && "OFC1" %in% initialgroups[1,,k]){countUniform36=countUniform36+1}}}
  if(validity==1){ totalvalid=totalvalid+1
    draws[[totalvalid]]=initialgroups
    print(paste("Simulation",totalvalid))}
} 


# probofvalid = sum(validity)/length(validity)
total = length(draws)


# mean(probofvalid)
# var(probofvalid)
# 
# p2.1 = countUniform1*100/total #5.74%
# p2.2 = countUniform2*100/total #5.82%
# p2.3 = countUniform3*100/total #7.61%
# p2.4 = countUniform4*100/total #5.85%
# p2.5 = countUniform5*100/total #5.28%
# p2.6 = countUniform6*100/total #5.97%
# p2.7 = countUniform7*100/total #5.66%
# p2.8 = countUniform8*100/total #7.55%
# p2.9 = countUniform9*100/total #8.55%
# p2.10 = countUniform10*100/total #8.28%
# p2.11 = countUniform11*100/total #8.89%
# p2.12 = countUniform12*100/total #7.5%
# p2.13 = countUniform13*100/total #8.14%
# p2.14 = countUniform14*100/total #8.54%
# p2.15 = countUniform15*100/total #5.88%
# p2.16 = countUniform16*100/total #5.47%
# p2.17 = countUniform17*100/total #6.22%
# p2.18 = countUniform18*100/total #7.74%
# p2.19 = countUniform19*100/total #6.2%
# p2.20 = countUniform20*100/total #6.3%
# p2.21 = countUniform21*100/total #5.68%
# p2.22 = countUniform22*100/total #8.14%
# p2.23 = countUniform23*100/total #6.97%
# p2.24 = countUniform24*100/total #4.02%
# p2.25 = countUniform25*100/total #6.4%
# p2.26 = countUniform26*100/total #4.62%
# p2.27 = countUniform27*100/total #7.22%
# p2.28 = countUniform28*100/total #4.87%
# p2.29 = countUniform29*100/total #6.57%
# p2.30 = countUniform30*100/total #5.33%
# p2.31 = countUniform31*100/total #10.02%
# p2.32 = countUniform32*100/total #11.81%
# p2.33 = countUniform33*100/total #10.24%
# p2.34 = countUniform34*100/total #7.36%
# p2.35 = countUniform35*100/total #10.12%
# p2.36 = countUniform36*100/total #7.45%




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

for(i in 1:length(draws)){
  mapply(incMatrix,draws[[i]][1,1,],draws[[i]][1,2,])
}
countMatrixU=countMatrixU/total;countMatrixU

countMatrix2U <- matrix(0, 16, 16)
row.names(countMatrix2U) <- pot1;
colnames(countMatrix2U) <- pot3;

for(i in 1:length(draws)){
  mapply(incMatrix2,draws[[i]][1,1,],draws[[i]][1,3,])
}

countMatrix2U=countMatrix2U/total;countMatrix2U


countMatrix3U <- matrix(0, 16, 16)
row.names(countMatrix3U) <- pot2;
colnames(countMatrix3U) <- pot3;

for(i in 1:length(draws)){
  mapply(incMatrix3,draws[[i]][1,2,],draws[[i]][1,3,])
}

countMatrix3U=countMatrix3U/total;countMatrix3U

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
