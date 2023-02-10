

set.seed(1926002)
initialgroups = array(rep(NA, 16), dim = c(1, 3, 16)) #Gives us all groups but are empty
total = 0
drawsUniform = list()
totalvalid = length(drawsUniform)

probofvalid = 0
validity = 0

#Start of algorithm

system.time(while (length(drawsUniform) < 50000) {
  pot1 = c(
    "CONMEBOL1",
    "EU1",
    "CONMEBOL2",
    "EU2",
    "EU3",
    "EU4",
    "EU5",
    "EU6",
    "EU7",
    "EU8",
    "EU9",
    "EU10",
    "CONCACAF1",
    "CONMEBOL3",
    "CONCACAF2",
    "CONCACAF3"
  )
  pot2 = c(
    "EU11",
    "CONMEBOL4",
    "CAF1",
    "EU12",
    "AFC1",
    "EU13",
    "CAF2",
    "CONMEBOL5",
    "AFC2",
    "EU14",
    "EU15",
    "EU16",
    "AFC3",
    "CONMEBOL6",
    "CAF3",
    "CONMEBOL7"
  )
  pot3 = c(
    "CAF4",
    "CAF5",
    "OFC1",
    "CAF6",
    "CAF7",
    "CAF8",
    "CAF9",
    "AFC4",
    "AFC5",
    "CAF10",
    "CONCACAF4",
    "CONCACAF5",
    "AFC6",
    "AFC7",
    "CONCACAF6",
    "AFC8"
  )
  
  group = rep("", 3)
  
  initialgroups = array(rep(group, 16), dim = c(1, 3, 16)) #Gives us all groups but are empty
  
  initialgroups[1, 1, c(1, 7, 13)] = pot1[c(13, 15, 16)]
  pot1 = pot1[-c(13, 15, 16)]
  
  for (i in 1:length(pot1)) {
    selected = sample(pot1, 1)
    initialgroups[1, 1, which(initialgroups[1, 1, ] == "")[1]] = selected
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
  
  initialgroups[1, 2, ] = sample(pot2, 16)  #if using european being placed need to add in group indec which(eurplace==1) and change 16 to 10
  initialgroups[1, 3, ] = sample(pot3, 16)
  
  
  reg = initialgroups
  
  
  valid = c()
  for (k in 1:16) {
    #Checks whether the sampled draw is valid
    if (length(grep("^AFC", reg[1, , k])) <= 1) {
      if (length(grep("^EU", reg[1, , k])) <= 2) {
        # if(length(grep("^EU",reg[1,,k]))>=1){
        if (length(grep("^CAF", reg[1, , k])) <= 1) {
          if (length(grep("^CONCACAF", reg[1, , k])) <= 1) {
            if (length(grep("^CONMEBOL", reg[1, , k])) <= 1) {
              if (length(grep("^OFC", reg[1, , k])) <= 1) {
                valid[k] = 1
              } else{
                valid[k] = 0
              }
            } else{
              valid[k] = 0
            }
          } else{
            valid[k] = 0
          }
        } else{
          valid[k] = 0
        }
      } else{
        valid[k] = 0
      }
    } else{
      valid[k] = 0
    }
    #  } else{valid[k]=0}
  }
  
  validity = prod(valid)
  if (validity == 1) {
    totalvalid = totalvalid + 1
    drawsUniform[[totalvalid]] = initialgroups
    print(paste("Simulation", totalvalid))
  }
})

total = length(drawsUniform)


#Creates match up probabilities between pots

pot1 = c(
  "CONMEBOL1",
  "EU1",
  "CONMEBOL2",
  "EU2",
  "EU3",
  "EU4",
  "EU5",
  "EU6",
  "EU7",
  "EU8",
  "EU9",
  "EU10",
  "CONCACAF1",
  "CONMEBOL3",
  "CONCACAF2",
  "CONCACAF3"
)
pot2 = c(
  "EU11",
  "CONMEBOL4",
  "CAF1",
  "EU12",
  "AFC1",
  "EU13",
  "CAF2",
  "CONMEBOL5",
  "AFC2",
  "EU14",
  "EU15",
  "EU16",
  "AFC3",
  "CONMEBOL6",
  "CAF3",
  "CONMEBOL7"
)
pot3 = c(
  "CAF4",
  "CAF5",
  "OFC1",
  "CAF6",
  "CAF7",
  "CAF8",
  "CAF9",
  "AFC4",
  "AFC5",
  "CAF10",
  "CONCACAF4",
  "CONCACAF5",
  "AFC6",
  "AFC7",
  "CONCACAF6",
  "AFC8"
)



incMatrix <- function(i, j) {
  countMatrixU[i, j] <<- countMatrixU[i, j] + 1
  
  return(0)
  
}
incMatrix2 <- function(i, j) {
  countMatrix2U[i, j] <<- countMatrix2U[i, j] + 1
  
  return(0)
  
}
incMatrix3 <- function(i, j) {
  countMatrix3U[i, j] <<- countMatrix3U[i, j] + 1
  
  return(0)
  
}

countMatrixU <- matrix(0, 16, 16)
row.names(countMatrixU) <- pot1

colnames(countMatrixU) <- pot2


for (i in 1:length(drawsUniform)) {
  mapply(incMatrix, drawsUniform[[i]][1, 1, ], drawsUniform[[i]][1, 2, ])
}
countMatrixU = countMatrixU / total
countMatrixU

countMatrix2U <- matrix(0, 16, 16)
row.names(countMatrix2U) <- pot1

colnames(countMatrix2U) <- pot3


for (i in 1:length(drawsUniform)) {
  mapply(incMatrix2, drawsUniform[[i]][1, 1, ], drawsUniform[[i]][1, 3, ])
}

countMatrix2U = countMatrix2U / total
countMatrix2U


countMatrix3U <- matrix(0, 16, 16)
row.names(countMatrix3U) <- pot2

colnames(countMatrix3U) <- pot3


for (i in 1:length(drawsUniform)) {
  mapply(incMatrix3, drawsUniform[[i]][1, 2, ], drawsUniform[[i]][1, 3, ])
}

countMatrix3U = countMatrix3U / total
countMatrix3U

#big count of all mathcups

pot1 = c(
  "CONMEBOL1",
  "EU1",
  "CONMEBOL2",
  "EU2",
  "EU3",
  "EU4",
  "EU5",
  "EU6",
  "EU7",
  "EU8",
  "EU9",
  "EU10",
  "CONCACAF1",
  "CONMEBOL3",
  "CONCACAF2",
  "CONCACAF3"
)
pot2 = c(
  "EU11",
  "CONMEBOL4",
  "CAF1",
  "EU12",
  "AFC1",
  "EU13",
  "CAF2",
  "CONMEBOL5",
  "AFC2",
  "EU14",
  "EU15",
  "EU16",
  "AFC3",
  "CONMEBOL6",
  "CAF3",
  "CONMEBOL7"
)
pot3 = c(
  "CAF4",
  "CAF5",
  "OFC1",
  "CAF6",
  "CAF7",
  "CAF8",
  "CAF9",
  "AFC4",
  "AFC5",
  "CAF10",
  "CONCACAF4",
  "CONCACAF5",
  "AFC6",
  "AFC7",
  "CONCACAF6",
  "AFC8"
)

countArrayU = array(0,
                    dim = c(16, 16, 16),
                    dimnames = list(pot1, pot2, pot3))

incArrayU <- function(i, j, k) {
  countArrayU[i, j, k] <<- countArrayU[i, j, k] + 1
  return(0)
}

for (i in 1:length(drawsFIFA)) {
  mapply(incArrayU, drawsUniform[[i]][1, 1, ], drawsUniform[[i]][1, 2, ], drawsUniform[[i]][1, 3, ])
}

countArrayU = countArrayU / length(drawsUniform)

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


####################################
## Skewness


incMatrixgen <- function(i, j) {
  countmatrix[i, j] <<- countmatrix[i, j] + 1
  return(0)
}

incArraygen <- function(i, j, k) {
  countarray[i, j, k] <<- countarray[i, j, k] + 1
  return(0)
}

set.seed(1926002)
index = sample(1:length(drawsUniform))

for (i in 1:100) {
  low = (1000 * (i - 1) + 1)
  up = 1000 * i
  assign(paste("subdrawsUniform", i, sep = ""), drawsUniform[index[low:up]], envir =
           .GlobalEnv)
  assign(paste("countmatrixu", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  x = get(paste("countmatrixu", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot1
  colnames(x) = pot2
  assign(paste("countmatrixu", i, sep = ""), x, envir = .GlobalEnv)
  assign(paste("countmatrix2u", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  y = get(paste("countmatrix2u", i, sep = ""), env = .GlobalEnv)
  rownames(y) = pot1
  colnames(y) = pot3
  assign(paste("countmatrix2u", i, sep = ""), y, envir = .GlobalEnv)
  assign(paste("countmatrix3u", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  z = get(paste("countmatrix3u", i, sep = ""), env = .GlobalEnv)
  rownames(z) = pot2
  colnames(z) = pot3
  assign(paste("countmatrix3u", i, sep = ""), z, envir = .GlobalEnv)
  #array
  assign(paste("countarray", i, sep = ""), array(0, dim = c(16, 16, 16)), envir = .GlobalEnv)
  z = get(paste("countarray", i, sep = ""), env = .GlobalEnv)
  dimnames(z) = list(pot1, pot2, pot3)
  assign(paste("countarray", i, sep = ""), z, envir = .GlobalEnv)
  
  #Skewness matrices
  assign(paste("countmatrixuskew", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  x = get(paste("countmatrixuskew", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot1
  colnames(x) = pot2
  assign(paste("countmatrixuskew", i, sep = ""), x, envir = .GlobalEnv)
  assign(paste("countmatrix2uskew", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  y = get(paste("countmatrix2uskew", i, sep = ""), env = .GlobalEnv)
  rownames(y) = pot1
  colnames(y) = pot3
  assign(paste("countmatrix2uskew", i, sep = ""), y, envir = .GlobalEnv)
  assign(paste("countmatrix3uskew", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  z = get(paste("countmatrix3uskew", i, sep = ""), env = .GlobalEnv)
  rownames(z) = pot2
  colnames(z) = pot3
  assign(paste("countmatrix3uskew", i, sep = ""), z, envir = .GlobalEnv)
  #array
  assign(paste("countarrayskew", i, sep = ""), array(0, dim = c(16, 16, 16)), envir = .GlobalEnv)
  z = get(paste("countarrayskew", i, sep = ""), env = .GlobalEnv)
  dimnames(z) = list(pot1, pot2, pot3)
  assign(paste("countarrayskew", i, sep = ""), z, envir = .GlobalEnv)
}

for (i in 1:100) {
  for (j in 1:length(subdrawsUniform1)) {
    x = get(paste("subdrawsUniform", i, sep = ""), env = .GlobalEnv)[[j]]
    y = get(paste("countmatrixu", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 16, ncol = 16)
    rownames(countmatrix) = pot1
    colnames(countmatrix) = pot2
    mapply(incMatrixgen, x[1, 1, ], x[1, 2, ])
    assign(paste("countmatrixu", i, sep = ""), y + countmatrix, envir =
             .GlobalEnv)
    
    z = get(paste("countmatrix2u", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 16, ncol = 16)
    rownames(countmatrix) = pot1
    colnames(countmatrix) = pot3
    mapply(incMatrixgen, x[1, 1, ], x[1, 3, ])
    assign(paste("countmatrix2u", i, sep = ""), z + countmatrix, envir =
             .GlobalEnv)
    
    w = get(paste("countmatrix3u", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 16, ncol = 16)
    rownames(countmatrix) = pot2
    colnames(countmatrix) = pot3
    mapply(incMatrixgen, x[1, 2, ], x[1, 3, ])
    assign(paste("countmatrix3u", i, sep = ""), w + countmatrix, envir =
             .GlobalEnv)
    
  }
  assign(paste("countmatrixu", i, sep = ""),
         get(paste("countmatrixu", i, sep = "")) / length(subdrawsUniform1),
         envir = .GlobalEnv)
  assign(paste("countmatrix2u", i, sep = ""),
         get(paste("countmatrix2u", i, sep = "")) / length(subdrawsUniform1),
         envir = .GlobalEnv)
  assign(paste("countmatrix3u", i, sep = ""),
         get(paste("countmatrix3u", i, sep = "")) / length(subdrawsUniform1),
         envir = .GlobalEnv)
}

for (i in 1:100) {
  for (j in 1:length(subdrawsUniform1)) {
    x = get(paste("subdrawsUniform", i, sep = ""))[j]
    y = get(paste("countarray", i, sep = ""))
    
    countarray = array(0,
                       dim = c(16, 16, 16),
                       dimnames = list(pot1, pot2, pot3))
    mapply(incArraygen, x[[1]][1, 1, ], x[[1]][1, 2, ], x[[1]][1, 3, ])
    assign(paste("countarray", i, sep = ""), y + countarray, envir = .GlobalEnv)
  }
  assign(paste("countarray", i, sep = ""),
         get(paste("countarray", i, sep = "")) / length(subdrawsUniform1),
         envir = .GlobalEnv)
}

bernskewness = function(x) {
  y = ((1 - 2 * x) / sqrt(x * (1 - x))) / sqrt(length(subdrawsUniform1))
  y[which(y == Inf)] = NA
  return(y)
}

bernexkurt = function(x) {
  y = (1 - 6 * x * (1 - x)) / (x * (1 - x))
  y[which(y == Inf)] = NA
  return(y)
}

for (i in 1:100) {
  assign(paste("countmatrixuskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrixu", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
  assign(paste("countmatrix2uskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrix2u", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
  assign(paste("countmatrix3uskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrix3u", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
  assign(paste("countarrayskew", i, sep = ""),
         bernskewness(get(paste(
           "countarray", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
}

countmatrixf1skewarray = array(get(paste("countmatrixuskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(16, 16, 1))
countmatrixf2skewarray = array(get(paste("countmatrix2uskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(16, 16, 1))
countmatrixf3skewarray = array(get(paste("countmatrix3uskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(16, 16, 1))
countarrayskewarray = array(get(paste("countarrayskew", 1, sep = ""), env =
                                  .GlobalEnv), dim = c(16, 16, 16, 1))

for (i in 2:100) {
  countmatrixf1skewarray = array(c(countmatrixf1skewarray, get(
    paste("countmatrixuskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(16, 16, i))
  countmatrixf2skewarray = array(c(countmatrixf2skewarray, get(
    paste("countmatrix2uskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(16, 16, i))
  countmatrixf3skewarray = array(c(countmatrixf3skewarray, get(
    paste("countmatrix3uskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(16, 16, i))
  countarrayskewarray = array(c(countarrayskewarray, get(
    paste("countarrayskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(16, 16, 16, i))
}

apply(countmatrixf1skewarray, c(1, 2), mean)
apply(countmatrixf2skewarray, c(1, 2), mean)
apply(countmatrixf3skewarray, c(1, 2), mean)
apply(countarrayskewarray, c(1, 2, 3), mean)

apply(countmatrixf1skewarray, c(1, 2), var)
apply(countmatrixf2skewarray, c(1, 2), var)
apply(countmatrixf3skewarray, c(1, 2), var)
apply(countarrayskewarray, c(1, 2, 3), var)


countmatrixf1array = array(get(paste("countmatrixu", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(16, 16, 1))
countmatrixf2array = array(get(paste("countmatrix2u", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(16, 16, 1))
countmatrixf3array = array(get(paste("countmatrix3u", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(16, 16, 1))
countarrayarray = array(get(paste("countarray", 1, sep = ""), env = .GlobalEnv), dim =
                          c(16, 16, 16, 1))

for (i in 2:100) {
  countmatrixf1array = array(c(countmatrixf1array, get(paste(
    "countmatrixu", i, sep = ""
  ), env = .GlobalEnv)), dim = c(16, 16, i))
  countmatrixf2array = array(c(countmatrixf2array, get(paste(
    "countmatrix2u", i, sep = ""
  ), env = .GlobalEnv)), dim = c(16, 16, i))
  countmatrixf3array = array(c(countmatrixf3array, get(paste(
    "countmatrix3u", i, sep = ""
  ), env = .GlobalEnv)), dim = c(16, 16, i))
  countarrayarray = array(c(countarrayarray, get(paste(
    "countarray", i, sep = ""
  ), env = .GlobalEnv)), dim = c(16, 16, 16, i))
}

apply(countmatrixf1array, c(1, 2), mean)
apply(countmatrixf2array, c(1, 2), mean)
apply(countmatrixf3array, c(1, 2), mean)
apply(countarrayarray, c(1, 2, 3), mean)

apply(countmatrixf1array, c(1, 2), var)
apply(countmatrixf2array, c(1, 2), var)
apply(countmatrixf3array, c(1, 2), var)
apply(countarrayarray, c(1, 2, 3), var)


#tests for normality

hist(countarrayarray[1, 1, 1, ], probability = TRUE, breaks = 15)

hist(countmatrixf1array[1, 1, ],
     probability = TRUE,
     breaks = 15)



library(tseries)
library(nortest)

#for count matrices

Shap = array(0, dim = c(16, 16, 3))
JB = array(0, dim = c(16, 16, 3))
AD = array(0, dim = c(16, 16, 3))
CVM = array(0, dim = c(16, 16, 3))
Pearson = array(0, dim = c(16, 16, 3))

for (i in 1:3) {
  countarray = get(paste("countmatrixu", i, "array", sep = ""), env = .GlobalEnv)
  for (j in 1:16) {
    for (k in 1:16) {
      if (sum(countarray[j, k, ]) > 0) {
        Shap[j, k, i] = shapiro.test(countarray[j, k, ])$p.value
        JB[j, k, i] = jarque.bera.test(countarray[j, k, ])$p.value
        AD[j, k, i] = ad.test(countarray[j, k, ])$p.value
        CVM[j, k, i] = cvm.test(countarray[j, k, ])$p.value
        Pearson[j, k, i] = pearson.test(countarray[j, k, ])$p.value
      }
    }
  }
}

Shapaccept = length(which(Shap[which(Shap > 0)] >= 0.05))
totalShap = length(Shap[which(Shap > 0)])
propShap = Shapaccept / totalShap
JBaccept = length(which(JB[which(JB > 0)] >= 0.05))
totalJB = length(JB[which(JB > 0)])
propJB = JBaccept / totalJB
ADaccept = length(which(AD[which(AD > 0)] >= 0.05))
totalAD = length(AD[which(AD > 0)])
propAD = ADaccept / totalAD
CVMaccept = length(which(CVM[which(CVM > 0)] >= 0.05))
totalCVM = length(CVM[which(CVM > 0)])
propCVM = CVMaccept / totalCVM
Pearsonaccept = length(which(Pearson[which(Pearson > 0)] >= 0.05))
totalPearson = length(Pearson[which(Pearson > 0)])
propPearson = Pearsonaccept / totalPearson

#for count array

Shap = array(0, dim = c(16, 16, 16))
JB = array(0, dim = c(16, 16, 16))
AD = array(0, dim = c(16, 16, 16))
CVM = array(0, dim = c(16, 16, 16))
Pearson = array(0, dim = c(16, 16, 16))
countarray = countarrayarray

for (i in 1:16) {
  for (j in 1:16) {
    for (k in 1:16) {
      if (sum(countarray[1, i, j, k]) > 0) {
        Shap[i, j, k] = shapiro.test(countarray[i, j, k, ])$p.value
        JB[i, j, k] = jarque.bera.test(countarray[i, j, k, ])$p.value
        AD[i, j, k] = ad.test(countarray[i, j, k, ])$p.value
        CVM[i, j, k] = cvm.test(countarray[i, j, k, ])$p.value
        Pearson[i, j, k] = pearson.test(countarray[i, j, k, ])$p.value
      }
    }
  }
}

Shapaccept = length(which(Shap[which(Shap > 0)] >= 0.05))
totalShap = length(Shap[which(Shap > 0)])
propShap = Shapaccept / totalShap
JBaccept = length(which(JB[which(JB > 0)] >= 0.05))
totalJB = length(JB[which(JB > 0)])
propJB = JBaccept / totalJB
ADaccept = length(which(AD[which(AD > 0)] >= 0.05))
totalAD = length(AD[which(AD > 0)])
propAD = ADaccept / totalAD
CVMaccept = length(which(CVM[which(CVM > 0)] >= 0.05))
totalCVM = length(CVM[which(CVM > 0)])
propCVM = CVMaccept / totalCVM
Pearsonaccept = length(which(Pearson[which(Pearson > 0)] >= 0.05))
totalPearson = length(Pearson[which(Pearson > 0)])
propPearson = Pearsonaccept / totalPearson


#Lyapunov condition

pot1 = c(
  "CONMEBOL1",
  "EU1",
  "CONMEBOL2",
  "EU2",
  "EU3",
  "EU4",
  "EU5",
  "EU6",
  "EU7",
  "EU8",
  "EU9",
  "EU10",
  "CONCACAF1",
  "CONMEBOL3",
  "CONCACAF2",
  "CONCACAF3"
)
pot2 = c(
  "EU11",
  "CONMEBOL4",
  "CAF1",
  "EU12",
  "AFC1",
  "EU13",
  "CAF2",
  "CONMEBOL5",
  "AFC2",
  "EU14",
  "EU15",
  "EU16",
  "AFC3",
  "CONMEBOL6",
  "CAF3",
  "CONMEBOL7"
)
pot3 = c(
  "CAF4",
  "CAF5",
  "OFC1",
  "CAF6",
  "CAF7",
  "CAF8",
  "CAF9",
  "AFC4",
  "AFC5",
  "CAF10",
  "CONCACAF4",
  "CONCACAF5",
  "AFC6",
  "AFC7",
  "CONCACAF6",
  "AFC8"
)

x = which(countArrayU %in% min(countArrayU[which(countArrayU > 0)]))

countArrayU = array(0,
                    dim = c(16, 16, 16),
                    dimnames = list(pot1, pot2, pot3))


Lyapunov = c()

for (i in 1:length(drawsUniform)) {
  mapply(incArrayU, drawsUniform[[i]][1, 1, ], drawsUniform[[i]][1, 2, ], drawsUniform[[i]][1, 3, ])
  
  probArray = countArrayU / i
  
  Lyapunov[i] = probArray[x] * (1 - probArray[x])
}

plot(cumsum(Lyapunov), type = "l", lwd = 1.5)

