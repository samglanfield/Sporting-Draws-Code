set.seed(20317)

#Below function checks if current partial draw is valid

checkvalid = function() {
  valid = matrix(0, nrow = 16, ncol = 6)
  regcheckerrem = matrix(0, nrow = 16, ncol = 6)
  for (j in 1:16) {
    regcheckerrem[j, 1] = length(grep("^EU", initialgroups[1, , j]))
    regcheckerrem[j, 2] = length(grep("^CONMEBOL", initialgroups[1, , j]))
    regcheckerrem[j, 3] = length(grep("^CONCACAF", initialgroups[1, , j]))
    regcheckerrem[j, 4] = length(grep("^AFC", initialgroups[1, , j]))
    regcheckerrem[j, 5] = length(grep("^CAF", initialgroups[1, , j]))
    regcheckerrem[j, 6] = length(grep("^OFC", initialgroups[1, , j]))
    if (regcheckerrem[j, 1] <= 1) {
      valid[j, 1] = 1
    } else{
      valid[j, 1] = 0
    }
    for (l in 2:6) {
      if (regcheckerrem[j, l] <= 1) {
        valid[j, l] = 1
      } else{
        valid[j, l] = 0
      }
    }
  }
  return(prod(valid))
}

#Main function

assignment = function(rowposition) {
  numplaced = 0
  placedat = rep(0, 16)
  
  for (i in 1:16) {
    if (is.na(initialgroups[1, rowposition, i]) == FALSE) {
      numplaced = numplaced + 1
      placedat[i] = 1
      
    }
    
  }
  
  if (numplaced >= 16) {
    if (rowposition >= 3) {
      return(1)
    } else{
      return(assignment(rowposition + 1))
    }
    
  } else{
    while (length(is.na(initialgroups[1, rowposition,]) == TRUE) > 0) {
      selected = sample(pot23[rowposition - 1,!pot23[rowposition - 1,] %in% initialgroups[1, rowposition,]], 1)
      
      for (i in 1:16) {
        if (placedat[i] == 0) {
          x = i
          if (length(grep("^EU", selected)) == 1) {
            eurplace = rep(0, 16)
            for (i in 1:16) {
              if (length(grep("^EU", initialgroups[1, , i])) == 1) {
                eurplace[i] = 1
              }
            }
            x = which(eurplace == 0)[1]
          } #If havig 2 EU teams per group comment out this section and update checkvalid to allow for 2 EU teams
          initialgroups[1, rowposition, x] = selected
          assign("initialgroups", initialgroups, envir = .GlobalEnv)
          
          validity = checkvalid()
          
          if (validity == 1 && assignment(rowposition) == 1) {
            return(1)
            
          } else{
            initialgroups[1, rowposition, x] = NA
            assign("initialgroups", initialgroups, envir = .GlobalEnv)
            
          }
          
        }
        
      }
      
      return(0)
      
    }
    
  }
  
}

#Below starts algorithm

drawsFIFA = list()
b = c()

system.time(for (k in 1:10000) {
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
  pot23 = rbind(pot2, pot3)
  initialgroups = array(rep(NA, 16), dim = c(1, 3, 16))
  
  initialgroups[1, 1, c(1, 7, 13)] = pot1[c(13, 15, 16)]
  pot1 = pot1[-c(13, 15, 16)]
  
  for (i in 1:length(pot1)) {
    selected = sample(pot1, 1)
    initialgroups[1, 1, which(is.na(initialgroups[1, 1, ]) == TRUE)[1]] = selected
    pot1 = pot1[!pot1 %in% selected]
  }
  
  b[k] = assignment(2)
  drawsFIFA[[k]] = initialgroups
  print(paste("Simulation", k))
})

#Below produces the match up probabilities between pots

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
  countMatrixF[i, j] <<- countMatrixF[i, j] + 1
  
  return(0)
  
}
incMatrix2 <- function(i, j) {
  countMatrix2F[i, j] <<- countMatrix2F[i, j] + 1
  
  return(0)
  
}
incMatrix3 <- function(i, j) {
  countMatrix3F[i, j] <<- countMatrix3F[i, j] + 1
  
  return(0)
  
}

countMatrixF <- matrix(0, 16, 16)
row.names(countMatrixF) <- pot1

colnames(countMatrixF) <- pot2


for (i in 1:length(drawsFIFA)) {
  mapply(incMatrix, drawsFIFA[[i]][1, 1,], drawsFIFA[[i]][1, 2,])
}
countMatrixF = countMatrixF / length(drawsFIFA)
countMatrixF

countMatrix2F <- matrix(0, 16, 16)
row.names(countMatrix2F) <- pot1

colnames(countMatrix2F) <- pot3


for (i in 1:length(drawsFIFA)) {
  mapply(incMatrix2, drawsFIFA[[i]][1, 1,], drawsFIFA[[i]][1, 3,])
}

countMatrix2F = countMatrix2F / length(drawsFIFA)
countMatrix2F


countMatrix3F <- matrix(0, 16, 16)
row.names(countMatrix3F) <- pot2

colnames(countMatrix3F) <- pot3


for (i in 1:length(drawsFIFA)) {
  mapply(incMatrix3, drawsFIFA[[i]][1, 2,], drawsFIFA[[i]][1, 3,])
}

countMatrix3F = countMatrix3F / length(drawsFIFA)
countMatrix3F

## Count Matrix for the groups


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

countArrayF = array(0,
                    dim = c(16, 16, 16),
                    dimnames = list(pot1, pot2, pot3))

incArrayF <- function(i, j, k) {
  countArrayF[i, j, k] <<- countArrayF[i, j, k] + 1
  return(0)
}

for (i in 1:length(drawsFIFA)) {
  mapply(incArrayF, drawsFIFA[[i]][1, 1, ], drawsFIFA[[i]][1, 2, ], drawsFIFA[[i]][1, 3, ])
}

countArrayF = countArrayF / length(drawsFIFA)

##############################
## Variances


varmatrix1 = countMatrixF * (1 - countMatrixF) / length(drawsFIFA)
round(varmatrix1, 4)
confintmatrix1 = qnorm(0.975, 0, 1) * sqrt(varmatrix1)
round(confintmatrix1, 4)
lbmatrix1 = countMatrixF - confintmatrix1
round(lbmatrix1, 4)
ubmatrix1 = countMatrixF + confintmatrix1
round(ubmatrix1, 4)


varmatrix2 = countMatrix2F * (1 - countMatrix2F) / length(drawsFIFA)
round(varmatrix2, 4)
confintmatrix2 = qnorm(0.975, 0, 1) * sqrt(varmatrix2)
round(confintmatrix2, 4)
lbmatrix2 = countMatrix2F - confintmatrix2
round(lbmatrix2, 4)
ubmatrix2 = countMatrix2F + confintmatrix2
round(ubmatrix2, 4)


varmatrix3 = countMatrix3F * (1 - countMatrix3F) / length(drawsFIFA)
round(varmatrix3, 4)
confintmatrix3 = qnorm(0.975, 0, 1) * sqrt(varmatrix3)
round(confintmatrix3, 4)
lbmatrix3 = countMatrix3F - confintmatrix3
round(lbmatrix3, 4)
ubmatrix3 = countMatrix3F + confintmatrix3
round(ubmatrix3, 4)

#Variance of groups counts

varArray = countArrayF * (1 - countArrayF) / length(drawsFIFA)
round(varArray, 4)
confintArray = qnorm(0.975, 0, 1) * sqrt(varArray)
round(confintArray, 4)
lbArray = countArrayF - confintArray
round(lbArray, 4)
ubArray = countArrayF + confintArray
round(ubArray, 4)

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
index = sample(1:length(drawsFIFA))

for (i in 1:100) {
  low = (1000 * (i - 1) + 1)
  up = 1000 * i
  assign(paste("subdrawsFIFA", i, sep = ""), drawsFIFA[index[low:up]], envir =
           .GlobalEnv)
  assign(paste("countmatrixf", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  x = get(paste("countmatrixf", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot1
  colnames(x) = pot2
  assign(paste("countmatrixf", i, sep = ""), x, envir = .GlobalEnv)
  assign(paste("countmatrix2f", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  y = get(paste("countmatrix2f", i, sep = ""), env = .GlobalEnv)
  rownames(y) = pot1
  colnames(y) = pot3
  assign(paste("countmatrix2f", i, sep = ""), y, envir = .GlobalEnv)
  assign(paste("countmatrix3f", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  z = get(paste("countmatrix3f", i, sep = ""), env = .GlobalEnv)
  rownames(z) = pot2
  colnames(z) = pot3
  assign(paste("countmatrix3f", i, sep = ""), z, envir = .GlobalEnv)
  #array
  assign(paste("countarray", i, sep = ""), array(0, dim = c(16, 16, 16)), envir = .GlobalEnv)
  z = get(paste("countarray", i, sep = ""), env = .GlobalEnv)
  dimnames(z) = list(pot1, pot2, pot3)
  assign(paste("countarray", i, sep = ""), z, envir = .GlobalEnv)
  
  #Skewness matrices
  assign(paste("countmatrixfskew", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  x = get(paste("countmatrixfskew", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot1
  colnames(x) = pot2
  assign(paste("countmatrixfskew", i, sep = ""), x, envir = .GlobalEnv)
  assign(paste("countmatrix2fskew", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  y = get(paste("countmatrix2fskew", i, sep = ""), env = .GlobalEnv)
  rownames(y) = pot1
  colnames(y) = pot3
  assign(paste("countmatrix2fskew", i, sep = ""), y, envir = .GlobalEnv)
  assign(paste("countmatrix3fskew", i, sep = ""),
         matrix(0, nrow = 16, ncol = 16),
         envir = .GlobalEnv)
  z = get(paste("countmatrix3fskew", i, sep = ""), env = .GlobalEnv)
  rownames(z) = pot2
  colnames(z) = pot3
  assign(paste("countmatrix3fskew", i, sep = ""), z, envir = .GlobalEnv)
  #array
  assign(paste("countarrayskew", i, sep = ""), array(0, dim = c(16, 16, 16)), envir = .GlobalEnv)
  z = get(paste("countarrayskew", i, sep = ""), env = .GlobalEnv)
  dimnames(z) = list(pot1, pot2, pot3)
  assign(paste("countarrayskew", i, sep = ""), z, envir = .GlobalEnv)
}

for (i in 1:100) {
  for (j in 1:length(subdrawsFIFA1)) {
    x = get(paste("subdrawsFIFA", i, sep = ""), env = .GlobalEnv)[[j]]
    y = get(paste("countmatrixf", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 16, ncol = 16)
    rownames(countmatrix) = pot1
    colnames(countmatrix) = pot2
    mapply(incMatrixgen, x[1, 1, ], x[1, 2, ])
    assign(paste("countmatrixf", i, sep = ""), y + countmatrix, envir =
             .GlobalEnv)
    
    z = get(paste("countmatrix2f", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 16, ncol = 16)
    rownames(countmatrix) = pot1
    colnames(countmatrix) = pot3
    mapply(incMatrixgen, x[1, 1, ], x[1, 3, ])
    assign(paste("countmatrix2f", i, sep = ""), z + countmatrix, envir =
             .GlobalEnv)
    
    w = get(paste("countmatrix3f", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 16, ncol = 16)
    rownames(countmatrix) = pot2
    colnames(countmatrix) = pot3
    mapply(incMatrixgen, x[1, 2, ], x[1, 3, ])
    assign(paste("countmatrix3f", i, sep = ""), w + countmatrix, envir =
             .GlobalEnv)
    
  }
  assign(paste("countmatrixf", i, sep = ""),
         get(paste("countmatrixf", i, sep = "")) / length(subdrawsFIFA1),
         envir = .GlobalEnv)
  assign(paste("countmatrix2f", i, sep = ""),
         get(paste("countmatrix2f", i, sep = "")) / length(subdrawsFIFA1),
         envir = .GlobalEnv)
  assign(paste("countmatrix3f", i, sep = ""),
         get(paste("countmatrix3f", i, sep = "")) / length(subdrawsFIFA1),
         envir = .GlobalEnv)
}

for (i in 1:100) {
  for (j in 1:length(subdrawsFIFA1)) {
    x = get(paste("subdrawsFIFA", i, sep = ""))[j]
    y = get(paste("countarray", i, sep = ""))
    
    countarray = array(0,
                       dim = c(16, 16, 16),
                       dimnames = list(pot1, pot2, pot3))
    mapply(incArraygen, x[[1]][1, 1, ], x[[1]][1, 2, ], x[[1]][1, 3, ])
    assign(paste("countarray", i, sep = ""), y + countarray, envir = .GlobalEnv)
  }
  assign(paste("countarray", i, sep = ""),
         get(paste("countarray", i, sep = "")) / length(subdrawsFIFA1),
         envir = .GlobalEnv)
}

bernskewness = function(x) {
  y = ((1 - 2 * x) / sqrt(x * (1 - x))) / sqrt(length(subdrawsFIFA1))
  y[which(y == Inf)] = NA
  return(y)
}

bernexkurt = function(x) {
  y = (1 - 6 * x * (1 - x)) / (x * (1 - x))
  y[which(y == Inf)] = NA
  return(y)
}

for (i in 1:100) {
  assign(paste("countmatrixfskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrixf", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
  assign(paste("countmatrix2fskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrix2f", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
  assign(paste("countmatrix3fskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrix3f", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
  assign(paste("countarrayskew", i, sep = ""),
         bernskewness(get(paste(
           "countarray", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
}

countmatrixf1skewarray = array(get(paste("countmatrixfskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(16, 16, 1))
countmatrixf2skewarray = array(get(paste("countmatrix2fskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(16, 16, 1))
countmatrixf3skewarray = array(get(paste("countmatrix3fskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(16, 16, 1))
countarrayskewarray = array(get(paste("countarrayskew", 1, sep = ""), env =
                                  .GlobalEnv), dim = c(16, 16, 16, 1))

for (i in 2:100) {
  countmatrixf1skewarray = array(c(countmatrixf1skewarray, get(
    paste("countmatrixfskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(16, 16, i))
  countmatrixf2skewarray = array(c(countmatrixf2skewarray, get(
    paste("countmatrix2fskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(16, 16, i))
  countmatrixf3skewarray = array(c(countmatrixf3skewarray, get(
    paste("countmatrix3fskew", i, sep = ""), env = .GlobalEnv
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


countmatrixf1array = array(get(paste("countmatrixf", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(16, 16, 1))
countmatrixf2array = array(get(paste("countmatrix2f", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(16, 16, 1))
countmatrixf3array = array(get(paste("countmatrix3f", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(16, 16, 1))
countarrayarray = array(get(paste("countarray", 1, sep = ""), env = .GlobalEnv), dim =
                          c(16, 16, 16, 1))

for (i in 2:100) {
  countmatrixf1array = array(c(countmatrixf1array, get(paste(
    "countmatrixf", i, sep = ""
  ), env = .GlobalEnv)), dim = c(16, 16, i))
  countmatrixf2array = array(c(countmatrixf2array, get(paste(
    "countmatrix2f", i, sep = ""
  ), env = .GlobalEnv)), dim = c(16, 16, i))
  countmatrixf3array = array(c(countmatrixf3array, get(paste(
    "countmatrix3f", i, sep = ""
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
  countarray = get(paste("countmatrixf", i, "array", sep = ""), env = .GlobalEnv)
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

x = which(countArrayF %in% min(countArrayF[which(countArrayF > 0)]))

countArrayF = array(0,
                    dim = c(16, 16, 16),
                    dimnames = list(pot1, pot2, pot3))


Lyapunov = c()

for (i in 1:length(drawsFIFA)) {
  mapply(incArrayF, drawsFIFA[[i]][1, 1, ], drawsFIFA[[i]][1, 2, ], drawsFIFA[[i]][1, 3, ])
  
  probArray = countArrayF / i
  
  Lyapunov[i] = probArray[x] * (1 - probArray[x])
}

plot(cumsum(Lyapunov), type = "l", lwd = 1.5)


## Hellinger estimator 1

#install.packages("textmineR")
library("textmineR")

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

countArrayS = array(0,
                    dim = c(16, 16, 16),
                    dimnames = list(pot1, pot2, pot3))
countArrayU = array(0,
                    dim = c(16, 16, 16),
                    dimnames = list(pot1, pot2, pot3))

incArrayS <- function(i, j, k) {
  countArrayS[i, j, k] <<- countArrayS[i, j, k] + 1
  return(0)
}
incArrayU <- function(i, j, k) {
  countArrayU[i, j, k] <<- countArrayU[i, j, k] + 1
  return(0)
}

HeVector = c()

for (i in 1:length(drawsFIFA)) {
  mapply(incArrayS, drawsFIFA[[i]][1, 1, ], drawsFIFA[[i]][1, 2, ], drawsFIFA[[i]][1, 3, ])
  mapply(incArrayU, drawsUniform[[i]][1, 1, ], drawsUniform[[i]][1, 2, ], drawsUniform[[i]][1, 3, ])
  
  countArrayU = countArrayU / i
  countArrayS = countArrayS / i
  
  indexS = which(countArrayS == 0)
  indexU = which(countArrayU == 0)
  if (length(indexU) >= length(indexS)) {
    index = indexU
  } else {
    index = indexS
  }
  HeVector[i] = CalcHellingerDist(c(countArrayS[-index]), c(countArrayU[-index]))
  
  countArrayU = countArrayU * i
  countArrayS = countArrayS * i
  print(paste("Simulation", i))
}

plot(HeVector, type = "l", lwd = 1.5)
