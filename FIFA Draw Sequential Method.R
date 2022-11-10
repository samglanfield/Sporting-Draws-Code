

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
    if (regcheckerrem[j, 1] <= 2) {
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
    while (length(is.na(initialgroups[1, rowposition, ]) == TRUE) > 0) {
      selected = sample(pot23[rowposition - 1, !pot23[rowposition - 1, ] %in% initialgroups[1, rowposition, ]], 1) 
      

      for (i in 1:16) {
        if (placedat[i] == 0) {
          
          
          initialgroups[1, rowposition, i] = selected
          assign("initialgroups", initialgroups, envir = .GlobalEnv)
          
          validity = checkvalid()
          
          if (validity == 1 && assignment(rowposition) == 1) {
          
            
            return(1)
            
          } else{
            
            initialgroups[1, rowposition, i] = NA
            assign("initialgroups", initialgroups, envir = .GlobalEnv)
            
          }
          
        }
        
      }
      
      return(0)
      
    }
    
  }
  
}

draws = list()
b = c()

for (k in 1:10000) {
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
  
  for (i in 1:length(pot1)) {
    selected = sample(pot1, 1)
    initialgroups[1, 1, i] = selected
    pot1 = pot1[!pot1 %in% selected]
  }
  
  b[k] = assignment(2)
  draws[[k]] = initialgroups
}







countFIFA1 = 0
countFIFA2 = 0
countFIFA3 = 0
countFIFA4 = 0
countFIFA5 = 0
countFIFA6 = 0
countFIFA7 = 0
countFIFA8 = 0
countFIFA9 = 0
countFIFA10 = 0
countFIFA11 = 0
countFIFA12 = 0
countFIFA13 = 0
countFIFA14 = 0
countFIFA15 = 0
countFIFA16 = 0
countFIFA17 = 0
countFIFA18 = 0
countFIFA19 = 0
countFIFA20 = 0
countFIFA21 = 0
countFIFA22 = 0
countFIFA23 = 0
countFIFA24 = 0
countFIFA25 = 0
countFIFA26 = 0
countFIFA27 = 0
countFIFA28 = 0
countFIFA29 = 0
countFIFA30 = 0
countFIFA31 = 0
countFIFA32 = 0
countFIFA33 = 0
countFIFA34 = 0
countFIFA35 = 0
countFIFA36 = 0
# 
# for (i in 1:length(draws)) {
#   for (k in 1:16) {
#     if ("EU1" %in% draws[[i]][1, , k] &&
#         "CAF1" %in% draws[[i]][1, , k]) {
#       countFIFA1 = countFIFA1 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU1" %in% draws[[i]][1, , k] &&
#         "EU11" %in% draws[[i]][1, , k]) {
#       countFIFA2 = countFIFA2 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU1" %in% draws[[i]][1, , k] &&
#         "CONMEBOL4" %in% draws[[i]][1, , k]) {
#       countFIFA3 = countFIFA3 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU1" %in% draws[[i]][1, , k] &&
#         "AFC1" %in% draws[[i]][1, , k]) {
#       countFIFA4 = countFIFA4 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONCACAF1" %in% draws[[i]][1, , k] &&
#         "CAF1" %in% draws[[i]][1, , k]) {
#       countFIFA5 = countFIFA5 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONCACAF1" %in% draws[[i]][1, , k] &&
#         "AFC1" %in% draws[[i]][1, , k]) {
#       countFIFA6 = countFIFA6 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONCACAF1" %in% draws[[i]][1, , k] &&
#         "EU11" %in% draws[[i]][1, , k]) {
#       countFIFA7 = countFIFA7 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONCACAF1" %in% draws[[i]][1, , k] &&
#         "CONMEBOL4" %in% draws[[i]][1, , k]) {
#       countFIFA8 = countFIFA8 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL1" %in% draws[[i]][1, , k] &&
#         "CAF1" %in% draws[[i]][1, , k]) {
#       countFIFA9 = countFIFA9 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL1" %in% draws[[i]][1, , k] &&
#         "EU11" %in% draws[[i]][1, , k]) {
#       countFIFA10 = countFIFA10 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL1" %in% draws[[i]][1, , k] &&
#         "AFC1" %in% draws[[i]][1, , k]) {
#       countFIFA11 = countFIFA11 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONCACAF1" %in% draws[[i]][1, , k] &&
#         "CAF9" %in% draws[[i]][1, , k]) {
#       countFIFA12 = countFIFA12 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONCACAF1" %in% draws[[i]][1, , k] &&
#         "AFC5" %in% draws[[i]][1, , k]) {
#       countFIFA13 = countFIFA13 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONCACAF1" %in% draws[[i]][1, , k] &&
#         "OFC1" %in% draws[[i]][1, , k]) {
#       countFIFA14 = countFIFA14 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU1" %in% draws[[i]][1, , k] &&
#         "CAF9" %in% draws[[i]][1, , k]) {
#       countFIFA15 = countFIFA15 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU1" %in% draws[[i]][1, , k] &&
#         "OFC1" %in% draws[[i]][1, , k]) {
#       countFIFA16 = countFIFA16 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU1" %in% draws[[i]][1, , k] &&
#         "AFC5" %in% draws[[i]][1, , k]) {
#       countFIFA17 = countFIFA17 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU1" %in% draws[[i]][1, , k] &&
#         "CONCACAF4" %in% draws[[i]][1, , k]) {
#       countFIFA18 = countFIFA18 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL1" %in% draws[[i]][1, , k] &&
#         "CAF9" %in% draws[[i]][1, , k]) {
#       countFIFA19 = countFIFA19 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL1" %in% draws[[i]][1, , k] &&
#         "OFC1" %in% draws[[i]][1, , k]) {
#       countFIFA20 = countFIFA20 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL1" %in% draws[[i]][1, , k] &&
#         "AFC5" %in% draws[[i]][1, , k]) {
#       countFIFA21 = countFIFA21 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL1" %in% draws[[i]][1, , k] &&
#         "CONCACAF4" %in% draws[[i]][1, , k]) {
#       countFIFA22 = countFIFA22 + 1
#     }
#   }
#   #ABOVE IS ALL POT 1 PERMUTATIONS WITH BOTH GROUP 1 AND 2 - BELOW IS POT2 AND 3 PERMUATIONS
#   for (k in 1:16) {
#     if ("CONMEBOL4" %in% draws[[i]][1, , k] &&
#         "CAF9" %in% draws[[i]][1, , k]) {
#       countFIFA23 = countFIFA23 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL4" %in% draws[[i]][1, , k] &&
#         "OFC1" %in% draws[[i]][1, , k]) {
#       countFIFA24 = countFIFA24 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL4" %in% draws[[i]][1, , k] &&
#         "AFC5" %in% draws[[i]][1, , k]) {
#       countFIFA25 = countFIFA25 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CONMEBOL4" %in% draws[[i]][1, , k] &&
#         "CONCACAF4" %in% draws[[i]][1, , k]) {
#       countFIFA26 = countFIFA26 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU11" %in% draws[[i]][1, , k] &&
#         "CAF9" %in% draws[[i]][1, , k]) {
#       countFIFA27 = countFIFA27 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU11" %in% draws[[i]][1, , k] &&
#         "OFC1" %in% draws[[i]][1, , k]) {
#       countFIFA28 = countFIFA28 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU11" %in% draws[[i]][1, , k] &&
#         "AFC5" %in% draws[[i]][1, , k]) {
#       countFIFA29 = countFIFA29 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("EU11" %in% draws[[i]][1, , k] &&
#         "CONCACAF4" %in% draws[[i]][1, , k]) {
#       countFIFA30 = countFIFA30 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CAF1" %in% draws[[i]][1, , k] &&
#         "CONCACAF4" %in% draws[[i]][1, , k]) {
#       countFIFA31 = countFIFA31 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CAF1" %in% draws[[i]][1, , k] &&
#         "AFC5" %in% draws[[i]][1, , k]) {
#       countFIFA32 = countFIFA32 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("CAF1" %in% draws[[i]][1, , k] &&
#         "OFC1" %in% draws[[i]][1, , k]) {
#       countFIFA33 = countFIFA33 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("AFC1" %in% draws[[i]][1, , k] &&
#         "CONCACAF4" %in% draws[[i]][1, , k]) {
#       countFIFA34 = countFIFA34 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("AFC1" %in% draws[[i]][1, , k] &&
#         "CAF9" %in% draws[[i]][1, , k]) {
#       countFIFA35 = countFIFA35 + 1
#     }
#   }
#   for (k in 1:16) {
#     if ("AFC1" %in% draws[[i]][1, , k] &&
#         "OFC1" %in% draws[[i]][1, , k]) {
#       countFIFA36 = countFIFA36 + 1
#     }
#   }
# }

# 
# p1.1 = countFIFA1 * 100 / length(draws) #5.74%
# p1.2 = countFIFA2 * 100 / length(draws) #5.82%
# p1.3 = countFIFA3 * 100 / length(draws) #7.61%
# p1.4 = countFIFA4 * 100 / length(draws) #5.85%
# p1.5 = countFIFA5 * 100 / length(draws) #5.28%
# p1.6 = countFIFA6 * 100 / length(draws) #5.97%
# p1.7 = countFIFA7 * 100 / length(draws) #5.66%
# p1.8 = countFIFA8 * 100 / length(draws) #7.55%
# p1.9 = countFIFA9 * 100 / length(draws) #8.55%
# p1.10 = countFIFA10 * 100 / length(draws) #8.28%
# p1.11= countFIFA11 * 100 / length(draws) #8.89%
# p1.12 = countFIFA12 * 100 / length(draws) #7.5%
# p1.13 = countFIFA13 * 100 / length(draws) #8.14%
# p1.14 = countFIFA14 * 100 / length(draws) #8.54%
# p1.15 = countFIFA15 * 100 / length(draws) #5.88%
# p1.16 = countFIFA16 * 100 / length(draws) #5.47%
# p1.17 = countFIFA17 * 100 / length(draws) #6.22%
# p1.18 = countFIFA18 * 100 / length(draws) #7.74%
# p1.19 = countFIFA19 * 100 / length(draws) #6.2%
# p1.20 = countFIFA20 * 100 / length(draws) #6.3%
# p1.21 = countFIFA21 * 100 / length(draws) #5.68%
# p1.22 = countFIFA22 * 100 / length(draws) #8.14%
# p1.23 = countFIFA23 * 100 / length(draws) #6.97%
# p1.24 = countFIFA24 * 100 / length(draws) #4.02%
# p1.25 = countFIFA25 * 100 / length(draws) #6.4%
# p1.26 = countFIFA26 * 100 / length(draws) #4.62%
# p1.27 = countFIFA27 * 100 / length(draws) #7.22%
# p1.28 = countFIFA28 * 100 / length(draws) #4.87%
# p1.29 = countFIFA29 * 100 / length(draws) #6.57%
# p1.30 = countFIFA30 * 100 / length(draws) #5.33%
# p1.31 = countFIFA31 * 100 / length(draws) #10.02%
# p1.32 = countFIFA32 * 100 / length(draws) #11.81%
# p1.33 = countFIFA33 * 100 / length(draws) #10.24%
# p1.34 = countFIFA34 * 100 / length(draws) #7.36%
# p1.35 = countFIFA35 * 100 / length(draws) #10.12%
# p1.36 = countFIFA36 * 100 / length(draws) #7.45%

#countmatrices are below
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


for (i in 1:length(draws)) {
  mapply(incMatrix, draws[[i]][1, 1, ], draws[[i]][1, 2, ])
}
countMatrixF = countMatrixF / length(draws)
countMatrixF

countMatrix2F <- matrix(0, 16, 16)
row.names(countMatrix2F) <- pot1

colnames(countMatrix2F) <- pot3


for (i in 1:length(draws)) {
  mapply(incMatrix2, draws[[i]][1, 1, ], draws[[i]][1, 3, ])
}

countMatrix2F = countMatrix2F / length(draws)
countMatrix2F


countMatrix3F <- matrix(0, 16, 16)
row.names(countMatrix3F) <- pot2

colnames(countMatrix3F) <- pot3


for (i in 1:length(draws)) {
  mapply(incMatrix3, draws[[i]][1, 2, ], draws[[i]][1, 3, ])
}

countMatrix3F = countMatrix3F / length(draws)
countMatrix3F



##############################
## Variances


varmatrix1 = countMatrixF * (1 - countMatrixF) / sqrt(length(draws))
round(varmatrix1, 4)
confintmatrix1 = qnorm(0.975, 0, 1) * varmatrix1
round(confintmatrix1, 4)
lbmatrix1 = countMatrixF - confintmatrix1
round(lbmatrix1, 4)
ubmatrix1 = countMatrixF + confintmatrix1
round(ubmatrix1, 4)


varmatrix2 = countMatrix2F * (1 - countMatrix2F) / sqrt(length(draws))
round(varmatrix2, 4)
confintmatrix2 = qnorm(0.975, 0, 1) * varmatrix2
round(confintmatrix2, 4)
lbmatrix2 = countMatrix2F - confintmatrix2
round(lbmatrix2, 4)
ubmatrix2 = countMatrix2F + confintmatrix2
round(ubmatrix2, 4)


varmatrix3 = countMatrix3F * (1 - countMatrix3F) / sqrt(length(draws))
round(varmatrix3, 4)
confintmatrix3 = qnorm(0.975, 0, 1) * varmatrix3
round(confintmatrix3, 4)
lbmatrix3 = countMatrix3F - confintmatrix3
round(lbmatrix3, 4)
ubmatrix3 = countMatrix3F + confintmatrix3
round(ubmatrix3, 4)

