set.seed(310101)

checkvalid = function() {
  valid = matrix(0, nrow = 8, ncol = 7)
  validpair = c()
  validrusukr = c()
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
    if (length(grep("UKR", initialgroups[1, , j])) > 0 &&
        length(grep("RUS", initialgroups[1, , j])) > 0) {
      validrusukr[j] = 0
    } else{
      validrusukr[j] = 1
    }
  }
  
  for (i in 1:11) {
    pairing = get(paste("pair", i, sep = ""), env = .GlobalEnv)
    firsthalf = length(which(pairing %in% initialgroups[1, , 1:4]))
    secondhalf = length(which(pairing %in% initialgroups[1, , 5:8]))
    if (max(firsthalf, secondhalf) >= 2) {
      validpair[i] = 0
    } else{
      validpair[i] = 1
    }
    
  }
  return(prod(valid, validpair, validrusukr))
}



validgroups = function(selected, rowposition) {
  possibleindices = c()
  finalindices = c()
  possibleindices2 = c()
  regcheckerrem = c()
  empty = c()
  for (j in 1:8) {
    regcheckerrem[j] = length(grep(paste("^", substr(selected, 1, 3), sep =
                                           ""), initialgroups[1, , j]))
    if (is.na(initialgroups[1, rowposition, j]) == TRUE) {
      empty[j] = 1
    } else{
      empty[j] = 0
    }
  }
  
  
  possibleindices = which(regcheckerrem == 0)
  possibleindices = possibleindices[which(!possibleindices %in% which(empty ==
                                                                        0))]
  
  if (length(which(selected %in% pairedteams)) >= 1) {
    possibleindices2 = c()
    index = ceiling(which(pairedteams %in% selected) / 2)
    pair = get(paste("pair", index, sep = ""), env = .GlobalEnv)
    firsthalf = length(which(pair %in% initialgroups[1, , 1:4]))
    secondhalf = length(which(pair %in% initialgroups[1, , 5:8]))
    if (firsthalf >= 1) {
      possibleindices2 = 1:4
    } else if (secondhalf >= 1) {
      possibleindices2 = 5:8
    } else{
      possibleindices2 = c()
    }
    
  }
  
  finalindices = possibleindices[which(!possibleindices %in% possibleindices2)]
  return(finalindices)
  
}


#Main function

assignment = function(rowposition) {
  numplaced = 0
  #placedat = rep(0, 8)
  
  for (i in 1:8) {
    if (is.na(initialgroups[1, rowposition, i]) == FALSE) {
      numplaced = numplaced + 1
      # placedat[i] = 1
      
    }
    
  }
  
  if (numplaced >= 8) {
    if (rowposition >= 4) {
      return(1)
    } else{
      return(assignment(rowposition + 1))
    }
    
  } else{
    while (length(is.na(initialgroups[1, rowposition,]) == TRUE) > 0) {
      selected = sample(pot234[rowposition ,!pot234[rowposition ,] %in% initialgroups[1, rowposition,]], 1)
      index = validgroups(selected, rowposition)
      
      if (length(index) == 1) {
        x = index
        
        initialgroups[1, rowposition, x] = selected
        assign("initialgroups", initialgroups, envir = .GlobalEnv)
        
        validity = checkvalid()
        if (validity == 1 && assignment(rowposition) == 1) {
          return(1)
          
        } else{
          initialgroups[1, rowposition, x] = NA
          assign("initialgroups", initialgroups, envir = .GlobalEnv)
          
        }
      } else if (length(index) > 1) {
        x = sample(index)
        
        for (j in 1:length(x)) {
          i = x[j]
          initialgroups[1, rowposition, i] = selected
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

drawsUEFA = list()
b = c()

system.time(for (k in 1:100000) {
  pot1 = c("ENG1", "ESP1", "GER1", "ENG2", "ESP2", "ITA1", "POR1", "FRA1")
  pot2 = c("ESP3", "ESP4", "ITA2", "ENG3", "FRA2", "ENG4", "ESP5", "GER2")
  pot3 = c("POR2", "NED1", "UKR1", "GER3", "AUT1", "POR3", "ITA3", "RUS1")
  pot4 = c("TUR1", "UKR2", "BEL1", "SUI1", "ITA4", "SWE1", "GER4", "MDA1")
  
  pair1 = c("ENG1", "ENG2")
  pair2 = c("ESP1", "ESP4")
  pair3 = c("ITA1", "ITA2")
  pair4 = c("GER1", "GER2")
  pair5 = c("FRA1", "FRA2")
  pair6 = c("ESP2", "ESP3")
  pair7 = c("ENG3", "ENG4")
  pair8 = c("POR2", "POR3")
  pair9 = c("UKR1", "UKR2")
  pair10 = c("GER3", "GER4")
  pair11 = c("ITA3", "ITA4")
  pairedteams = c(pair1,
                  pair2,
                  pair3,
                  pair4,
                  pair5,
                  pair6,
                  pair7,
                  pair8,
                  pair9,
                  pair10,
                  pair11)
  nations = c(
    "ENG",
    "FRA",
    "ESP",
    "GER",
    "ITA",
    "UKR",
    "POR",
    "NED",
    "AUT",
    "RUS",
    "TUR",
    "BEL",
    "SUI",
    "SWE",
    "MDA"
  )
  
  
  
  pot234 = rbind(pot1, pot2, pot3, pot4)
  initialgroups = array(rep(NA, 8), dim = c(1, 4, 8))
  
  
  
  b[k] = assignment(1)
  drawsUEFA[[k]] = initialgroups
  print(paste("Simulation", k))
})


pot1 = c("ENG1", "ESP1", "GER1", "ENG2", "ESP2", "ITA1", "POR1", "FRA1")
pot2 = c("ESP3", "ESP4", "ITA2", "ENG3", "FRA2", "ENG4", "ESP5", "GER2")
pot3 = c("POR2", "NED1", "UKR1", "GER3", "AUT1", "POR3", "ITA3", "RUS1")
pot4 = c("TUR1", "UKR2", "BEL1", "SUI1", "ITA4", "SWE1", "GER4", "MDA1")


incMatrix <- function(i, j) {
  countMatrixUEFAS[i, j] <<- countMatrixUEFAS[i, j] + 1
  
  return(0)
  
}
incMatrix2 <- function(i, j) {
  countMatrix2UEFAS[i, j] <<- countMatrix2UEFAS[i, j] + 1
  
  return(0)
  
}
incMatrix3 <- function(i, j) {
  countMatrix3UEFAS[i, j] <<- countMatrix3UEFAS[i, j] + 1
  
  return(0)
  
}
incMatrix4 <- function(i, j) {
  countMatrix4UEFAS[i, j] <<- countMatrix4UEFAS[i, j] + 1
  
  return(0)
  
}
incMatrix5 <- function(i, j) {
  countMatrix5UEFAS[i, j] <<- countMatrix5UEFAS[i, j] + 1
  
  return(0)
  
}
incMatrix6 <- function(i, j) {
  countMatrix6UEFAS[i, j] <<- countMatrix6UEFAS[i, j] + 1
  
  return(0)
  
}

countMatrixUEFAS <- matrix(0, 8, 8)
row.names(countMatrixUEFAS) <- pot1
colnames(countMatrixUEFAS) <- pot2


for (i in 1:length(drawsUEFA)) {
  mapply(incMatrix, drawsUEFA[[i]][1, 1,], drawsUEFA[[i]][1, 2,])
}
countMatrixUEFAS = countMatrixUEFAS / length(drawsUEFA)
countMatrixUEFAS

countMatrix2UEFAS <- matrix(0, 8, 8)
row.names(countMatrix2UEFAS) <- pot1

colnames(countMatrix2UEFAS) <- pot3


for (i in 1:length(drawsUEFA)) {
  mapply(incMatrix2, drawsUEFA[[i]][1, 1,], drawsUEFA[[i]][1, 3,])
}

countMatrix2UEFAS = countMatrix2UEFAS / length(drawsUEFA)
countMatrix2UEFAS


countMatrix3UEFAS <- matrix(0, 8, 8)
row.names(countMatrix3UEFAS) <- pot1

colnames(countMatrix3UEFAS) <- pot4


for (i in 1:length(drawsUEFA)) {
  mapply(incMatrix3, drawsUEFA[[i]][1, 1,], drawsUEFA[[i]][1, 4,])
}

countMatrix3UEFAS = countMatrix3UEFAS / length(drawsUEFA)
countMatrix3UEFAS

countMatrix4UEFAS <- matrix(0, 8, 8)
row.names(countMatrix4UEFAS) <- pot2

colnames(countMatrix4UEFAS) <- pot3


for (i in 1:length(drawsUEFA)) {
  mapply(incMatrix4, drawsUEFA[[i]][1, 2,], drawsUEFA[[i]][1, 3,])
}

countMatrix4UEFAS = countMatrix4UEFAS / length(drawsUEFA)
countMatrix4UEFAS

countMatrix5UEFAS <- matrix(0, 8, 8)
row.names(countMatrix5UEFAS) <- pot2

colnames(countMatrix5UEFAS) <- pot4


for (i in 1:length(drawsUEFA)) {
  mapply(incMatrix5, drawsUEFA[[i]][1, 2,], drawsUEFA[[i]][1, 4,])
}

countMatrix5UEFAS = countMatrix5UEFAS / length(drawsUEFA)
countMatrix5UEFAS

countMatrix6UEFAS <- matrix(0, 8, 8)
row.names(countMatrix6UEFAS) <- pot3

colnames(countMatrix6UEFAS) <- pot4


for (i in 1:length(drawsUEFA)) {
  mapply(incMatrix6, drawsUEFA[[i]][1, 3,], drawsUEFA[[i]][1, 4,])
}

countMatrix6UEFAS = countMatrix6UEFAS / length(drawsUEFA)
countMatrix6UEFAS

###############################
### Count for large groups

countArrayU <-
  array(0,
        dim = c(8, 8, 8, 8),
        dimnames = list(pot1, pot2, pot3, pot4))

incArray <- function(i, j, k, s) {
  countArrayU[i, j, k, s] <<- countArrayU[i, j, k, s] + 1
  return(0)
}

for (i in 1:length(drawsUEFA)) {
  mapply(incArray, drawsUEFA[[i]][1, 1,], drawsUEFA[[i]][1, 2,], drawsUEFA[[i]][1, 3,], drawsUEFA[[i]][1, 4,])
}

countArrayU = countArrayU / length(drawsUEFA)
countArrayU


##############################
## Variances


varmatrix1 = countMatrixUEFAS * (1 - countMatrixUEFAS) / length(drawsUEFA)
round(varmatrix1, 4)
confintmatrix1 = qnorm(0.975, 0, 1) * sqrt(varmatrix1)
round(confintmatrix1, 4)
lbmatrix1 = countMatrixUEFAS - confintmatrix1
round(lbmatrix1, 4)
ubmatrix1 = countMatrixUEFAS + confintmatrix1
round(ubmatrix1, 4)


varmatrix2 = countMatrix2UEFAS * (1 - countMatrix2UEFAS) / length(drawsUEFA)
round(varmatrix2, 4)
confintmatrix2 = qnorm(0.975, 0, 1) * sqrt(varmatrix2)
round(confintmatrix2, 4)
lbmatrix2 = countMatrix2UEFAS - confintmatrix2
round(lbmatrix2, 4)
ubmatrix2 = countMatrix2UEFAS + confintmatrix2
round(ubmatrix2, 4)


varmatrix3 = countMatrix3UEFAS * (1 - countMatrix3UEFAS) / length(drawsUEFA)
round(varmatrix3, 4)
confintmatrix3 = qnorm(0.975, 0, 1) * sqrt(varmatrix3)
round(confintmatrix3, 4)
lbmatrix3 = countMatrix3UEFAS - confintmatrix3
round(lbmatrix3, 4)
ubmatrix3 = countMatrix3UEFAS + confintmatrix3
round(ubmatrix3, 4)

varmatrix4 = countMatrix4UEFAS * (1 - countMatrix4UEFAS) / length(drawsUEFA)
round(varmatrix4, 4)
confintmatrix4 = qnorm(0.975, 0, 1) * sqrt(varmatrix4)
round(confintmatrix4, 4)
lbmatrix4 = countMatrix4UEFAS - confintmatrix4
round(lbmatrix4, 4)
ubmatrix4 = countMatrix4UEFAS + confintmatrix4
round(ubmatrix4, 4)


varmatrix5 = countMatrix5UEFAS * (1 - countMatrix5UEFAS) / length(drawsUEFA)
round(varmatrix5, 4)
confintmatrix5 = qnorm(0.975, 0, 1) * sqrt(varmatrix5)
round(confintmatrix5, 4)
lbmatrix5 = countMatrix5UEFAS - confintmatrix5
round(lbmatrix5, 4)
ubmatrix5 = countMatrix5UEFAS + confintmatrix5
round(ubmatrix5, 4)


varmatrix6 = countMatrix6UEFAS * (1 - countMatrix6UEFAS) / length(drawsUEFA)
round(varmatrix6, 4)
confintmatrix6 = qnorm(0.975, 0, 1) * sqrt(varmatrix6)
round(confintmatrix6, 4)
lbmatrix6 = countMatrix6UEFAS - confintmatrix6
round(lbmatrix6, 4)
ubmatrix6 = countMatrix6UEFAS + confintmatrix6
round(ubmatrix6, 4)




####################################
## Skewness


incMatrixgen <- function(i, j) {
  countmatrix[i, j] <<- countmatrix[i, j] + 1
  return(0)
}

for (i in 1:100) {
  assign(
    paste("subdrawsUEFA", i, sep = ""),
    sample(
      drawsUEFA,
      size = length(drawsUEFA) / 100,
      replace = FALSE
    ),
    envir = .GlobalEnv
  )
  assign(paste("countmatrixf", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  x = get(paste("countmatrixf", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot1
  colnames(x) = pot2
  assign(paste("countmatrixf", i, sep = ""), x, envir = .GlobalEnv)
  assign(paste("countmatrix2f", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  y = get(paste("countmatrix2f", i, sep = ""), env = .GlobalEnv)
  rownames(y) = pot1
  colnames(y) = pot3
  assign(paste("countmatrix2f", i, sep = ""), y, envir = .GlobalEnv)
  assign(paste("countmatrix3f", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  z = get(paste("countmatrix3f", i, sep = ""), env = .GlobalEnv)
  rownames(z) = pot1
  colnames(z) = pot4
  assign(paste("countmatrix3f", i, sep = ""), z, envir = .GlobalEnv)
  assign(paste("countmatrix4f", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  x = get(paste("countmatrix4f", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot2
  colnames(x) = pot3
  assign(paste("countmatrix4f", i, sep = ""), x, envir = .GlobalEnv)
  assign(paste("countmatrix5f", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  y = get(paste("countmatrix5f", i, sep = ""), env = .GlobalEnv)
  rownames(y) = pot2
  colnames(y) = pot4
  assign(paste("countmatrix5f", i, sep = ""), y, envir = .GlobalEnv)
  assign(paste("countmatrix6f", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  z = get(paste("countmatrix6f", i, sep = ""), env = .GlobalEnv)
  rownames(z) = pot3
  colnames(z) = pot4
  assign(paste("countmatrix6f", i, sep = ""), z, envir = .GlobalEnv)
  
  #Skewness matrices
  assign(paste("countmatrixfskew", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  x = get(paste("countmatrixfskew", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot1
  colnames(x) = pot2
  assign(paste("countmatrixfskew", i, sep = ""), x, envir = .GlobalEnv)
  assign(paste("countmatrix2fskew", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  y = get(paste("countmatrix2fskew", i, sep = ""), env = .GlobalEnv)
  rownames(y) = pot1
  colnames(y) = pot3
  assign(paste("countmatrix2fskew", i, sep = ""), y, envir = .GlobalEnv)
  assign(paste("countmatrix3fskew", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  z = get(paste("countmatrix3fskew", i, sep = ""), env = .GlobalEnv)
  rownames(z) = pot1
  colnames(z) = pot4
  assign(paste("countmatrix3fskew", i, sep = ""), z, envir = .GlobalEnv)
  assign(paste("countmatrix4fskew", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  x = get(paste("countmatrix4fskew", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot2
  colnames(x) = pot3
  assign(paste("countmatrix4fskew", i, sep = ""), x, envir = .GlobalEnv)
  assign(paste("countmatrix5fskew", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  y = get(paste("countmatrix5fskew", i, sep = ""), env = .GlobalEnv)
  rownames(y) = pot2
  colnames(y) = pot4
  assign(paste("countmatrix5fskew", i, sep = ""), y, envir = .GlobalEnv)
  assign(paste("countmatrix6fskew", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  z = get(paste("countmatrix6fskew", i, sep = ""), env = .GlobalEnv)
  rownames(z) = pot3
  colnames(z) = pot4
  assign(paste("countmatrix6fskew", i, sep = ""), z, envir = .GlobalEnv)
}

for (i in 1:100) {
  for (j in 1:length(subdrawsUEFA1)) {
    x = get(paste("subdrawsUEFA", i, sep = ""), env = .GlobalEnv)[[j]]
    y = get(paste("countmatrixf", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 8, ncol = 8)
    rownames(countmatrix) = pot1
    colnames(countmatrix) = pot2
    mapply(incMatrixgen, x[1, 1, ], x[1, 2, ])
    assign(paste("countmatrixf", i, sep = ""), y + countmatrix, envir =
             .GlobalEnv)
    
    z = get(paste("countmatrix2f", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 8, ncol = 8)
    rownames(countmatrix) = pot1
    colnames(countmatrix) = pot3
    mapply(incMatrixgen, x[1, 1, ], x[1, 3, ])
    assign(paste("countmatrix2f", i, sep = ""), z + countmatrix, envir =
             .GlobalEnv)
    
    w = get(paste("countmatrix3f", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 8, ncol = 8)
    rownames(countmatrix) = pot1
    colnames(countmatrix) = pot4
    mapply(incMatrixgen, x[1, 1, ], x[1, 4, ])
    assign(paste("countmatrix3f", i, sep = ""), w + countmatrix, envir =
             .GlobalEnv)
    
    y = get(paste("countmatrix4f", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 8, ncol = 8)
    rownames(countmatrix) = pot2
    colnames(countmatrix) = pot3
    mapply(incMatrixgen, x[1, 2, ], x[1, 3, ])
    assign(paste("countmatrix4f", i, sep = ""), y + countmatrix, envir =
             .GlobalEnv)
    
    z = get(paste("countmatrix5f", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 8, ncol = 8)
    rownames(countmatrix) = pot2
    colnames(countmatrix) = pot4
    mapply(incMatrixgen, x[1, 2, ], x[1, 4, ])
    assign(paste("countmatrix5f", i, sep = ""), z + countmatrix, envir =
             .GlobalEnv)
    
    w = get(paste("countmatrix6f", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 8, ncol = 8)
    rownames(countmatrix) = pot3
    colnames(countmatrix) = pot4
    mapply(incMatrixgen, x[1, 3, ], x[1, 4, ])
    assign(paste("countmatrix6f", i, sep = ""), w + countmatrix, envir =
             .GlobalEnv)
    
  }
  assign(paste("countmatrixf", i, sep = ""),
         get(paste("countmatrixf", i, sep = "")) / length(subdrawsUEFA1),
         envir = .GlobalEnv)
  assign(paste("countmatrix2f", i, sep = ""),
         get(paste("countmatrix2f", i, sep = "")) / length(subdrawsUEFA1),
         envir = .GlobalEnv)
  assign(paste("countmatrix3f", i, sep = ""),
         get(paste("countmatrix3f", i, sep = "")) / length(subdrawsUEFA1),
         envir = .GlobalEnv)
  assign(paste("countmatrix4f", i, sep = ""),
         get(paste("countmatrix4f", i, sep = "")) / length(subdrawsUEFA1),
         envir = .GlobalEnv)
  assign(paste("countmatrix5f", i, sep = ""),
         get(paste("countmatrix5f", i, sep = "")) / length(subdrawsUEFA1),
         envir = .GlobalEnv)
  assign(paste("countmatrix6f", i, sep = ""),
         get(paste("countmatrix6f", i, sep = "")) / length(subdrawsUEFA1),
         envir = .GlobalEnv)
}

bernskewness = function(x) {
  y = ((1 - 2 * x) / sqrt(x * (1 - x))) / sqrt(length(subdrawsUEFA1))
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
  assign(paste("countmatrix4fskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrix4f", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
  assign(paste("countmatrix5fskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrix5f", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
  assign(paste("countmatrix6fskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrix6f", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
}

countmatrixf1skewarray = array(get(paste("countmatrixfskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(8, 8, 1))
countmatrixf2skewarray = array(get(paste("countmatrix2fskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(8, 8, 1))
countmatrixf3skewarray = array(get(paste("countmatrix3fskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(8, 8, 1))
countmatrixf4skewarray = array(get(paste("countmatrix4fskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(8, 8, 1))
countmatrixf5skewarray = array(get(paste("countmatrix5fskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(8, 8, 1))
countmatrixf6skewarray = array(get(paste("countmatrix6fskew", 1, sep = ""), env =
                                     .GlobalEnv), dim = c(8, 8, 1))

for (i in 2:100) {
  countmatrixf1skewarray = array(c(countmatrixf1skewarray, get(
    paste("countmatrixfskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(8, 8, i))
  countmatrixf2skewarray = array(c(countmatrixf2skewarray, get(
    paste("countmatrix2fskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(8, 8, i))
  countmatrixf3skewarray = array(c(countmatrixf3skewarray, get(
    paste("countmatrix3fskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(8, 8, i))
  countmatrixf4skewarray = array(c(countmatrixf4skewarray, get(
    paste("countmatrix4fskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(8, 8, i))
  countmatrixf5skewarray = array(c(countmatrixf5skewarray, get(
    paste("countmatrix5fskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(8, 8, i))
  countmatrixf6skewarray = array(c(countmatrixf6skewarray, get(
    paste("countmatrix6fskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(8, 8, i))
}

apply(countmatrixf1skewarray, c(1, 2), mean)
apply(countmatrixf2skewarray, c(1, 2), mean)
apply(countmatrixf3skewarray, c(1, 2), mean)
apply(countmatrixf4skewarray, c(1, 2), mean)
apply(countmatrixf5skewarray, c(1, 2), mean)
apply(countmatrixf6skewarray, c(1, 2), mean)

apply(countmatrixf1skewarray, c(1, 2), var)
apply(countmatrixf2skewarray, c(1, 2), var)
apply(countmatrixf3skewarray, c(1, 2), var)
apply(countmatrixf4skewarray, c(1, 2), var)
apply(countmatrixf5skewarray, c(1, 2), var)
apply(countmatrixf6skewarray, c(1, 2), var)

countmatrixf1array = array(get(paste("countmatrixf", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(8, 8, 1))
countmatrixf2array = array(get(paste("countmatrix2f", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(8, 8, 1))
countmatrixf3array = array(get(paste("countmatrix3f", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(8, 8, 1))
countmatrixf4array = array(get(paste("countmatrix4f", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(8, 8, 1))
countmatrixf5array = array(get(paste("countmatrix5f", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(8, 8, 1))
countmatrixf6array = array(get(paste("countmatrix6f", 1, sep = ""), env =
                                 .GlobalEnv), dim = c(8, 8, 1))

for (i in 2:100) {
  countmatrixf1array = array(c(countmatrixf1array, get(paste(
    "countmatrixf", i, sep = ""
  ), env = .GlobalEnv)), dim = c(8, 8, i))
  countmatrixf2array = array(c(countmatrixf2array, get(paste(
    "countmatrix2f", i, sep = ""
  ), env = .GlobalEnv)), dim = c(8, 8, i))
  countmatrixf3array = array(c(countmatrixf3array, get(paste(
    "countmatrix3f", i, sep = ""
  ), env = .GlobalEnv)), dim = c(8, 8, i))
  countmatrixf4array = array(c(countmatrixf4array, get(paste(
    "countmatrix4f", i, sep = ""
  ), env = .GlobalEnv)), dim = c(8, 8, i))
  countmatrixf5array = array(c(countmatrixf5array, get(paste(
    "countmatrix5f", i, sep = ""
  ), env = .GlobalEnv)), dim = c(8, 8, i))
  countmatrixf6array = array(c(countmatrixf6array, get(paste(
    "countmatrix6f", i, sep = ""
  ), env = .GlobalEnv)), dim = c(8, 8, i))
}

apply(countmatrixf1array, c(1, 2), mean)
apply(countmatrixf2array, c(1, 2), mean)
apply(countmatrixf3array, c(1, 2), mean)
apply(countmatrixf4array, c(1, 2), mean)
apply(countmatrixf5array, c(1, 2), mean)
apply(countmatrixf6array, c(1, 2), mean)

apply(countmatrixf1array, c(1, 2), var)
apply(countmatrixf2array, c(1, 2), var)
apply(countmatrixf3array, c(1, 2), var)
apply(countmatrixf4array, c(1, 2), var)
apply(countmatrixf5array, c(1, 2), var)
apply(countmatrixf6array, c(1, 2), var)

#tests for normality

hist(countmatrixf1array[1, 1, ],
     probability = TRUE,
     breaks = 15)


library(tseries)
library(nortest)

#for count matrices

Shap = array(0, dim = c(8, 8, 8, 8))
JB = array(0, dim = c(8, 8, 8, 8))
AD = array(0, dim = c(8, 8, 8, 8))
CVM = array(0, dim = c(8, 8, 8, 8))
Pearson = array(0, dim = c(8, 8, 8, 8))

for (i in 1:6) {
  countarray = get(paste("countmatrixf", i, "array", sep = ""), env = .GlobalEnv)
  for (j in 1:8) {
    for (k in 1:8) {
      if (sum(countarray[i, j, k, ]) > 0) {
        Shap[1, i, j, k] = shapiro.test(countarray[i, j, k,])$p.value
        JB[1, i, j, k] = jarque.bera.test(countarray[i, j, k,])$p.value
        AD[1, i, j, k] = ad.test(countarray[i, j, k,])$p.value
        CVM[1, i, j, k] = cvm.test(countarray[i, j, k,])$p.value
        Pearson[1, i, j, k] = pearson.test(countarray[i, j, k,])$p.value
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


#Lyapunov Condition

incArrayS <- function(i, j, k, r) {
  countArrayS[i, j, k, r] <<- countArrayS[i, j, k, r] + 1
  return(0)
}

x = 10 # choose index to determine if Laypunov condition is satisfied

countArrayS = array(0,
                    dim = c(8, 8, 8),
                    dimnames = list(pot1, pot2, pot3, pot4))


Lyapunov = c()

for (i in 1:length(drawsUEFA)) {
  mapply(incArrayS,
         drawsUEFA[[i]][1, 1,],
         drawsUEFA[[i]][1, 2,],
         drawsUEFA[[i]][1, 3,],
         drawsUEFA[[i]][1, 4, ])
  
  probArray = countArrayU / i
  
  Lyapunov[i] = probArray[x] * (1 - probArray[x])
}

plot(cumsum(Lyapunov), type = "l", lwd = 1.5)


## Hellinger Distance Estimate
library("textmineR")


pot1 = c("ENG1", "ESP1", "GER1", "ENG2", "ESP2", "ITA1", "POR1", "FRA1")
pot2 = c("ESP3", "ESP4", "ITA2", "ENG3", "FRA2", "ENG4", "ESP5", "GER2")
pot3 = c("POR2", "NED1", "UKR1", "GER3", "AUT1", "POR3", "ITA3", "RUS1")
pot4 = c("TUR1", "UKR2", "BEL1", "SUI1", "ITA4", "SWE1", "GER4", "MDA1")

countArrayS = array(0,
                    dim = c(8, 8, 8, 8),
                    dimnames = list(pot1, pot2, pot3, pot4))
countArrayU = array(0,
                    dim = c(8, 8, 8, 8),
                    dimnames = list(pot1, pot2, pot3, pot4))

incArrayS <- function(i, j, k, r) {
  countArrayS[i, j, k, r] <<- countArrayS[i, j, k, r] + 1
  return(0)
}
incArrayU <- function(i, j, k, r) {
  countArrayU[i, j, k, r] <<- countArrayU[i, j, k, r] + 1
  return(0)
}

HeVector = c()

for (i in 1:length(drawsUEFA)) {
  mapply(incArrayS,
         drawsUEFA[[i]][1, 1, ],
         drawsUEFA[[i]][1, 2, ],
         drawsUEFA[[i]][1, 3, ],
         drawsUEFA[[i]][1, 4, ])
  mapply(
    incArrayU,
    drawsUniformUEFA[[i]][1, 1, ],
    drawsUniformUEFA[[i]][1, 2, ],
    drawsUniformUEFA[[i]][1, 3, ],
    drawsUniformUEFA[[i]][1, 4, ]
  )
  
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