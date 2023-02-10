set.seed(1926002)

#Checks if given draw is valid

checkvalid = function() {
  valid1 = c()
  valid = matrix(0, nrow = 8, ncol = 3)
  regcheckerrem = matrix(0, nrow = 8, ncol = 3)
  for (j in 1:8) {
    regcheckerrem[j, 1] = length(grep("^UK", initialgroups[1, , j]))
    regcheckerrem[j, 2] = length(grep("^GER", initialgroups[1, , j]))
    regcheckerrem[j, 3] = length(grep("^IT", initialgroups[1, , j]))
    for (l in 1:3) {
      if (regcheckerrem[j, l] <= 1) {
        valid[j, l] = 1
      } else{
        valid[j, l] = 0
      }
    }
  }
  for (k in 1:8) {
    if (which(pots[1, ] %in% initialgroups[1, 1, k]) == which(pots[2, ] %in% initialgroups[1, 2, k])) {
      valid1[k] = 0
    } else{
      valid1[k] = 1
    }
  }
  return(prod(valid, valid1))
}

incMatrix <- function(i, j) {
  countMatrixu[i, j] <<- countMatrixu[i, j] + 1
  
  return(0)
  
}

countMatrixu <- matrix(0, 8, 8)
totalvalid = 0
pot1 = c("ITNap",
         "PORPor",
         "GERBay",
         "UKTot",
         "UKChe",
         "SPARM",
         "UKMC",
         "PORBen")
pot2 = c("UKLiv",
         "BELCB",
         "ITIM",
         "GERFra",
         "ITMil",
         "GERLei",
         "GERDort",
         "FRAPSG")
row.names(countMatrixu) <- pot1

colnames(countMatrixu) <- pot2

pots = rbind(pot1, pot2)

alldraws = list()
validity = c()

#begin algorithm

while (length(alldraws) < 100000) {
  initialgroups = array(rep(NA, 8), dim = c(1, 2, 8))
  pot1 = c("ITNap",
           "PORPor",
           "GERBay",
           "UKTot",
           "UKChe",
           "SPARM",
           "UKMC",
           "PORBen")
  pot2 = c("UKLiv",
           "BELCB",
           "ITIM",
           "GERFra",
           "ITMil",
           "GERLei",
           "GERDort",
           "FRAPSG")
  
  initialgroups[1, 1, ] = sample(pot1, 8)
  initialgroups[1, 2, ] = sample(pot2, 8)
  
  validity = checkvalid()
  if (validity == 1) {
    alldraws[[totalvalid + 1]] = initialgroups
    totalvalid = totalvalid + 1
  }
  print(paste("Simulation", totalvalid))
}


#creates mathcup probabilities

for (i in 1:length(drawsU)) {
  mapply(incMatrix, drawsU[[i]][1, 1, ], drawsU[[i]][1, 2, ])
}
countMatrixu = countMatrixu / length(drawsU)
countMatrixu

#variances

varmatrixu = countMatrixu * (1 - countMatrixu) / length(drawsU)
round(varmatrixu, 4)
confintmatrixu = qnorm(0.975, 0, 1) * sqrt(varmatrixu)
round(confintmatrixu, 4)
lbmatrixu = countMatrixu - confintmatrixu
round(lbmatrixu, 4)
ubmatrixu = countMatrixu + confintmatrixu
round(ubmatrixu, 4)

incMatrixgen <- function(i, j) {
  countmatrix[i, j] <<- countmatrix[i, j] + 1
  return(0)
}

######################################

#############################

set.seed(1926002)
index = sample(1:length(drawsU))

for (i in 1:100) {
  low = (1000 * (i - 1) + 1)
  up = 1000 * i
  assign(paste("subdrawsCL", i, sep = ""), drawsU[index[low:up]], envir =
           .GlobalEnv)
  assign(paste("countmatrixs", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  x = get(paste("countmatrixs", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot1
  colnames(x) = pot2
  assign(paste("countmatrixs", i, sep = ""), x, envir = .GlobalEnv)
  
  #Skewness matrices
  assign(paste("countmatrixsskew", i, sep = ""),
         matrix(0, nrow = 8, ncol = 8),
         envir = .GlobalEnv)
  x = get(paste("countmatrixsskew", i, sep = ""), env = .GlobalEnv)
  rownames(x) = pot1
  colnames(x) = pot2
  assign(paste("countmatrixsskew", i, sep = ""), x, envir = .GlobalEnv)
}

for (i in 1:100) {
  for (j in 1:length(subdrawsCL1)) {
    x = get(paste("subdrawsCL", i, sep = ""), env = .GlobalEnv)[[j]]
    y = get(paste("countmatrixs", i, sep = ""), env = .GlobalEnv)
    
    countmatrix = matrix(0, nrow = 8, ncol = 8)
    rownames(countmatrix) = pot1
    colnames(countmatrix) = pot2
    mapply(incMatrixgen, x[1, 1, ], x[1, 2, ])
    assign(paste("countmatrixs", i, sep = ""), y + countmatrix, envir =
             .GlobalEnv)
    
  }
  assign(paste("countmatrixs", i, sep = ""),
         get(paste("countmatrixs", i, sep = "")) / length(subdrawsCL1),
         envir = .GlobalEnv)
}


bernskewness = function(x) {
  y = ((1 - 2 * x) / sqrt(x * (1 - x))) / sqrt(length(subdrawsCL1))
  y[which(y == Inf)] = NA
  return(y)
}

bernexkurt = function(x) {
  y = (1 - 6 * x * (1 - x)) / (x * (1 - x))
  y[which(y == Inf)] = NA
  return(y)
}

for (i in 1:100) {
  assign(paste("countmatrixsskew", i, sep = ""),
         bernskewness(get(paste(
           "countmatrixs", i, sep = ""
         ), env = .GlobalEnv)),
         envir = .GlobalEnv)
}

countmatrixsskewarray = array(get(paste("countmatrixsskew", 1, sep = ""), env =
                                    .GlobalEnv), dim = c(8, 8, 1))

for (i in 2:100) {
  countmatrixsskewarray = array(c(countmatrixsskewarray, get(
    paste("countmatrixsskew", i, sep = ""), env = .GlobalEnv
  )), dim = c(8, 8, i))
}

apply(countmatrixsskewarray, c(1, 2), mean)


apply(countmatrixsskewarray, c(1, 2), var)


countmatrixsarray = array(get(paste("countmatrixs", 1, sep = ""), env =
                                .GlobalEnv), dim = c(8, 8, 1))

for (i in 2:100) {
  countmatrixsarray = array(c(countmatrixsarray, get(paste(
    "countmatrixs", i, sep = ""
  ), env = .GlobalEnv)), dim = c(8, 8, i))
}

apply(countmatrixsarray, c(1, 2), mean)


apply(countmatrixsarray, c(1, 2), var)


#tests for normality

hist(
  countmatrixsarray[1, 7, ],
  probability = TRUE,
  breaks = 15,
  xlab = "Distribution of probabilities \n (b)",
  main = ""
)


library(tseries)
library(nortest)


#for count matrices

Shap = matrix(0, nrow = 8, ncol = 8)
JB = matrix(0, nrow = 8, ncol = 8)
AD = matrix(0, nrow = 8, ncol = 8)
CVM = matrix(0, nrow = 8, ncol = 8)
Pearson = matrix(0, nrow = 8, ncol = 8)

countarray = countmatrixsarray
for (j in 1:8) {
  for (k in 1:8) {
    if (sum(countarray[j, k, ]) > 0) {
      Shap[j, k] = shapiro.test(countarray[j, k, ])$p.value
      JB[j, k] = jarque.bera.test(countarray[j, k, ])$p.value
      AD[j, k] = ad.test(countarray[j, k, ])$p.value
      CVM[j, k] = cvm.test(countarray[j, k, ])$p.value
      Pearson[j, k] = pearson.test(countarray[j, k, ])$p.value
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

######## Lyapunov

pot1 = c("ITNap",
         "PORPor",
         "GERBay",
         "UKTot",
         "UKChe",
         "SPARM",
         "UKMC",
         "PORBen")
pot2 = c("UKLiv",
         "BELCB",
         "ITIM",
         "GERFra",
         "ITMil",
         "GERLei",
         "GERDort",
         "FRAPSG")

x = which(countMatrixu %in% min(countMatrixu[which(countMatrixu > 0)]))

countMatrixu = matrix(0,
                      nrow = 8,
                      ncol = 8,
                      dimnames = list(pot1, pot2))


Lyapunov = c()

for (i in 1:length(drawsU)) {
  mapply(incMatrix, drawsU[[i]][1, 1, ], drawsU[[i]][1, 2, ])
  
  probArray = countMatrixu / i
  
  Lyapunov[i] = probArray[x] * (1 - probArray[x])
}

plot(cumsum(Lyapunov), type = "l", lwd = 1.5)


