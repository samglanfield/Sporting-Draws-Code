#Load in the draws before running the algorithm
#Create arrays for estimation

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

countMatrixu = matrix(0,
                      nrow = 8,
                      ncol = 8,
                      dimnames = list(pot1, pot2))
countMatrix = matrix(0,
                     nrow = 8,
                     ncol = 8,
                     dimnames = list(pot1, pot2))

incMatrixU <- function(i, j) {
  countMatrixu[i, j] <<- countMatrix[i, j] + 1
  return(0)
}
incMatrix <- function(i, j) {
  countMatrix[i, j] <<- countMatrix[i, j] + 1
  return(0)
}

m = length(countMatrix)

k = m #Value of k here is all possible values

TVEstimate = c()

for (i in 1:length(drawsS)) {
  mapply(incMatrixU, drawsU[[i]][1, 1, ], drawsU[[i]][1, 2, ])
  mapply(incMatrix, drawsS[[i]][1, 1, ], drawsS[[i]][1, 2, ])
  
  probArrayU = countMatrixu / i
  probArrayF = countMatrix / i
  
  
  diffarray = abs(probArrayU - probArrayF)
  
  estimate = sum(diffarray) #This corresponds to the estimates given in the project report
  
  TVEstimate[i] = estimate
}


plot(
  50000:100000,
  TVEstimate[50000:100000],
  type = "l",
  lwd = 1.5,
  xlab = "Iterations",
  ylab = "Upper Bound for the TV Estimate"
) #Only one value here as k is chosen so plot the latter terms
