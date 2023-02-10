#Load in the draws before running the algorithm
#Create arrays for estimation

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
countArrayF = array(0,
                    dim = c(16, 16, 16),
                    dimnames = list(pot1, pot2, pot3))

incArrayU <- function(i, j, k) {
  countArrayU[i, j, k] <<- countArrayU[i, j, k] + 1
  return(0)
}
incArrayF <- function(i, j, k) {
  countArrayF[i, j, k] <<- countArrayF[i, j, k] + 1
  return(0)
}

m = length(countArrayF)

k = 4096 #Value of k here is all possible values

TVEstimate = c()

for (i in 1:length(drawsFIFA)) {
  mapply(incArrayU, drawsUniform[[i]][1, 1, ], drawsUniform[[i]][1, 2, ], drawsUniform[[i]][1, 3, ])
  mapply(incArrayF, drawsFIFA[[i]][1, 1, ], drawsFIFA[[i]][1, 2, ], drawsFIFA[[i]][1, 3, ])
  
  probArrayU = countArrayU / i
  probArrayF = countArrayF / i
  
  
  diffarray = abs(probArrayU - probArrayF)
  
  estimate = sum(diffarray) #This corresponds to the estimates given in the project report
  
  TVEstimate[i] = estimate
}


plot(50000:10000, TVEstimate[50000:100000], type = "l", lwd = 1.5) #Only one value here as k is chosen so plot the latter terms
