#Load in the draws before running the algorithm
#Create arrays for estimation

pot1 = c("ENG1", "ESP1", "GER1", "ENG2", "ESP2", "ITA1", "POR1", "FRA1")
pot2 = c("ESP3", "ESP4", "ITA2", "ENG3", "FRA2", "ENG4", "ESP5", "GER2")
pot3 = c("POR2", "NED1", "UKR1", "GER3", "AUT1", "POR3", "ITA3", "RUS1")
pot4 = c("TUR1", "UKR2", "BEL1", "SUI1", "ITA4", "SWE1", "GER4", "MDA1")

countArrayU = array(0,
                    dim = c(8, 8, 8, 8),
                    dimnames = list(pot1, pot2, pot3, pot4))
countArrayF = array(0,
                    dim = c(8, 8, 8, 8),
                    dimnames = list(pot1, pot2, pot3, pot4))

incArrayU <- function(i, j, k, r) {
  countArrayU[i, j, k, r] <<- countArrayU[i, j, k, r] + 1
  return(0)
}
incArrayF <- function(i, j, k, r) {
  countArrayF[i, j, k, r] <<- countArrayF[i, j, k, r] + 1
  return(0)
}

m = length(countArrayF)

k = 4096 #Value of k here is all possible values

TVEstimate = c()

for (i in 1:length(drawsUEFA)) {
  mapply(
    incArrayU,
    drawsUniformUEFA[[i]][1, 1, ],
    drawsUniformUEFA[[i]][1, 2, ],
    drawsUniformUEFA[[i]][1, 3, ],
    drawsUniformUEFA[[i]][1, 4, ]
  )
  mapply(incArrayF,
         drawsUEFA[[i]][1, 1, ],
         drawsUEFA[[i]][1, 2, ],
         drawsUEFA[[i]][1, 3, ],
         drawsUEFA[[i]][1, 4, ])
  
  probArrayU = countArrayU / i
  probArrayF = countArrayF / i
  
  
  diffarray = abs(probArrayU - probArrayF)
  
  estimate = sum(diffarray) #This corresponds to the estimates given in the project report
  
  TVEstimate[i] = estimate
}


plot(50000:10000, TVEstimate[50000:100000], type = "l", lwd = 1.5) #Only one value here as k is chosen so plot the latter terms
