#Load in the draws prior to running this code

#Create the arrays to produce counts in the algorithm

set.seed(3101301)

pot1 = c("CONMEBOL1","EU1","CONMEBOL2","EU2","EU3","EU4","EU5","EU6","EU7","EU8","EU9","EU10","CONCACAF1","CONMEBOL3","CONCACAF2","CONCACAF3")
pot2 = c("EU11","CONMEBOL4","CAF1","EU12","AFC1","EU13","CAF2","CONMEBOL5","AFC2","EU14","EU15","EU16","AFC3","CONMEBOL6","CAF3","CONMEBOL7")
pot3 = c("CAF4","CAF5","OFC1","CAF6","CAF7","CAF8","CAF9","AFC4","AFC5","CAF10","CONCACAF4","CONCACAF5","AFC6","AFC7","CONCACAF6","AFC8")

countArrayU = array(0,dim=c(16,16,16),dimnames=list(pot1,pot2,pot3))
countArrayF = array(0,dim=c(16,16,16),dimnames=list(pot1,pot2,pot3))

incArrayU <- function(i, j, k) {
  countArrayU[i, j, k] <<- countArrayU[i, j, k] + 1
  return(0)
}
incArrayF <- function(i, j, k) {
  countArrayF[i, j, k] <<- countArrayF[i, j, k] + 1
  return(0)
}
.#start of algorithm

for(j in 1:100){

countArrayU = array(0,dim=c(16,16,16),dimnames=list(pot1,pot2,pot3))
countArrayF = array(0,dim=c(16,16,16),dimnames=list(pot1,pot2,pot3))

m=length(countArrayF)

k=sample(1:4096,1) #randomly sample number of indices to use

indices = sample(1:4096,k)

TVEstimate=c()

for(i in 1:length(drawsFIFA)){
  mapply(incArrayU,drawsUniform[[i]][1,1,],drawsUniform[[i]][1,2,],drawsUniform[[i]][1,3,])
  mapply(incArrayF,drawsFIFA[[i]][1,1,],drawsFIFA[[i]][1,2,],drawsFIFA[[i]][1,3,])

  probArrayU = countArrayU/i
  probArrayF = countArrayF/i
  
  
diffarray = abs(probArrayU-probArrayF)

estimate = 4096*(sum(diffarray[indices])/k) #use the TV estimate given in the report on the given indices

TVEstimate[i] = estimate

}
assign(paste("TVEstimate",j,sep=""),TVEstimate,envir=.GlobalEnv)
print(paste("Simulation",j))
}

#Create vectors of the minimum, maximum and mean for each element

TVMin = pmin(TVEstimate1,TVEstimate2,TVEstimate3,TVEstimate4,TVEstimate5,TVEstimate6,TVEstimate7,TVEstimate8,TVEstimate9,TVEstimate10,
             TVEstimate11,TVEstimate12,TVEstimate13,TVEstimate14,TVEstimate15,TVEstimate16,TVEstimate17,TVEstimate18,TVEstimate19,TVEstimate20,
             TVEstimate21,TVEstimate22,TVEstimate23,TVEstimate24,TVEstimate25,TVEstimate26,TVEstimate27,TVEstimate28,TVEstimate29,TVEstimate30,
             TVEstimate31,TVEstimate32,TVEstimate33,TVEstimate34,TVEstimate35,TVEstimate36,TVEstimate37,TVEstimate38,TVEstimate39,TVEstimate40,
             TVEstimate41,TVEstimate42,TVEstimate43,TVEstimate44,TVEstimate45,TVEstimate46,TVEstimate47,TVEstimate48,TVEstimate49,TVEstimate50,
             TVEstimate51,TVEstimate52,TVEstimate53,TVEstimate54,TVEstimate55,TVEstimate56,TVEstimate57,TVEstimate58,TVEstimate59,TVEstimate60,
             TVEstimate61,TVEstimate62,TVEstimate63,TVEstimate64,TVEstimate65,TVEstimate66,TVEstimate67,TVEstimate68,TVEstimate69,TVEstimate70,
             TVEstimate71,TVEstimate72,TVEstimate73,TVEstimate74,TVEstimate75,TVEstimate76,TVEstimate77,TVEstimate78,TVEstimate79,TVEstimate80,
             TVEstimate81,TVEstimate82,TVEstimate83,TVEstimate84,TVEstimate85,TVEstimate86,TVEstimate87,TVEstimate88,TVEstimate89,TVEstimate90,
             TVEstimate91,TVEstimate92,TVEstimate93,TVEstimate94,TVEstimate95,TVEstimate96,TVEstimate97,TVEstimate98,TVEstimate99,TVEstimate100)
TVMax = pmax(TVEstimate1,TVEstimate2,TVEstimate3,TVEstimate4,TVEstimate5,TVEstimate6,TVEstimate7,TVEstimate8,TVEstimate9,TVEstimate10,
             TVEstimate11,TVEstimate12,TVEstimate13,TVEstimate14,TVEstimate15,TVEstimate16,TVEstimate17,TVEstimate18,TVEstimate19,TVEstimate20,
             TVEstimate21,TVEstimate22,TVEstimate23,TVEstimate24,TVEstimate25,TVEstimate26,TVEstimate27,TVEstimate28,TVEstimate29,TVEstimate30,
             TVEstimate31,TVEstimate32,TVEstimate33,TVEstimate34,TVEstimate35,TVEstimate36,TVEstimate37,TVEstimate38,TVEstimate39,TVEstimate40,
             TVEstimate41,TVEstimate42,TVEstimate43,TVEstimate44,TVEstimate45,TVEstimate46,TVEstimate47,TVEstimate48,TVEstimate49,TVEstimate50,
             TVEstimate51,TVEstimate52,TVEstimate53,TVEstimate54,TVEstimate55,TVEstimate56,TVEstimate57,TVEstimate58,TVEstimate59,TVEstimate60,
             TVEstimate61,TVEstimate62,TVEstimate63,TVEstimate64,TVEstimate65,TVEstimate66,TVEstimate67,TVEstimate68,TVEstimate69,TVEstimate70,
             TVEstimate71,TVEstimate72,TVEstimate73,TVEstimate74,TVEstimate75,TVEstimate76,TVEstimate77,TVEstimate78,TVEstimate79,TVEstimate80,
             TVEstimate81,TVEstimate82,TVEstimate83,TVEstimate84,TVEstimate85,TVEstimate86,TVEstimate87,TVEstimate88,TVEstimate89,TVEstimate90,
             TVEstimate91,TVEstimate92,TVEstimate93,TVEstimate94,TVEstimate95,TVEstimate96,TVEstimate97,TVEstimate98,TVEstimate99,TVEstimate100)
TVMean = colMeans(rbind(TVEstimate1,TVEstimate2,TVEstimate3,TVEstimate4,TVEstimate5,TVEstimate6,TVEstimate7,TVEstimate8,TVEstimate9,TVEstimate10,
                        TVEstimate11,TVEstimate12,TVEstimate13,TVEstimate14,TVEstimate15,TVEstimate16,TVEstimate17,TVEstimate18,TVEstimate19,TVEstimate20,
                        TVEstimate21,TVEstimate22,TVEstimate23,TVEstimate24,TVEstimate25,TVEstimate26,TVEstimate27,TVEstimate28,TVEstimate29,TVEstimate30,
                        TVEstimate31,TVEstimate32,TVEstimate33,TVEstimate34,TVEstimate35,TVEstimate36,TVEstimate37,TVEstimate38,TVEstimate39,TVEstimate40,
                        TVEstimate41,TVEstimate42,TVEstimate43,TVEstimate44,TVEstimate45,TVEstimate46,TVEstimate47,TVEstimate48,TVEstimate49,TVEstimate50,
                        TVEstimate51,TVEstimate52,TVEstimate53,TVEstimate54,TVEstimate55,TVEstimate56,TVEstimate57,TVEstimate58,TVEstimate59,TVEstimate60,
                        TVEstimate61,TVEstimate62,TVEstimate63,TVEstimate64,TVEstimate65,TVEstimate66,TVEstimate67,TVEstimate68,TVEstimate69,TVEstimate70,
                        TVEstimate71,TVEstimate72,TVEstimate73,TVEstimate74,TVEstimate75,TVEstimate76,TVEstimate77,TVEstimate78,TVEstimate79,TVEstimate80,
                        TVEstimate81,TVEstimate82,TVEstimate83,TVEstimate84,TVEstimate85,TVEstimate86,TVEstimate87,TVEstimate88,TVEstimate89,TVEstimate90,
                        TVEstimate91,TVEstimate92,TVEstimate93,TVEstimate94,TVEstimate95,TVEstimate96,TVEstimate97,TVEstimate98,TVEstimate99,TVEstimate100))
  

#plot the last 50,000 values to ensure accurate values are given
  
plot(TVMean[50000:100000],type="l",lwd=1.5,ylim=c(min(TVMin[50000:100000]),max(TVMax[50000:100000])))

polygon(c(1:50001,rev(1:50001)),c(TVMin[50000:100000],rev(TVMax[50000:100000])),col="grey") #creates a shaded plot to indicate confidence region
lines(TVMean[50000:100000],col="red",lwd=2)
