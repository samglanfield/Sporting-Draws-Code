set.seed(3252525)

#Subset teams

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

subset1a = c()
subset1b = c()
subset1c = c()
subset1d = c()
subset2a = c()
subset2b = c()
subset2c = c()
subset2d = c()
subset3a = c()
subset3b = c()
subset3c = c()
subset3d = c()

x = sample(pot1, 16, replace = FALSE)
y = sample(pot2, 16, replace = FALSE)
z = sample(pot3, 16, replace = FALSE)

subset1a = x[1:4]
subset1b = x[5:8]
subset1c = x[9:12]
subset1d = x[13:16]

subset2a = y[1:4]
subset2b = y[5:8]
subset2c = y[9:12]
subset2d = y[13:16]

subset3a = z[1:4]
subset3b = z[5:8]
subset3c = z[9:12]
subset3d = z[13:16]

initialstate = list(
  subset1a,
  subset1b,
  subset1c,
  subset1d,
  subset2a,
  subset2b,
  subset2c,
  subset2d,
  subset3a,
  subset3b,
  subset3c,
  subset3d
)

#Now form the algorithm, subset modifier changes the subsets

subsetmodifier = function() {
  u = runif(1, 0, 1)
  if (u < 1 / 2) {
    letter = c("a", "b", "c", "d")
    x = sample(c(1, 2, 3), 1)
    assign("potnumber", x, envir = .GlobalEnv)
    y = sample(letter, 1)
    assign("subsetletter1", y, envir = .GlobalEnv)
    currentsub = get(paste("subset", x, y, sep = ""), env = .GlobalEnv)
    team = sample(currentsub, 1)
    proposedsubletter = sample(letter[!letter == y], 1)
    assign("subsetletter2", proposedsubletter, envir = .GlobalEnv)
    proposedsub = get(paste("subset", x, proposedsubletter, sep = ""), env =
                        .GlobalEnv)
    proposedteam = sample(proposedsub, 1)
    currentsub = currentsub[!currentsub == team]
    currentsub = c(currentsub, proposedteam)
    proposedsub = proposedsub[!proposedsub == proposedteam]
    proposedsub = c(proposedsub, team)
    assign(paste("subset", x, y, sep = ""), currentsub, envir = .GlobalEnv)
    assign(paste("subset", x, proposedsubletter, sep = ""),
           proposedsub,
           envir = .GlobalEnv)
    assign("newsubset1", currentsub, envir = .GlobalEnv)
    assign("newsubset2", proposedsub, envir = .GlobalEnv)
  }  else{
    subset1 = list(subset1a, subset1b, subset1c, subset1d)
    subset2 = list(subset2a, subset2b, subset2c, subset2d)
    subset3 = list(subset3a, subset3b, subset3c, subset3d)
    
    subset1count = length(which(lengths(subset1) > 1))
    subset2count = length(which(lengths(subset2) > 1))
    subset3count = length(which(lengths(subset3) > 1))
    
    assign("subset1count", subset1count, envir = .GlobalEnv)
    assign("subset2count", subset2count, envir = .GlobalEnv)
    assign("subset3count", subset3count, envir = .GlobalEnv)
    
    possibletosample = c()
    for (i in 1:3) {
      if (get(paste("subset", i, "count", sep = ""), env = .GlobalEnv) >= 2) {
        possibletosample[i] = 1
      } else {
        possibletosample[i] = 0
      }
    }
    foo = which(possibletosample == 1)
    x = foo[sample(length(foo), 1)]
    assign("potnumber", x, envir = .GlobalEnv)
    bar = which(lengths(get(paste(
      "subset", x, sep = ""
    ))) > 1)
    yno = bar[sample(length(bar), 1)]
    y = letters[yno]
    assign("subsetletter1", y, envir = .GlobalEnv)
    currentsub = get(paste("subset", x, y, sep = ""), env = .GlobalEnv)
    team = sample(currentsub, 1)
    currentsub = currentsub[!currentsub == team]
    assign(paste("subset", x, y, sep = ""), currentsub, envir = .GlobalEnv)
    
    ####### Keep x need a different y
    swapsample = which(lengths(get(paste(
      "subset", x, sep = ""
    ))) < 5)
    swapsample = swapsample[which(!swapsample %in% yno)]
    yno2 = swapsample[sample(length(swapsample), 1)]
    y2 = letters[yno2]
    assign("subsetletter2", y2, envir = .GlobalEnv)
    proposedsub = get(paste("subset", x, y2, sep = ""), env = .GlobalEnv)
    proposedsub = c(proposedsub, team)
    assign(paste("subset", x, y2, sep = ""), proposedsub, envir = .GlobalEnv)
    assign("newsubset1", currentsub, envir = .GlobalEnv)
    assign("newsubset2", proposedsub, envir = .GlobalEnv)
  }
  return(1)
}


#The below functions calculate all of the subset vs subset probabilities for both FIFA and Uniform method

valueofinterestFIFA = c()
valueofinterestUniform = c()

probupdate = function(subset1, subset2, draws, i) {
  count = 0
  positions = substr(c(deparse(substitute(subset1)), deparse(substitute(subset2))), 7, 7)
  for (k in 1:length(draws)) {
    groupindex = which(draws[[k]][1, as.numeric(positions[1]), ] %in% subset1)
    groupindex2 = which(draws[[k]][1, i, ] %in% subset2)
    if (length(which(groupindex %in% groupindex2)) > 0) {
      count = count + 1
    }
  }
  return(count / length(draws))
}

updateallprobsFIFA = function() {
  letters = c("a", "b", "c", "d")
  for (i in 2:3) {
    for (j in 1:length(letters)) {
      for (k in 1:length(letters)) {
        subset1 = get(paste("subset", 1, letters[j], sep = ""), env = .GlobalEnv)
        subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
        newprob = probupdate(subset1, subset2, drawsFIFA, i)
        valueofinterestFIFA = c(valueofinterestFIFA, newprob)
      }
    }
  }
  for (r in 1:length(letters)) {
    for (s in 1:length(letters)) {
      subset2 = get(paste("subset", 2, letters[r], sep = ""), env = .GlobalEnv)
      subset3 = get(paste("subset", 3, letters[s], sep = ""), env = .GlobalEnv)
      newprob = probupdate(subset2, subset3, drawsFIFA, 3)
      valueofinterestFIFA = c(valueofinterestFIFA, newprob)
    }
  }
  return(valueofinterestFIFA)
}

updateallprobsUniform = function() {
  letters = c("a", "b", "c", "d")
  for (i in 2:3) {
    for (j in 1:length(letters)) {
      for (k in 1:length(letters)) {
        subset1 = get(paste("subset", 1, letters[j], sep = ""), env = .GlobalEnv)
        subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
        newprob = probupdate(subset1, subset2, drawsUniform, i)
        valueofinterestUniform = c(valueofinterestUniform, newprob)
      }
    }
  }
  for (r in 1:length(letters)) {
    for (s in 1:length(letters)) {
      subset2 = get(paste("subset", 2, letters[r], sep = ""), env = .GlobalEnv)
      subset3 = get(paste("subset", 3, letters[s], sep = ""), env = .GlobalEnv)
      newprob = probupdate(subset2, subset3, drawsUniform, 3)
      valueofinterestUniform = c(valueofinterestUniform, newprob)
    }
  }
  return(valueofinterestUniform)
}

### Only update changed probs

probupdatev2 = function(subset1, subset2, draws, i) {
  count = 0
  positions = substr(c(deparse(substitute(subset1)), deparse(substitute(subset2))), 7, 7)
  for (k in 1:length(draws)) {
    groupindex = which(draws[[k]][1, potnumber, ] %in% subset1)
    groupindex2 = which(draws[[k]][1, i, ] %in% subset2)
    if (length(which(groupindex %in% groupindex2)) > 0) {
      count = count + 1
    }
  }
  return(count / length(draws))
}

updatefewprobsFIFA = function() {
  letters = c("a", "b", "c", "d")
  for (i in 1:3) {
    for (k in 1:length(letters)) {
      subset1 = get("newsubset1", env = .GlobalEnv)
      subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
      newprob = probupdatev2(subset1, subset2, drawsFIFA, i)
      valueofinterestFIFA = c(valueofinterestFIFA, newprob)
    }
  }
  for (i in 1:3) {
    for (k in 1:length(letters)) {
      subset1 = get("newsubset2", env = .GlobalEnv)
      subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
      newprob = probupdatev2(subset1, subset2, drawsFIFA, i)
      valueofinterestFIFA = c(valueofinterestFIFA, newprob)
    }
  }
  return(valueofinterestFIFA)
}

updatefewprobsUniform = function() {
  letters = c("a", "b", "c", "d")
  for (i in 1:3) {
    for (k in 1:length(letters)) {
      subset1 = get("newsubset1", env = .GlobalEnv)
      subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
      newprob = probupdatev2(subset1, subset2, drawsUniform, i)
      valueofinterestUniform = c(valueofinterestUniform, newprob)
    }
  }
  for (i in 1:3) {
    for (k in 1:length(letters)) {
      subset1 = get("newsubset2", env = .GlobalEnv)
      subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
      newprob = probupdatev2(subset1, subset2, drawsUniform, i)
      valueofinterestUniform = c(valueofinterestUniform, newprob)
    }
  }
  return(valueofinterestUniform)
}

# TV matric

matrixofinterest = matrix(0, nrow = 8, ncol = 8)
colnames(matrixofinterest) = c(
  "subset1a",
  "subset1b",
  "subset1c",
  "subset1d",
  "subset2a",
  "subset2b",
  "subset2c",
  "subset2d"
)
rownames(matrixofinterest) = c(
  "subset2a",
  "subset2b",
  "subset2c",
  "subset2d",
  "subset3a",
  "subset3b",
  "subset3c",
  "subset3d"
)

#Below function calcualtes the updated TV



TVfewCalculator = function() {
  valueofinterestFIFA = updatefewprobsFIFA()
  valueofinterestUniform = updatefewprobsUniform()
  valueofinterest = valueofinterestFIFA - valueofinterestUniform
  return(valueofinterest)
}

TVCalculator = function() {
  valueofinterestFIFA = updateallprobsFIFA()
  valueofinterestUniform = updateallprobsUniform()
  valueofinterest = valueofinterestFIFA - valueofinterestUniform
  TV = max(abs(valueofinterest))
  return(TV)
}

#Algorithm starts below

TVvector = c()
e = new.env()
beta.t = 1
#set.seed(864512)

valueofinterestFIFA = updateallprobsFIFA()
valueofinterestUniform = updateallprobsUniform()
valueofinterest2 = valueofinterestFIFA - valueofinterestUniform

matrixofinterest[1:32] = valueofinterest2[1:32]
matrixofinterest[c(37:40, 45:48, 53:56, 61:64)] = valueofinterest2[33:48]


TVvector[1] = max(abs(matrixofinterest))

for (i in 2:1000) {
  assign(
    "currentstate",
    list(
      subset1a,
      subset1b,
      subset1c,
      subset1d,
      subset2a,
      subset2b,
      subset2c,
      subset2d,
      subset3a,
      subset3b,
      subset3c,
      subset3d
    ),
    envir = e
  )
  
  subsetmodifier()
  
  TVOld = TVvector[i - 1]
  
  valueofinterestFIFA = c()
  valueofinterestUniform = c()
  
  assign("currentmatrixofinterest", matrixofinterest, envir = e)
  
  TVNew = TVfewCalculator()
  if (potnumber == 1) {
    TVNew = TVNew[-c(1:4, 13:16)]
    indexno = which(letters %in% c(subsetletter1, subsetletter2))
    matrixofinterest[, indexno] = TVNew
  } else if (potnumber == 2) {
    TVNew = TVNew[-c(5:8, 17:20)]
    indexno = which(letters %in% c(subsetletter1, subsetletter2))
    foo = t(matrixofinterest)
    foo[1:4, indexno] = TVNew[c(1:4, 9:12)]
    matrixofinterest = t(foo)
    matrixofinterest[5:8, indexno + 4] = TVNew[c(5:8, 13:16)]
  } else{
    TVNew = TVNew[-c(9:12, 21:24)]
    indexno = which(letters %in% c(subsetletter1, subsetletter2))
    foo = t(matrixofinterest)
    foo[, 4 + indexno] = TVNew
    matrixofinterest = t(foo)
  }
  
  TVNew = max(abs(matrixofinterest))
  
  u = runif(1, 0, 1)
  if (u >= exp(-beta.t * (TVOld - TVNew))) {
    #rejected
    currentstate = get("currentstate", env = e)
    subset1a = currentstate[[1]]
    subset1b = currentstate[[2]]
    subset1c = currentstate[[3]]
    subset1d = currentstate[[4]]
    subset2a = currentstate[[5]]
    subset2b = currentstate[[6]]
    subset2c = currentstate[[7]]
    subset2d = currentstate[[8]]
    subset3a = currentstate[[9]]
    subset3b = currentstate[[10]]
    subset3c = currentstate[[11]]
    subset3d = currentstate[[12]]
    TVvector[i] = TVOld
    matrixofinterest = get("currentmatrixofinterest", env = e)
  } else{
    TVvector[i] = TVNew
  }
  beta.t = beta.t * 1.01
  print(paste("Simulation", i, TVvector[i]))
}

plot(TVvector, type = "l", lwd = 1.5)

finalstate = list(
  subset1a,
  subset1b,
  subset1c,
  subset1d,
  subset2a,
  subset2b,
  subset2c,
  subset2d,
  subset3a,
  subset3b,
  subset3c,
  subset3d
)

results = list(initialstate, TVvector, finalstate, matrixofinterest)

#save(results,file="Metropolis on State Space - FIFA EU Draw 1.RData")

