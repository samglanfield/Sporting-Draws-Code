set.seed(911101)

#Subset teams

pot1 = c("ENG1", "ESP1", "GER1", "ENG2", "ESP2", "ITA1", "POR1", "FRA1")
pot2 = c("ESP3", "ESP4", "ITA2", "ENG3", "FRA2", "ENG4", "ESP5", "GER2")
pot3 = c("POR2", "NED1", "UKR1", "GER3", "AUT1", "POR3", "ITA3", "RUS1")
pot4 = c("TUR1", "UKR2", "BEL1", "SUI1", "ITA4", "SWE1", "GER4", "MDA1")

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
subset4a = c()
subset4b = c()
subset4c = c()
subset4d = c()

x = sample(pot1, 8, replace = FALSE)
y = sample(pot2, 8, replace = FALSE)
z = sample(pot3, 8, replace = FALSE)
w = sample(pot4, 8, replace = FALSE)

subset1a = x[1:2]
subset1b = x[3:4]
subset1c = x[5:6]
subset1d = x[7:8]

subset2a = y[1:2]
subset2b = y[3:4]
subset2c = y[5:6]
subset2d = y[7:8]

subset3a = z[1:2]
subset3b = z[3:4]
subset3c = z[5:6]
subset3d = z[7:8]

subset4a = w[1:2]
subset4b = w[3:4]
subset4c = w[5:6]
subset4d = w[7:8]

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
  subset3d,
  subset4a,
  subset4b,
  subset4c,
  subset4d
)


#Done subsetting the teams, now need the algorithm

subsetmodifier = function() {
  u = runif(1, 0, 1)
  if (u < 1 / 2) {
    letter = c("a", "b", "c", "d")
    x = sample(c(1, 2, 3, 4), 1)
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
    subset4 = list(subset4a, subset4b, subset4c, subset4d)
    
    subset1count = length(which(lengths(subset1) > 1))
    subset2count = length(which(lengths(subset2) > 1))
    subset3count = length(which(lengths(subset3) > 1))
    subset4count = length(which(lengths(subset4) > 1))
    
    assign("subset1count", subset1count, envir = .GlobalEnv)
    assign("subset2count", subset2count, envir = .GlobalEnv)
    assign("subset3count", subset3count, envir = .GlobalEnv)
    assign("subset4count", subset4count, envir = .GlobalEnv)
    
    subset1count2 = length(which(lengths(which(
      is.na(subset1) == FALSE
    )) < 3))
    subset2count2 = length(which(lengths(which(
      is.na(subset2) == FALSE
    )) < 3))
    subset3count2 = length(which(lengths(which(
      is.na(subset3) == FALSE
    )) < 3))
    subset4count2 = length(which(lengths(which(
      is.na(subset4) == FALSE
    )) < 3))
    
    assign("subset1count2", subset1count2, envir = .GlobalEnv)
    assign("subset2count2", subset2count2, envir = .GlobalEnv)
    assign("subset3count2", subset3count2, envir = .GlobalEnv)
    assign("subset4count2", subset4count2, envir = .GlobalEnv)
    
    possibletosample = c()
    for (i in 1:4) {
      if (get(paste("subset", i, "count", sep = ""), env = .GlobalEnv) >= 2 &&
          get(paste("subset", i, "count2", sep = ""), env = .GlobalEnv) >= 2) {
        possibletosample[i] = 1
      } else {
        possibletosample[i] = 0
      }
    }
    
    if (sum(possibletosample) == 0) {
      return(1)
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
    ))) < 3)
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

valueofinterestUEFA = c()
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

updateallprobsUEFA = function() {
  letters = c("a", "b", "c", "d")
  for (i in 2:4) {
    for (j in 1:length(letters)) {
      for (k in 1:length(letters)) {
        subset1 = get(paste("subset", 1, letters[j], sep = ""), env = .GlobalEnv)
        subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
        newprob = probupdate(subset1, subset2, drawsUEFA, i)
        valueofinterestUEFA = c(valueofinterestUEFA, newprob)
      }
    }
  }
  for (i in 3:4) {
    for (r in 1:length(letters)) {
      for (s in 1:length(letters)) {
        subset2 = get(paste("subset", 2, letters[r], sep = ""), env = .GlobalEnv)
        subset3 = get(paste("subset", i, letters[s], sep = ""), env = .GlobalEnv)
        newprob = probupdate(subset2, subset3, drawsUEFA, i)
        valueofinterestUEFA = c(valueofinterestUEFA, newprob)
      }
    }
  }
  for (r in 1:length(letters)) {
    for (s in 1:length(letters)) {
      subset3 = get(paste("subset", 3, letters[r], sep = ""), env = .GlobalEnv)
      subset4 = get(paste("subset", 4, letters[s], sep = ""), env = .GlobalEnv)
      newprob = probupdate(subset3, subset4, drawsUEFA, 4)
      valueofinterestUEFA = c(valueofinterestUEFA, newprob)
    }
  }
  return(valueofinterestUEFA)
}

updateallprobsUniform = function() {
  letters = c("a", "b", "c", "d")
  for (i in 2:4) {
    for (j in 1:length(letters)) {
      for (k in 1:length(letters)) {
        subset1 = get(paste("subset", 1, letters[j], sep = ""), env = .GlobalEnv)
        subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
        newprob = probupdate(subset1, subset2, drawsUniformUEFA, i)
        valueofinterestUniform = c(valueofinterestUniform, newprob)
      }
    }
  }
  for (i in 3:4) {
    for (r in 1:length(letters)) {
      for (s in 1:length(letters)) {
        subset2 = get(paste("subset", 2, letters[r], sep = ""), env = .GlobalEnv)
        subset3 = get(paste("subset", i, letters[s], sep = ""), env = .GlobalEnv)
        newprob = probupdate(subset2, subset3, drawsUniformUEFA, i)
        valueofinterestUniform = c(valueofinterestUniform, newprob)
      }
    }
  }
  for (r in 1:length(letters)) {
    for (s in 1:length(letters)) {
      subset3 = get(paste("subset", 3, letters[r], sep = ""), env = .GlobalEnv)
      subset4 = get(paste("subset", 4, letters[s], sep = ""), env = .GlobalEnv)
      newprob = probupdate(subset3, subset4, drawsUniformUEFA, 4)
      valueofinterestUniform = c(valueofinterestUniform, newprob)
    }
  }
  return(valueofinterestUniform)
}

#Only update changed probabilities

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

updatefewprobsUEFA = function() {
  letters = c("a", "b", "c", "d")
  for (i in 1:4) {
    for (k in 1:length(letters)) {
      subset1 = get("newsubset1", env = .GlobalEnv)
      subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
      newprob = probupdatev2(subset1, subset2, drawsUEFA, i)
      valueofinterestUEFA = c(valueofinterestUEFA, newprob)
    }
  }
  for (i in 1:4) {
    for (k in 1:length(letters)) {
      subset1 = get("newsubset2", env = .GlobalEnv)
      subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
      newprob = probupdatev2(subset1, subset2, drawsUEFA, i)
      valueofinterestUEFA = c(valueofinterestUEFA, newprob)
    }
  }
  return(valueofinterestUEFA)
}

updatefewprobsUniform = function() {
  letters = c("a", "b", "c", "d")
  for (i in 1:4) {
    for (k in 1:length(letters)) {
      subset1 = get("newsubset1", env = .GlobalEnv)
      subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
      newprob = probupdatev2(subset1, subset2, drawsUniformUEFA, i)
      valueofinterestUniform = c(valueofinterestUniform, newprob)
    }
  }
  for (i in 1:4) {
    for (k in 1:length(letters)) {
      subset1 = get("newsubset2", env = .GlobalEnv)
      subset2 = get(paste("subset", i, letters[k], sep = ""), env = .GlobalEnv)
      newprob = probupdatev2(subset1, subset2, drawsUniformUEFA, i)
      valueofinterestUniform = c(valueofinterestUniform, newprob)
    }
  }
  return(valueofinterestUniform)
}

#TV Matrix

matrixofinterest = matrix(0, nrow = 12, ncol = 12)
colnames(matrixofinterest) = c(
  "subset1a",
  "subset1b",
  "subset1c",
  "subset1d",
  "subset2a",
  "subset2b",
  "subset2c",
  "subset2d",
  "subset3a",
  "subset3b",
  "subset3c",
  "subset3d"
)
rownames(matrixofinterest) = c(
  "subset2a",
  "subset2b",
  "subset2c",
  "subset2d",
  "subset3a",
  "subset3b",
  "subset3c",
  "subset3d",
  "subset4a",
  "subset4b",
  "subset4c",
  "subset4d"
)


#TV Calcuator
TVfewCalculator = function() {
  valueofinterestUEFA = updatefewprobsUEFA()
  valueofinterestUniform = updatefewprobsUniform()
  valueofinterest = valueofinterestUEFA - valueofinterestUniform
  return(valueofinterest)
}

TVCalculator = function() {
  valueofinterestUEFA = updateallprobsUEFA()
  valueofinterestUniform = updateallprobsUniform()
  valueofinterest = valueofinterestUEFA - valueofinterestUniform
  TV = max(abs(valueofinterest))
  return(TV)
}

#Start of algorithm

TVvector = c()
e = new.env()
beta.t = 1

valueofinterestUEFA = updateallprobsUEFA()
valueofinterestUniform = updateallprobsUniform()
valueofinterest2 = valueofinterestUEFA - valueofinterestUniform

matrixofinterest[1:48] = valueofinterest2[1:48]
matrixofinterest[c(53:60, 65:72, 77:84, 89:96, 105:108, 117:120, 129:132, 141:144)] = valueofinterest2[49:96]

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
      subset3d,
      subset4a,
      subset4b,
      subset4c,
      subset4d
    ),
    envir = e
  )
  
  subsetmodifier()
  
  TVOld = TVvector[i - 1]
  
  valueofinterestUEFA = c()
  valueofinterestUniform = c()
  
  assign("currentmatrixofinterest", matrixofinterest, envir = e)
  
  TVNew = TVfewCalculator()
  
  if (potnumber == 1) {
    TVNew = TVNew[-c(1:4, 17:20)]
    indexno = which(letters %in% c(subsetletter1, subsetletter2))
    matrixofinterest[, indexno] = TVNew
  } else if (potnumber == 2) {
    TVNew = TVNew[-c(5:8, 21:24)]
    indexno = which(letters %in% c(subsetletter1, subsetletter2))
    foo = t(matrixofinterest)
    foo[1:4, indexno] = TVNew[c(1:4, 13:16)]
    matrixofinterest = t(foo)
    matrixofinterest[5:12, indexno + 4] = TVNew[c(5:12, 17:24)]
  } else if (potnumber == 3) {
    TVNew = TVNew[-c(9:12, 25:28)]
    indexno = which(letters %in% c(subsetletter1, subsetletter2))
    foo = t(matrixofinterest)
    foo[1:8, indexno + 4] = TVNew[c(1:8, 13:20)]
    matrixofinterest = t(foo)
    matrixofinterest[9:12, 8 + indexno] = TVNew[c(9:12, 21:24)]
  } else{
    TVNew = TVNew[-c(13:16, 29:32)]
    indexno = which(letters %in% c(subsetletter1, subsetletter2))
    foo = t(matrixofinterest)
    foo[, 8 + indexno] = TVNew
    matrixofinterest = t(foo)
  }
  
  matrixofinterest
  
  TVNew = max(abs(matrixofinterest))
  
  
  u = runif(1, 0, 1)
  if (u >= exp(-beta.t * (TVOld - TVNew))) {
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
    subset4a = currentstate[[13]]
    subset4b = currentstate[[14]]
    subset4c = currentstate[[15]]
    subset4d = currentstate[[16]]
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
  subset3d,
  subset4a,
  subset4b,
  subset4c,
  subset4d
)

results = list(initialstate, TVvector, finalstate, matrixofinterest)

save(results, file = "Metropolis on State Space - UEFA Draw 1.RData")
