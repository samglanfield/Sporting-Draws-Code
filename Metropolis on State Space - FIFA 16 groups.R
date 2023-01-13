set.seed(310101)

#Subset teams

pot1 = c("CONMEBOL1","EU1","CONMEBOL2","EU2","EU3","EU4","EU5","EU6","EU7","EU8","EU9","EU10","CONCACAF1","CONMEBOL3","CONCACAF2","CONCACAF3")
pot2 = c("EU11","CONMEBOL4","CAF1","EU12","AFC1","EU13","CAF2","CONMEBOL5","AFC2","EU14","EU15","EU16","AFC3","CONMEBOL6","CAF3","CONMEBOL7")
pot3 = c("CAF4","CAF5","OFC1","CAF6","CAF7","CAF8","CAF9","AFC4","AFC5","CAF10","CONCACAF4","CONCACAF5","AFC6","AFC7","CONCACAF6","AFC8")

subset1a = c();subset1b=c();subset1c=c();subset1d=c()
subset2a = c();subset2b=c();subset2c=c();subset2d=c()
subset3a = c();subset3b=c();subset3c=c();subset3d=c()

x = sample(pot1,16,replace=FALSE); y = sample(pot2,16,replace=FALSE); z = sample(pot3,16,replace=FALSE)

subset1a = x[1:4]; subset1b = x[5:8]; subset1c = x[9:12]; subset1d = x[13:16]

subset2a = y[1:4]; subset2b = y[5:8]; subset2c = y[9:12]; subset2d = y[13:16]

subset3a = z[1:4]; subset3b = z[5:8]; subset3c = z[9:12]; subset3d = z[13:16]


#Now form the algorithm, subset modifier changes the subsets

subsetmodifier = function(){
  
  u = runif(1,0,1)
  if(u<1/2){
    
    letter = c("a","b","c","d")
    x = sample(c(1,2,3),1)
    y = sample(letter,1)
    currentsub = get(paste("subset",x,y,sep=""),env=.GlobalEnv)
    team = sample(currentsub,1)
    proposedsubletter = sample(letter[!letter==y],1)
    proposedsub = get(paste("subset",x,proposedsubletter,sep=""),env=.GlobalEnv)
    proposedteam = sample(proposedsub,1)
    currentsub = currentsub[!currentsub==team]; currentsub = c(currentsub,proposedteam)
    proposedsub = proposedsub[!proposedsub==proposedteam]; proposedsub = c(proposedsub,team)
    assign(paste("subset",x,y,sep=""),currentsub,envir=.GlobalEnv); 	assign(paste("subset",x,proposedsubletter,sep=""),proposedsub,envir=.GlobalEnv)
  }  else{
    subset1 = list(subset1a,subset1b,subset1c,subset1d); subset2 = list(subset2a,subset2b,subset2c,subset2d)
    subset3 = list(subset3a,subset3b,subset3c,subset3d)
    
    subset1count = length(which(lengths(subset1)>1)); subset2count = length(which(lengths(subset2)>1))
    subset3count = length(which(lengths(subset3)>1))
    
    assign("subset1count",subset1count,envir = .GlobalEnv); assign("subset2count",subset2count,envir = .GlobalEnv)
    assign("subset3count",subset3count,envir = .GlobalEnv)
    
    possibletosample = c()
    for(i in 1:3){
      if(get(paste("subset",i,"count",sep=""),env=.GlobalEnv)>=2){possibletosample[i]=1
      } else {possibletosample[i]=0}
    }
    
    x = sample(which(possibletosample==1),1)
    yno = sample(which(lengths(get(paste("subset",x,sep="")))>1),1); y = letters[yno] #here and above is correct
    currentsub = get(paste("subset",x,y,sep=""),env=.GlobalEnv)
    team = sample(currentsub,1)
    currentsub = currentsub[!currentsub==team]
    assign(paste("subset",x,y,sep=""),currentsub,envir=.GlobalEnv)
    
    ####### Keep x need a different y
    swapsample = which(lengths(get(paste("subset",x,sep="")))<5); swapsample = swapsample[which(! swapsample %in% yno)]
    yno2 = sample(swapsample,1); y2 = letters[yno2]
    proposedsub = get(paste("subset",x,y2,sep=""),env=.GlobalEnv)
    proposedsub = c(proposedsub,team)
    assign(paste("subset",x,y2,sep=""),proposedsub,envir=.GlobalEnv)
  }
  return(1)
}


#The below functions calculate all of the subset vs subset probabilities for both FIFA and Uniform method

valueofinterestFIFA = c(); valueofinterestUniform = c()

probupdate = function(subset1,subset2,draws){
  count=0
  positions = substr(c(deparse(substitute(subset1)),deparse(substitute(subset2))),7,7)
  for(k in 1:length(draws)){
    groupindex = which(draws[[k]][1,as.numeric(positions[1]),] %in% subset1)
    groupindex2 = which(draws[[k]][1,as.numeric(positions[2]),] %in% subset2)
    if(length(which(groupindex %in% groupindex2))>0){
      count=count+1
    }
  }
  return(count/length(draws))
}

updateallprobsFIFA = function(){
  letters = c("a","b","c","d")
  for(i in 2:3){
    for(j in 1:length(letters)){
      for(k in 1:length(letters)){
        subset1 = get(paste("subset",1,letters[j],sep=""),env=.GlobalEnv)
        subset2 = get(paste("subset",i,letters[k],sep=""),env=.GlobalEnv)
        newprob = probupdate(subset1,subset2,drawsFIFA)
        valueofinterestFIFA = c(valueofinterestFIFA,newprob)
      }	
    }
  }
  for(r in 1:length(letters)){
    for(s in 1:length(letters)){
      subset1 = get(paste("subset",2,letters[r],sep=""),env=.GlobalEnv)
      subset2 = get(paste("subset",3,letters[s],sep=""),env=.GlobalEnv)
      newprob = probupdate(subset1,subset2,drawsFIFA)
      valueofinterestFIFA = c(valueofinterestFIFA,newprob)
    }
  }
  return(valueofinterestFIFA)
}

updateallprobsUniform = function(){
  letters = c("a","b","c","d")
  for(i in 2:3){
    for(j in 1:length(letters)){
      for(k in 1:length(letters)){
        subset1 = get(paste("subset",1,letters[j],sep=""),env=.GlobalEnv)
        subset2 = get(paste("subset",i,letters[k],sep=""),env=.GlobalEnv)
        newprob = probupdate(subset1,subset2,drawsUniform)
        valueofinterestUniform = c(valueofinterestUniform,newprob)
      }	
    }
  }
  for(r in 1:length(letters)){
    for(s in 1:length(letters)){
      subset1 = get(paste("subset",2,letters[r],sep=""),env=.GlobalEnv)
      subset2 = get(paste("subset",3,letters[s],sep=""),env=.GlobalEnv)
      newprob = probupdate(subset1,subset2,drawsUniform)
      valueofinterestUniform = c(valueofinterestUniform,newprob)
    }
  }
  return(valueofinterestUniform)
}

#Below function calcualtes the updated TV 

TVCalculator = function(){
  valueofinterestFIFA = updateallprobsFIFA()
  valueofinterestUniform = updateallprobsUniform()
  valueofinterest = valueofinterestFIFA - valueofinterestUniform
  TV = max(abs(valueofinterest))
  return(TV)
}

#Algorithm starts below

TVvector = c();e=new.env()
beta.t=1
set.seed(864512)

for(i in 1:1000){
  TVOld = TVCalculator()
  TVvector[i] = TVOld
  assign("currentstate",list(subset1a,subset1b,subset1c,subset1d,subset2a,subset2b,subset2c,subset2d,subset3a,subset3b,subset3c,subset3d),envir=e)
  
  subsetmodifier()
  TVNew = TVCalculator()
  
  u=runif(1,0,1)
  if(u>=exp(-beta.t*(TVOld-TVNew))){ #rejected
    currentstate = get("currentstate",env=e)
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
  }
  beta.t = beta.t*1.01
  print(paste("Simulation",i,TVvector[i]))
}

plot(TVvector,type="l",lwd=1.5)

