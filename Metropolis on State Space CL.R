set.seed(310101)

#Subset teams

pot1 = c("ITNap","PORPor","GERBay","UKTot","UKChe","SPARM","UKMC","PORBen")
pot2 = c("UKLiv","BELCB","ITIM","GERFra","ITMil","GERLei","GERDort","FRAPSG")

subset1a = c();subset1b=c();subset1c=c();subset1d=c()
subset2a = c();subset2b=c();subset2c=c();subset2d=c()

x = sample(pot1,8,replace=FALSE); y = sample(pot2,8,replace=FALSE)

subset1a = x[1:2]; subset1b = x[3:4]; subset1c = x[5:6]; subset1d = x[7:8]

subset2a = y[1:2]; subset2b = y[3:4]; subset2c = y[5:6]; subset2d = y[7:8]

#Produce the algorithm, subssetmodifier changes the subsets by swapping or adding/deleting

subsetmodifier = function(){
  
  u = runif(1,0,1)
  if(u<1/2){
    
    letter = c("a","b","c","d")
    x = sample(c(1,2),1)
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
    
    subset1count = length(which(lengths(subset1)>1)); subset2count = length(which(lengths(subset2)>1))
    
    assign("subset1count",subset1count,envir = .GlobalEnv); assign("subset2count",subset2count,envir = .GlobalEnv)
    
    possibletosample = c()
    for(i in 1:2){
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
    swapsample = which(lengths(get(paste("subset",x,sep="")))<3); swapsample = swapsample[which(! swapsample %in% yno)]
    yno2 = sample(swapsample,1); y2 = letters[yno2]
    proposedsub = get(paste("subset",x,y2,sep=""),env=.GlobalEnv)
    proposedsub = c(proposedsub,team)
    assign(paste("subset",x,y2,sep=""),proposedsub,envir=.GlobalEnv)
  }
  return(1)
}


#The below functions calculate all of the subset vs subset probabilities for both CL and Uniform method

valueofinterestCL = c(); valueofinterestUniform = c()

probupdate = function(subset1,subset2,draws){
  count=0
  for(k in 1:length(draws)){
    groupindex = which(draws[[k]][1,1,] %in% subset1) #this will need to change once the format of the output has happened
    groupindex2 = which(draws[[k]][1,2,] %in% subset2)
    if(length(which(groupindex %in% groupindex2))>0){
      count=count+1
    }
  }
  return(count/length(draws))
}

updateallprobsCL = function(){
  letters = c("a","b","c","d")
    for(j in 1:length(letters)){
      for(k in 1:length(letters)){
        subset1 = get(paste("subset",1,letters[j],sep=""),env=.GlobalEnv)
        subset2 = get(paste("subset",2,letters[k],sep=""),env=.GlobalEnv)
        newprob = probupdate(subset1,subset2,drawsS)
        valueofinterestCL = c(valueofinterestCL,newprob)
      }	
    }
  return(valueofinterestCL)
}

updateallprobsUniform = function(){
  letters = c("a","b","c","d")
    for(j in 1:length(letters)){
      for(k in 1:length(letters)){
        subset1 = get(paste("subset",1,letters[j],sep=""),env=.GlobalEnv)
        subset2 = get(paste("subset",2,letters[k],sep=""),env=.GlobalEnv)
        newprob = probupdate(subset1,subset2,drawsU)
        valueofinterestUniform = c(valueofinterestUniform,newprob)
      }	
    }
  return(valueofinterestUniform)
}

#Below function calculates the updated TV

TVCalculator = function(){
  valueofinterestCL = updateallprobsCL()
  valueofinterestUniform = updateallprobsUniform()
  valueofinterest = valueofinterestCL - valueofinterestUniform
  TV = max(abs(valueofinterest))
  return(TV)
}

#Below is the algorithm

TVvector = c();e=new.env()
beta.t=1

for(i in 1:1000){
  TVOld = TVCalculator()
  TVvector[i] = TVOld
  assign("currentstate",list(subset1a,subset1b,subset1c,subset1d,subset2a,subset2b,subset2c,subset2d),env=e)
  
  subsetmodifier()
  TVNew = TVCalculator()
  
  u=runif(1,0,1)
  if(u>=exp(-beta.t*(TVOld-TVNew))){ #Rejected, acceptance already done if no changes made
    currentstate = get("currentstate",env=e)
    subset1a = currentstate[[1]]
    subset1b = currentstate[[2]]
    subset1c = currentstate[[3]]
    subset1d = currentstate[[4]]
    subset2a = currentstate[[5]]
    subset2b = currentstate[[6]]
    subset2c = currentstate[[7]]
    subset2d = currentstate[[8]]
  }
  print(paste("Simulation",i,TVvector[i]))
  beta.t = beta.t*1.01 
}

plot(TVvector,type="l",lwd=1.5)

