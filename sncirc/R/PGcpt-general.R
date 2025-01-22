# Periodic-global changepoint search method for users, v2
# got rid of the parallisation
# possible fix for the minseglen problem: missing penalty (not tackled the fisch problem yet)

pgcpt = function(data, period.len=12, minseglen.periodic=1, minseglen.global=2*period.len,
                 method.periodic="SNcirc", method.global="PELT", max.periodic.cpts=5,
                 penalty.periodic=3, penalty.global=3*max.periodic.cpts*log(length(data)), cost="Likelihood",
                 dist="Normal meanvar", restrict=FALSE, circData=TRUE, ...
){
  # stumped on what to do about penalty terms. We have different penalty terms for periodic-global cpts
    # need to ask Rebecca about it. Periodic is SIC, but what about macro?
  # Penalty.periodic assumes SIC. I should change this in the future.
  ## the problem comes with how to define it in the sncirc function later, as the size of the data changes as we go through the algorithm, hence log(n) will also change
  
  # assume data comes in two forms, matrix or vector.
  # vector: just the data. matrix/dataframe: two columns, first is data, second could be time index
  if(is.null(dim(data))){
    n = length(data)
  } else if(is.numeric(dim(data))){
    n = nrow(data)
  } else{stop("Data must be a vector or matrix/dataframe with 1st column as data")}
  
  if(!is.numeric(period.len)){stop("Period length must be numeric")}
  if(period.len<3){stop("Period length must be at least 3 to contain changepoints")}
  
  if(!is.numeric(max.periodic.cpts)){stop("Maximum number of changepoints must be numeric")}
  if(max.periodic.cpts>floor(period.len/minseglen.periodic)){stop(paste('M is larger than the maximum number of changepoints: ',floor(period.len/minseglen.periodic)))}
  
  if(!is.numeric(minseglen.periodic)){stop("Periodic minimum segment length must be numeric")}
  if(minseglen.periodic<1){stop("Periodic minimum segment length needs to be at least 1.")}
  
  if(!is.numeric(minseglen.global)){stop("global minimum segment length must be numeric")}
  if(minseglen.global<2){stop("global minimum segment length needs to be at least 2 periods.")}
  
  if(isTRUE(circData)){
    data.input = data_circ(data,period.len,n)
  } else{
    data.input=data
  }
  
  if(method.global=="PELT"){
    out = pgcpt.PELT(data=data.input, period.len=period.len, minseglen.periodic=minseglen.periodic, 
                     minseglen.global=minseglen.global, method.periodic=method.periodic, 
                     max.periodic.cpts=max.periodic.cpts, penalty.periodic=penalty.periodic, 
                     penalty.global=penalty.global, dist=dist,restrict=restrict)
  } else{
    out = "haven't written code yet for any other method but PELT."
  }
  
  return(list(pgcpt.results=out,period.len=period.len,minseglen.periodic=minseglen.periodic,minseglen.global=minseglen.global,
              method.periodic=method.periodic,method.global=method.global,max.periodic.cpts=max.periodic.cpts,
              penalty.periodic=penalty.periodic,penalty.global=penalty.global,cost=cost,dist=dist,restrict=restrict))
}


# Periodic-global changepoint search method specifically using PELT
pgcpt.PELT = function(data, period.len=12, minseglen.periodic=1, minseglen.global=2*period.len,
                      method.periodic="SNcirc", max.periodic.cpts=5,
                      penalty.periodic=3, penalty.global=3*max.periodic.cpts*log(nrow(data)), cost="Likelihood",
                      dist="Normal meanvar", restrict=FALSE, ...
){
  if(is.character(penalty.periodic)){
    stop("Change penalty to numeric value. Haven't written code yet to handle types of penalties")
  }
  if(is.character(penalty.global)){
    stop("Change penalty to numeric value. Haven't written code yet to handle types of penalties")
  }
  
  dist.list = c("Normal mean","Normal meanvar","Bernoulli") #list of distributions that has been coded
  
  if(method.periodic=="SNcirc"){
    if(any(dist==dist.list)){
      if(restrict==TRUE){
        out = pgcpt.PELT.SNcirc.restrict(data,period.len,minseglen.periodic, minseglen.global,max.periodic.cpts, 
                                penalty.periodic, penalty.global, cost, dist)
      } else{
        out = pgcpt.PELT.SNcirc(data,period.len,minseglen.periodic, minseglen.global,max.periodic.cpts, 
                                penalty.periodic, penalty.global, cost, dist)
      }
    } else{
      stop("Haven't written code yet for any other distributions")
    }
  } else{
    stop("Haven't written code yet for any other periodic method apart from SNcirc")
  }
  
  return(out)
}




# PELT.SNcirc dist functions ----------------------------------------------


# PELT.SNcirc
pgcpt.PELT.SNcirc <- function(data,period.len=12,minseglen.periodic=1, minseglen.global=2*period.len,max.periodic.cpts=5, 
                              penalty.periodic=3, penalty.global=3*max.periodic.cpts*log(nrow(data)), cost="Likelihood",
                              dist="Normal meanvar"){
  # start of the PELT algorithm
  n = nrow(data)
  
  lastchangecpts = 0 # R index at 1 => time index 0. lastchangecpts[i] shows last cpt prior to time point i-1.
  lastchangelike = 0 # periodic-level likelihood
  cps = list() # micro-cpts prior to time index i (r index i+1)
  
  # Set up initialisation manually for tstar 1:((2*minseglen)-1)
  for(i in minseglen.global:(2*minseglen.global-1)){
    lastchangecpts[i+1] = 0
    result = sncirc(data[1:i,],max.cpts=max.periodic.cpts,pen.val=penalty.periodic*log(i),
                    period.len=period.len, minseglen=minseglen.periodic,dist=dist,pgcpt.method=TRUE)
    
    lastchangelike[i+1] = result$op.like + penalty.global # the likelihood, incl. pen, for data from 1:i, choosing the optimal number of cpts
    cps[[i+1]] = result # saving all results from sncirc so it can all be called back later
    
  }
  checklist=0
  
  for(tstar in (2*minseglen.global):n){
    tmplike <- NULL; tmpcp <- list();
    tmpt <- c(checklist, tstar-minseglen.global)
    for(i in 1:length(tmpt)){
      result = sncirc(data[(tmpt[i]+1):tstar,],max.cpts=max.periodic.cpts,pen.val=penalty.periodic*log(tstar-tmpt[i]),
                      period.len=period.len, minseglen=minseglen.periodic,dist=dist,pgcpt.method=TRUE)
      
      tmpcp[[i]] = result
      tmplike[i] = lastchangelike[tmpt[i]+1] + result$op.like + penalty.global #this penalty is penalising a cpt in the macro-level
    }
    lastchangelike[tstar+1] = min(tmplike,na.rm = TRUE) #full likelihood
    lastchangecpts[tstar+1] = tmpt[which.min(tmplike)]
    cps[[tstar+1]] = tmpcp[[which.min(tmplike)]]
    
    checklist = tmpt[tmplike <= lastchangelike[tstar+1]+penalty.global]
    
  }
  
  
  fcpt=NULL
  cpl = NULL
  cpp = list()
  count.seg=1
  
  last=n
  while(last!=0){ # macro cpts. cpp=micro cpts
    fcpt=c(fcpt,lastchangecpts[last+1])
    last.old = last
    last=lastchangecpts[last+1]
    for (i in 1:ceiling((last.old-last)/period.len)){
      tmp=last+(i-1)*period.len + as.numeric(cps[[last.old+1]]$op.cpt.loc) # getting the within-period cpts on the original scale - note that the start of the period is the across period cpt so it unlikely to be the same for each segment
      cpl = c(cpl,tmp[tmp<last.old] ) # for partial segments tmp may be larger than last.old
    }
    cpp[[count.seg]] = cps[[last.old+1]]
    count.seg=count.seg+1
  }
  # need to reverse cpp as it starts at the last segment and then goes to the first
  tmp=cpp
  for(i in 1:length(cpp)){
    cpp[[i]]=tmp[[length(cpp)-i+1]]
  }
  
  # Optimal periodic changepoints for each global segment
  cpp.opt = list()
  for(i in 1:length(cpp)){
    cpp.opt[[i]] = cpp[[i]]$op.cpt.loc
  }
  
  return(list(Global_cpt=sort(fcpt/period.len),Global_cpt_act=sort(fcpt),Periodic_cpt = cpp.opt, Periodic_cpt_act = sort(cpl), period.len=period.len,like.pen=lastchangelike[n+1],glob.segs.results=cpp))
}






# PELT.SNcirc - restricted to only iterate on the start of every period
pgcpt.PELT.SNcirc.restrict <- function(data,period.len=12,minseglen.periodic=1, minseglen.global=2*period.len,max.periodic.cpts=5, 
                              penalty.periodic=3, penalty.global=3*max.periodic.cpts*log(nrow(data)), cost="Likelihood",
                              dist="Normal meanvar"){
  # start of the PELT algorithm
  n = nrow(data)
  
  # to restrict:
  periods=rep(1:(nrow(data)/period.len +1),each=period.len)[1:nrow(data)] # indicates the fixed period no. each time index is in
  end.period=which(diff(periods)==1) # RK add. The (linear) time points which marks the end of a period.
  
  lastchangecpts = 0 # R index at 1 => time index 0. lastchangecpts[i] shows last cpt prior to time point i-1.
  lastchangelike = 0 # periodic-level likelihood
  cps = list() # micro-cpts prior to time index i (r index i+1)
  
  # Set up initialisation manually for tstar 1:((2*minseglen)-1)
  for(i in end.period[which(end.period>=minseglen.global & end.period<2*minseglen.global)]){
    lastchangecpts[i+1] = 0
    result = sncirc(data[1:i,],max.cpts=max.periodic.cpts,pen.val=penalty.periodic*log(i),
                    period.len=period.len, minseglen=minseglen.periodic,dist=dist,pgcpt.method=TRUE)
    
    lastchangelike[i+1] = result$op.like + penalty.global # the likelihood, incl. pen, for data from 1:i, choosing the optimal number of cpts
    cps[[i+1]] = result # saving all results from sncirc so it can all be called back later
    
  }
  checklist=0
  
  # for(tstar in c(start.period[start.period>=(2*minseglen)],n)){
  for(tstar in c(end.period[end.period>=(2*minseglen.global)],n)){
    tmplike <- NULL; tmpcp <- list();
    tmpt <- c(checklist, tstar-minseglen.global)
    for(i in 1:length(tmpt)){
      result = sncirc(data[(tmpt[i]+1):tstar,],max.cpts=max.periodic.cpts,pen.val=penalty.periodic*log(tstar-tmpt[i]),
                      period.len=period.len, minseglen=minseglen.periodic,dist=dist,pgcpt.method=TRUE)
      
      tmpcp[[i]] = result
      tmplike[i] = lastchangelike[tmpt[i]+1] + result$op.like + penalty.global #this penalty is penalising a cpt in the macro-level
    }
    lastchangelike[tstar+1] = min(tmplike,na.rm = TRUE) #full likelihood
    lastchangecpts[tstar+1] = tmpt[which.min(tmplike)]
    cps[[tstar+1]] = tmpcp[[which.min(tmplike)]]
    
    checklist = tmpt[tmplike <= lastchangelike[tstar+1]+penalty.global]
    
  }
  
  
  fcpt=NULL
  cpl = NULL
  cpp = list()
  count.seg=1
  
  last=n
  while(last!=0){ # macro cpts. cpp=micro cpts
    fcpt=c(fcpt,lastchangecpts[last+1])
    last.old = last
    last=lastchangecpts[last+1]
    for (i in 1:ceiling((last.old-last)/period.len)){
      tmp=last+(i-1)*period.len + as.numeric(cps[[last.old+1]]$op.cpt.loc) # getting the within-period cpts on the original scale - note that the start of the period is the across period cpt so it unlikely to be the same for each segment
      cpl = c(cpl,tmp[tmp<last.old] ) # for partial segments tmp may be larger than last.old
    }
    cpp[[count.seg]] = cps[[last.old+1]]
    count.seg=count.seg+1
  }
  # need to reverse cpp as it starts at the last segment and then goes to the first
  tmp=cpp
  for(i in 1:length(cpp)){
    cpp[[i]]=tmp[[length(cpp)-i+1]]
  }
  
  # Optimal periodic changepoints for each global segment
  cpp.opt = list()
  for(i in 1:length(cpp)){
    cpp.opt[[i]] = cpp[[i]]$op.cpt.loc
  }
  
  return(list(Global_cpt=sort(fcpt/period.len),Global_cpt_act=sort(fcpt),Periodic_cpt = cpp.opt, Periodic_cpt_act = sort(cpl), period.len=period.len,like.pen=lastchangelike[n+1],glob.segs.results=cpp))
}








