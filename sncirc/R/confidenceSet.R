# Code to get a confidence set from SNcirc
# 21/09/22: This code outputs, for a given m, the optimal cpt locations for all
source("SN-circ-general.R")

confidenceSet <- function(sncirc.results,threshold.method="quantile",threshold.quantile.val=0.05){
  # like.M,threshold,cp,cps,period.len,op.k - extract these arguments
  like.M = sncirc.results$like.M; cp=sncirc.results$cp; cps=sncirc.results$cps; period.len=sncirc.results$period.len; op.k=sncirc.results$op.k; pen.val=sncirc.results$pen.val;
  # threshold should be a probability, e.g. you want the top 5% of likelihoods so threshold=0.05
  
  # will need to extract the certain likes and k to focus on for a given m
  M = dim(like.M)[1] # max no. of cpts
  
  cps.M.CS = list()
  for(m in 2:M){
    likes.m = NULL # all the full likelihoods for this specific m
    for(k in 1:period.len){
      wrapAround=(k-1+period.len)%%period.len
      if(wrapAround==0){wrapAround=period.len}
      likes.m=c(likes.m,like.M[m,wrapAround,k])
    }
    if(threshold.method=="quantile"){
      threshold.value=quantile(likes.m,prob=1-threshold.quantile.val)
    } else if(threshold.method=="penalty"){
      threshold.value=max(likes.m)-(pen.val/2)
    }
    # threshold.value = penalty
    k.close = which(likes.m>=threshold.value) # this gives the kvals for the top threshold% of likelihoods (we are maximising here)
    k.close = k.close[-which(k.close==op.k[m])]
    # does not include the "optimal" starting position for this m as found by SNcirc, so we don't recalculate the op.cpt.loc
    # maybe I should get rid of all op.cpt.locs and not just the op.k?
    
    # now to find the optimal changepoint locations for each kval in k.close
    
    CSmat = matrix(NA,nrow=length(k.close),ncol=m) # confidence set matrix. 
    # fix a kval on a row, cpts are added along the column
    first.cpts = NULL
    for(k in 1:length(k.close)){
      # cps.M=matrix(NA,ncol=m,nrow=m) #goes back and finds the optimal cpt locations for each m
      # k=start of a segment. So "first" changepoint is at k-1.
      kval = k.close[k]
      f = (kval-1)%%period.len
      if(f==0){f=period.len}
      first.cpts=c(first.cpts,f)
      CSmat[k,1] = f
      for(i in 1:(m-1)){
        CSmat[k,(i+1)] = cp[m-i+1,CSmat[k,i],kval]
      }
    }
    CSmat = apply(CSmat,1,sort) # rows are the cpt.loc (nrow=m). columns are the "different" changepoint vectors
    
    cpt.loc.range = c(CSmat) # allowing repeated vals just to highlight how often a certain cpt gets picked
    Conf.set = as.list(cps[,m][!is.na(cps[,m])]) # first value in each element is the op.cpts for m
    op.cpts.circ = cps[,m][!is.na(cps[,m])]
    for(t in 1:length(cpt.loc.range)){
      # calculate the absolute distances, adjust for circular distance, minimise to find closeset op.cpt.loc 
      abs.dist = abs(cpt.loc.range[t]-op.cpts.circ)
      abs.dist[which(abs.dist>period.len/2)] = abs(abs.dist[which(abs.dist>period.len/2)]-period.len)
      closest.op.cpt = which.min(abs.dist)
      
      # closest.op.cpt = which.min(abs(cpt.loc.range[t]-op.cpts.circ))
      # if(closest.op.cpt==1){
      #   closest.op.cpt = length(op.cpts.circ)
      # } else{
      #   closest.op.cpt=closest.op.cpt-1 # needs to shift it back by 1 as I added the wrapAround cpt at the start of op.cpts.circ
      # }
      
      
      Conf.set[[closest.op.cpt]] = c(Conf.set[[closest.op.cpt]],cpt.loc.range[t])
    }
    
    cps.M.CS[[m]] = list(CSmat=CSmat,first.cpts=first.cpts,likes.m.close=likes.m[k.close],likes.m=likes.m,Conf.set=Conf.set,threshold.value=threshold.value)
  }
  
  return(cps.M.CS)
}









