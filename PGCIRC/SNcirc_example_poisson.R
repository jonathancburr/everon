# Example code to run Poisson data

# source the SNcirc file
source("~/SNcirc.R")

# Manually creating periodic poisson data
nperiods = 10
N = 96 # length of a period
p.cpts = c(8,24,seq(40,96,by=12),N) # periodic cpts
set.seed(100)
lambdas = sample(seq(1,15,0.5),length(p.cpts),replace=TRUE) # randomly pick some Poisson parameters
rbind(p.cpts,lambdas)
p.cpts=c(0,p.cpts) # add 0 to the cpts list
# now create the data
set.seed(200)
dat = NULL
for(period.no in 1:nperiods){
  # data for the day
  dat.period = NULL
  for(cpt.no in 2:length(p.cpts)){
    dat.period = c(dat.period, rpois(n=p.cpts[cpt.no]-p.cpts[cpt.no-1], lambda=lambdas[cpt.no-1]))
  }
  # add this day's data to the overall data
  dat = c(dat, dat.period)
}
# View(dat)

# Plot the data
withinPeriod_plot(data=dat, period.len=N, test.stats="Poisson",title="Periodic Poisson example")
abline(v=p.cpts,col='blue')
rbind(p.cpts[-1],lambdas) # true values

# Run SNcirc on this data, and plot the optimal changepoints
results = sncirc(dat,period.len=N,dist="Poisson",max.cpts=15,pen.val=3*log(length(dat)))
results$sncirc.results$op.cpt.loc # estimated optimal cpts
rbind(p.cpts[-1],lambdas) # true values
abline(v=results$sncirc.results$op.cpt.loc, col='red',lty=2)








