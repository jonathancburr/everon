# An example of how to generate a periodic changepoint structure time series and how to run the SNcirc algorithm

### sources
source('sncirc/R/PGcpt-general.R')
source('sncirc/R/pg_data_gen.R')
source('sncirc/R/SN-circ-general.R')
source('PGCircRoutine.R')

# Create a periodic changepoint time series (with stable periodicity)
period.len=96 # number of observations within one fixed period. 24 => hourly data for daily patterns
per.cpts = list(c(28,88))
params = list()
params[[1]] = list(mean=c(2,30),var=c(1,3)) # if dist=Bernoulli, no need to include the var argument
n = period.len*14 # total number of observations. 14 periods of data, i.e. 2 weeks
set.seed(283040)
dat = pgcptSim(period=period.len,dist="Normal",glob.cpts=c(0,n),per.cpts=per.cpts,params=params,reps=1)

period.len=96 # number of observations within one fixed period. 24 => hourly data for daily patterns
per.cpts = list(c(32,90))
params = list()
params[[1]] = list(mean=c(2,40),var=c(1,3)) # if dist=Bernoulli, no need to include the var argument
n = period.len*14 # total number of observations. 14 periods of data, i.e. 2 weeks
dat = rbind(dat, pgcptSim(period=period.len,dist="Normal",glob.cpts=c(0,n),per.cpts=per.cpts,params=params,reps=1))


# Plot the data
plot(dat,pch=16) # linear time plot
withinPeriod_plot(dat,period.len=period.len,test.stats="Normal meanvar",circData=TRUE) # circular time plot

# Run SNcirc
result = sncirc(dat, period.len,dist="Normal meanvar",max.cpts=5,minseglen=1,pen.val=3*log(length(dat)))
result$op.cpt.loc
## Plot the data and the identified periodic changepoints
withinPeriod_plot(dat,period.len=period.len,test.stats="Normal meanvar",circData=TRUE) # circular time plot
abline(v=result$op.cpt.loc,col='blue') # plot the optimal changepoint locations onto the circular data

# Extract specific number of changepoints
m=5 # no. of changepoints
result$cps[,m]
withinPeriod_plot(dat,period.len=period.len,test.stats="Normal meanvar",circData=TRUE)
abline(v=result$cps[,m],col='blue')

dfdat <- data.frame(activity = dat)
dc <- PGCirc::data_circ(data = dfdat$activity, period.len = 96)
results <- PGCirc::pgcpt(data = dc,period.len = 96,minseglen.periodic = 8, minseglen.global = 7*96, method.periodic = 'SNcirc', max.periodic.cpts = 4,
                method.global = 'PELT',dist = 'Normal meanvar',restrict = TRUE, circData = FALSE)


dfdat <- 
  dfdat |>
  dplyr::mutate(rown = dplyr::row_number()) |>
  dplyr::mutate(global_segment = ifelse(rown %in% (results$Global_cpt_act+1),1,0)) |>
  dplyr::mutate(global_segment = cumsum(global_segment)) |>
  dplyr::mutate(periodic_segment = ifelse(rown %in% (c(results$Periodic_cpt_act+1)),1,NA)) |>
  dplyr::mutate(day = (rown %/% 96) +1) |>
  dplyr::group_by(day) |>
  dplyr::mutate(periodic_segment = dplyr::row_number(periodic_segment)) |>
  dplyr::group_by(global_segment) |>
  dplyr::mutate(periodic_segment = zoo::na.locf(periodic_segment, na.rm = FALSE)) |>
  dplyr::mutate(periodic_segment = ifelse(is.na(periodic_segment), max(periodic_segment, na.rm = TRUE), periodic_segment)) 

dfdat |>
  dplyr::group_by(global_segment, periodic_segment) |>
  dplyr::summarise(activity = mean(activity))

  

