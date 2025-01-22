PGCircRoutine <-
  function(data,
           tz = 'Europe/London',
           bins = 96,
           max.days = 90,
           max.date = Sys.Date()-1,
           minseglen.periodic = 20,
           minseglen.global.days = 7,
           penalty.periodic=1,
           pen.global.mult=1,
           max.periodic.cpts = 4,
           wt.confidence = .99,
           nightat='03:00',
           plotdays=90,
           title='',
           subtitle='',
           caption = TRUE)
{

if(nrow(data)==0){
  summary <-
    data.frame('days'=NA,
               'firstdate' = NA,
               'lastdate' = NA,
               'max.days'=max.days,
               'max.date'=max.date,
               'minseglen.periodic'=minseglen.periodic,
               'minseglen.global'=minseglen.global.days*7,
               'max.periodic.cpts'=max.periodic.cpts,
               'wt.confidence' = wt.confidence,
               'penalty.periodic' = penalty.periodic,
               'nightat' = nightat,
               'cpts'=NA,
               'nightstart'= NA,
               'nightend'= NA,
               'night.wt'= NA,
               'day.wt'= NA,
               'transition.wt'= NA,
               'daystart'=NA,
               'last.change'= NA,
               'n.sensors'= NA)
  
  g <- ggplot2::ggplot()+ggplot2::labs(title='No data')
  gthumb <- ggplot2::ggplot()+ggplot2::labs(title='No data')

  } else {  
    
options(dplyr.summarise.inform = FALSE)
binsize <- 86400/bins

night.start.setting <-
  as.POSIXct('1970-01-01 00:00',tz='GMT')

night.end.setting <-
  as.POSIXct('1970-01-01 06:00',tz='GMT')

events <-
    data |>
    dplyr::select(datecreated) |>
    dplyr::mutate(tbin=datecreated %/% binsize *binsize) |>
    dplyr::select(tbin) |>
    unique() |>
    dplyr::mutate(tbin=as.POSIXct(tbin,origin='1970-01-01',tz=tz)) |>
    dplyr::mutate(bin=howzfunc::dechour(as.POSIXct(tbin,origin='1970-01-01'))/24*bins+1) |>
    dplyr::mutate(date=as.Date(substr(tbin,1,10))) |>
    dplyr::mutate(active=as.integer(1)) 

min.date <-
  max.date - max.days

events <-
    events |>
    dplyr::filter(date >= min.date & date <=max.date)

n.sensors <- length(unique(data$sensor_name))

all.bins <-
    data.frame(date=seq.Date(from = min(events$date),to=max(events$date),by = 1)) |>
    dplyr::inner_join(data.frame(bin=seq(1,bins,1)),by=character())

events <-
    events |>
    dplyr::right_join(all.bins,by=c('bin','date')) |>
    dplyr::mutate(active=ifelse(is.na(active),as.integer(0),active)) |>
    dplyr::arrange(date,bin) |>
    #dplyr::select(-tbin) |>
    dplyr::ungroup() |>
    dplyr::mutate(bin=ifelse(bin>bins/2,bin-bins,bin)) |>
    dplyr::mutate(bin=as.POSIXct((bin-1)*binsize,origin='1970-01-01',tz='UTC'))

ndays <- length(unique(events$date))

minseglen.global.days <- min(minseglen.global.days,ndays)
minseglen.global <- minseglen.global.days * bins  

  
eventsa <-
        events

dat = data_circ(eventsa$active,bins)

      results = pgcpt(dat, period.len=bins, minseglen.periodic=minseglen.periodic, minseglen.global=minseglen.global,
                      method.periodic="SNcirc", method.global="PELT", max.periodic.cpts=max.periodic.cpts,penalty.periodic=penalty.periodic,
                      penalty.global=pen.global.mult*log(nrow(dat)),dist="Bernoulli",restrict=TRUE,circData=FALSE)

      start.date <-
        min(eventsa$date)

      cpt.starts <-
        start.date + c(results$pgcpt.results$Global_cpt_ac)/bins
      cpt.ends <-
        start.date + c(results$pgcpt.results$Global_cpt_ac[-1],length(eventsa$bin))/bins

      cpts <-
        data.frame(x=as.Date(as.character()),
                   y=as.integer(),
                   xend=as.Date(as.character()),
                   yend=as.integer())
      for (i in 1:length(cpt.starts)){
        cpts <-
          cpts |>
          rbind(data.frame(
            x=cpt.starts[i],
            y=as.integer(results$pgcpt.results$Periodic_cpt[[i]]),
            xend=cpt.ends[i],
            yend=as.integer(results$pgcpt.results$Periodic_cpt[[i]])
          ))
      }

      cpts <-
        cpts |>
        dplyr::mutate(y=ifelse(y>bins/2,y-bins,y)) |>
        dplyr::mutate(y=as.POSIXct((y-1)*binsize,origin='1970-01-01',tz='UTC')) |>
        dplyr::mutate(yend=y)

      complete.segment.days <-
        as.integer(tail(cpt.starts,1)-head(cpt.starts,1))+1
      n.cpts <- length(cpt.starts)-1

      startdate <- ifelse(nightat<='12:00','1970-01-01','1969-12-31')
      reftime <- as.POSIXct(paste(startdate,nightat),tz='UTC')
      global.starts <- unique(cpts$x)

      cpts <-
        cpts |>
        dplyr::mutate(type='normal')

      for (g in global.starts){
        cptsa <-
          cpts |>
          dplyr::filter(x==g)

        for (x in 1:nrow(cptsa)){
          a<-cptsa[x,'y']>=reftime
          if(a){
            break
          }
        }

        if (x==nrow(cptsa) & !a) {
          start <- cptsa[x,'y']
          end <- cptsa[1,'y']
        } else if (x==1) {
          start <- cptsa[nrow(cptsa),'y']
          end <- cptsa[1,'y']
        } else {
          start <- cptsa[x-1,'y']
          end <- cptsa[x,'y']
        }

        cpts <-
          cpts |>
          dplyr::mutate(type=ifelse(y %in% start & g==x,'nightstart',type)) |>
          dplyr::mutate(type=ifelse(y %in% end & g==x,'nightend',type))
      }

      eventsa <-
        cpts |>
        dplyr::group_by(x)|>
        dplyr::summarise() |>
        dplyr::rename(date=x) |>
        dplyr::mutate(global=date) |>
        dplyr::right_join(eventsa,by=c('date')) |>
        dplyr::arrange(date,bin) |>
        dplyr::mutate(global=zoo::na.locf(global)) |>
        dplyr::ungroup()

      eventsa <-
        cpts |>
        dplyr::select(x,y,type) |>
        dplyr::mutate(period=y) |>
        dplyr::rename(global=x,bin=y) |>
        dplyr::right_join(eventsa,by=c('global','bin')) |>
        dplyr::arrange(date,bin) |>
        dplyr::mutate(period=zoo::na.locf(period,na.rm=FALSE)) |>
        dplyr::mutate(period=zoo::na.locf(period,na.rm=FALSE,fromLast=TRUE)) |>
        dplyr::mutate(type=zoo::na.locf(type,na.rm=FALSE)) |>
        dplyr::mutate(tod = ifelse(type == 'nightstart','Night','Day')) |>
        dplyr::filter(! is.na(tod)) |>
        #na.omit() |> 
        #dplyr::group_by(global) |> dplyr::mutate(psegment = dplyr::dense_rank(period)) |>
        dplyr::group_by(global,tod) |>
        dplyr::mutate(pactive=mean(active))

      waittimes <-
        eventsa |>
        dplyr::group_by(global,type) |>
        dplyr::summarise(r=sum(active)/dplyr::n()) |>
        dplyr::mutate(wt=ceiling(qexp(wt.confidence,r))/4) |>
        dplyr::mutate(period=ifelse(type=='nightend','first day',type)) |>
        dplyr::mutate(period=ifelse(type=='nightstart','night',period)) |>
        dplyr::mutate(period=ifelse(type=='normal','day',period)) |>
        dplyr::ungroup() |>
        dplyr::mutate(tod = ifelse(period=='night','night','day')) |>
        dplyr::group_by(global,tod) |>
        dplyr::mutate(min.wt = min (wt)) |>
        dplyr::ungroup()
      
      routine.waittimes <-  # reformat so as to return results with routine table
        waittimes |>
        dplyr::mutate(tod = paste0(tod,'.waittime')) |>
        dplyr::group_by(global,tod,min.wt) |>
        dplyr::summarise() |>
        tidyr::spread(key=tod,value=min.wt) |>
        dplyr::mutate(global=format(global,format='%d/%m/%Y')) 
      
      night.wt <-   #the wt for the most recent global segment 
        waittimes |>
        dplyr::filter(global==max(global)) |>
        dplyr::filter(tod == 'night') |>
        dplyr::select(min.wt) |>
        as.numeric()

      day.wt <-     #the wt for the most recent global segment (section can be refactored)
        waittimes |>
        dplyr::group_by(global,tod,min.wt) |>
        dplyr::summarise() |>
        dplyr::ungroup() |>
        dplyr::filter(global==max(global)) |>
        dplyr::filter(tod == 'day') |> 
        dplyr::select(min.wt) |>
        as.numeric()

      transition.wt <-
        waittimes |>
        dplyr::filter(global==max(global)) |>
        dplyr::filter(period=='first day') |>
        dplyr::select(wt) |>
        as.numeric()

      nightstart <-
        cpts |>
        dplyr::filter(type=='nightstart') |>
        dplyr::top_n(n=1,wt = x) |>
        dplyr::select(y) |>
        dplyr::mutate(y=howzfunc::dechour(y)) |>
        as.numeric()

      nightend <-
        cpts |>
        dplyr::filter(type=='nightend') |>
        dplyr::top_n(n=1,wt = x) |>
        dplyr::select(y) |>
        dplyr::mutate(y=howzfunc::dechour(y)) |>
        as.numeric()
      
      activity <-
        eventsa |>
        dplyr::mutate(tod = ifelse(type == 'nightstart','night','day')) |>
        dplyr::group_by(global,tod) |>
        dplyr::summarise(active=mean(active)) |>
        dplyr::arrange(tod,global) |>
        dplyr::group_by(tod) |>
        dplyr::mutate(change=(active/dplyr::lag(active)-1)) |>
        tidyr::pivot_wider(names_from = c('tod'),values_from = c('active','change')) |>
        dplyr::mutate(days=as.integer(dplyr::lead(global)-global)) |>
        dplyr::relocate(days,.after = global) |>
        dplyr::mutate(global=format(global,format='%d/%m/%Y')) |>
        dplyr::mutate(ratio = round(active_day / active_night,1))
      
      routine <-
        eventsa |>
        dplyr::filter(type == 'nightstart' | type == 'nightend') |>
        dplyr::group_by(global,type) |>
        dplyr::summarise(start=min(period)) |>
        dplyr::arrange(type,global) |>
        dplyr::group_by(type) |>
        dplyr::mutate(change=difftime(start,dplyr::lag(start),units = 'hours')) |>
        tidyr::pivot_wider(names_from = c('type'),values_from = c('start','change')) |>
        dplyr::mutate(nightlength=difftime(start_nightend,start_nightstart,units='hours')) |>
        dplyr::mutate(change_nightlength=nightlength-dplyr::lag(nightlength)) |>
        dplyr::mutate(days=as.integer(dplyr::lead(global)-global)) |>
        dplyr::select(global,days,start_nightstart,start_nightend,nightlength,change_nightstart,change_nightend,change_nightlength) |>
        dplyr::mutate(global=format(global,format='%d/%m/%Y')) |>
        dplyr::mutate(across(3:4, ~ format(.x,format='%H:%M'))) |>
        dplyr::mutate(across(5:8, ~ as.numeric(.x))) |>
        dplyr::left_join(routine.waittimes,by='global')
      
      discharge.line <-
        ifelse((max(eventsa$date)-min(eventsa$date)) < 49, min(eventsa$date) + 42, NA)

      summary <-
        data.frame('days'=ndays,
                   'firstdate' = min(events$date),
                   'lastdate' = max(events$date),
                   'max.days'=max.days,
                   'minseglen.periodic'=minseglen.periodic,
                   'minseglen.global'=minseglen.global,
                   'max.periodic.cpts'=max.periodic.cpts,
                   'wt.confidence' = wt.confidence,
                   'nightat' = nightat, 
                   'cpts'=n.cpts,
                   'nightstart'= nightstart,
                   'nightend'= nightend,
                   'night.wt'= night.wt,
                   'day.wt'= day.wt,
                   'transition.wt'= transition.wt
        ) |>
        dplyr::mutate('daystart'=nightend+day.wt) |>        #dplyr::mutate('daystart'=nightend+transition.wt)
        dplyr::mutate('last.change'=max(eventsa$global)) |>
        dplyr::mutate('n.sensors' = n.sensors)


      pcaption <- ifelse(caption,
        paste0('period.len=',bins,
               ' minseglen.global=',minseglen.global,
               ' minseglen.periodic=',minseglen.periodic,
               ' max.periodic.cpts=',max.periodic.cpts,
               ' night at=',nightat,
               ' wait time confidence=',wt.confidence),
        '')

      g <-
        eventsa |>
        dplyr::mutate(date=ifelse(bin>='1970-01-01 00:00:00',date-1,date)) |>
        dplyr::mutate(date=as.Date(date)) |>
        dplyr::filter(active==1) |>
        dplyr::mutate(period=as.factor(format(period,'%H:%M'))) |>
        ggplot2::ggplot()+
        ggplot2::geom_point(ggplot2::aes(date,bin))+ #color=pactive*100
        ggplot2::geom_vline(xintercept = cpt.ends-1,color='gray6')+ # to line up with plotting convention
        ggplot2::geom_segment(data=dplyr::filter(cpts,type=='nightstart'),ggplot2::aes(x=x-1,y=y,xend=xend-1,yend=yend),color='green',linetype='solid')+
        ggplot2::geom_segment(data=dplyr::filter(cpts,type=='nightend'),ggplot2::aes(x=x-1,y=y,xend=xend-1,yend=yend),color='red',linetype='solid')+
        ggplot2::geom_segment(data=dplyr::filter(cpts,type=='normal'),ggplot2::aes(x=x-1,y=y,xend=xend-1,yend=yend),color='gray6',linetype='solid')+
        ggplot2::geom_vline(xintercept = discharge.line,linetype='dashed')+
        ggplot2::labs(title=title,subtitle=subtitle,x='',y='Direction of time >>>',caption=pcaption)+
        ggplot2::scale_y_datetime(date_breaks = '2 hour',date_labels = '%H:%M',timezone = 'GMT')+
        ggplot2::scale_x_date(breaks = seq.Date(min(eventsa$date)-1,min(eventsa$date)+plotdays,by = 7),
                              limits = c(min(eventsa$date)-1,min(eventsa$date)+plotdays),name='Date from noon',labels = scales::date_format("%d %b"),
                              sec.axis = ggplot2::sec_axis(~ .+1,name='Date from midnight',labels = scales::date_format("%d %b"),
                                                           breaks = seq.Date(min(eventsa$date),min(eventsa$date)+plotdays,by = 7)))+                                                                                                                            
        ggplot2::theme_bw()+
        ggplot2::scale_color_gradient(low = "grey63", high = "black", na.value = NA,name = "% Time Active")+
        ggplot2::theme(legend.position="bottom")

      gthumb <-
        eventsa |>
        dplyr::filter(active==1) |>
        ggplot2::ggplot()+
        ggplot2::geom_point(ggplot2::aes(date,bin),alpha=0.25,size=.005)+
        ggplot2::geom_vline(xintercept = cpt.ends,color='red',linewidth=.1)+
        ggplot2::geom_segment(data=cpts,ggplot2::aes(x=x,y=y,xend=xend,yend=yend),color='red',linewidth=.1)+
        ggplot2::labs(x='',y='')+
        ggplot2::scale_y_datetime(date_breaks = '2 hour',date_labels = '%H:%M',timezone = 'GMT')+
        ggplot2::scale_x_date(date_breaks = "1 month",date_labels = '%d-%m-%y',limits = c(max(cpts$xend)-90,max(cpts$xend)))+
        ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                       axis.text.x=ggplot2::element_blank(),
                       axis.ticks.x=ggplot2::element_blank())+
        ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                       axis.text.y=ggplot2::element_blank(),
                       axis.ticks.y=ggplot2::element_blank())+
        ggplot2::theme(plot.margin = ggplot2::margin(-0,0,0,0, "pt"))

}
    return=list('summary'=summary,'plot'=g,'thumb'=gthumb,'events'=eventsa,'activity'=activity,'routines'=routine)
    }

