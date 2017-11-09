timeSliders <- function(datetime){
  
  # Sliders Parameters for aggregation stage
  # input
  # datatime - initial timestamp vector in POSIX fortmat
  # output
  # sliders - list of sliders parameters:
  #           aggregation type (seconds, minutes, hours, days, month, years):
  #           start - minimal value of slider
  #           end - maximal value of slider
  # sliders$<aggregation type>$start
  # sliders$<aggregation type>$end
  
  d1=abs(difftime(datetime[1:(length(datetime)-1)], 
                  datetime[2:(length(datetime))], units = "secs"))
  
  d2=difftime(datetime[length(datetime)], datetime[1], units = "secs")
  d1=as.numeric(d1)
  d2=as.numeric(d2)
  d_min=min(d1)
  lag=d2
  
  start_val=c(seconds=d_min/1, 
              minutes=max(floor(d_min/60),1), 
              hours=max(floor(d_min/3600),1), 
              days=max(floor(d_min/86400),1), 
              months=max(floor(d_min/(86400*31)),1), 
              years=max(floor(d_min/(86400*365)),1))
  
  
  max_lag= c(seconds=min(floor(lag/2),60), 
             minutes=min(floor(max(lag/120,1)),60), 
             hours=min(floor(max(lag/3600/2,1)),24), 
             days=min(floor(max(lag/86400/2,1)),31), 
             months=min(floor(max(lag/(86400*31)/2,1)),12), 
             years=floor(max(lag/(86400*365)/2,1)))
  
  ind=which(start_val < max_lag)
  start_val=start_val[ind]
  max_lag=max_lag[ind]
  
  sl_names = names(start_val)
  sliders = list()
  for (name in sl_names)
    sliders[[name]]=c(start = as.numeric(start_val[name]), end = as.numeric(max_lag[name]))

  return(sliders)
}
