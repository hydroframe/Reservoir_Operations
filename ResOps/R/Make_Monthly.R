#' Make Monthly Matrix
#'
#'Function to aggregate daily timeseries to monthly values. Aggregation opptions are (1) average, (2) sum,
#'(3) first value of the month, (4) last value of the month. Required inputs are vectors of dates and daily values that
#'will be converted. The function returns a times series of monthly values and a matrix with one column per month.

#' @inheritParams make.xts
#' @param  fun The name of the function to use for aggreagating values. Choices are mean, sum, first and last. Mean and sum return the average and total values for the month
#' while first and last return the values on the first an last days of the month respectively. Default is mean.

#'
#' @return
#' \enumerate{
#' \item mat: A matrix with 13 columns, column 1 is the year and columns 2-13 are the monthly aggregated values
#' \item ts: A xts time series of monthly values column1 is the date, column 2 is the monthly aggregated value
#'}
#'
#' @examples
#' make.monthly(dates=datevec, values=atributevec, fun="mean", na.val=-999)
#'
#' @export
#'

make.monthly=function(dates, values, fun, na.val=NA){
  #check that the dates and values have the same length
  if(length(dates)!=length(values)){stop("Dates and Values must have the same length")}

  #if a na value other than NA is used then replace all of these with NA's
  if(is.na(na.val)==F){
    print(paste("Replacing", na.val, "with NAs"))
    ilist=which(values==na.val)
    values[ilist]=NA
  }

  TimeSeries=xts::xts(values, as.Date(dates))
  TimeSeries=na.omit(TimeSeries)

  if(fun=="mean"){
    print("Calculating Monthly Average")
    TimeSeriesM=apply.monthly(TimeSeries, FUN=mean, drop.time=T, indexAt='firstof')

  }else if(fun=="sum"){
    print("Calculating Monthly Total")
    TimeSeriesM=xts::apply.monthly(TimeSeries, FUN=sum, drop.time=T)
   # TimeSeriesM2=xts::to.monthly(TimeSeries, FUN=sum, drop.time=T)

  } else if(fun=="first"){
    print("Calculating first value of the month")
    #starts=xts::endpoints(TimeSeries, on="months", k=1)+1
    #starts=starts[-length(starts)]
    #TimeSeriesM=TimeSeries[starts]
    TimeSeriesM=xts::to.monthly(TimeSeries, FUN=sum, drop.time=T, indexAt='firstof')[,1]

  } else if(fun=="last"){
    print("Calculating last value of the month")
    #ends=xts::endpoints(TimeSeries, on="months", k=1)
    #ends=ends[-1]
    #TimeSeriesM=TimeSeries[ends]
    TimeSeriesM=xts::to.monthly(TimeSeries, FUN=sum, drop.time=T, indexAt='lastof')[,4]

  } else{
    stop("Must select aggregation type. Choices are: mean, sum, first or last")
  }


  #make a matrix of the monthly values
  starty = year(start(TimeSeriesM))
  startm = month(start(TimeSeriesM))

  endy = year(end(TimeSeriesM))
  endm = month(end(TimeSeriesM))

  nyear=endy-starty+1
  MatrixM=matrix(NA, ncol=13, nrow=nyear)
  for(y in 1:nyear){
    ytemp=starty+y-1
    MatrixM[y,1]=ytemp

    for(m in 1:12){
      tempval=TimeSeriesM[paste(ytemp, m, sep="-")]
      if(length(tempval>0)){MatrixM[y,(m+1)]=tempval}
    }
  }

  #setup the outputs
  output_list=list("ts"=TimeSeriesM, "mat"=MatrixM)
  return(output_list)

} # end function
