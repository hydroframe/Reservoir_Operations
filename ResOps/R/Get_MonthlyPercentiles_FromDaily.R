#' Get monthly Percentiles from daily
#'
#'Calculates the monthly percentiles of flow from a time series of daily values
#'
#' @inheritParams get.monthlypctl
#' @param timeseries a time series of daily values to calcualte percentiles from. This must be formatted as an xts time series. See \code{\link{make.xts}}.
#' @return a matrix of the percentile values with 12 columns (one for each month) and a row for each percentile value given in percentile.list
#'
#' @examples
#' get.monthlypctl(monthlydata=monthmatrix, percentile.list=c(0.25, 0.5, 0.75), year.rang3=c(1950, 2000))
#'
#'@seealso \code{\link{make.xts}}
#'
#' @export
#'

monthlypctl.fromday=function(timeseries, percentile.list, start="1940-01-01", stop="1950-03-01"){
  timeseries.clip=timeseries[paste(start,stop, sep="/")]
  months=month(timeseries.clip)
  pctl_mat=matrix(NA, nrow=length(percentile.list), ncol=12)

  for(m in 1:12){
    mlist=which(months==m)
    pctl_mat[,m]=quantile(timeseries[mlist], percentile.list)
  }

  return(pctl_mat)
} # end function
