#' Get monthly Percentiles
#'
#'Calculates the monthly percentiles for a matrix of monthly values give a range of years

#' @param  monthly.data a matrix of values where the first column is the year and the next 12 columns are monthly values for that year.
#' @param  percentile.list A vector of the percentile values to be calcualted.
#' @param  year.range The range of years from the original input data that should be used for calculation (firstyear, last year).
#' NOTE: If the beginning of the range is before the first year of the data it will default to starting with the first year of data and similarly if the
#' last year is after the end of the data it will default to the last year available.
#' @return a matrix of the percentile values with 12 columns (one for each month) and a row for each percentile value given in percentile.list
#'
#' @examples
#' get.monthlypctl(monthlydata=monthmatrix, percentile.list=c(0.25, 0.5, 0.75), year.rang3=c(1950, 2000))
#'
#' @export
#'

get.monthlypctl=function(monthlydata, percentile.list, year.range){
  #get starting and ending years
  year.start=year.range[1]
  year.end=year.range[2]

  #find the rows these correspond to in the monthly mat
  istart = which(monthlydata[,1]==year.start)
  iend = which(monthlydata[,1]==year.end)

  #Adjust the start/end dates if necessary given the range of the data
  if(length(istart)==0){
    print(paste("WARNING: Starting year is before the start of the dataset, changing start year to ", monthlydata[1,1]))
    istart=1
  }

  if(length(iend)==0){
    print(paste("WARNING: Ending year is after the end of the dataset, changing end year to ", monthlydata[nrow(monthlydata),1]))
    iend=nrow(monthlydata)
  }

  #clip the data for the year range of interest
  dataclip=monthlydata[istart:iend,]
  #calculate the percentiles
  pctl_mat=apply(dataclip[,-1],2,quantile, percentile.list, na.rm=T)

  return(pctl_mat)
} # end function

