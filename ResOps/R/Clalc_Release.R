#' Calculate Reservoir release
#'
#' Function to calculate reservoir release given current storage and operating thresholds
#' Assumes that the dead pool storage and release values are the first in the release pctl and storage pctl vectors
#'
#'
#' @param release.th vector of release thresholds units, thesee can be calculated with \code{\link{monthlypctl.fromday}} [l3/t]
#' @param storage.th vector of storage thresholds, these can be calcualted with \code{\link{monthlypctl.fromday}}  [l3]
#' @param storage current storage [l3]
#' @param inflow current inflow, defalut is NA. (see inflow.th, this parameter is only used if inflow.th!=NA) [l3/t]
#' @param inflow.th the storage threshold above which inflow should be considered when calcualting outlfows
#'  NA value indicates that inflows are never included (default is NA)
#' @param stor.max maximum storage capacity [l3]
#' @param stor.min dead pool storage cpacity [l3]
#' @param dt timestep [t]
#'
#' @return The simulated outlfow rate from the reservoir [l3/t]

#' @examples
#' calc.release(release.th=c(0,50,75,90), storage.th=c(100,200, 600, 1000),  storage= 500, inflow=20, stor.max=1200, dt=0.1, )
#'
#'@seealso \code{\link{monthlypctl.fromday}}
#'
#' @export
#'
#'
calc.release=function(release.th, storage.th, storage, inflow=NA, inflow.th=NA, stor.max, dt=1){
  bin=findInterval(storage, storage.th)

  #find the storage bin above which inflow should be taken into account
  if(is.na(inflow.th)==F){inflow.bin=findInterval(inflow.th,storage.th)}

  # If storage is less than dead pool storage then release at the dead pool release rate (usulaly 0)
  if(bin==0){
    print("minimum release")
    outflow=release.th[1]
  # if storage is > dead pool and less than the max trehshold value then linearly interpolate to get the flow
  } else if (bin>0 && bin<length(storage.th)){
    outflow=release.th[bin] + (storage-storage.th[bin])/(storage.th[bin+1]-storage.th[bin]) * (release.th[bin+1]-release.th[bin])
    print("linear interpolating")

    #if you are above the threhshold where inflow influences outflow then do this instead
    if(is.na(inflow.th)==F & bin>inflow.bin){
      outflow=release.th[bin] + (storage-storage.th[bin])/(storage.th[bin+1]-storage.th[bin]) *
               max((inflow-release.th[bin]), (release.th[bin+1]-release.th[bin]))
      print("using inflows")
    }

  # if storage is greater than top threshold then release either the (1) upper release theshold,
  # or (2) the total storage volume above the upper storage threshold
  } else{
    outflow= max(release.th[bin], (storage-storage.th[bin])/dt)
    print("Max releases")

  }

  return(outflow)
}
