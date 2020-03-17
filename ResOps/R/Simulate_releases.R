#' Simulate daily reservoir releases
#'
#' Simulate releases given and operating  policy
#'
#'
#' @param release.pctl a matrix of release thresholds
#' with 12 columns (one for each month) and a row for each percentile threhold
#' (see \code{\link{monthlypctl.fromday}} [l3/t])
#' @param stor.pctl a matrix of storage thresholds a matrix of the percentile values
#' with 12 columns (one for each month) nd a row for each percentile threhold
#' (see \code{\link{monthlypctl.fromday}} [l3])
#' @param inflow.ts an xts timeseries of inflows (see \code{\link{make.xts}},
#' note that NA's should be filled so that there are no missing days in the inflows) [l3/t]
#' @param stor.init an initial storage value for the reservoir [l3]
#' @param stor.max the maximum storage capacity of the reservor [l3]
#' @param stor.min the minimum usable storage capacity of the reservoir (i.e. the dead pool storage) [l3]
#' @param sim.start date to start simulation on (should be a string "yyyy-mm-dd")
#' (NOTE - if this is before the start date of the inflow time series simulation will by adjusted to start at the start of the inflows)
#' @param sim.stop date to end simulation on (should be a string "yyyy-mm-dd")
#' (NOTE - if this is after the end date of the inflow time series simulation will by adjusted to end at the last of the inflows)
#' @param nsteps the number of simulation steps to take within one timestep of the inflow timeseries, default is 1
#' (i.e. how often reservoir releases will be adjusted within a timestep)
#' @param dt the length of a time step in the inflow timeseries, default is 1.
#' (e.g. if you are providing daily inflows and the unts are m3/hour the dt would be 24, if the unts are m3/s it would be 86400 )
#'
#' @return returns a matrix of reservoir operations with four columns (1) date, (2)reservoir inflow [l3/t], (3) reervoir release [l3/t],
#'  (4) storage at the end of the day [l3]. The lenth (l) and time (t) units of the ouput will be the same as the inflow timeseries provided.
#' (NOTE even if your nstep is >1 this will return the aggreageted operations at the same time step as the inflow timeseries)

#' @examples
#' calc.release(release.pctl, stor.pctl, inflow.ts, stor.init=100, stor.max=10000, stor.min=2, sim.start="1940-01-01",
#' sim.stop="1950-03-01", nsteps=240, dt=24) )
#'
#'@seealso \code{\link{monthlypctl.fromday}} \code{\link{calc.release}}
#'
#' @export

res.sim.day=function(release.pctl, stor.pctl, inflow.ts, stor.init, stor.max, stor.min, sim.start,
                 sim.stop, nsteps=1, dt=1){

  #clip the inflow time series to the selected simulation period
  stor_now=stor.init
  inflow.clip=inflow.ts[paste(sim.start, sim.stop, sep="/")]

  #make a vector of dates for the simulation
  dates=seq(as.Date(sim.start), as.Date(sim.stop), by = "days")
  nday=length(dates)

  ressim=matrix(NA, nrow=nday, ncol=2)

  for(d in 1:nday){
       #grab the storage and release thresholds for the given month
       mtemp=month(dates[d])
       stormonth=stor.pctl[,m]
       releasemonth=flow.pctl[,m]

        #setup temporary time series for releases with the day
        releasetemp=rep(0, nsteps)
        stortemp=rep(0, nsteps)

    #loop over the number of time steps in a given day
    for(istep in 1:nsteps){
        releasetemp[i]=calc.release(release.th=c(0,releasemonth), storage.th=c(2,stormonth), storage=stor_now, inflow=NA, inflow.th=NA, stor.max=stor.max, dt=dt/nsteps)
        stortemp[i] = stor_now - (release[i]*dt/nsteps) + (inflow.clip[d]*dt/nsteps)

         #if you would be over max storage at the end of the time step release everything above max storage
        if(stortemp[i] > stor.max){
          releasetemp[i]=releasetemp[i]+ (stortemp[i]-stor.max)
        }

        stor_now=stortemp[i]
    } #end for istep
      ressim[d,1] = inflow.clip[d]
      ressim[d,2] = average(releasetemp[i])
      ressim[d,3] = stor_now  #storage at the end of the day

  } # end for day

    #add dates to the ressim matrix
   ressim=cbind(dates,ressim)

  return(ressim)

} #end function

