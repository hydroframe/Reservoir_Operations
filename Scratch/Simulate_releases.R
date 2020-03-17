#' Simulate reservoir releases
#'
#' Simulate releases given and operating  policy
#
# Inputs:
# - icor - need to correct  for ET flag?
# - if cse =1 inflow is not used in zone 3, else it is used
# - Ld - dead storage fraction
# - niter -  number of iteration to estimate the relaese
# - wrmp - warm-up peroid to calcualte the metrics
# - tinterval - 1 time interval in day (e.g. 6hr = 0.25day)
# - date1 = starting date
# - date2 = ending date

#Read from spreadsheet
# smax    = resdata(1,7);                                                 % maximum reservoir storage (m3)
# Intflow = resdata(2,7);                                                 % Initial release (m3/s)
# Intstor1= resdata(3,7);                                                 % Initial reservoir storage (m3)
# qds     = resdata(4,7);                                                 % d/s channel capacity (m3/s)

#MY TRANSLATION
storage_temp=storage[i-1]+dt/2(Inflow[i]+ Inflow[i-1]-outflow[i-1])
Inflow=flowI[i-1]
Fu=storage_temp/smax  #fractional storage
Outflow[i]= f(res policy, current storage, Inflow)
for x in 1: niter:
  storage[i]=storage_temp - dt/2(Outflow[i])
  Fu=storage[i]/smax
  outflow[i]=f(res_policy, current storate, inflow)
}
if{
  Storage[i]>smax
  outflow[i]=outflow[i]+ (stoSIM-smax)/dt
  stosim[i]=smax
}


#FUADS
for i = 2:1:numel(stoObs(:,1))
[yy, mm] = datevec(datelooping);
stoflo = stoSIM(i-1,1) + (dt/2)*(flowI(i,1)*icor + flowI(i-1,1)*icor - flowSIM(i-1,1));
Inflow = flowI(i,1);
Fu = stoflo/smax;
flowSIM(i,1) = DZTR(Fu,Ld,Lc(mm),Ln(mm),Lf(mm),qmin(mm),qnorm(mm),qnd(mm),dt,smax,Inflow,qds,cse);
for it=1:niter
stoSIM(i,1)  = stoflo - (dt/2)*(flowSIM(i,1));
Fu = stoSIM(i,1)/smax;
flowSIM(i,1) = DZTR(Fu,Ld,Lc(mm),Ln(mm),Lf(mm),qmin(mm),qnorm(mm),qnd(mm),dt,smax,Inflow,qds,cse);
end
if stoSIM(i,1)>smax
flowSIM(i,1) = flowSIM(i,1) + ((stoSIM(i,1)-smax)/dt);
stoSIM(i,1) = smax;
end
datelooping = datelooping + tinterval;
end




sim.res=function(Release, ){


  clearvars -except respar res_info_out resdata
  icor    = 1.00;                                                  % if there is a need to correct inflow or adjust for Evaporation
  cse     = 1;                                                     % if cse =1 inflow is not used in zone 3, else it is used
  Ld      = 0.1;                                                   % dead storage fraction
  niter   = 1000;                                                  % number of iteration to estimate the relaese
  wrmp    = 366;                                                   % warm-up peroid to calcualte the metrics
  tinterval   = 1;                                                 % time interval in day (e.g. 6hr = 0.25day)
  date1 = datenum(1970,1,1);       date2 = datenum(2000,12,31);    % simulation period

  t1          = find(res_info_out.timed(:,1)==date1);
  t2          = find(res_info_out.timed(:,1)==date2);
  timee       = res_info_out.timed(t1:t2,1);
  stoObs  = res_info_out.stoobs(t1:t2,1);                                 % observed reservoir storage
  flowObs = res_info_out.outflow(t1:t2,1);                                % observed d/s discharge
  flowI   = res_info_out.inflow(t1:t2,1);                                 % observed u/s inflow
  smax    = resdata(1,7);                                                 % maximum reservoir storage (m3)
  Intflow = resdata(2,7);                                                 % Initial release (m3/s)
  Intstor1= resdata(3,7);                                                 % Initial reservoir storage (m3)
  qds     = resdata(4,7);                                                 % d/s channel capacity (m3/s)
  Lc      = respar(1,:)./smax;                                            % monthly lower storage threshold
  Ln      = respar(2,:)./smax;                                            % monthly upper storge threshold
  Lf      = respar(3,:)./smax;                                            % inflow starts affecting release
  qmin   =  respar(4,:); qnorm   =  respar(5,:); qnd   =  respar(6,:);    % default parameters for release
  dt          = 60*60*(tinterval*24);                                     % time interval in seconds
  datelooping = date1+tinterval;                                          % used in the loop
  flowSIM     = zeros(numel(flowObs),1);                                  % create zero meteris for later saving results
  flowSIM(1,1)= Intflow(1,1);                                             % the first release equals to inflow
  stoSIM      = zeros(numel(flowObs),1);                                  % create zero meteris for later saving results
  stoSIM(1,1) = stoObs(1,1);                                              % initial storage equals to first storage
  for i = 2:1:numel(stoObs(:,1))
  [yy, mm] = datevec(datelooping);
  stoflo = stoSIM(i-1,1) + (dt/2)*(flowI(i,1)*icor + flowI(i-1,1)*icor - flowSIM(i-1,1));
  Inflow = flowI(i,1);
  Fu = stoflo/smax;
  flowSIM(i,1) = DZTR(Fu,Ld,Lc(mm),Ln(mm),Lf(mm),qmin(mm),qnorm(mm),qnd(mm),dt,smax,Inflow,qds,cse);
  for it=1:niter
  stoSIM(i,1)  = stoflo - (dt/2)*(flowSIM(i,1));
  Fu = stoSIM(i,1)/smax;
  flowSIM(i,1) = DZTR(Fu,Ld,Lc(mm),Ln(mm),Lf(mm),qmin(mm),qnorm(mm),qnd(mm),dt,smax,Inflow,qds,cse);
  end
  if stoSIM(i,1)>smax
  flowSIM(i,1) = flowSIM(i,1) + ((stoSIM(i,1)-smax)/dt);
  stoSIM(i,1) = smax;
  end
  datelooping = datelooping + tinterval;
  end


## From FUAD

#Releases
if stor_frac < Ld (dead storage fraction)
  outflow = 0

else if  Dead storage < stor_frac < Lower threhold (10%)
  outflow=min(qmin,(stor_frac-LD)*smax/dt))

else if Lower threhold (10%) < stor_frac < Upper threhold (10%)
  outflow= qmin + (qnorm-qmin)*((Fu-Lc)/(Ln-Lc));
  #linearly  interpolate your  flow thresholds

elseif Fu > Ln && Fu <= Lf
if cse==1
qout = qnorm + ((Fu-Ln)/(Lf-Ln))*(qnd-qnorm);
else
  qout = qnorm + ((Fu-Ln)/(Lf-Ln))*max((Inflow-qnorm),(qnd-qnorm));
end
else
  qout = min(max(((Fu-Lf)*smax/dt),qnd),qds);
end




function [qout] = DZTR(Fu,Ld,Lc,Ln,Lf,qmin,qnorm,qnd,dt,smax,Inflow,qds,cse)

if Fu <= Ld
  qout = 0;
elseif Fu > Ld && Fu <= Lc
  qout = min(qmin,((Fu-Ld)*smax/(dt)));
elseif Fu > Lc && Fu <= Ln
  qout = qmin + (qnorm-qmin)*((Fu-Lc)/(Ln-Lc));
elseif Fu > Ln && Fu <= Lf
if cse==1
qout = qnorm + ((Fu-Ln)/(Lf-Ln))*(qnd-qnorm);
else
  qout = qnorm + ((Fu-Ln)/(Lf-Ln))*max((Inflow-qnorm),(qnd-qnorm));
end
else
  qout = min(max(((Fu-Lf)*smax/dt),qnd),qds);
end

end
