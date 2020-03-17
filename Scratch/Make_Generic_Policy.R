
#inputs - Outflow-TS
#        - Storage TS
# Storage percentiles
# Outflow percentiles
# Date range to use
# Need a good way to remove NA's
# Output
# Matrix with storage probabilites by month
# Matrix with flow probabilities by month


# qcl, qc, qcu, qnl, qn, qnu, qml, qm, qmu
# c  =, n=ormal? m=
# l= lowerbound, u=upper bound
th=c(5, 10, 35, 45, 75,	85,	95)
#  what I need



#Step 1 tingle  columns of  dates, inflow and outflow for a  period of interest
res_info_out.timed   = timed2;
res_info_out.stoobs  = resdata(k1:k2,4).*1000000;                            % converts volume unit from Mm3 to m3 (col 4 is storage)
res_info_out.inflow  = resdata(k1:k2,5);                                     % col 5 is inflow   (m3/s)
res_info_out.outflow = resdata(k1:k2,6);                                     % col 6 is outflow  (m3/s)

#Step 0- setting thresholds for opearting  policies
par_sel_release = [10,45,85,5,35,75,35,75,95];                               % percentile for qc, qn, qm, qcl, qnl, qml, qcu, qnu, qmu
par_sel_storage = [10,45,85,5,35,75,35,75,95];                               % percentile for sc, sn, sm, scl, snl, sml, scu, snu, smu

#  I think  this just gets  percentiles  of inflow, outflow  and  storage
onan=~isnan(res_info_out.outflow); inan=~isnan(res_info_out.inflow);  snan=~isnan(res_info_out.stoobs);  % to index out missing values
fdurn  =dztr_param(res_info_out.outflow(onan,1),'MonthPartition',res_info_out.timed(onan,1));            % release monthly percentile
indurn =dztr_param(res_info_out.inflow(inan,1),'MonthPartition',res_info_out.timed(inan,1));             % inflow monthly percentile (non required)
sdurn  =dztr_param(res_info_out.stoobs(snan,1),'MonthPartition',res_info_out.timed(snan,1));             % storge monthly percentile

#Reservoir parameters
# Storage parameters (First 3 rows,  12  columns)
# Row  1 =  10th percentile storage - ("sc monthly no-exe value)
# Row  2 = 45th percentile storage - ("sn monthly non-exe value)
# Row  3 = 85th percentile storage ("sm  monthly non-exe  value)
% default parameters for storage. row 1 sc monthly non-exe value, row 2 sn monthly non-exe value, row 3 sm monthly non-exe value
for i=1:3
respar(i,:) = sdurn.percentiles((sdurn.percentiles(:,1)==par_sel_storage(i)),3:14);
end

# Outflow parameters (Rows 4-6,  12  columns)
# Row  1 =  10th percentile storage - ("qc monthly no-exe value)
# Row  2 = 45th percentile storage - ("qn monthly non-exe value)
# Row  3 = 85th percentile storage ("qm  monthly non-exe  value)
% default parameters for release. row 4 qc monthly non-exe value, row 5 qn monthly non-exe value, row 6 qm monthly non-exe value
for i=4:6
respar(i,:) = fdurn.percentiles((fdurn.percentiles(:,1)==par_sel_release(i-3)),3:14);    % storage 1 value
end

# Outflow parameters (Rows 4-6,  12  columns)
# Row  1 =  10th percentile storage - ("qc monthly no-exe value)
# Row  2 = 45th percentile storage - ("qn monthly non-exe value)
# Row  3 = 85th percentile storage ("qm  monthly non-exe  value)
% release monthly lower and upper bound non-exe value from row 7 to row 12
% row order (qcl, qnl, qml, qcu, qnu, qmu)
for i=7:12
respar(i,:) = fdurn.percentiles((fdurn.percentiles(:,1)==par_sel_release(i-3)),3:14);    % storage 1 value
end

% storage monthly lower and upper bound non-exe value from row 13 to row 18
% row order (scl, snl, sml, scu, snu, smu)
for i=13:18
respar(i,:) = sdurn.percentiles((sdurn.percentiles(:,1)==par_sel_storage(i-9)),3:14);    % storage 1 value
end
