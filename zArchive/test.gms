sets
    t                   time steps (15-minute resolution)           /t1*t672/

parameter time_fault(t);
$offlisting
$ondelim
$include data%ds%fault_time.csv
$offdelim
$onlisting
/
;

display time_fault