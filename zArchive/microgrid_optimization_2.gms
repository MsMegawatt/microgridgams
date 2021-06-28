************
***HEADER***
************

*Microgrid Analysis for 9-bus System
*Megan Rose
*Last Updated April 12, 2021

*********************
***SYSTEM SETTINGS***
*********************

*Setting the default slash for PC users
$setglobal ds \

*Change the default slash if in UNIX (MAC users)
$ifthen.unix %system.filesys% == UNIX
$setglobal ds /
$endif.unix

**********
***CASE***
**********

$setglobal case MGResults


*************************
***SETS AND PARAMETERS***
*************************

**GENERAL SETS**

sets
    t                   time steps (minutes in one hour)            /t1*t672/
    n                   nodes                                       /n1*n9/
    G                   generators                                  /g1*g3/
    s                   battery storage                             /s1/
    fossilG(G)          fossil fuel generators                      /g1*g2/
    coalG(fossilG)      coal generators                             /g1/
    gasG(fossilG)       natural gas generators                      /g2/
    renG(G)             renewable generators                        /g3/
    slack(n)            specify a reference node for DC OPF         /n2/
;

alias(t,tt);
alias(n,nn);

**SCALARS**

scalar
    penalty             "--$/MWh--penalty price for unserved or dumped energy"     /1000000/
    sbase               "--MVA--define per unit basis for power flow"              /100/
;

**GENERATOR CONNECTIONS TO NODES**

set gconnect(n,G) 
/
$offlisting
$ondelim
$include data%ds%gen_nodes.csv
$offdelim
$onlisting
/
;

**GENERATOR PARAMETERS**

table genpar(G,*) 
$offlisting
$ondelim
$include data%ds%generators9.csv
$offdelim
$onlisting
;

parameter
    hr_marg(G)         "--MMBTU/MWh-- marginal heat rate (MMBTU/MWh)" 
    hr_base(G)         "--MMBTU/hour-- base heat rate when committed (MMBTU/hour)"
    max_gen(G)         "--MW-- maximiun generation capacity(MW)"
    min_gen(G)         "--MW-- min generation levels when committed(MW)"
    ramp(G)            "--MW/hour-- maximum upward/downward ramping (MW/hour)"
    vom(G)             "--$/MWh-- variable operation and maintenance cost ($/MWh)"
    start_cost(G)      "--$-- start cost of generator ($)"
    fuel(G)            "--$/MMBTU-- fuel price ($/MMBTU)"
    min_downtime(G)    "--hours-- min amount of time a unit must be off before turning on again (hours)"
;

hr_marg(G) = genpar(G, 'hr_marg') ;
hr_base(G) = genpar(G, 'hr_base');
max_gen(G) = genpar(G, 'max_gen') ;
min_gen(G) = genpar(G, 'min_gen');
ramp(G) = genpar(G, 'ramp') ;
vom(G) = genpar(G, 'vom') ;
start_cost(G) = genpar(G, 'start_cost');
fuel(G) = genpar(G, 'fuel') ;
min_downtime(g) = genpar(G, 'min_downtime')


**NODE LOAD PARTICIPATION FACTORS**

*'crit_load' is share of critical load (MW)
*'load' is share of total load (MW)

table nodeloadfactor(n,*)
$offlisting
$ondelim
$include data%ds%nodeloadfactors.csv
$offdelim
$onlisting
;


**TIMESERIES DATA**

*define time series for every time step  (South-Central ERCOT Region 15-min Load, 2018)
*'win_pk' is the week of peak load in winter (MW)
*'sum_pk' is the week of peak load in summer (MW)
*'max_ramp' is the week of the largest ramp of the year (MW)
*'solar_cf' is g3 available generation capacity factor for solar microgrid (0 to 1)

table ts(t,*)
$offlisting
$ondelim
$include data%ds%timeseries_15min.csv
$offdelim
$onlisting
;

display ts;

*allocate load to each node by combining timeseries and nodeloadfactors data
parameter crit_load(n,t);
crit_load(n,t) = nodeloadfactor(n,'crit_load')*ts(t,'win_pk');

parameter load(n,t);
load(n,t) = nodeloadfactor(n,'load')*ts(t,'win_pk');

*link capacity factors of renewable generators to timeseries data
parameter cf(renG,t);
cf('g3',t) = ts(t,'solar_cf');


**LINE DATA**

table line(n,nn,*)
$offlisting
$ondelim
$include data%ds%line_data_full.csv
$offdelim
$onlisting
;

*define lines interconnecting nodes
set nconnect(n,nn) ;

*if there is data on the line, those two nodes are connected
nconnect(n,nn)$[line(n, nn, "lim")] = YES;

*define all lines to be bi-directional lines
nconnect(n,nn)$[nconnect(nn,n)]=1;

*define line properties for both flow directions
line(n,nn,'lim')$[line(n,nn,'lim')=0] = line(nn,n,'lim') ;
line(n,nn,'x')$[line(n,nn,'x')=0] = line(nn,n,'x') ;

*compute line susceptance from reactance data
line(n,nn,'B')$nconnect(n,nn) = 1/line(n,nn,'x');


**STORAGE**

*specify which nodes have storage devices
set sconnect(n,s) /n3.s1/ ;

*Storage capacity (MW) 
parameter scap(s)
/s1 125/
;

*Storage energy (MWh) 
parameter senergy(s)
/s1 500/
;

*Storage roundtrip efficiency
scalar seff / 0.8 / ;


***************
***VARIABLES***
***************

**GENERAL VARIABLES**

variables
    cost                    "--$-- the operational cost during the entire optimization horizon"
    delta(n,t)              "--radians-- voltage angle at node n and time t"
    flow(n,nn,t)            "--MW-- the power flow between nodes n and nn at time t"
;


**POSITIVE VARIABLES**

positive variables 
    gen(G,t)                "--MW-- generation of generator G at time t"
    unservedenergy(n,t)     "--MW-- unserved energy of node n at time t"
    dumpenergy(G,t)         "--MW-- excess generation of node n at time t"
    soc(s,t)                "--MWh-- state of charge of storage device at time t"
    sgen(s,t)               "--MW-- generation storage s at time t"
    sload(s,t)              "--MW-- charging of storage s at time t"

;


**BINARY VARIABLES**

binary variables
    on(fossilG,t)               "on/off status of fossil units"
    turnon(fossilG,t)           "whether a fossil unit is started in time period t"
    turnoff(fossilG,t)          "whether a fossil unit is decommitted in time period t"
;

    
***************
***EQUATIONS***
***************

equations
    eq_cost                         "--$-- objective function to minimize system cost"
    eq_load_balance                 "--MW-- ensure that generation equals load in all hours"
    eq_power_flow                   "--MVA-- dc-opf powerflow calculation"
    eq_rampup                       "--MW-- upward ramping constraint on generators"
    eq_rampdown                     "--MW-- pward ramping constraint on generators"
    eq_max_fossil                   "--MW-- installed capacity constraint on fossil generators"
    eq_max_re_gen                   "--MW-- limit renewable generation to available resource"
    eq_mingen                       "--MW-- enforce min generation requirements for committed fossil"
    eq_commitment                   "--unitless-- defines relationship between on, turnon, and turnoff"
    eq_onoff                        "--unitless-- ensure units do not turnon and turnoff in the same period"
    eq_min_downtime                 "--unitless-- min downtime constraint for periods t <= T - DT"
    eq_min_downtime2                "--unitless-- min downtime constraint for periods t > T - DT"
    soceq1, soceq2, soceq3, soceq4, sgenmax, sloadmax "storage equations"
;


**OBJECTIVE FUNCTION**
eq_cost..
* total operational costs = 
    cost
    
    =e=
* sum of costs over all time periods
    sum(t,
* vom costs of all generators in each hour          
      sum(G, vom(G)*gen(G,t))
* fuel costs of generators that use fuel
      + sum(fossilG, fuel(fossilG)*(hr_base(fossilG)*on(fossilG, t) +
                                    hr_marg(fossilG)*gen(fossilG,t)))
* start up costs
      + sum(fossilG, turnon(fossilG,t) * start_cost(fossilG))
* costs of unserved load
      + sum(n,unservedenergy(n,t))*penalty
* costs of dumped energy
      + sum(G,dumpenergy(G,t))*penalty
    )
;


**GEN=LOAD**

eq_load_balance(n,t)..
* generation at a node
    sum(G$gconnect(n,G), gen(G,t)) 
* plus imports from other nodes, minus exports to other nodes
* now defining flows to be bidirectionl, so only need one statemnt here
    + sum(nn$nconnect(n,nn), flow(nn,n,t))
* plus unserved energy
    + unservedenergy(n,t)
* less any dumped energy from excess generation    
    - sum(G$gconnect(n,G), dumpenergy(G,t))
* plus generation from storage 
    + sum(s$sconnect(n,s), sgen(s,t))
    
    =e=
    
* must equal load at each node in each time step
    load(n,t)
* plus load from storage charging
    + sum(s$sconnect(n,s), sload(s,t))

;


**POWER FLOW (p.u.)**

eq_power_flow(n,nn,t)$[nconnect(n,nn)]..
    flow(n,nn,t) / sbase
    
    =e=
    
    line(n,nn,'B')*(delta(n,t)-delta(nn,t))
;
    


**MAX RAMP UP FOR FOSSIL GENS**

eq_rampup(fossilG,t)$[ord(t) GT 1]..

    gen(fossilG,t) - gen(fossilG,t-1)
    
    =l=
    
    on(fossilG,t)*ramp(fossilG) + turnon(fossilG,t)*min_gen(fossilG)
;

    
**MAX RAMP DOWN FOR FOSSIL GENS**

eq_rampdown(fossilG,t)$[ord(t) GT 1]..

    gen(fossilG,t-1) - gen(fossilG,t)
    
    =l=
    
    on(fossilG,t)*ramp(fossilG) + turnoff(fossilG,t)*min_gen(fossilG)
;

**MAX CAPACITY OF FOSSIL GENS**

eq_max_fossil(fossilG,t)..
    gen(fossilG,t)
    
    =l=
    
    on(fossilG, t)*max_gen(fossilG)
;


**MAX CAPACITY FOR RENEWABLE GENS**

eq_max_re_gen(renG,t)..

    gen(renG,t)
    
    =l=
    
    max_gen(renG)*cf(renG,t)
;


**MIN GENERATION OF FOSSIL GENS**

eq_mingen(fossilG,t)..
    gen(fossilG,t)
    
    =g=
    
    on(fossilG,t)*min_gen(fossilG)
;
    
**UNIT COMMITMENT LOGIC**
eq_commitment(fossilG,t)$[ord(t) GT 1]..

    on(fossilG,t)
    
    =e=
    
    on(fossilG,t-1) + turnon(fossilG,t) - turnoff(fossilG,t)
;

* units cannot start and shut down in the same period
eq_onoff(fossilG,t)..

    turnon(fossilG,t) + turnoff(fossilG,t)
    
    =l=
    
    1
;


**MIN DOWNTIME - IN WINDOW**

eq_min_downtime(fossilG,t)$[ord(t) <= card(t) - min_downtime(fossilG) + 1]..
    sum(tt$[(ord(tt)>=ord(t) and (ord(tt)<(ord(t)+min_downtime(fossilG))))], 1 - on(fossilG,tt))
    
    =g=
    
    min_downtime(fossilG) * turnoff(fossilG,t) 
;


**MIN DOWNTIME - END OF WINDOW**

eq_min_downtime2(fossilG,t)$[ord(t) > card(t) - min_downtime(fossilG) + 1]..
    1 - on(fossilG,t) - turnoff(fossilG,t)
    
    =g=
    
    0
    
;

**STORAGE EQUATIONS**
*Defining state of charge of every storage at every time step (4 steps)

* part 1: storage starts out (i.e. t=1) at 50% charge 
soceq1(s,t)$(ord(t) = 1)..

    soc(s,t)
        
    =e=
    
    0.5*senergy(s)
;

* part 2: for t>1, storage charge in period t equals the
* charge in the last period (t-1) less discharge + charge, accounting for efficiency losses
soceq2(s,t)$(ord(t) > 1 and ord(t) < card(t))..

    soc(s,t)
    
    =e=
    
    soc(s,t-1) - sgen(s,t-1) + seff*sload(s,t-1)
    
;
    
* part 3: in the last period, storage must return to 50% charge                   
soceq3(s,t)$(ord(t) = card(t))..

    soc(s,t) + seff*sload(s,t) - sgen(s,t)
    
    =e=
                                 
    0.5*senergy(s)
;

* part 4: state of charge must always be less than the energy capacity of the battery
soceq4(s,t)..

    soc(s,t)
    
    =l=
    
    senergy(s)
;

*Defining maximum storage generation and load
sgenmax(s,t)..
    sgen(s,t)
    =l=
    scap(s)
;

sloadmax(s,t)..
    sload(s,t)
    =l=
    scap(s)
;


******************
***DEFINE MODEL***
******************

Model MGResults /all/;

*Defining voltage angle limits
delta.lo(n,t) = -pi;
delta.up(n,t) = pi;

* defining voltage angle of reference/slack bus
delta.fx(slack,t) = 0;

* define line limits
flow.up(n,nn,t)$nconnect(n,nn) = line(n,nn,'lim');
flow.lo(n,nn,t)$nconnect(n,nn) = -line(n,nn,'lim');

*Defining maximum MIP GAP
option optcr = 0.001    ;

*call solve statement
Solve MGResults using MIP min cost;

