*Modified IEEE 24-bus test system
*Unit Commitment and Economic Dispatch example
*2/25/2021

*Setting the default slash for PC users
$setglobal ds \

*Change the default slash if in UNIX (MAC users)
$ifthen.unix %system.filesys% == UNIX
$setglobal ds /
$endif.unix

* set case name for saving outputs
* note that there must be a folder with this name in 'outputs'
$setglobal case MGResults

*define main sets
sets
    t                   time steps (minutes in one hour)            /t1*t2016/
    n                   nodes                                       /n1*n9/
    G                   generators                                  /g1*g3/
    fossilG(G)          fossil fuel generators                      /g1*g2/
    coalG(fossilG)      coal generators                             /g1/
    gasG(fossilG)       natural gas generators                      /g2/
    renG(G)             renewable generators                        /g3/
;

*define alias for sets t and n
alias(t,tt);
alias(n,nn);

*define any useful scalars
scalar
    penalty             "--$/MWh--penalty price for unserved or dumped energy"     /1000000/
;

*define generators' connection to nodes based on data in a csv file
set gconnect(n,G) 
/
$offlisting
$ondelim
$include data%ds%gen_nodes.csv
$offdelim
$onlisting
/
;

*load data on fuel consumption coefficients & operational limits for generators (G)
*'hr_marg' is the marginal heat rate (MMBTU/MWh)
*'hr_base' is the base heat rate (MMBTU/hour)
*'max_gen' is the maximum generation capacity (MW)
*'min_gen' is the minimum generation capacity (MW)
*'ramp' is the maximum upward/downward ramping (MW/hour)
*'vom' is the variable operation and maintenance cost, VO&M ($/MWh)
*'start_cost' is the startup cost ($)
*'fuel' is the fuel price ($/MMBTU)
*'min_downtime' is the minimum down time constraint (hours)

* note that not all of these are being used yet!

table genpar(G,*) 
$offlisting
$ondelim
$include data%ds%generators9.csv
$offdelim
$onlisting
;

* assign generator parameters to more meaningful parameter names
parameter
    hr_marg(G)         "--MMBTU/MWh-- marginal heat rate"
    hr_base(G)         "--MMBTU/hour-- base heat rate when committed"
    max_gen(G)         "--MW-- maximiun generation capacity"
    min_gen(G)         "--MW-- min generation levels when committed"
    ramp(G)            "--MW/hour-- maximum upward/downward ramping"
    vom(G)             "--$/MWh-- variable operation and maintenance cost"
    start_cost(G)      "--$-- start cost of generator"
    fuel(G)            "--$/MMBTU-- fuel price"
    min_downtime(G)    "--hours-- min amount of time a unit must be off before turning on again"
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


table ts(t,*)
$offlisting
$ondelim
$include data%ds%timeseries.csv
$offdelim
$onlisting
;


display ts;