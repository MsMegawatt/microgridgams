
*************
***OUTPUTS***
*************

set
    cost_cat            /vom, fuel, start/
    penalty_cat         /use, dump/
    on_cat              /commit, start, stop /
;

parameters
    generation(G,t)
    storage_gen(s,t)
    storage_charge(s,t)
    storage_soc(s,t)
    lineflow(n,nn,t)
    LMP(n,t)
    use(n,t)
    dump(G,t)
    costs(G,t,cost_cat)
    penalties(n,t,penalty_cat)    
    commit(fossilG,t,on_cat)
    curtailment(renG,t)
;

* extract generation results
generation(G,t) = gen.l(G,t) ;

* for lineflow, define in reference direction only
lineflow(n,nn,t)$[ord(n) < ord(nn)] = flow.l(n,nn,t);

use(n,t) = unservedenergy.l(n,t) ;    
dump(G,t) = dumpenergy.l(G,t) ;         

* lmps are the marginal on the load balance equation
LMP(n,t) = eq_load_balance.m(n,t) ;

* total cost metrics
costs(G,t,"vom") = vom(G)*gen.l(G,t) ;
costs(fossilG,t,"fuel") = fuel(fossilG)*(hr_marg(fossilG)*gen.l(fossilG,t) + hr_base(fossilG)*on.l(fossilG, t));
costs(fossilG,t,"start") = turnon.l(fossilG,t) * start_cost(fossilG);


penalties(n,t,"use") = unservedenergy.l(n,t)*penalty ;
penalties(n,t, "dump") = sum(G$gconnect(n,G), dumpenergy.l(G,t)*penalty) ;

commit(fossilG,t,"commit") = on.l(fossilG,t) ;
commit(fossilG,t,"start") = turnon.l(fossilG,t);
commit(fossilG,t,"stop") = turnoff.l(fossilG,t);

* calculate curtailment levels
curtailment(renG,t) = max_gen(renG)*cf(renG,t) - gen.l(renG, t);

* storage results
storage_gen(s,t) = sgen.l(s,t) ;
storage_charge(s,t) = sload.l(s,t) ;
storage_soc(s,t) = soc.l(s,t) ;


* save results to a gdx file
execute_unload "outputs%ds%%case%%ds%dispatch_results.gdx"
    generation, lineflow, LMP, use, dump,
    costs, penalties, commit, delta, flow, curtailment,
    storage_gen, storage_charge, storage_soc
;

* extract results to csv files

* for PC users
* execute 'gdxxrw.exe outputs%ds%dispatch_results.gdx o=generation.csv par=generation'
* execute 'gdxxrw.exe outputs%ds%dispatch_results.gdx o=lineflow.csv par=lineflow'
* execute 'gdxxrw.exe outputs%ds%dispatch_results.gdx o=LMP.csv par=LMP'

* MAC users should run the separate "save_results.gms" script