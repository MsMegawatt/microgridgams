$setglobal case ED

* this utility will dump gdx outputs to csv files for MAC users
$call 'gdxdump outputs/%case%/dispatch_results.gdx format=csv epsout=0 symb=generation > outputs/%case%/generation.csv'
$call 'gdxdump outputs/%case%/dispatch_results.gdx format=csv epsout=0 symb=lineflow > outputs/%case%/lineflow.csv'
$call 'gdxdump outputs/%case%/dispatch_results.gdx format=csv epsout=0 symb=LMP > outputs/%case%/LMP.csv'
$call 'gdxdump outputs/%case%/dispatch_results.gdx format=csv epsout=0 symb=use > outputs/%case%/unserved_energy.csv'
$call 'gdxdump outputs/%case%/dispatch_results.gdx format=csv epsout=0 symb=dump > outputs/%case%/dump_energy.csv'
$call 'gdxdump outputs/%case%/dispatch_results.gdx format=csv epsout=0 symb=curtailment > outputs/%case%/curtailment.csv'

$call 'gdxdump outputs/%case%/dispatch_results.gdx format=csv epsout=0 symb=costs > outputs/%case%/costs.csv'
$call 'gdxdump outputs/%case%/dispatch_results.gdx format=csv epsout=0 symb=penalties > outputs/%case%/penalties.csv'

* only need this for UCED results
* $call 'gdxdump outputs/%case%/dispatch_results.gdx format=csv epsout=0 symb=commit > outputs/%case%/commit.csv'
