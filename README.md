# README
Marcus W. Beck, beck.marcus@epa.gov, Janne Alahuhta, Janne.Alahuhta@oulu.fi  

### Files

* `veg_dat.RData` DNR vegetation transect data from 1992 to 2014, created in `fishveg.RProj`, combination of nrri processed data, current transect data, and gaps filled with another file
* `veg_rch.RData` Summarized vegetation richness data from veg_dat, includes total richness (`richtot`), submersed species richness (`richsub`), presence/absence of coontail (as 1/0, `cd_pres`), presence/absence of curly-leaf (`pc_pres`), presence/absence of milfoil (`ms_pres`). Scientific names were manually verified during processing to remove duplicates (all species had scientific names except 'filamentous algae', 'planktonic algae', and 'Fern group', these were removed - 2642 records out of 894920 ~0.3% of total)

### To Do

* Create combined veg transect dataset with covariates from Cross paper
* Check with Cross to see if we can use the dataset
* Run simple GAM looking at effect of curly-leaf, milfoil, and coontail on ind species, fixed and interactions (tensor product)
* a block design (lakes w/ and w/o invasives, etc.)
