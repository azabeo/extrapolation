#standard procedure
standard_procedure <- function(seed=1){
  assign(paste0("res",seed),extrapolation('NC_mice','example.data/michael/bmd.csv','example.data/michael/efs.csv','example.data/michael/calc.csv',seed),.GlobalEnv)
  #res1 = extrapolation('NC_mice','example.data/michael/bmd.csv','example.data/michael/efs.csv','example.data/michael/calc.csv',1)
  assign(paste0("stat",seed),do.stats(res1$bmds),.GlobalEnv)
  #stat1 <- do.stats(res1$bmds)

  #export(res1$bmds,'res1')
  export(get(paste0('res',seed))$bmds,paste0('res',seed))
  #export(stat1)
  export(get(paste0("stat",seed)),paste0("stat",seed))
}
