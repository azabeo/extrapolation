#standard procedure
#' @export
standard_procedure <- function(seed=1){
  #res1 = extrapolation('NC_mice','example.data/michael/bmd.csv','example.data/michael/efs.csv','example.data/michael/calc.csv',1)
  #assign(paste0("res",seed),extrapolation('NC_mice','example.data/michael/bmd.csv','example.data/michael/efs.csv','example.data/michael/calc.csv',seed),.GlobalEnv)
  assign(paste0("resCheng",seed),extrapolation('chengfang','example.data/cheng/bmd.csv','example.data/cheng/Efs.cheng.csv','example.data/cheng/calc.cheng.csv',seed),.GlobalEnv)

  assign(paste0("statCheng",seed),do.stats(res1$bmds),.GlobalEnv)
  #stat1 <- do.stats(res1$bmds)

  #export(res1$bmds,'res1')
  export(get(paste0('resCheng',seed))$bmds,paste0('resCheng',seed))
  #export(stat1)
  export(get(paste0("statCheng",seed)),paste0("statCheng",seed))
}

standard_procedure_cheng <- function(){
  cheng<- loadData("example.data/cheng2.csv",c('id'))
  plotalllogged(cheng,"LogICED","log.IEXP","LogIMOE","LogCED_IEXP","Cheng2","norm",c(-2,6))
}
