#' extrapolation
#'
#' applies EF factors to BMD with specified distributions
#'
#' from the BMD values extracts GM and GSD, randomly picks the same amount from the corresponding
#' lognormal distriution, then for each row in the EF table picks the corresponding random values
#' and applies the corresponding operator in cascade
#'
#' @param bmd.file.name string. the location of the input file for bmds (al least it
#' has to have 'bmd' and 'id' columns)
#' @param efs.file.name string. table of EFs to be applied (see excel file in inst for instructions)
#' @param above.threshold numeric. If present values less equal to it are discarded from generated
#' bmd distribution
#' @return list of two tables, the updated bmds table with results and the EFs table used
#' @author Alex Zabeo
#' @examples
#' res <- extrapolation("example.data/bmd.csv","example.data/Efs.csv",0)
#' @export
extrapolation <- function(bmd.file.name = "example.data/bmd.csv", efs.file.name = "example.data/Efs.csv",above.threshold = 0){

  bmd.boot.col = 'bmd'
  bmd.distrib = 'bmd.distrib'
  bmd.id = c('id')
  efs.id = c('name')

  logd('loading')
  bmds <- loadData(bmd.file.name,bmd.id)
  efs <- loadData(efs.file.name,efs.id)

  logd('CALCULATING GMEAN, GSD AND BMD RAND VALUES ----------')
  gm = geomean(bmds[,get(bmd.boot.col)])
  gsd = geosd(bmds[,get(bmd.boot.col)])

  addRandCol(bmds,bmd.distrib,gm,gsd,rlnorm,TRUE,above.threshold)

  logd('INTEGRATING FACTORS')

  # position of the first column to utilize
  col = match(bmd.distrib,names(bmds))

  for(i in seq(1,nrow(efs))){
    logd(paste0("step ",i," "),efs$name[i])
    addRandColAndOperate(bmds,names(bmds)[col],paste0(efs$name[i],'.rand'),paste0(names(bmds)[col],".",efs$name[i]),efs$mu[i],efs$sigma[i],efs$dist.type[i],efs$operation[i],efs$is.geom[i],efs$above.threshold[i])
    col = col + 2
  }

  return(list('bmds'=bmds,'efs'=efs))
}
