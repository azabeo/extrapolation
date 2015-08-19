#' @export
plotMichael <- function(r){
  bmds=c("bmd.distrib","bmd.distrib")
  effs=c("eff","eff")
  exps=c("Near.Manufacturer.Manual.Loading.Trays.In.Booth.rand","Near.Manufacturer.Dumping.Into.Mixing.Tank.rand")
  exp.types=c("fixed","fixed")
  bounds=c(c(-2,8),c(-2,8))
  lengths=c(10000,10000)

  export.images(r,bmds,effs,exps,exp.types,bounds,lengths,winch=3,hinch=3)
}
