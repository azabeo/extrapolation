#' @export
export <- function(d,name=NULL,dir='export/'){
  if(is.null(name)){
    name = deparse(substitute(d))
  }
  name = paste(dir,name,'.txt',sep="")
  logd('exporting: ',name)
  write.table(d, name, sep="\t", row.names = FALSE)
}


# -------- NOT working as plotting plots instead of ggplots --------------

print.export <- function(file.name,p,winch=3,hinch=3,ppi=300){
  logd('Saving: ',file.name)
  #   png(file.name, width=inch*ppi, height=inch*ppi, res=ppi)
  #   print(p)
  #   dev.off()
  ggsave(plot=p, file=file.name, units="in", width=winch, height=hinch, dpi=ppi)
}


export.images <- function(r,bmds,effs,exps,exp.types,bounds,lengths,winch=3,hinch=3,ppi=300,dir='export'){

  for(i in 1:length(bmds)){

    #p <- plotit(res,fittings,where,"C0","rcf","k",print = FALSE)
    p <- plotall(r,bmds[i],effs[i],exps[i],exp.types[i],c(bounds[(i*2)-1],bounds[(i*2)]),lengths[i])
    file.name = exps[i]

    print.export(file.name,p,inch,ppi)
  }

}
