#' @export
export <- function(d,name=NULL,dir='export/'){
  if(is.null(name)){
    name = deparse(substitute(d))
  }
  name = paste(dir,name,'.txt',sep="")
  logd('exporting: ',name)
  write.table(d, name, sep="\t", row.names = FALSE)
}
