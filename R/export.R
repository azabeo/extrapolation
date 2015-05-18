#' @export
export <- function(d,name='results',dir='export/'){
  name = paste(dir,name,'.txt',sep="")
  logd('exporting: ',name)
  write.table(d, name, sep="\t", row.names = FALSE)
}
