#' loadData
#'
#' loads necessary data from files
#'
#' loads necessary data from files
#'
#' @param file.name string. name of csv file to load
#' @param ids vector. vector of column names to be used as ids
#' @param var.name string. name of the variable where to load the data in the main workspace
#' @return nothing (creates variables with provided names)
#' @author Alex Zabeo
#' @examples
#' loadData("data.csv",c('id'),'dat')
#' @export
loadData <- function(file.name="data.csv",ids=c('id'),var.name=NULL){
  if(!is.null(var.name)){
    do.call("<<-",list(var.name, loadTable(file.name,ids)))
  }
  loadTable(file.name,ids)
  #e <<- loadTable("inst/exposure.csv",c('id'),colClasses=c('numeric',"character",rep("numeric",2)))
}


loadTable<-function(data="data.csv",keys=c('id'),colClasses=NULL){

  if(is.null(colClasses)){
    dat <- read.table(data,header=TRUE,sep=",",stringsAsFactors=F)
  }else{
    dat <- read.table(data,header=TRUE,sep=",",stringsAsFactors=F,colClasses = colClasses)
  }

  dat <- data.table::as.data.table(dat)
  data.table::setkeyv(dat,keys)

  return(dat)
}
