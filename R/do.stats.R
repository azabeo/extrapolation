#' @export
do.stats <- function(d){
  names = names(d)
  funcs = c('median','mean','sd','min','max','geomean','geosd','count.leq.1','perc.leq.1','count.leq.0','lognorm.q5')
  first = TRUE
  for (f in funcs){
    if(first){
      r <- d[,lapply(.SD,f,na.rm=TRUE),.SDcols=names]
      first = FALSE
    }else{
      r <- rbind(r,d[,lapply(.SD,f,na.rm=TRUE),.SDcols=names])
    }
  }
  r[,stat:=funcs]
  setcolorder(r,c('stat',names))
  return (r)
}

count.leq.1 <- function(x, na.rm=FALSE){
  if(na.rm){
    length(x[!is.na(x) & x<=1])
  }else{
    length(x[x<=1])
  }

}

perc.leq.1 <- function(x, na.rm=FALSE){
  if(na.rm){
    length(x[!is.na(x) & x<=1])/length(x)
  }else{
    length(x[x<=1])/length(x)
  }
}

count.leq.0 <- function(x, na.rm=FALSE){
  if(na.rm){
    length(x[!is.na(x) & x<=0])
  }else{
    length(x[x<=0])
  }
}

lognorm.q5 <- function(x, na.rm=FALSE){
  qlnorm(0.05,mean(log(x[x > 0]), na.rm=na.rm),sd(log(x[x > 0]), na.rm=na.rm))
}
