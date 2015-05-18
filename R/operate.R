##' @import data.table
addRandColAndOperate <- function(d,col,randcol,newcol,mean,sd,rand.f,operator='mult',dolog=FALSE,above.threshold=NA){

  addRandCol(d,randcol,mean,sd,rand.f,dolog,above.threshold)

  switch (operator,
    div = d[,eval(newcol):= get(col) / get(randcol)],
    mult = d[,eval(newcol):= get(col) * get(randcol)],
    sum = d[,eval(newcol):= get(col) + get(randcol)]
  )
#
#   if(operator=='div'){
#     d[,eval(newcol):= get(col)/get(randcol)]
#   }else{
#     d[,eval(newcol):= get(col)*get(randcol)]
#   }

}

addRandCol <- function(d,randcol,mean,sd,rand.f,dolog=FALSE,above.threshold=NA){
  if(dolog){
    l <- do.call(rand.f,list(nrow(d), log(mean), log(sd)))
    # l <- rand.f(nrow(d), log(mean), log(sd))
  }else{
    l <- do.call(rand.f,list(nrow(d), mean, sd))
    # l <- rand.f(nrow(d), mean, sd)
  }

  d[, eval(randcol):=l]

  if(!is.na(above.threshold)){
    d[get(randcol) <= above.threshold,eval(randcol):=NA]
  }

}
