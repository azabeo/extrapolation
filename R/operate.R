##' @import data.table
addRandColAndOperate <- function(d,col,randcol,newcol,mean,sd,rand.f,operator='mult',dolog=FALSE,above.threshold=NA){

  addRandCol(d,randcol,mean,sd,rand.f,dolog,above.threshold)

  operate(d,col,randcol,operator,newcol)

#   switch (operator,
#     div = d[,eval(newcol):= get(col) / get(randcol)],
#     mult = d[,eval(newcol):= get(col) * get(randcol)],
#     sum = d[,eval(newcol):= get(col) + get(randcol)]
#   )

}

operate <- function(d,operand1,operand2,operator,result){
  switch (operator,
          div = d[,eval(result):= get(operand1) / get(operand2)],
          mult = d[,eval(result):= get(operand1) * get(operand2)],
          sum = d[,eval(result):= get(operand1) + get(operand2)],
          sub = d[,eval(result):= get(operand1) - get(operand2)]
  )
}

#' @import data.table
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
