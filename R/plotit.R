#' @export
plotit <- function(d,xname,islog=FALSE,breaks=100,bar.col='red',line.col='blue',xlab='value',mainlab='Title'){

  # Add a Normal Curve (Thanks to Peter Dalgaard)
  #x <- mtcars$mpg
  x <- d[,eval(xname),with=FALSE][[1]]
  h <- hist(x, breaks=breaks, col=bar.col, xlab=xlab, main=mainlab)

  xfit <- seq(min(x),max(x),length=2*breaks)

  #yfit <- dlnorm(xfit,mean=mean(x),sd=sd(x))
  #yfit <- yfit*diff(h$mids[1:2])*length(x)

  if(islog){
    yfit <- dlnorm(xfit, meanlog = mean(log(x)), sdlog = sd(log(x)))
  }else{
    yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
  }

  yfit <- yfit*diff(h$mids[1:2])*length(x)

  lines(xfit, yfit, col=line.col, lwd=2)
}

# x <- rlnorm(1000, 1, 1)  # for example
# r <- range(x)
# d <- dlnorm(r[1]:r[2], meanlog = mean(log(x)), sdlog = sd(log(x)))
# hist(x, prob = TRUE, ylim = range(d))
# lines(r[1]:r[2], d, col="red")
