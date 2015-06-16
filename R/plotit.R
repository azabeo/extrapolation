#' @export
plotit <- function(d,xname,islog=FALSE,breaks=100,bar.col='red',line.col='blue',xlab='value',mainlab='Title'){

  # Add a Normal Curve (Thanks to Peter Dalgaard)
  #x <- mtcars$mpg
  x <- d[,eval(xname),with=FALSE][[1]]
  h <- hist(x, breaks=breaks, col=bar.col, xlab=xlab, main=mainlab)

  xfit <- seq(min(x,na.rm = TRUE),max(x,na.rm = TRUE),length=2*breaks)

  #yfit <- dlnorm(xfit,mean=mean(x),sd=sd(x))
  #yfit <- yfit*diff(h$mids[1:2])*length(x)

  if(islog){
    yfit <- dlnorm(xfit, meanlog = mean(log(x),na.rm = TRUE), sdlog = sd(log(x),na.rm = TRUE))
  }else{
    yfit <- dnorm(xfit, mean = mean(x,na.rm = TRUE), sd = sd(x,na.rm = TRUE))
  }

  yfit <- yfit*diff(h$mids[1:2])*length(x)

  lines(xfit, yfit, col=line.col, lwd=2)
}

#' @export
pivot <- function(d,names,by='id'){
  d[, list(factor = names, value = unlist(.SD[,names,with=FALSE], use.names = F)), by = by]
}

#' @export
create.fits <- function(d,names,isLogs,length=100,fit.from.zero=TRUE,remove.outl=TRUE,H.left = 1.5,H.right = 1.5){

  id = numeric()
  x.fit = numeric()
  y.fit = numeric()
  factor = factor()

  for(i in seq(length(names))){
    x <- d[,eval(names[i]),with=FALSE][[1]]

    if(remove.outl){
      x <- remove.outliers(x,H.left,H.right)
    }

    if(fit.from.zero){
      xfit <- seq(0,max(x,na.rm = TRUE),length=length)
    }else{
      xfit <- seq(min(x,na.rm = TRUE),max(x,na.rm = TRUE),length=length)
    }

    if(isLogs[i]){
      yfit <- dlnorm(xfit, meanlog = mean(log(x),na.rm = TRUE), sdlog = sd(log(x),na.rm = TRUE))
    }else{
      yfit <- dnorm(xfit, mean = mean(x,na.rm = TRUE), sd = sd(x,na.rm = TRUE))

    }

    id = c(id,seq(1,length))
    x.fit = c(x.fit,xfit)
    y.fit = c(y.fit,yfit)
    factor = c(factor,rep(names[i],length))
  }

  data.table::data.table(id,x.fit, y.fit, factor)
}

#' @export
create.bins <- function(d,names,length=100,remove.outl=TRUE,H.left = 1.5,H.right = 1.5,precision = 100){

  id = numeric()
  factor = factor()
  count = numeric()
  x = numeric()
  ncount = numeric()
  density = numeric()
  xmin = numeric()
  xmax = numeric()

#   if(is.density){
#     aes = ggplot2::aes(x=orig.data, y=..density.., fill=factor)
#   }else{
#     aes = ggplot2::aes(x=orig.data, fill=factor)
#   }
#
  #aes = ggplot2::aes(x=orig.data, fill=factor)
  aes = ggplot2::aes(x=x.data)

  for(i in seq(length(names))){
    x.data <- d[,eval(names[i]),with=FALSE][[1]]

    if(remove.outl){
      x.data <- remove.outliers(x.data,H.left,H.right)
    }

    range = max(x.data,na.rm = TRUE) - min(x.data,na.rm = TRUE)

    binwidth = (range+(range/precision))/length

    df = data.table::data.table(x.data)

    g = ggplot2::ggplot(data = df) + ggplot2::geom_histogram(aes, binwidth = binwidth)

    dt = data.table::data.table(ggplot2::ggplot_build(g)$data[[1]])

    #dt[,c('fill','y', 'ndensity', 'PANEL', 'group', 'ymin', 'ymax'):=NULL]
    #dt[,c('id','factor'):=list(seq(1,length),rep(names[i],length))]

    id = c(id,seq(1,nrow(dt)))
    factor = c(factor,rep(names[i],nrow(dt)))
    count = c(count,dt[,'count',with=FALSE][[1]])
    x = c(x,dt[,'x',with=FALSE][[1]])
    ncount = c(ncount,dt[,'ncount',with=FALSE][[1]])
    density = c(density,dt[,'density',with=FALSE][[1]])
    xmin = c(xmin,dt[,'xmin',with=FALSE][[1]])
    xmax = c(xmax,dt[,'xmax',with=FALSE][[1]])
  }

  data.table::data.table(id, factor,x,count,ncount,density,xmin,xmax)


#   g = ggplot2::ggplot(data = df) + ggplot2::geom_histogram(aes, binwidth = binwidth)
#
#   ggplot2::ggplot_build(g)$data
}

#' @export
plotMany <- function(d,names,isLogs,fit.from.zero=TRUE,do.plot=TRUE,isHist=TRUE,isFit=FALSE,x.range=NULL,x.range.breaks=NULL,y.range=NULL,remove.outl=TRUE,H.left = 1.5,H.right = 1.5,id='id',breaks=100){

#   col = rainbow(length(names))
#   plot(0,0,xlim = c(0,50),ylim = c(0,1),type = "n")

  hist.data = numeric()
  x.data = numeric()
  y.data = numeric()
  fac = factor()

  rows = nrow(d)

  for(i in seq(length(names))){
    x <- d[,eval(names[i]),with=FALSE][[1]]

    if(remove.outl){
      x <- remove.outliers(x,H.left,H.right)
    }

    if(isFit){
      if(fit.from.zero){
        xfit <- seq(0,max(x,na.rm = TRUE),length=rows)
      }else{
        xfit <- seq(min(x,na.rm = TRUE),max(x,na.rm = TRUE),length=rows)
      }

      if(isLogs[i]){
        yfit <- dlnorm(xfit, meanlog = mean(log(x),na.rm = TRUE), sdlog = sd(log(x),na.rm = TRUE))
        logd('MEAN',mean(log(x),na.rm = TRUE))
        logd('SD',sd(log(x),na.rm = TRUE))
      }else{
        yfit <- dnorm(xfit, mean = mean(x,na.rm = TRUE), sd = sd(x,na.rm = TRUE))

      }
    }else{
      xfit = rep(NA,rows)
      yfit = rep(NA,rows)
    }

    orig.data = c(hist.data,x)
    x.data = c(x.data,xfit)
    y.data = c(y.data,yfit)
    fac = c(fac,rep(names[i],rows))
    #lines(xfit, yfit, col=col[i], lwd=2)
  }

  df = data.table::data.table(orig.data, x.data, y.data, fac)

  if(do.plot){

    g = ggplot2::ggplot(data = df)

    if(isHist){
      if(!is.null(x.range.breaks)){
        binwidth = (x.range[2] - x.range[1])/x.range.breaks
      }else{
        binwidth = (max(x,na.rm = TRUE) - min(x,na.rm = TRUE))/breaks
      }

      g = g + ggplot2::geom_histogram(ggplot2::aes(x=orig.data, y=..density.., fill=fac),binwidth=binwidth , alpha=.5, position="identity")
    }

    if(isFit){
      g = g + ggplot2::geom_line(ggplot2::aes(x=x.data, y=y.data, colour=fac))
    }

    if(!is.null(x.range)){
      g= g + ggplot2::coord_cartesian(xlim = x.range)
    }
    if(!is.null(y.range)){
      g= g + ggplot2::coord_cartesian(ylim = y.range)
    }
    if(!is.null(x.range) & !is.null(y.range)){
      g= g + ggplot2::coord_cartesian(xlim = x.range,ylim = y.range)
    }

    plot(g)
  }

  return(df)

#   dat <- d[, list(variable = names, value = unlist(.SD[,names,with=FALSE], use.names = F)), by = id]
#   dat[,fac:=as.factor(variable)]
#   ggplot(dat, aes(x=value, fill=fac)) + geom_histogram(binwidth=.5, alpha=.5, position="identity")

#   df1<-data.frame(x=1:10,y=rnorm(10))
#   df2<-data.frame(x=1:10,y=rnorm(10))
#
#   y.name = 'prob'
#   p <- ggplot()
#
#   for(name in names){
#
#     p <- p +
#       geom_line(data = d, aes_string(x = name, y = y.name, color = name)) +
#       geom_line(data = jobsAFAM2, aes(x = data_date, y = Percent.Change, color = "blue"))  +
#       xlab('data_date') +
#       ylab('percent.change')
#
#     ggplot(df1, aes(x,y))+geom_line(aes(color="First line"))+
#       geom_line(data=df2,aes(color="Second line"))+
#       labs(color="Legend text")
#   }
}

# x <- rlnorm(1000, 1, 1)  # for example
# r <- range(x)
# d <- dlnorm(r[1]:r[2], meanlog = mean(log(x)), sdlog = sd(log(x)))
# hist(x, prob = TRUE, ylim = range(d))
# lines(r[1]:r[2], d, col="red")
