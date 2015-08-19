#' @export
plotallraw <- function(r,bmd,eff,exp,title=NULL,exp.type="norm",bounds=c(-5,15),length=10000){
  iced.orig = r$bmds[[eff]]
  iexp.orig = r$bmds[[exp]]

  imoe.orig = iced.orig / iexp.orig
  ced.orig = r$bmds[[bmd]]
  ced.orig = ced.orig / iexp.orig

  if(is.null(title)){
    title=exp
  }

  iced.out <- remove.outliers(iced.orig,1.5,1.5)
  iexp.out <- remove.outliers(iexp.orig,1.5,1.5)
  imoe.out <- remove.outliers(imoe.orig,1.5,1.5)
  ced.out <- remove.outliers(ced.orig,1.5,1.5)

  iced <- log(iced.out)
  iexp <- log(iexp.out)
  #iexp <- iexp.out
  imoe <- log(imoe.out)
  ced <- log(ced.out)

  plotall(r,ced,iced,imoe,iexp,title,exp.type,bounds,length)
}

#' @export
plotalllogged <- function(r,iced,iexp,imoe,ced_exp,title=NULL,exp.type="norm",bounds=c(-5,15),length=10000){
  iced.orig = r[[iced]]
  iexp.orig = r[[iexp]]

  imoe.orig = r[[imoe]]
  ced.orig = r[[ced_exp]]

  if(is.null(title)){
    title=iexp
  }

  iced.out <- remove.outliers(iced.orig,1.5,1.5)
  iexp.out <- remove.outliers(iexp.orig,1.5,1.5)
  imoe.out <- remove.outliers(imoe.orig,1.5,1.5)
  ced.out <- remove.outliers(ced.orig,1.5,1.5)

  plotall(r,ced.out,iced.out,imoe.out,iexp.out,title,exp.type,bounds,length)
}

#' plot.all
#'
#' plot all together
#'
#' plots bmd, effect and exposure all together in log scale
#'
#' @param r list. result of extrapolation()
#' @param bmd string. Name of the column containing the original BMD bootstrap data
#' @param eff string. Name of the column containing the calculated effect threshold
#' @param exp string. Name of the column containing the calculated exposure value
#' @param exp.type c(norm,fixed). type of distribution for exposure, normal or fixed value
#' @return nothing, just plots
#' @author Alex Zabeo
#' @examples
#' plot.all(r,bmd,eff,exp)
#'
#' @export
plotall <- function(r,ced,iced,imoe,iexp,title,exp.type="norm",bounds=c(-5,15),length=10000)
{

  iced.mn = mean(iced,na.rm = TRUE)
  iced.sd = sd(iced,na.rm = TRUE)

  #----- the log od a normal is a left skewed which I don't know
  iexp.mn = mean(iexp,na.rm = TRUE)
  iexp.sd = sd(iexp,na.rm = TRUE)
  #-----

  imoe.mn = mean(imoe,na.rm = TRUE)
  imoe.sd = sd(imoe,na.rm = TRUE)

  ced.mn = mean(ced,na.rm = TRUE)
  ced.sd = sd(ced,na.rm = TRUE)

  #------------

  iced.min = min(iced,na.rm = TRUE)
  iced.max = max(iced,na.rm = TRUE)

  iexp.min = min(iexp,na.rm = TRUE)
  iexp.max = max(iexp,na.rm = TRUE)

  imoe.min = min(imoe,na.rm = TRUE)
  imoe.max = max(imoe,na.rm = TRUE)

  ced.min = min(ced,na.rm = TRUE)
  ced.max = max(ced,na.rm = TRUE)

  global.min = min(iced.min,iexp.min,imoe.min,ced.min)
  global.max = max(iced.max,iexp.max,imoe.max,ced.max)

  if(is.null(bounds)){
    x <- seq(global.min,global.max,length=length)
  }else{
    x <- seq(bounds[1],bounds[2],length = length)
  }

#   y.global.iced <- dlnorm(x, meanlog = iced.mn, sdlog = iced.sd)
#   y.global.iexp <- dnorm(x, mean = iexp.mn, sd = iexp.sd)
#   y.global.imoe <- dlnorm(x, meanlog = imoe.mn, sdlog = imoe.sd)
#   y.global.ced <- dlnorm(x, meanlog = ced.mn, sdlog = ced.sd)

  y.global.iced <- dnorm(x, mean = iced.mn, sd = iced.sd)
  # --- left skewed not norm
  y.global.iexp <- dnorm(x, mean = iexp.mn, sd = iexp.sd)
  # ---
  y.global.imoe <- dnorm(x, mean = imoe.mn, sd = imoe.sd)
  y.global.ced <- dnorm(x, mean = ced.mn, sd = ced.sd)

  max.y = max(y.global.iced, y.global.iexp, y.global.imoe, y.global.ced)
  bounds.y = c(0,max.y)

  # plot(x,y.global.iced,type="l",col="red")
  # lines(x,y.global.iexp,col="green")

  #plot(x,y.global.ced,type = "l",log="x",ylim=bounds.y,col = "purple")
  plot(x,y.global.ced,type = "l",ylim=bounds.y,col = "purple")
  lines(x,y.global.iced,col = "red")
  lines(x,y.global.imoe,col = "blue")
  if(exp.type=="norm"){
    lines(x,y.global.iexp,col = "green")
  }else{
    abline(v=iexp[1],col = "green")
  }
  abline(v=0,col="red",lty=2)

  title(main = title)

  legend(
    'topright', c('CEDa / IEXP','ICED','IMoE','IEXP') ,
    lty = 1, col = c('purple', 'red', 'blue','green'), bty = 'n', cex =
      .75
  )

  if(exp.type=="norm"){
    x.iexp <- seq(iexp.min,iexp.max,length = length)
    y.iexp <- dnorm(x.iexp, mean = iexp.mn, sd = iexp.sd)
    plot(x.iexp,y.iexp,type = "l",col = "green")
  }

  x.iced <- seq(iced.min,iced.max,length = length)
  x.imoe <- seq(imoe.min,imoe.max,length = length)
  x.ced <- seq(ced.min,ced.max,length = length)

  y.iced <- dnorm(x.iced, mean = iced.mn, sd = iced.sd)
  y.imoe <- dnorm(x.imoe, mean = imoe.mn, sd = imoe.sd)
  y.ced <- dnorm(x.ced, mean = ced.mn, sd = ced.sd)

  plot(x.iced,y.iced,type = "l",col = "red")
  plot(x.imoe,y.imoe,type = "l",col = "blue")
  plot(x.ced,y.ced,type = "l",col = "purple")
}

