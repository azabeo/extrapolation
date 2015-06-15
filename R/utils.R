#' @export
remove.outliers <- function(x, H.left = 1.5,H.right = 1.5, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H.l <- H.left * IQR(x, na.rm = na.rm)
  H.r <- H.right * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H.l)] <- NA
  y[x > (qnt[2] + H.r)] <- NA
  y
}

#' decimals
#'
#' rounds number to a specific decimal and converts to string
#'
#' rounds number to a specific decimal and converts to string
#'
#' @param x numeric. Number to be rounded
#' @param k numeric. Decimal positions
#' @return string of rounded x with k decimals
#' @author Alex Zabeo
#' @examples
#' decimals(84.05041,2)
#' @export
decimals <- function(x, k) format(round(x, k), nsmall=k)

#' rpm_to_rcf
#'
#' converts rpm to rcf
#'
#' converts rpm to rcf
#'
#' @param rpm numeric.
#' @param const numeric. Conversion constant
#' @param dist numeric. Distance from top of couvette
#' @return rpf
#' @author Alex Zabeo
#' @examples
#' rpm_to_rcf(800)
#' @export
rpm_to_rcf<-function(rpm,const=1.1179,dist=117.4781){const*10^(-6)*rpm^2*dist}

#' add
#'
#' adds elements to lists
#'
#' adds elements to lists at the end. If a list is passed to be added it is not flattened (i.e. it remains a list
#' by its own)
#'
#' @param l list. list to add to, *** it must have named elements ***
#' @param name string. Name of the new element
#' @param x. Element to be added (can also be a list)
#' @return new list with added item
#' @author Alex Zabeo
#' @examples
#' add(list(a=1,b=2),'sublist',list(3,4))
add<-function(l,name,x){
  names<-c(names(l),name)
  l[[length(l)+1]]<-x
  names(l)=names
  return(l)
}

#' geomean
#'
#' geometric mean
#'
#' evaluates geometric mean
#'
#' @param x array. numbers to which calculate the geom mean
#' @param na.rm Bool. remove na or not
#' @return geometric mean
#' @author Alex Zabeo
#' @examples
#' geomean(x)
geomean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#' geosd
#'
#' geometric standard deviaiton
#'
#' evaluates geometric standard deviaiton
#'
#' @param x array. numbers to which calculate the geom sd
#' @param na.rm Bool. remove na or not
#' @return geometric standard deviaiton
#' @author Alex Zabeo
#' @examples
#' geosd(x)
geosd <- function(x, na.rm = FALSE)
{
  exp(sd(log(x[x > 0]), na.rm = na.rm))
}

# #' f_mean
# #'
# #' mean of a function
# #'
# #' Calculates numerically the mean of a function ina given interval using a given number of bins
# #'
# #' @param func function. Function to be averaged
# #' @param interval vector.
# #' @param n number. Number of bins (should be >= 5000)
# #' @return mean of the function
# #' @author Alex Zabeo
# #' @examples
# #' f_mean()
# f_mean = function(func, interval, n = 5000) {
#   interval_samples = seq(interval[1], interval[2], length = n)
#   function_values = sapply(interval_samples, func)
#   return(c(mean=mean(function_values),sd=sd(function_values)))
# }
#
# #' f_mean
# #'
# #' mean of a function
# #'
# #' Calculates numerically the mean of a function ina given interval using a given number of bins
# #'
# #' @param func function. Function to be averaged
# #' @param interval vector.
# #' @param n number. Number of bins (should be >= 5000)
# #' @return mean of the function
# #' @author Alex Zabeo
# #' @examples
# #' f_mean()
# f_root <- function(func, y, interval, tol = 0.1){
#   library(rootSolve)
#   f <- function(x){ func(x) - y }
#   ret <- NA
#   tryCatch(ret <- uniroot.all(f,interval = interval, tol = tol), error=function(e) {print(e)})
#   return(max(ret))
# }

# # prepares data for betareg, going from [0,1] to (0,1) and back
# prepare<-function(data,y){
#   n=nrow(data)
#   data[y] = (data[y]*(n-1)+0.5)/n
#   return (data)
# }
#
# restore<-function(data,y){
#   n=nrow(data)
#   data[y] = (n*data[y]-0.5)/(n-1)
#   return (data)
# }

# loadRequired = function(pack){
#
#   if(require(pack)){
#     print(paste(pack,"is loaded correctly"))
#   } else {
#     print(paste("trying to install",pack))
#     install.packages(pack)
#     if(require(pack)){
#       print(paste(pack,"installed and loaded"))
#     } else {
#       stop(paste("could not install",pack))
#     }
#   }
#
# #   if(require("lme4")){
# #     print("lme4 is loaded correctly")
# #   } else {
# #     print("trying to install lme4")
# #     install.packages("lme4")
# #     if(require(lme4)){
# #       print("lme4 installed and loaded")
# #     } else {
# #       stop("could not install lme4")
# #     }
# #   }
# }

# log<-function(x){
#   if(exists("trace")){
#     print(x)
#   }
# }
