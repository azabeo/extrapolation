logd<-function(name,var=NULL,level='DEBUG'){
  l<-switch(level,
            DEBUG = logging::logdebug,
            INFO = logging::loginfo)

  l(paste0("- ",name))
  if(!(is.null(var))){
    l(paste0("- ",capture.output(var)))
  }
  l("\n")
}
