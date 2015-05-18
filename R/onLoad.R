.onAttach <- function(libname, pkgname) {
  #logging::basicConfig('DEBUG')
  lev=logLevel
  logging::logReset()
  logging::setLevel(lev)
  logging::addHandler(logging::writeToConsole,level=lev,formatter=formatter.debug)

  set.seed(seedVal)
}

formatter.debug<-function(record){
  record$msg
}

formatter.fewsdiagnostics <- function(record) {
  if(record$level <= loglevels[['INFO']])
    level <- 3
  else if(record$level <= loglevels[['WARNING']])
    level <- 2
  else if(record$level <= loglevels[['ERROR']])
    level <- 1
  else
    level <- 0

  sprintf('  <line level="%d" description="LizardScripter :: %s :: %s"/>\n', level, record$timestamp, record$msg)
}
