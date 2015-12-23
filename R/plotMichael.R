#' @export
plotMichael <- function(r){
  bmds=c("bmd.distrib","bmd.distrib")
  effs=c("eff","eff")
  exps=c("Near.Manufacturer.Manual.Loading.Trays.In.Booth.rand","Near.Manufacturer.Dumping.Into.Mixing.Tank.rand")
  exp.types=c("fixed","fixed")
  bounds=c(c(-2,8),c(-2,8))
  lengths=c(10000,10000)

  export.images(r,bmds,effs,exps,exp.types,bounds,lengths,winch=3,hinch=3)
}

#' @export
callit <- function(export.dir=NULL,to.do=c('exp','eff','risk','all')){
  # plot exposures
  if('exp' %in% to.do){
    exposures = c("Near.Laser.Ablation.MARINA.Data.rand","Near.Manufacturer.Manual.Loading.Trays.In.Booth.rand","Near.Manufacturer.Dumping.Into.Mixing.Tank.rand","Near.Lab.Transfer.During.Weighing.and.Solution.Prep.rand","Near.Lab.Creating.Stock.Solutions.Fume.Hood.rand","Near.Dumping.Large.Amount.of.Poweder.In.Vessel.rand","Near.Bag.Bin.Filling.rand","Near.Laser.Ablation.rand","Near.Weighing.of.Powder.rand","Far.Laser.Ablation.MARINA.Data.rand","Far.Manufacturer.Manual.Loading.Trays.In.Booth.rand","Far.Manufacturer.Dumping.Into.Mixing.Tank.rand","Far.Lab.Transfer.During.Weighing.and.Solution.Prep.rand","Far.Lab.Creating.Stock.Solutions.Fume.Hood.rand","Far.Dumping.Large.Amount.of.Poweder.In.Vessel.rand","Far.Bag.Bin.Filling.rand","Far.Laser.Ablation.rand","Far.Weighing.of.Powder.rand")
    exp.names = c("ES1 NEAR","ES2 NEAR","ES3 NEAR","ES4 NEAR","ES5 NEAR","ES6 NEAR","ES7 NEAR","ES8 NEAR","ES9 NEAR","ES1 FAR","ES2 FAR","ES3 FAR","ES4 FAR","ES5 FAR","ES6 FAR","ES7 FAR","ES8 FAR","ES9 FAR")
    #logged originals = c(-13.12236338,-1.280134165,-0.113168698,-7.20212634,-13.12236338,3.589059119,-0.192371893,-9.18078157,-13.81551056,-13.81551056,-2.419118909,-1.350927217,-8.435613204,-13.81551056,2.468099531,-1.832581464,-10.63745673,-15.01948336)
    originals=c(0.000002,0.278,0.893,0.000745,0.000002,36.2,0.825,0.000103,0.000001,0.000001,0.089,0.259,0.000217,0.000001,11.8,0.16,0.000024,0.0000003)

    for(i in 1:length(exposures)){
      data.names = c(exp.names[i])
      data = list(res1$bmds[[exposures[i]]])
      data.dist.types = c('lnorm')
      data.do.log = c(FALSE)
      vlines = c(originals[i])
      vlines.names = c('Model')
#     vlines=NULL
#     vlines.names=NULL

      plotGroup(data, data.names, data.dist.types, data.do.log, vlines, vlines.names, NULL,'Concentration','Density',export.dir)
    }
  }

  # plot effects
  if('eff' %in% to.do){
    data.names = c('bmd.distrib')
    data = list(res1$bmds[[data.names[1]]])
    data.dist.types = c('lnorm')
    data.do.log = c(FALSE)
    vlines = c(2)
    vlines.names = c('NOAEL')
    title = 'Effect (without EF)'

    plotGroup(data, data.names, data.dist.types, data.do.log, vlines, vlines.names, title=title, xaxis.name='Concentration', yaxis.name='Density',export.dir=export.dir)
  }

  # plot risks with NOAEL + NIOSH
  if('risk' %in% to.do){
    risks = c("Risk.Near.Laser.Ablation.MARINA.Data","Risk.Near.Manufacturer.Manual.Loading.Trays.In.Booth","Risk.Near.Manufacturer.Dumping.Into.Mixing.Tank","Risk.Near.Lab.Transfer.During.Weighing.and.Solution.Prep","Risk.Near.Lab.Creating.Stock.Solutions.Fume.Hood","Risk.Near.Dumping.Large.Amount.of.Poweder.In.Vessel","Risk.Near.Bag.Bin.Filling","Risk.Near.Laser.Ablation","Risk.Near.Weighing.of.Powder","Risk.Far.Laser.Ablation.MARINA.Data","Risk.Far.Manufacturer.Manual.Loading.Trays.In.Booth","Risk.Far.Manufacturer.Dumping.Into.Mixing.Tank","Risk.Far.Lab.Transfer.During.Weighing.and.Solution.Prep","Risk.Far.Lab.Creating.Stock.Solutions.Fume.Hood","Risk.Far.Dumping.Large.Amount.of.Poweder.In.Vessel","Risk.Far.Bag.Bin.Filling","Risk.Far.Laser.Ablation","Risk.Far.Weighing.of.Powder")
    risks.names = c("RISK ES1 NEAR","RISK ES2 NEAR","RISK ES3 NEAR","RISK ES4 NEAR","RISK ES5 NEAR","RISK ES6 NEAR","RISK ES7 NEAR","RISK ES8 NEAR","RISK ES9 NEAR","RISK ES1 FAR","RISK ES2 FAR","RISK ES3 FAR","RISK ES4 FAR","RISK ES5 FAR","RISK ES6 FAR","RISK ES7 FAR","RISK ES8 FAR","RISK ES9 FAR")

    for(i in 1:length(risks)){
      data.names = c(risks.names[i])
      data = list(res1$bmds[[risks[i]]])
      data.dist.types = c('lnorm')
      data.do.log = c(FALSE)
      #vlines = c(0.004,0.02,1)
      #vlines.names = c('Niosh','DNEL','Risk')
      vlines=NULL
      vlines.names=NULL

      #plotGroup(data, data.names, data.dist.types, vlines, vlines.names, NULL,'Concentration','Density', 'export')
      plotGroup(data, data.names, data.dist.types, data.do.log, vlines, vlines.names, NULL,'Concentration','Density',export.dir)
    }
  }

  # plot all together
  if('all' %in% to.do){
    for(i in 1:length(exposures)){
      data.names = c(exp.names[i],'eff', risks.names[i])
      data = list(res1$bmds[[exposures[i]]], res1$bmds[['eff']], res1$bmds[[risks[i]]])
      data.dist.types = c('lnorm','lnorm','lnorm')
      data.do.log = c(TRUE,TRUE,TRUE)
      vlines = c(log(0.004/1000),log(2/1000),0)
      vlines.names = c('Niosh','NOAEL','Risk')
      title = paste0(risks.names[i],' overlay')

      #plotGroup(data, data.names, data.dist.types, vlines, vlines.names, NULL,'Concentration','Density', 'export')
      plotGroup(data, data.names, data.dist.types, data.do.log, vlines, vlines.names, title,'Concentration','Density',export.dir)
    }
  }

}


plotGroup <- function(data, data.names, data.dist.types, data.do.log, vlines=c(), vlines.names=c(), title=NULL, xaxis.name=NULL, yaxis.name=NULL, export.dir=NULL, export.width=1024, export.height=768, bounds=NULL,length=10000,minq=0.001,maxq=0.999,colors=c('purple','red','green','blue','orange','black')){

  for(i in 1:length(data.dist.types)){
    if(data.dist.types[i] == 'norm' && data.do.log[i]){
      return('ERROR - not possible to plot log of normal')
    }
    if(data.dist.types[i] == 'lnorm' && data.do.log[i]){
      data.dist.types[i] = 'norm'
    }
  }

  if(is.null(title)){
    title = data.names[1]
  }

  data.out = list()
  data.log = list()

  data.mean = c()
  data.sd = c()
  data.min = c()
  data.max = c()
  data.y.max = c()

  for( i in 1:length(data)){
    data.out <- add(data.out, data.names[i], remove.outliers(data[[i]],1.5,1.5))
    if(data.do.log[i]){
      data.log <- add(data.log, data.names[i], log(data.out[[i]]))
    }else{
      data.log <- add(data.log, data.names[i], data.out[[i]])
    }

    if(data.dist.types[i] == 'norm'){
      data.mean[i] <- mean(data.log[[i]],na.rm = TRUE)
      data.sd[i] <- sd(data.log[[i]],na.rm = TRUE)
      data.min[i] <- qnorm(minq,data.mean[i],data.sd[i])
      data.max[i] <- qnorm(maxq,data.mean[i],data.sd[i])
    }else{
      data.mean[i] <- log(geomean(data.log[[i]],na.rm = TRUE))
      data.sd[i] <- log(geosd(data.log[[i]],na.rm = TRUE))
      data.min[i] <- qlnorm(minq,data.mean[i],data.sd[i])
      data.max[i] <- qlnorm(maxq,data.mean[i],data.sd[i])
    }

  }

  global.min = min(data.min,vlines,na.rm = TRUE)
  global.max = max(data.max,vlines,na.rm = TRUE)

  if(is.null(bounds)){
    x <- seq(global.min,global.max,length.out = length)
  }else{
    x <- seq(bounds[1],bounds[2],length.out = length)
  }

  # maximum density of a normal corresponds to density of the mean, for lognormal is density of mode = e^(mu-sigma^2)
  max.density = NULL
  for( i in 1:length(data.dist.types)){
    if(data.dist.types[i] == 'norm'){
      max.density = max(max.density,dnorm(data.mean[i],data.mean[i],data.sd[i]))
    }else{
      max.density = max(max.density, dlnorm( exp(data.mean[i]-data.sd[i]^2) ,data.mean[i],data.sd[i]) )
    }
  }

  bounds.y = c(0,max.density)

  data.dens = list()

  for( i in 1:length(data)){
    if(data.dist.types[i] == 'norm'){
      data.dens <- add(data.dens, data.names[i], dnorm(x, data.mean[i], data.sd[i]))
    }else{
      data.dens <- add(data.dens, data.names[i], dlnorm(x, data.mean[i], data.sd[i]))
    }

    if(i==1){
      plot(x, data.dens[[i]], type = "l", ylim = bounds.y, xlab=xaxis.name, ylab=yaxis.name, col = colors[1+((i-1) %% length(colors))])
    }else{
      lines(x, data.dens[[i]], col = colors[1+((i-1) %% length(colors))])
    }
  }

  for( i in 1:length(vlines)){
    abline(v = vlines[i], col = colors[1+(((i-1)+length(data)) %% length(colors))], lty = 2)
  }

  title(main = title)

  legend(
    'topright', c(data.names,vlines.names), lty = 1, col = colors, bty = 'n', cex = .75
  )
  grid()

  if(!is.null(export.dir)){
    dev.copy(device = png, filename = paste0(export.dir,'/',title,'.png'), width = export.width, height = export.height)
    dev.off()
  }


  return (data.dens)
}
