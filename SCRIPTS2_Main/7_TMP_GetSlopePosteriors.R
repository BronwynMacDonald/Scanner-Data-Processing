# this is a temporary script to generate a detailed output of 
# the posterior distribution of the slope calculation
# For now, just doing last year of Ok Ck, for Brooke's FSAR work








library(WSPMetrics)

trend.series <- WSPMetrics::smoothSeries(vec.in = cu.series,gen = cu.avggen,
                                         filter.sides=cu.slope.specs$filter.sides,
                                         log.transform = cu.slope.specs$log.transform,
                                         out.exp = cu.slope.specs$out.exp,
                                         na.rm=cu.slope.specs$na.rm)



if(!cu.slope.specs$slope.smooth){trend.series <- cu.series}

if(FALSE){ # for debug only
  test.out  <- WSPMetrics::calcPercChangeMCMC(vec.in = log(tail(trend.series,12)),
                                              method = "jags",
                                              model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                                              perc.change.bm = -25,
                                              na.skip = FALSE,
                                              out.type = "short",
                                              mcmc.plots = FALSE,
                                              convergence.check = FALSE, # ??Conv check crashes on ts() ??? -> change to Rhat check
                                              logged = TRUE
  )
  
  test.out
}