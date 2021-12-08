waldian_t_test <- function(A, B, design, h1,
                           prior, mu, gamma, kappa, sigma, tau2,
                           info, t, nx, ny, 
                           mx, my, sx, sy, rxy,
                           ci){

  type <- switch(h1, 
                 "d_0" = "two-sided", 
                 "d_plus" = "plus-sided", 
                 "d_min" = "min-sided")
  
  ci <- ci/100
  
  if(info == "means"){
    if(design == "one"){
      t <- mx/sx * sqrt(nx)
    }else if(design == "two"){
      sp <- sqrt(((nx - 1)*sx^2 + (ny - 1)*sy^2)/(nx + ny - 2))
      t <- (mx - my)/sp * sqrt(nx * ny / (nx + ny))
    }else{
      t <- (mx - my)/sqrt(sx^2 + sy^2 - 2*rxy*sx*sy) * sqrt(nx)
    }
  }
  
  if(design == "two"){
    independent <- TRUE
  }else{
    independent <- FALSE
  }
  
  
  if (prior %in% c("cauchy", "t")) {
    
    bflist <- bf10_t(t = t, n1 = nx, n2 = ny, 
                     independentSamples = independent,
                     prior.location = mu, prior.scale = gamma,
                     prior.df = kappa)
    post <- ciPlusMedian_t(t,
                           nx,
                           ny,
                           independent,
                           mu,
                           gamma,
                           kappa,
                           ci,
                           type)
    
    
  } else if (prior == "normal"){
    bflist <- bf10_normal(t = t, n1 = nx, n2 = ny, 
                          independentSamples = independent,
                          prior.mean = mu, prior.variance = sigma)
    
    post <- ciPlusMedian_normal(t,
                                nx,
                                ny,
                                independent,
                                mu,
                                sigma,
                                ci,
                                type)
  } else {
    
    bflist <- bf10_nap(t = t, n1 = nx, n2 = ny, 
                          independentSamples = independent,
                          tau2 = tau2)
    
    post <- ciPlusMedian_nap(t,
                                nx,
                                ny,
                                independent,
                                tau2,
                                ci,
                                type)
    
  }
  
  bf <- switch(h1,
               "d_0" = bflist[[1]],
               "d_plus" = bflist[[2]],
               "d_min" = bflist[[3]])
  
  dec <- "continue sampling"
  
  if(!is.na(bf)){
    if(bf >= A) 
      dec <- "accept H1"
    else if(bf <= B)
      dec <- "accept H0"
  }
  
  if(design == "two"){
    
    cohen <- ci.smd(ncp = t, n.1 = nx, n.2 = ny,
                    conf.level = ci)
    
    out <- data.frame(n1 = nx,
                      n2 = ny,
                      t = t,
                      bf = bf,
                      dec = dec,
                      post_d = post$median,
                      post_ll = post$ciLower,
                      post_ul = post$ciUpper,
                      cohen_d = cohen$smd[[1]],
                      cohen_ll = cohen$Lower.Conf.Limit.smd,
                      cohen_ul = cohen$Upper.Conf.Limit.smd)
    
  }else{
    
    cohen <- quietly(ci.sm)(ncp = t, N = nx, conf.level = ci)$result
    
    out <- data.frame(n1 = nx,
                      n2 = NA,
                      t = t,
                      bf = bf,
                      dec = dec,
                      post_d = post$median,
                      post_ll = post$ciLower,
                      post_ul = post$ciUpper,
                      cohen_d = cohen$Standardized.Mean[[1]],
                      cohen_ll = cohen$Lower.Conf.Limit.Standardized.Mean,
                      cohen_ul = cohen$Upper.Conf.Limit.Standardized.Mean)
  }
  return(out)
}


check_input <- function(alpha, beta, A, B, 
                        mu, gamma, sigma, tau2, t, nx, ny,
                        mx, my, sx, sy, rxy, ci, prior, info, design){
  
  warnings <- makeAssertCollection()
  
  assert_numeric(alpha, add = warnings, .var.name = "Type I error",
                 lower = 0, upper = 1)
  assert_numeric(beta, add = warnings, .var.name = "Type II error",
                 lower = 0, upper = 1)
  assert_numeric(A, add = warnings, .var.name = "Upper Threshold",
                 lower = 1)
  assert_numeric(B, add = warnings, .var.name = "Lower Threshold",
                 lower = 0, upper = 1)
  assert_numeric(mu, add = warnings, .var.name = "Prior Location")
  assert_numeric(nx, add = warnings, .var.name = "Sample Size")
  assert_numeric(ci, add = warnings, .var.name = "CI Level")
  if(prior %in% c("t", "cauchy")){
    assert_numeric(gamma, add = warnings, .var.name = "Prior Scale")
  } else if (prior == "normal") {
    assert_numeric(sigma, add = warnings, .var.name = "Prior Variance",
                   lower = 0)
  } else {
    assert_numeric(tau2, add = warnings, .var.name = "Prior Scale")
  }
  if(info == "t"){
    assert_numeric(t, add = warnings, .var.name = "t Value")
  }else{
    assert_numeric(mx, add = warnings, .var.name = "Sample Mean")
    assert_numeric(sx, add = warnings, .var.name = "Sample SD", lower = 0)
    if(design != "one"){
      assert_numeric(my, add = warnings, .var.name = "Sample Mean")
      assert_numeric(sy, add = warnings, .var.name = "Sample SD", lower = 0)
      if(design == "paired"){
        assert_numeric(rxy, add = warnings, .var.name = "Group Correlation")
      }
    }
  }
  if(design == "two"){
    assert_numeric(ny, add = warnings, .var.name = "Sample Size Group 2")
  }
  
  return(list(
    check = warnings$isEmpty(),
    messages = warnings$getMessages(),
    args = as.list(environment())
  ))
}

initial_df <- data.frame(n1 = NA,
                         n2 = NA,
                         t = NA,
                         bf = NA,
                         dec = NA,
                         post_d = NA,
                         post_ll = NA,
                         post_ul = NA,
                         cohen_d = NA,
                         cohen_ll = NA,
                         cohen_ul = NA)
