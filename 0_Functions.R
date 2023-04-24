# ----------------------------------------------------------------------------------------------------#
# Codes for replicating the paper Misperceptions
# author: Vanessa di Lego, Sonja Spitzer and Patrick Lazarevic
# Wittgenstein Centre for Demography and Global Human Capital(IIASA/OeAW/UniWien)
# Vienna Institute of Demography at the Austrian Academy of Sciences
# ----------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------#
# Functions used in this project
#-----------------------------------------------------------------------------------------------------#
start.age = 60
open.age = 80

Sullivan.fun = function (rates,age=seq(start.age,open.age,5)) {
  # 1) First, we split from our single vector 'rates' the set of age-specific death rates (mx) 
  # and age-specific prevalence of disability (wx)
  lengthvec <- length(rates)
  mx <- rates[1:(lengthvec / 2)]
  wx <- rates[(lengthvec / 2 + 1):lengthvec]
  # 2) Calculating period life table functions
  # ax
  n <- c(diff(age), 5)
  ax <- 0.5 * n
  # probability of dying (qx) and surviving (px)
  qx <- (n * mx)/(1 + (n - ax) * mx)
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  px <- 1 - qx
  # survivors at age x (lx)
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths between ages x and x+n (dx)
  dx <- lx * qx
  # person-years lived between ages x and x+n (Lx)
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  Tx  <- rev(cumsum(rev(Lx)))
  ex = Tx/lx
  # 3) getting the person-years lived with health (without disability) between ages x and x+n
  Lx.health <-  Lx*(1-wx)
  Tx.health  <- rev(cumsum(rev(Lx.health)))
  # 4) healthy life expectancy 
  ex.health <- Tx.health/lx
  return.df <- data.frame(age, n, ax, qx, px, lx, dx, Lx, Tx,ex,Lx.health, Tx.health, ex.health)
  return(ex.health[1])
} 

# Sullivan function to generate the confidence intervals
Sullivan.CI = function (mx,wx,age=seq(start.age,open.age,5)) {
  # 1) First, we split from our single vector 'rates' the set of age-specific death rates (mx) 
  # and age-specific prevalence of disability (wx)
  lengthvec <- length(mx)
  mx <- mx
  wx <- wx
  # 2) Calculating period life table functions
  # ax
  n <- c(diff(age), 10)
  ax <- 0.5 * n
  # probability of dying (qx) and surviving (px)
  qx <- (n * mx)/(1 + (n - ax) * mx)
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  px <- 1 - qx
  # survivors at age x (lx)
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths between ages x and x+n (dx)
  dx <- lx * qx
  # person-years lived between ages x and x+n (Lx)
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  Tx  <- rev(cumsum(rev(Lx)))
  ex = Tx/lx
  # 3) getting the person-years lived with health (without disability) between ages x and x+n
  Lx.health <-  Lx*(1-wx)
  Tx.health  <- rev(cumsum(rev(Lx.health)))
  # 4) healthy life expectancy 
  ex.health <- Tx.health/lx
  return.df <- data.frame(age, n, ax, qx, px, lx, dx, Lx, Tx,ex,wx,Lx.health, Tx.health, ex.health)
  return(return.df)
} 

## function for constructing a lifetable starting from probabilities
lifetable.qx.wx <- function(x, qx, wx){
  m <- length(x)
  # ax
  n <- c(diff(x), 10)
  ax <- 0.5 * n
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]*ax[length(x)]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Lx.health <-  Lx*(1-wx)
  Tx  <- rev(cumsum(rev(Lx)))
  Tx.health  <- rev(cumsum(rev(Lx.health)))
  ex  <- Tx/lx
  ex.health  <- Tx.health/lx
  return.df <- data.frame(x, ax, qx, px, lx, dx, Lx, Tx, ex,wx,Lx.health, Tx.health, ex.health)
  return(return.df)
}

# function to calculate CI
CIex <- function(x, Nx, Dx, Rx,mx,wx, which.x, ns, level){
  ## point-estimated lifetable
  LT <- Sullivan.CI(mx,wx)
  ## number of ages
  m <- nrow(LT)
  ## estimated probs
  qx <- LT$qx
  ## estimated prevs
  wx <- LT$wx
  ## trials for binomial, rounded
  Ntil <- round(Dx/qx)
  Ntil.prev <- round(Rx/wx) # Rx is the number of respondents and wx the prevalence of disability
  ## ax for last age
  last.ax <- LT$ax[m]
  ## simulated death counts
  ## from Binomial distribution
  Y <- suppressWarnings(matrix(rbinom(m*ns,
                                      Ntil,
                                      qx),
                               m,ns))
  ## simulated number of unhealth individuals
  ## from Binomial distribution
  Z <- suppressWarnings(matrix(rbinom(m*ns,
                                      Ntil.prev,
                                      wx),
                               m,ns))
  ## simulated probabilities
  QX <- Y/Ntil
  ## simulated prevalences
  WX <- Z/Ntil.prev
  
  ## which age?
  wh <- which(x==which.x)
  fun.ex <- function(qx){
    return(lifetable.qx.wx(x=x, qx, wx)$ex[wh])
  }
  fun.ex.health <- function(wx){
    return(lifetable.qx.wx(x=x, qx, wx)$ex.health[wh])
  }  
  exsim <- apply(QX, 2, fun.ex)
  exsim.health <- apply(WX, 2, fun.ex.health)
  
  ## confidence interval
  CI <- quantile(exsim,
                 probs = c((1-level)/2,
                           1 - (1-level)/2))
  CI.health <- quantile(exsim.health,
                        probs = c((1-level)/2,
                                  1 - (1-level)/2))
  CI.Wx <- quantile(WX,
                    probs = c((1-level)/2,
                              1 - (1-level)/2))
  ## output
  out <- list(ex=LT$ex[wh],
              meanex=mean(exsim),
              CIex=CI,
              exsim=exsim,
              ex.health=LT$ex.health[wh],
              meanex.health=mean(exsim.health),
              CIex.health=CI.health,
              which.x=which.x,
              WX.mean=mean(WX),
              CI.Wx = CI.Wx,
              Y=Y,
              Z=Z,
              exsim=exsim,
              exsim.health=exsim.health,
              WX=WX)
  return(out)
}

# regular lifetable

### Life table

life.table <- function(mx){
  ax <- c(0.14, rep(0.5, length(mx)-1))
  qx <- mx/(1+(1-ax)*mx)
  qx[length(qx)] <- 1
  qx[qx > 1] <- 1
  px <- 1-qx
  lx <- c(100000, (cumprod(px)*100000)[1:(length(px)-1)])
  dx <- c(-diff(lx), lx[length(lx)])
  Lx1 <- lx[-1]+ax[-length(ax)]*dx[-length(dx)]
  open.Lx <-  ifelse( mx[length(mx)] == 0, 0, dx[length(dx)]/mx[length(mx)])
  Lx <- c(Lx1, open.Lx)
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  
  return(data.frame(qx=qx, px = px, ax = ax, lx = lx , dx = dx, Lx= Lx,
                    Tx = Tx, ex = ex))
}

