# Decomposition class - EDSD 2022
# FUNCTIONS AND INFORMATION FOR DAY 3

# Information for graphs
# Labels for causes of death
cause_names<-c("1"="Infectious and respiratory", "2"="Neoplasm","3"="Circulatory and heart",
               "4"="Birth","5"="Diabetes",
               "6"= "Homicide","7"="Other external",
               "8"="Other","9"="Undefined")

# Functions to use with DemoDecomp functions
# Life expectancy at birth
e0.frommx <- function(nmx =  mx, sex=1, start.age=1, nax = NULL){
  age <- 0:(length(nmx)-1)
  n   <- c(diff(age), 999)
  
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == 1) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == 2) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  e0 <- ex[start.age]
  
  return(e0)
}

e0.frommxc <- function(mxcvec,sex=1,start.age=1){
  dim(mxcvec) <- c(94,length(mxcvec)/94)
  mx          <- rowSums(mxcvec)
  e0.frommx(mx,sex,start.age=start.age)
}

# Lifespan disparity
edagger.frommx <- function(mx,sex=1, start.age=1){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[i.openage] <- if (mx[i.openage] == 0) 0.5 else 1/mx[i.openage]
  
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  
  
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- dx[i.openage ] * ax[i.openage ]
  Lx[is.na(Lx)] <- 0
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[is.na(ex)] <- 0
  ex[i.openage] <- if (ex[OPENAGE] == 0) 0 else ax[i.openage]
  
  v        <- (ax*c(ex[-1L],0) + (1-ax)*ex)
  v[length(ex)] <- ex[length(ex)]
  v <- dx*v
  e.dagger <- rev(cumsum(rev(v)))/lx
  e.dagger[start.age]
}


edagger.frommxc <- function(mxcvec,sex=1, start.age=1){
  dim(mxcvec) <- c(94,length(mxcvec)/94)
  mx          <- rowSums(mxcvec)
  edagger.frommx(mx,sex,start.age=start.age)
}


# Standard deviation
sd.frommx <- function(mx,sex=1, age=16:109,start.age=1){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[i.openage] <- if (mx[i.openage] == 0) 0.5 else 1/mx[i.openage]
  
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  
  
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- dx[i.openage ] * ax[i.openage ]
  Lx[is.na(Lx)] <- 0
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[is.na(ex)] <- 0
  ex[i.openage] <- if (ex[OPENAGE] == 0) 0 else ax[i.openage]
  # 
  sd <-  sqrt(sum(dx/lx[1]*(age + ax - ex[1])^2))
  
  return(sd)
}

sd.frommxc <- function(mxcvec,sex=1,age,start.age=1){
  dim(mxcvec) <- c(94,length(mxcvec)/94)
  mx          <- rowSums(mxcvec)
  sd.frommx(mx,sex,age=age,start.age=start.age)
}

# Relative Gini

# Gini function from PASH
Gini.fun <- function (x, nax, ndx, ex) {
  e = rep(1, length(x))
  D = outer(ndx, ndx)
  x_ = x+nax
  X_ = abs(e%*%t(x_) - x_%*%t(e))
  G = sum(D*X_)/(2*ex[1L])
  return(g=G)
}


rG.frommx <- function(mx,sex=1,age,start.age=1){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[i.openage] <- if (mx[i.openage] == 0) 0.5 else 1/mx[i.openage]
  
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  
  
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- dx[i.openage ] * ax[i.openage ]
  Lx[is.na(Lx)] <- 0
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[is.na(ex)] <- 0
  ex[i.openage] <- if (ex[OPENAGE] == 0) 0 else ax[i.openage]
  
  # 
  rG <- Gini.fun(x = age,nax = ax,ndx = dx/100000,ex = ex)
  return(rG[start.age])
}

rG.frommxc <- function(mxcvec,sex=1,age,start.age=1){
  dim(mxcvec) <- c(94,length(mxcvec)/94)
  mx          <- rowSums(mxcvec)
  rG.frommx(mx,sex,age=age,start.age=start.age)
}


# Continuous change decomposition algorithm (from DemoDecomp)

decomp_cont <- function (func, pars1, pars2, N, ...) 
{
  y1 <- func(pars1, ...)
  y2 <- func(pars2, ...)
  d <- pars2 - pars1 # difference between each parameter
  n <- length(pars1)
  
  delta <- d/N # how much of the difference we add at each iteration
  
  x <- pars1 + d * matrix(rep(0.5:(N - 0.5)/N, n), byrow = TRUE, # increase pars1 so it gets to the middle value of each iteration
                          ncol = N)
  cc <- matrix(0, nrow = n, ncol = N) # prepare results matrix
  zeros <- rep(0, n)
  for (j in 1:N) {
    DD <- diag(delta/2) # matrix to only increase the parameter we are interested in for that specific iteration
    for (i in 1:n) {
      # calculating what the difference would be between the two aggregate measures if
      # - the parameter of interest increased by d/N
      # - all other parameters were fixed at the midpoint of the iteration interval
      cc[i, j] <- func((x[, j] + DD[, i]), ...) - func((x[,j] - DD[, i]), ...) 
    }
  }
  return(rowSums(cc))
}



# Stepwise replacement decomposition algorithm (from DemoDecomp)

decomp_step <- function (func, pars1, pars2, symmetrical = TRUE, direction = "up", 
                         ...) 
{
  # Clean up
  direction <- tolower(direction)
  stopifnot(direction %in% c("up", "down", "both"))
  up <- direction %in% c("up", "both")
  down <- direction %in% c("down", "both")
  
  # Setup
  N <- length(pars1)
  # parameters for the two populations
  pars1Mat <- matrix(pars1, ncol = N + 1, nrow = N)
  pars2Mat <- matrix(pars2, ncol = N + 1, nrow = N)
  # Empty matrices for calculation
  RM_1_2_up <- matrix(ncol = N + 1, nrow = N)
  RM_1_2_down <- RM_1_2_up
  RM_2_1_up <- RM_1_2_up
  RM_2_1_down <- RM_1_2_up
  
  # Fill in the matrices for substitution from population 1 to population 2
  # Determine the right succession of parameters from population 1 and population 2 
  r1ind <- lower.tri(pars1Mat, TRUE)
  r2ind <- upper.tri(pars1Mat)
  
  RM_1_2_up[r1ind] <- pars1Mat[r1ind]  # Fill matrix's lower triangle with parameters from population 1
  RM_1_2_up[r2ind] <- pars2Mat[r2ind]  # Fill matrix's upper triangle with parameters from population 2
  
  RM_1_2_down[r1ind[N:1, ]] <- pars1Mat[r1ind[N:1, ]] # Invert the triangle for inverse direction
  RM_1_2_down[r2ind[N:1, ]] <- pars2Mat[r2ind[N:1, ]]
  
  # Fill in the matrices for substitution from population 2 to population 1
  RM_2_1_up[r1ind] <- pars2Mat[r1ind]
  RM_2_1_up[r2ind] <- pars1Mat[r2ind]
  RM_2_1_down[r1ind[N:1, ]] <- pars2Mat[r1ind[N:1, ]]
  RM_2_1_down[r2ind[N:1, ]] <- pars1Mat[r2ind[N:1, ]]
  
  # Empty results matrix
  dec <- matrix(NA, nrow = N, ncol = 4)
  
  # Substitute based on chosen settings
  if (up) {
    dec[, 1] <- diff(apply(RM_1_2_up, 2, func, ...))
  }
  if (down) {
    dec[, 2] <- diff(apply(RM_1_2_down, 2, func, ...))
  }
  if (symmetrical) {
    if (up) {
      dec[, 3] <- -diff(apply(RM_2_1_up, 2, func, ...))
    }
    if (down) {
      dec[, 4] <- -diff(apply(RM_2_1_down, 2, func, ...))
    }
  }
  dec_avg <- rowMeans(dec, na.rm = TRUE)
  dec_avg
}