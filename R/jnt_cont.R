#' Calculation and visualization of regions of non-significance to assess the
#' influence of continuous moderators
#'
#' Produces a plot showing which values of a continuous moderator have a
#' significant influence on the relationship between the dependent and
#' independent variables.
#' @param X A character string defining the name of the covariate (e.g., size in
#'  an allometry analysis). Must be the same as the name of the variable in the
#'  dataset (see argument “data”).
#' @param Y A character string defining the name of the dependent variable. Must
#'  be the same as the name of the variable in the dataset (see argument “data”).
#' @param g A character string defining the name of a continuous moderator (e.g., …, etc).
#'  Must be the same as the name of the variable in the dataset (see argument “data”).
#'  The variable must be continuous.
#' @param data The dataset.
#' @param phylo A logical. It indicates whether a phylogenetically-informed
#'  analysis will be performed (i.e., PGLS). “F” by default.
#' @param tree A “phylo” class object. A phylogeny that must match the species
#'  in the data.
#' @export
#' jnt_cont()

jnt_cont <- function(X,Y,g,data,phylo=F,tree){
  mod <- summary(lm(data[,Y]~data[,X]*data[,g]))
  mod.out <- mod
  if(phylo==T){
    Xi <- data[,X]
    Yi <- data[,Y]
    gi <- data[,g]
    mod <- gls(Yi~Xi*gi, correlation=corPagel(1,tree),
               na.action = na.omit)
    mod.out <- summary(mod)
  }
  varcov <- vcov(mod.out)
  coef1 <- mod.out$coefficients[2]
  coef3 <- mod.out$coefficients[4]
  var_coef1 <- varcov[2,2]
  var_coef3 <- varcov[4,4]
  cov_coefs1_3 <- varcov[2,4]

  a <- (1.96^2)*(var_coef3)-(coef3)^2
  b <- 2*((1.96^2)*(cov_coefs1_3)-(coef1*coef3))
  c <- (1.96^2)*var_coef1-(coef1)^2

  x1 <- (-b-sqrt((b^2)-(4*a*c)))/(2*a)
  x2 <- (-b+sqrt((b^2)-(4*a*c)))/(2*a)

  m <- c("It was not possible to calculate regions of non-significance. The difference between slopes might not be statistically significant")
  if(((b^2)-4*a*c)<0){
    warning(m)
  }

  plot(data[,X],data[,Y],xlab=X,ylab=Y)

  val <- min(min(data[,g],na.rm = T),min(x1,x2))
  valmin <- min(data[,g],na.rm = T)
  val_max <- max(max(data[,g],na.rm = T),max(x1,x2))
  valmax <- max(data[,g],na.rm = T)
  nn <- (val_max-val)/100
  aaa <- c()
  c <- 1
  while(val+nn<val_max){
    #if(val+nn>x2 & val+nn<x1){ #try changing to min(...) and max(...)
    if(val+nn>min(x1,x2) & val+nn<max(x1,x2)){
      abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*val),b=(mod.out$coefficients[2]+mod.out$coefficients[4]*val),col=alpha("grey",0.5))
      aaa[c] <- val
      val <- val+nn
      c <- c+1
    } else {
      abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*val),b=(mod.out$coefficients[2]+mod.out$coefficients[4]*val),col=alpha("lightblue",0.5))
      aaa[c] <- val
      val <- val+nn
      c <- c+1
    }
  }
  abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*min(data[,g],na.rm = T)),
         b=(mod.out$coefficients[2]+mod.out$coefficients[4]*min(data[,g],na.rm = T)),
         col="black",lwd=1,lty=2)
  abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*max(data[,g],na.rm = T)),
         b=(mod.out$coefficients[2]+mod.out$coefficients[4]*max(data[,g],na.rm = T)),
         col="black",lwd=1,lty=2)
  results <- list("coeff" = mod.out,"lower non-significance limit of moderator" = min(x1,x2),
                  "upper non-significance limit of moderator" = max(x1,x2),
                  "lower data limit" = valmin, "upper data limit" = valmax)
  return(results)
}
