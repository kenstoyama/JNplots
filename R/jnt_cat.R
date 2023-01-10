#' Calculation and visualization of regions of non-significance to assess the
#' influence of categorical moderators
#'
#' Produces a plot showing which values of a continuous moderator have a 
#' significant influence on the relationship between the dependent and 
#' independent variables. 
#' @param X A character string defining the name of the covariate (e.g., size in
#'  an allometry analysis). Must be the same as the name of the variable in the 
#'  dataset (see argument “data”).
#' @param g A character string defining the name of a categorical moderator 
#'  (e.g., males and females, herbivorous and carnivorous, etc). Must be the same
#'  as the name of the variable in the dataset (see argument “data”). The
#'  variable must have two levels.
#' @param data The dataset.
#' @param phylo A logical. It indicates whether a phylogenetically-informed analysis 
#'  will be performed (i.e., PGLS). “F” by default.
#' @param tree A “phylo” class object. A phylogeny that must match the species in the data.
#' @param plot.lim A logical. It indicates whether the plot should show the 
#'  JN non-significance regions even if they don’t or just slightly overlap the data. 
#'  The default option is “F”, meaning that the plot limits will depend only on the data.
#' @param cols A vector of strings defining the symbol colors to be used in the 
#'  plot. By default, c(“black”, “black”) is used, which combines with the default 
#'  in the argument “sym” to present two groups of datapoints as open and close.
#' @param sym A vector of strings defining the symbols to be used to represent 
#'  distinct groups in the plot. Use same symbol codes as in the argument “pch” 
#'  in the R base function “plot”. By default, “c(16, 1)” is used, which combines 
#'  with the default in the argument “cols” to present two groups of datapoints 
#'  as open and close.
#' @export
#' jnt_cat()

jnt_cat <- function(X,Y,g,data,plot.full=F,phylo=F,tree,cols=c("black","black"),sym=c(16,1)){
  data[,g] <- as.factor(data[,g])
  levs <- levels(as.factor(droplevels(data[,g])))
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
  
  group1 <- data[ which(data[,g]==levs[1]), ]
  group2 <- data[ which(data[,g]==levs[2]), ]
  
  n1 <- length(group1[,1])
  n2 <- length(group2[,1])
  X1 <- group1[,X]
  X2 <- group2[,X]
  Y1 <- group1[,Y]
  Y2 <- group2[,Y]
  
  Fcvalue <- qf(.95, df1=1, df2=n1+n2-4)
  xmean1 <- mean(X1)
  xmean2 <- mean(X2)
  xmeansq1 <- mean((X1)^2)
  xmeansq2 <- mean((X2)^2)
  sumx1 <- 0
  sumx2 <- 0
  sumy1 <- 0
  sumy2 <- 0
  sumxy1 <- 0
  sumxy2 <- 0
  
  xcoord_group1 <- X1    # X1
  ycoord_group1 <- Y1    # Y1
  
  xcoord_group2 <- X2    # X2
  ycoord_group2 <- Y2    # Y2
  
  zx1 <- ((sum(xcoord_group1))^2)/n1
  zx2 <- ((sum(xcoord_group2))^2)/n2
  zy1 <- ((sum(ycoord_group1))^2)/n1
  zy2 <- ((sum(ycoord_group2))^2)/n2
  zxy1 <- ((sum(xcoord_group1))*(sum(ycoord_group1)))/n1
  zxy2 <- ((sum(xcoord_group2))*(sum(ycoord_group2)))/n2
  
  ######## sumx1 ########
  c <- 0
  while (c<n1) {
    sumx1 <- sumx1 + (((xcoord_group1[c+1])^2) - zx1)
    c <- c+1
  }
  sumx1
  
  ######## sumx2 ########
  c <- 0
  while (c<n2) {
    sumx2 <- sumx2 + (((xcoord_group2[c+1])^2) - zx2)
    c <- c+1
  }
  sumx2
  
  ######## sumy1 ########
  c <- 0
  while (c<n1) {
    sumy1 <- sumy1 + (((ycoord_group1[c+1])^2) - zy1)
    c <- c+1
  }
  sumy1
  
  ######## sumy2 ########
  c <- 0
  while (c<n2) {
    sumy2 <- sumy2 + (((ycoord_group2[c+1])^2) - zy2)
    c <- c+1
  }
  sumy2
  
  ######## sumxy1 ########
  c <- 0
  while (c<n1) {
    sumxy1 <- sumxy1 + (((xcoord_group1[c+1])*(ycoord_group1[c+1]))-zxy1)
    c <- c+1
  }
  sumxy1
  
  ######## sumxy2 ########
  c <- 0
  while (c<n2) {
    sumxy2 <- sumxy2 + (((xcoord_group2[c+1])*(ycoord_group2[c+1]))-zxy2)
    c <- c+1
  }
  sumxy2
  
  ########################
  ########################
  ########################
  ######## SSres ########
  
  SSres <- (sumy1-(((sumxy1)^2)/sumx1))+(sumy2-(((sumxy2)^2)/sumx2))
  SSres
  
  ########################
  ########################
  ########################
  ######## ABC ########
  a1 <- mod$coefficients[1]
  a2 <- mod$coefficients[1]+mod$coefficients[3]
  b1 <- mod$coefficients[2]
  b2 <- mod$coefficients[2]+mod$coefficients[4]
  A <- (-Fcvalue/(n1+n2-4))*(SSres)*((1/sumx1)+(1/sumx2))+((b1-b2)^2)
  B <- (Fcvalue/(n1+n2-4))*(SSres)*((xmean1/sumx1)+(xmean2/sumx2))+((a1-a2)*(b1-b2))
  C <- (-Fcvalue/(n1+n2-4))*(SSres)*(((n1+n2)/(n1*n2))+(xmeansq1/sumx1)+(xmeansq2/sumx2))+((a1-a2)^2)
  
  ########################
  xlower <- (-B-sqrt((B^2)-A*C))/A
  xupper <- (-B+sqrt((B^2)-A*C))/A
  
  m <- c("It was not possible to calculate regions of non-significance. The difference between slopes might not be statistically significant")
  if(((B^2)-A*C)<0){
    warning(m)
  }
  
  #### PLOT
  if(plot.full==T){
    min.lim <- min(data[,X],xlower)
    max.lim <- max(data[,X],xupper)
    plot(data[,X],data[,Y],xlab=X,ylab=Y,xlim=c(min.lim,max.lim),type="n")
    points(group1[,X],group1[,Y],col=cols[1],pch=sym[1])
    points(group2[,X],group2[,Y],col=cols[2],pch=sym[2])
    abline(a1,b1,lty=2)
    abline(a2,b2)
    polygon(c(xlower,xlower,xupper,xupper),c(min(data[,Y])*0.5,max(data[,Y])*2,
                                             max(data[,Y])*2,min(data[,Y])*0.5),col=rgb(224, 224, 224,
                                                                                        maxColorValue=255,alpha=130), border=NA)
  }else{
    plot(data[,X],data[,Y],xlab=X,ylab=Y,type="n")
    points(group1[,X],group1[,Y],col=cols[1],pch=sym[1])
    points(group2[,X],group2[,Y],col=cols[2],pch=sym[2])
    abline(a1,b1,lty=2)
    abline(a2,b2)
    polygon(c(xlower,xlower,xupper,xupper),c(min(data[,Y])*0.5,max(data[,Y])*2,
                                             max(data[,Y])*2,min(data[,Y])*0.5),col=rgb(224, 224, 224,
                                                                                        maxColorValue=255,alpha=130), border=NA)
  }
  
  #
  if(((B^2)-A*C)<0){
    results <- list("coeff" = mod.out)
    return(results)
  }else{
    results <- list("coeff" = mod.out,"lower limit in X" = xlower, "upper limit in X" = xupper)
    return(results)
  }
}

