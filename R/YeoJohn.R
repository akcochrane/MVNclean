#' Apply optimal Yeo-Johnson transformation to a numeric vector
#'
#' Finds the Yeo-Johnson transformation that minimizes the skewness of the vector.
#' Returns the transformed vector with an attribute "lambda" (transformation parameter).
#' Uses \code{\link[VGAM]{yeo.johnson}}.
#'
#' @param x numeric vector to transform
#' @param keepMedian Should the result be shifted so that it has the same median as the input vector?
#' @param keepMAD Should the result be scaled so that it has the same dispersion (median absolute deviation) as the input vector?
#' @param trim  Numeric value between 0 and .5. For robustness against outliers, this amount of each of the distribution's tails is excluded when estimating the optimal lambda for the Yeo-Johnson transformation.
#'
#' @export
#'
YeoJohn <- function(x,keepMedian=T,keepMAD=T,trim = 0.05){

  yeojohnson <- VGAM::yeo.johnson

  require(psych)
  
  if(trim == 0){trim == .0001} # very small offset in the case of problematic tails

  xOut <- rep(NA,length(x))
  x_nonNA <- !is.na(x)

  x <- na.omit(x)
  
  
  # x_expanded_noTrim <- quantile(x,seq(.00001,1-.00001,length = 1E3))
  med_orig <- median(x)
  mad_orig <- mad(x)
  
  x_expanded <- quantile(x,seq(trim,1-trim,length = 2E3)) # for better performance with small sample sizes

  if(mad_orig == 0){
    warning('Your variable may have a problematically small dispersion. SD used instead of MAD')
    mad_orig <- sd(x_expanded)}

  rootFun <- function(lambda){return(skew(yeojohnson(x_expanded,lambda))^2)}
  bestLambda <- optimize(rootFun,interval = c(-20,20))

  xOut[x_nonNA] <- yeojohnson(x,bestLambda$minimum)
  lambda <- bestLambda$minimum
  rawDat <- x
  rawDat_skew <- skew(x)
  transformedDat <- xOut

  if(keepMedian || keepMAD){

    med_transf <- median(xOut,na.rm=T)

    xOut <- xOut - med_transf

    if(keepMAD){
      mad_new <- mad(xOut,na.rm=T)
      if(mad_new == 0){mad_new <- sd(xOut,na.rm=T)}
      xOut <- xOut/mad_new
      xOut <- xOut*mad_orig
      }

    if(keepMedian){
      xOut <- xOut+med_orig
    }else{xOut <- xOut + med_transf}


    attr(xOut,'transformedDat') <- transformedDat
  }

  attr(xOut,'lambda') <- lambda
  attr(xOut,'rawDat') <- rawDat
  attr(xOut,'rawDat_skew') <- rawDat_skew

  return(xOut)

}
