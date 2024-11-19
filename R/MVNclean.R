
#' Clean multivariate normal data
#' 
#' Applies a cleaning pipeline to a numeric dataset
#' 
#' The pipeline involves: \describe{
#'    \item{\code{\link{YeoJohn}}}{After some proportion of the tail is excluded, the optimal \code{lambda} parameter is found to minimize univariate skew for each variable. This \code{lambda} is then applied to all values of the respective variable.}
#'    \item{\code{\link{RUW}}}{Robust univariate winsorization is applied, shifting all tail values which were beyond \code{reject_alpha} (using \code{\link{median}} and \code{\link{mad}}) toward the \code{median} until they are at the \code{reject_alpha} point in the distribution's tail.}
#'    \item{\code{\link{RMW}}}{Robust multivariate winsorization is applied, shifting multivariate outliers toward the centroid. See also \code{\link{RobMulOutliers}}.}
#' }
#' 
#' The last step implements the robust covariance estimation and multivariate outlier rejection 
#' approach advocated by Leys et al. (2018; \code{doi.org/10.1016/j.jesp.2017.09.011}).
#' 
#' This pipeline is designed for variables that could be, in principle, multivariate normal. Multimodal distributions
#' or distributions with few unique numeric values are likely to provide inconsistent and unsatisfying results, as
#' are multivariate distributions with too few observations. Inherently skewed variables, such as those best characterized
#' by a Gamma distribution, will be approximately "Gaussian-ized" but may lose properties of their original 
#' distributions that would have been of interest.
#'
#' @param dat Data frame of numeric variables. If non-numeric variables (or numeric variables with fewer than 5 unique values) are included, a warning is given which can be ignored; these will be set aside while the cleaning is done and then re-combined before returning.
#' @param groupingVar Optional vector of length \code{nrow(dat)}. If supplied, the pipeline is applied separately for subsets of the data defined by \code{groupingVar}. This should be used with great caution; while by default medians and MADs are preserved, groups may be transformed separately in ways that lead to unexpected results if group comparisons are subsequently examined in, for example, an ANOVA.
#' @inheritParams RMW
#' @inheritParams RUW
#' @inheritParams YeoJohn
#' @param Additional arguments; currently unused.
#'
#' @export
#'
#' @examples
#' iris_clean <- MVNclean(iris)
#' 
MVNclean <- function(dat
                     , dat_proportion = .8
                     , reject_alpha = pnorm(-3)*2
                     , groupingVar = 'none'
                     , trim = .1
                     ,...){
  
  # To do:
  
  # > save initial medians and MADs and make sure they're actually matched in the end.
  
  # check data, exclude vars, give warnings
  
  datList <- check_MVN_compatibility(dat)
  
  datOut <- data.frame()
  
  # get stats on orig
  
  # split into groups and apply on each group
  
  if(length(groupingVar) == 1){
    groupingVar <- rep('entire_data',nrow(dat))
  }
  
  for(curGroup in unique(groupingVar)){
    
    ## make sure the user knows if the group is problematically small
    dTmp <- datList$dat[groupingVar == curGroup,]
    if(nrow(dTmp) < 10){
      stop(paste('The group',curGroup,'has too few observations.'))
    }    
    if(nrow(dTmp) < 25){
      warning(paste('Consider defining a different grouping variable; the group'
                    ,curGroup,'has very few observations.'))
    }
    
    # get & apply trimmed YJ
    
    dTmp <- apply(dTmp,2,YeoJohn, trim = trim)
    
    # apply RUW
    
    dTmp <- apply(dTmp,2,RUW, reject_alpha = reject_alpha)
    
    # apply RMW
    
    dTmp <- RMW(dTmp
                , dat_proportion = dat_proportion
                , reject_alpha = reject_alpha)
    
    datOut <- rbind(datOut, dTmp)
    rm(dTmp)
  }
  
  # determine correspondence with orig
  
  # return
  if(dim(datList$dat_nonNum)[1] > 0){
    datOut <- cbind(datList$dat_nonNum, datOut) # need to double check that this will always align correctly, given the breaking apart done with groupingVar
  }
  return(datOut)
}
