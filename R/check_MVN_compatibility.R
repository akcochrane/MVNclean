

#' Check for compatibility with applying multivariate normality
#' 
#' Returns a list with \code{dat}, the numeric data, and 
#' \code{dat_nonNum}, the data for which it would be difficult 
#' to apply multivariate normality.
#'
#' @param dat 
#'
#' @export
#' 
check_MVN_compatibility <- function(dat){
  
  isNonNumeric <- 
    ( as.numeric(sapply(dat,class) =='character') +
        as.numeric(sapply(dat,class) =='factor') +
        as.numeric(apply(dat
                         ,2
                         ,function(x){length(unique(x))<5}
        ))
    )>0
  
  if(any(isNonNumeric)){
    message('Only numeric variables with more than 4 unique values are included in this outlier rejection')
    if(sum(!isNonNumeric)<2){stop('not enough numeric variables with at least 5 unique values')}
    dat_nonNum <- data.frame(dat[,isNonNumeric])
    colnames(dat_nonNum) <- colnames(dat)[isNonNumeric]
    dat <- dat[,!isNonNumeric]
    
    return(list(dat = dat , dat_nonNum = dat_nonNum))
  }else{
    return(list(dat = dat , dat_nonNum = data.frame() )) 
  }
  
  
}