#' Computes the log of summed exponents.
#' @author Robert Trujillo
#' @description
#' Calculates the expression of the form:
#' log( sum_{i=1}^n  exp(x_i))
#' From a vector of values (x_1,...,x_i,...,x_n)
#' @param vec A vector of type integer or double, representing the values of the exponentials.
#' @returns A number representing the log of the sum of the exponentials.
#' @examples
#' log_summed_exp(c(1,2,3,4,5))
#' vec <- c(1:10)
#' log_summed_exp(vec)
#' @export
log_summed_exp <- function(vec){
  if (!is.vector(vec)){
    stop("input should be a vector")
  }
  else if (typeof(vec)!="integer" && typeof(vec)!="double"){
    stop("vec should be of type integer or double")
  }
  else if (length(vec) <= 1){
    return(log(sum(exp(vec))))
  }
  else{
    return(max(vec) + log(sum(exp(sort(vec,decreasing=TRUE)-max(vec)))))
  }
}

