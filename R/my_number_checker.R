#' A number-checking function
#'
#' @description This function allows you to test whether a number falls into the
#' Goldilocks range (5-9) or not
#' @param x A number
#' @export
#' @examples
#' my_number_checker(1)
#' my_number_checker(7)
#' my_number_checker(11)
my_number_checker <- function(x) {
  if(x < 5) {
    print("Too low!")
  } else if(x > 5 & x < 10) {
    print("Just right!!! :)")
  } else {
    print("Too high!")
  }
}
