#' my calculator function
#'
#' @description This function allows you to do basic calculations like sum, subtract,
#' divide and multiple on vector data
#' @param x A vector
#' @param y A vector
#' @param fun operation to be performed on the vectors
#' @export
#' @examples
#' my_calc(vector1, vector2, "sum)
my_calc <- function (x, y, fun) {
  if(fun == "sum") {
    output <- x + y
  } else if(fun == "subtract") {
    output <- x - y
  } else if(fun == "divide") {
    output <- x/y
  } else if(fun == "multiply") {
    output <- x * y
  } else {
    print("Function not supported. Buy a scientific calculator")
  }
  return(output)
}

