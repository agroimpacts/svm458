#' A Squirrel Function
#' @description This function allows you to express your admiration for squirrels
#' @param admire Do you admire squirrels? Defaults to TRUE.
#' @export
#' @examples
#' squirrel_function()
squirrel_function <- function(admire = TRUE) {
  if(admire == TRUE){
    print("I strongly admire squirrels")
  } else {
    print("I do not belong to the squirrel fan club")
  }
}
