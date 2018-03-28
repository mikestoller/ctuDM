
#' @name plus_collapse
#' @export plus_collapse
#' @title Collapses input (as rows) from clipboard with plus sign and adds optional square brackets.
#'
#' @description This function gets rows from the clipboard and outputs the rows (with optional square brackets) connected by plus signs.
#'
#' @param brackets TRUE by default. If TRUE, inserts square brackets around each row. If FALSE, does not insert brackets.

#' @return One cell with rows collapsed by plus sign.

#' @author Michael Stoller <mstoller84@gmail.com> <michael.stoller@ctu.unibe.ch>
#'
#' @examples
#' \dontrun{
#' #Run function without square brackets
#' plus_collapse(brackets = FALSE)
#' }
#' @importFrom utils readClipboard
#' @importFrom utils writeClipboard
#'
plus_collapse <- function(){

  x <- readClipboard()

  if (brackets){

    y <- paste0("[",x,"]",collapse=" + ")

  } else{

    y <- paste0(" ",x," ",collapse=" + ")

  }

  writeClipboard(y)

}
