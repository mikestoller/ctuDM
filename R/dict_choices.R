#' @name dict_choices
#' @export dict_choices
#' @title Creates choices from rows.
#'
#' @description This function gets variables (in rows) from the clipboard and outputs choices for pasting into the REDCap dictionary. It assigns values to the choices automatically.
#'
#' @param stepsize 1 by default. The stepsize by which to increment the choice value from one choice to the next.
#' @param stepmin 1 by default. The value for the first choice
#'
#' @return A one-cell string with formatting according to choices in REDCap dictionary.
#'
#' @author Michael Stoller <mstoller84@gmail.com> <michael.stoller@ctu.unibe.ch>
#'
#' @examples
#' \dontrun{
#' #Run function with stepsize 1 and initial step 0
#' dict_choices(stepsize = 1, stepmin = 0)
#' }
#' @importFrom utils readClipboard

dict_choices <- function(stepsize=1,stepmin=1){

  x <- readClipboard() # get clipboard input

  steps = seq(from=stepmin,by=stepsize,length.out=length(x))

  for(i in 1:length(x)){
    x[i] <- paste0(steps[i],", ",x[i])
  }

  # put together

  y <- paste(x[1:length(x)],collapse=" | ")

  writeClipboard(y)
}
