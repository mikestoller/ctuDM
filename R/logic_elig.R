
#' @name logic_elig
#' @export logic_elig
#' @title Create logic for eligibility criteria.
#'
#' @description This function gets variables (in rows) from the clipboard and outputs logic and calculation for eligibility.
#'
#' @param event_name NULL by default. Specify event_name, if field is used in events other than the event containing the eligibility criteria.
#' @param elin_spec Specifier for inclusion criterion. 'elin_' by default.
#' @param elex_spec Specifier for exclusion criterion. 'elex_' by default.
#'
#' @return A 2 row-table  with row 1 = header and row 2 = value is returned with the following headers and values,
#' @return* `inc` : logic which is TRUE, when all eligibility criteria are fullfilled (inclusion criteria = yes and exclusion critera = no)
#' @return* `out` : logic which is TRUE, when at least one eligibility criterion is not fullfilled (inclusion criterion = no or exclusion criterion = yes)
#' @return* `mis` : logic which is TRUE, when at least one eligibility criterion is missing AND other eligibility criteria are fullfilled (if an eligibility criterion is not fullfilled, this is not TRUE)
#' @return* `calc`: calculation which gives values: 0= not eligible, 1 = eligible, 2 = missing
#'
#' @author Michael Stoller <mstoller84@gmail.com> <michael.stoller@ctu.unibe.ch>
#'
#' @examples
#' \dontrun{
#' #Run function with specified event
#' logic_elig(event = "eligibility")
#' }
#' @importFrom utils readClipboard

logic_elig <- function(event=NULL, elin_spec='elin_',elex_spec='elex_') {

  x <- utils::readClipboard() # get clipboard input, one row = one variable

  # preallocate

  y <- vector("list",0)
  tmp <- vector("list",0)

  cin = 1 # initialize counter

  # logic for eligible = yes
  # set 1 for inclusion criterion, 0 for exclusion criterion

  for(i in 1:length(x)) {
    # if it's an inclusion criterion
    if(pmatch(elin_spec, x[i],nomatch = 0)){
      y$inc[cin] <- paste0("[",x[i],"] = '1'")
      cin = cin + 1
    }
    # if it's an exclusion criterion
    else if (pmatch(elex_spec, x[i],nomatch = 0)) {
      y$inc[cin] <- paste0("[",x[i],"] = '0'")
      cin = cin + 1
    }
  }

  y$inc <- paste(y$inc[1:length(y$inc)],collapse=" AND ")

  # logic for eligible = no
  # set 0 for inclusion criterion, 1 for exclusion criterion

  cout = 1 #initialize counter

  for(i in 1:length(x)) {
    if(pmatch(elin_spec, x[i],nomatch = 0)) # if it's an inclusion criterion
    {
      y$out[cout] <- paste0("[",x[i],"] = '0'")
      cout = cout + 1 # increase counter
    }
    else if (pmatch(elex_spec, x[i],nomatch = 0)) # if it's an exclusion criterion
    {
      y$out[cout] <- paste0("[",x[i],"] = '1'")
      cout = cout + 1 # increase counter
    }
  }

  y$out <- paste(y$out[1:length(y$out)],collapse=" OR ")

  # logic for missing eligibility warning
  # "" and <> 0 for inclusion criterion, "" and <> 1 for exclusion criterion

  cmis = 1

  for(i in 1:length(x)) {
    if(pmatch(elin_spec, x[i],nomatch = 0)) # if it's an inclusion criterion
    {
      tmp$mis1[cmis] <- paste0("[",x[i],"] = ''")
      tmp$mis2[cmis] <- paste0("[",x[i],"] <> '0'")
      cmis = cmis + 1 # increase counter
    }
    else if (pmatch(elex_spec, x[i],nomatch = 0)) # if it's an exclusion criterion
    {
      tmp$mis1[cmis] <- paste0("[",x[i],"] = ''")
      tmp$mis2[cmis] <- paste0("[",x[i],"] <> '1'")
      cmis = cmis + 1 # increase counter
    }
  }

  # collapse temporary strings
  tmp$mis1 <- paste(tmp$mis1[1:length(tmp$mis1)],collapse=" OR ")
  tmp$mis2 <- paste(tmp$mis2[1:length(tmp$mis2)],collapse=" AND ")

  # concatenate output for "missing" branching logic
  y$mis <- paste0("(",tmp$mis1,") AND ",tmp$mis2)

  y$calc <- paste0('if(',y$inc,',1,if(',y$out,',0,2))')

  # copy to clipboard
  copy.table(y)

}
