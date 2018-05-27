#' @param obj
#'
#' @param size
#'
#' @name copy.table
#' @export copy.table
#' @title Get table output to clipboard for easy pasting into excel.
#'
#' @description This function gives a table output to the clipboard.
#'
#'
#' @return A table is returned.
#'
#' @author NA
#'

copy.table <- function(obj, size = 4096, col.names = FALSE) {

  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, col.names = col.names,sep = '\t',na="")
  close(f)
}
