
#' @name repeat_lines
#' @export repeat_lines
#' @title Create repetitions for redcap dictionary
#'
#' @description This function gets rows from the clipboard and outputs the repetitions back to the clipboard.
#'
#' @param repeatn Specify number of repetitions. 1 by default (ouput = input)

#' @return Repetitions of the input

#' @author Michael Stoller <mstoller84@gmail.com> <michael.stoller@ctu.unibe.ch>
#'
#' @examples
#' \dontrun{
#' #Run function to create 3 repetitions
#' repeat_lines(repeatn = 3)
#' }
#' @importFrom utils readClipboard
#' @importFrom stringr str_extract
#'
repeat_lines <- function(repeatn=1){

  checkmate::assert_int(repeatn,lower=1)

  # # source function copy.table
  # source("copy.table.R")

  # get input = clipboard
  # ----------------------------------------------------------------------------
  x <- read.table(file = "clipboard",
                  sep = "\t",
                  header=FALSE,
                  stringsAsFactors = FALSE
                  )

  # check input
  checkmate::assertDataFrame(x,ncols = 18)


  message('table read from clipboard: number of rows: ',nrow(x),
          ', number of columns: ',ncol(x))

  # preallocate for output
  # ----------------------------------------------------------------------------
  y <-vector("list",0)

  # identify counter for repetitions
  # pattern is _X_ in first column (first column =variable name)
  # ----------------------------------------------------------------------------

  counter_str <-
    str_extract(string = x[1,1],
                pattern = '_[:digit:]_')

  if(is.na(counter_str))
    stop(
      "aborted: counter in variable name not found, must be _X_",
      call. = FALSE)

  counter_num <-
    as.numeric(
      str_extract(string = counter_str,
                pattern = '[:digit:]')
    )

  ## check that other variable names follow the format

  counter_str_check <-
    str_extract_all(string = x[1:(nrow(x)),1],
                pattern = paste0('_',
                                 counter_num,
                                 '_'
                                 )
    )

  counter_str_check <- unique(counter_str_check)

  if(length(counter_str_check)>1)
    stop(
      "aborted: not all variable names follow the format _X_",
      call. = FALSE)

  message('counter identified: ',counter_num)

  # identify counter in header of repetition
  # is digit equal to counter_num in field label of descriptive text
  # ----------------------------------------------------------------------------

  # stop if first row is not a descriptive
  if(!str_detect(x[1,4],"descriptive"))
    stop(
      "first row of repetition does not have field_type descriptive, aborting..",
      call. = FALSE)

  header_str <-
    str_extract(
      string = x[1,5],
      pattern = paste0('\\s',counter_num,'\\s?(</b>)?</div>')
      )


  # identify branching logic for repetitions
  # branching logic for repetition is identified by > operator
  # ----------------------------------------------------------------------------

  br_log_counter <- counter_num-1

  br_logic_str <-
    str_extract(string = x[1,12],
                pattern = paste0("\\[(.*?)\\]\\s?>\\s?",br_log_counter)
                )

  br_logic_str_repl <-
    str_extract(string= br_logic_str,
                pattern= paste0(">\\s?",br_log_counter)
                )

  # message
  message('identified branching logic for repetitions: ',br_logic_str)

  # identify other branching logic that needs adaptation
  br_logic_other_str <-
  str_extract_all(
    string = x[1:nrow(x),12],
    pattern =
      paste0(
        '\\[([:alnum:]*[:punct:]*)*_',counter_num,'_([:alnum:]*[:punct:]*)*')
  )

  # get unique
  br_logic_other_str <- unique(br_logic_other_str)

  # remove empty
  br_logic_other_str <-
    br_logic_other_str[lapply(br_logic_other_str,length) > 0]

  # message and get pattern to replace
  if(length(br_logic_other_str)!=0)
  {
    message('identified other branching logic in repetition: ',
            unlist(br_logic_other_str))

  } else {
    message('identified no other branching logic in repetition')
  }



  # repeat rows according to function input repeatn
  # ----------------------------------------------------------------------------

  y <-
    x[rep(seq_len(nrow(x)), repeatn),]


  # adapt variable names, header of repetition and branching logic
  # ----------------------------------------------------------------------------

  # parameters for sequence
  rows_tot  <-
    nrow(x)*repeatn

  rep_length <- nrow(x)

  mutate_seq <-
    seq(from  = 1,
        to    = rows_tot,
        by    = nrow(x)
        )

  for (i in mutate_seq)
  {
    # adapt variable names
    y[i:(i+rep_length-1),1] <-
      sapply(X=y[i:(i+rep_length-1),1],
               simplify = TRUE,
           FUN = function(x)
             str_replace(string  = x,
                             pattern = counter_str,
                             replacement =
                               paste0('_',
                                      counter_num+which(mutate_seq == i)-1,
                                      '_')
                             )
          )

    # adapt header

    y[i,5] <-
      sapply(X=y[i,5],
             simplify = TRUE,
             FUN = function(x)
               str_replace(string  = x,
                           pattern = header_str,
                           replacement =
                             paste0(' ',
                                    counter_num+which(mutate_seq == i)-1,
                                    ' </b></div>')
               )
      )

    # adapt counter branching logic

    y[i:(i+rep_length-1),12] <-
      sapply(X=y[i:(i+rep_length-1),12],
             FUN = function(x)
               str_replace(string  = x,
                           pattern = br_logic_str_repl,
                           replacement =
                             paste0('> ',
                                    br_log_counter+which(mutate_seq == i)-1)
               )
      )

    # adapt other branching logic

    if(length(br_logic_other_str)!=0) # if there is other branching logic
    {
      y[i:(i+rep_length-1),12] <-
        sapply(X=y[i:(i+rep_length-1),12],
               simplify = TRUE,
               FUN = function(x)
                 str_replace(string  = x,
                             pattern = paste0('_',counter_num,'_'),
                             replacement =
                               paste0('_',
                                      counter_num+which(mutate_seq == i)-1,
                                      '_')
                             )
               )

    } # end if

} # end loop

  # copy to clipboard
  copy.table(y)

  message('done, copied output to clipboard')

}# end function
