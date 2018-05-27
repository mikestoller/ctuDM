# -------------------------------------------------------------------------
# This is a R script
# -------------------------------------------------------------------------
# File          : rc_batch.R
# Author        : Michael Stoller, mstoller84@gmail.com
# Date Created  : 2018-01-16
# Last Change   : 2018-
# Institute     : CTU
# Topic         : Statistics
# -------------------------------------------------------------------------
# Description:
# - functions to automate choices, branching logic, etc.
#
# changelog:
#
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


event_br <- function(){

  x <- readClipboard() # get clipboard input

  for(i in 1:length(x)){
    x[i] <- paste0("event_id=",x[i])
  }

  # put together

  y <- paste(x[1:length(x)],collapse=" OR ")

  writeClipboard(y)

}

quick_br <- function(){

  x <- readClipboard() # get clipboard input

  y <- vector()

  for(i in 1:length(x)){
    y[i] <- paste0("[",x[i],"] = '1'")
  }

  # put together

  copy.table(y)

}

lab_diag <- function(){

  x <- readClipboard() # get clipboard input

  y <- vector()

  for(i in 1:length(x)){
    y[i] <- paste0("if( [",x[i],"] = 'NaN',99,if( [",x[i],"] <480,0,1))")
  }

  # put together

  copy.table(y)

}


div_class <- function(h = 2){

  x <- readClipboard() # get clipboard input

  y <- vector()

  for(i in 1:length(x)){
    y[i] <- paste0("<div class='blue'><h2><b>",x[i],"</b></h2></div>")
  }

  # put together

  copy.table(y)

}

warnbatch <- function(repeatn = 1, multiling = 0){


  x <- readClipboard() # get variables from clipboard
  len <- length(x) # number of variables

  # counter for questions
  c <- rep(seq(1,len/3),1,each=repeatn)

  # counter for language
  l <- rep(seq(1,repeatn),len/3,each=1)

  # preallocate
  y <- vector("list",0)

  #combinations
  for(i in 1:len){
    y$label[i] = paste0("<div class='red' style='text-align:center;'><h4>Please complete question #",c[i],'</h4></div>')
    y$varname[i] = paste0("wrn_",x[i],"_txt")
    if (multiling){
      y$brlog[i] = paste0("[",x[i],"]='' AND [language_radio]=",l[i])
    }else{
      y$brlog[i] = paste0("[",x[i],"]=''")
    }
  }

  #varname output
  copy.table(y)

}

quickor <- function(){

  x <- readClipboard()

  y <- paste0(" ",x," ",collapse=" OR ")

  writeClipboard(y)

}



extract_from_quotes <- function(){

  x <- readClipboard()

  y <- str_extract_all(x,"\".*?\"",simplify = TRUE)

  y <- str_sub(y,start=2,end=-2)


}

number_of_nonempty_records <- function(radio=FALSE){

  x <- readClipboard()

  y <- vector()

  if (radio){

  for (i in 1:length(x)){

    y[i] = paste0('if( [',x[i],'] = "NaN",0,1)')

  }

  } else {

    for (i in 1:length(x)){

      y[i] = paste0('if( [',x[i],'] = "",1,0)')

    }

  }

  y <- paste0(y,collapse = ",")
  y <- paste0('sum(',y,')')

  writeClipboard(y)

}


separate_whitespace <- function(){

  x <-
    readClipboard()

  y <-
    read.table(text = x,
               sep = "",
               quote = "",
               fill = TRUE)

  ctuDM::copy.table(y)

}
