#######
# These are relatively simple functions meant to make the most gnarly and obnoxious R syntax 
# easy to use.

require(pacman)

####
#Includes:
#Notes: wrapper for dropping values
drop_vals_list <- function(thedata, col_name, thelist, keep = FALSE, col_regex = FALSE)
{
  temp=0
  if(!is_array(thelist)){
    thelist = StringToArray(thelist,delim=",")
  }
  if(col_regex){
    #col_index = which(grepl(col_name,colnames(thedata)))[1]
    col_index = which(grepl(col_name,colnames(thedata)))
  }else{
    col_index = which(colnames(thedata) == col_name)
  }
  if(is_empty(col_index)){return(thedata)}
  #if(!keep){thedata = thedata[!(thedata[,col_index] %in% thelist),]}
  #if(keep){thedata = thedata[(thedata[,col_index] %in% thelist),]}
  for(i in 1:length(col_index)){
    if(!keep){thedata = thedata[!(thedata[,col_index[i]] %in% thelist),]}
    if(keep){thedata = thedata[(thedata[,col_index[i]] %in% thelist),]}
  }
  return(thedata)
}

####
#Includes:
#Notes: flags the highest value, using base r
flag_max <- function(thedata,key_field,value_field,flag_name=NA)
{
  temp = 0
  #kt = thedata[key_field]
  #vt = thedata[value_field]
  fields = length(thedata)
  max_flag <- ave(thedata[value_field], thedata[key_field], FUN = function(x) x == max(x))
  thedata$max_flag = max_flag[[1]]
  if(!is.na(flag_name)){
    colnames(thedata)[fields+1] <- flag_name
  }
  return(thedata)
}


####
#Includes:
#Notes: smart get length, if null or na return 0
length_sm <- function(thedata)
{
  if(is_null(thedata)){
    return(0)
  }else if(is.na(thedata)){
    return(0)
  }
  else{
    return(length(thedata))
  }
}

####
#Includes:
#Notes: Rename a single column
rename_column <- function(thedata,from,to,merge_dup=TRUE)
{
  temp = 0
  if(!to %in% colnames(thedata)){
    colnames(thedata)[colnames(thedata) == from] = to
  }
  else if(to %in% colnames(thedata) && merge_dup){
    #thedata[,to] = ifelse(is.na(thedata[,to]), thedata[,from],thedata[,to])
    thedata[,to] = ifelse(is_nazero(thedata[,to]) & !is_nazero(thedata[,from]), thedata[,from],thedata[,to])
    thedata = drop_vars(thedata,from)
  }
  return(thedata)
}

####
#Includes:
#Notes: Rename multiple columns
rename_columns <- function(thedata,froma,toa,merge_dup=TRUE)
{
  for(i in 1:length(froma)){
    from = froma[i]
    to = toa[i]
    thedata = rename_column(thedata,from,to,merge_dup)
  }
  return(thedata)
}

####
#Includes:
#Notes: Replace NA for a single column
replace_col_na <- function(thedata,thecol,replacement=0)
{
  thedata[thecol][is.na(thedata[thecol])] <- replacement
  return(thedata)
}

####
#Includes:
#Notes: Replace 0s for a single column
replace_col_zero <- function(thedata,thecol,replacement=NA)
{
  thedata[thecol][thedata[thecol] == 0] <- replacement
  return(thedata)
}

####
#Includes: 
#Notes: change a list of colnames from to
change_col_names <- function(thedata,from=NA,to=NA,str=NA,delim=";",switch_from_to=FALSE){
  temp=0
  if(!is.na(str)){
    from = colnames(thedata)
    to = from
    name_matches = unlist(strsplit(str,delim))
    name_matches_df = strsplit(name_matches,"=",TRUE)
    for(i in 1:length(name_matches_df)){
      match = unlist(name_matches_df[i])
      if(switch_from_to){
        old = match[2]
        new = match[1]
      }else{
        old = match[1]
        new = match[2]
      }
      from_index = which(colnames(thedata) == old)
      to[from_index] = new
    }
  }
  for(i in 1:length(from)){
    colnames(thedata)[which(names(thedata) == from[i])] <- to[i]
  }
  return(thedata)
}

####
#Includes: 
#Notes: 
is_error <- function(expr){
  temp = 0
  if("try-error" %in% class(expr)){
    return(TRUE)
  }
  tryCatch(
    expr = {
      expr
    },
    error = function(e){
      return(TRUE)
    },
    finally = {
      return(FALSE)
    }
  )
}

####
#Includes: 
#Notes: DOESN'T WORK IF PASSED FROM A FUNCTION UGH
get_name <- function(obj){
  return(deparse(substitute(obj)))
}

####
#Includes: 
#Notes: Checks if length is 0. I've been using this function, not sure where the one I was using came from, but it's not base R and in Linux it's not found.
is_empty = function(x) {
  return(length(x) == 0)
}

####
#Includes: 
#Notes:
is_date <- function(thedata){
  temp <- 0
  if(class(thedata[1])[1] == "Date"){return(TRUE)}
  if(all(is.na(thedata))){return(FALSE)}
  yyyymmdd <- grepl("\\b\\d{4}[\\.\\-\\/]\\d{1,2}[\\.\\-\\/]\\d{1,2}\\b",thedata,perl=T)
  ddmmyyyy <- grepl("\\b\\d{1,2}[\\.\\-\\/]\\d{1,2}[\\.\\-\\/]\\d{4}\\b",thedata,perl=T)
  if(yyyymmdd || ddmmyyyy){return(TRUE)}
  return(FALSE)
}

####
#Includes: 
#Notes: This seems useless, not sure why I made this
identical_value <- function(x,y)
{
  if (identical(x,y)) TRUE else FALSE
}

####
#Includes: 
#Notes:
is_empty_dataframe <- function(thedata)
{
  return(dim(thedata)[1] == 0)
}

####
#Includes: 
#Notes: because is.na is dumb, it throws an error if you give it a vector or a dataframe
is_nas <- function(thedata)
{
  return(all(is.na(thedata)))
}

####
#Includes: 
#Notes: Stupid R
is_nazero <- function(val)
{
  temp = 0
  if(is_array(val)){
    val = gsub("NA",NA,val)
    result1 = is.na(val)
    result2 = val == 0
    result = result1 | result2
    return(result)
  }else{
    if(is.na(val)){return(TRUE)}
    if(val == 0){return(TRUE)}
    if(val == "NA"){return(TRUE)}
    return(FALSE)
  }
}

####
#Includes:
#Notes: count number of character matches in a string or string array
count_string_matches <- function(stra,find)
{
  return(lengths(regmatches(stra, gregexpr(find, stra))))
}

####
#Includes:
#Notes: 
keep_newest_rows <- function(thedata,key,datef)
{
  temp = 0
  thedata = thedata[order(thedata[,key], thedata[,datef]),]
  thedata = thedata[!duplicated(thedata[,key],fromLast = TRUE), ]
  return(thedata)
}

####
#Includes:
#Notes: 
left_join_quick <- function(thedata1,thedata2,byx,byy=NA,smart=FALSE,ally=FALSE)
{
  temp = 0
  if(is.na(byy)){byy=byx}
  if(smart){
    #return(smerge(x=thedata1,y=thedata2,by.x=byx,by.y=byy,all.x=TRUE,all.y=all.y))
    date_cols = c()
    for(i in 1:ncol(thedata1)){
      if(is.Date(thedata1[,i])){
        date_cols = c(date_cols,colnames(thedata1)[i])
      }
    }
    # drop any NA on right side
    #temp = thedata2
    thedata2 = thedata2[!is.na(thedata2[byy]),]
    result = smerge(x=thedata1,y=thedata2,by.x=byx,by.y=byy,all.x=TRUE,all.y=ally)
    for(i in 1:ncol(result)){
      if(colnames(result)[i] %in% date_cols){
        result[,i] = as.Date(result[,i])
      }
    }
    return(result)
  }else{
    return(merge(x=thedata1,y=thedata2,by.x=byx,by.y=byy,all.x=TRUE,all.y=ally))
  }
}

####
#Includes: 
#Notes: Since R loves to move columns around, simple function to put them back where they were.
fix_column_order <- function(thedata,column_order)
{
  temp = 0
  #column_order <- colnames(original_thedata)
  if(ncol(thedata) == 1){return(thedata)}
  column_order = unique(c(column_order,colnames(thedata)))
  column_order = column_order[column_order %in% colnames(thedata)]
  thedata = thedata[, column_order]
  return(thedata)
}

####
#Includes: 
#Notes: 
date_to_year <- function(thedate)
{
  return(as.integer(format(thedate, format="%Y")))
}
