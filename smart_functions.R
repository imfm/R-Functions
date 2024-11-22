####
# These are functions that deal with R's inconsistency/tendency to throw unecessary errors, particularly when it comes to dealing with NA.
####

####
#Includes: 
#Notes: 'Smart' mean. Removes NA before pasting. Not really necessary since base mean has a na.rm parameter
mean_xna <- function(...){
  vals <- unlist(list(...))
  vals = na.omit(vals)
  if(length(vals) == 0){return(NA)}
  return(mean(vals))
}

####
#Includes: 
#Notes: get last value that's not null, optionally treating 0 as null
last_xna <-function(thedata, zero_is_na = TRUE)
{
  temp = 0
  if(all(is.na(thedata))){return(NA)}
  for(i in length(thedata):1)
  {
    val = thedata[i]
    if(!is.na(val)){
      if(!zero_is_na || thedata[i] != 0){
        #return(val)
        if(class(thedata) == 'Date'){
          return(as.Date(val))
        }else{
          return(val)
        }
      }
    }
  }
  # this shouldn't happen
  return(FALSE)
}

####
#Includes: 
#Notes: get first value that's not null, optionally treating 0 as null
first_xna<-function(thedata, zero_is_na = TRUE)
{
  temp = 0
  if(all(is.na(thedata))){return(NA)}
  for(i in 1:length(thedata))
  {
    val = thedata[i]
    if(!is.na(val)){
      if(!zero_is_na || thedata[i] != 0){
        return(val)
      }
    }
  }
  # this shouldn't happen
  return(FALSE)
}

####
#Includes: 
#Notes: get min value that's not null
min_xna<-function(thedata)
{
  temp = 0
  if(all(is.na(thedata))){return(NA)}
  vals = na.omit(thedata)
  return(min(vals))
}

####
#Includes: 
#Notes: get max value that's not null
max_xna<-function(thedata)
{
  temp = 0
  if(all(is.na(thedata))){return(NA)}
  vals = na.omit(thedata)
  return(max(vals))
}

####
#Includes: 
#Notes: sum value that's not null
sum_xna<-function(thedata)
{
  temp = 0
  if(all(is.na(thedata))){return(NA)}
  vals = na.omit(thedata)
  return(sum(vals))
}

####
#Includes: 
#Notes: Concatenate data, can be characters or integers.
concatenate<-function(thedata,return_as='character',sep=NA,undup=FALSE)
{
  if(is.na(sep)){
    if(return_as == 'character'){
      sep = ' '
    }else{
      sep = ''
    }
  }
  temp = 0
  result = ""
  for(i in 1:length(thedata))
  {
    cval = thedata[i]
    if(!is.na(cval)){
      if(i == 1){result = cval
      }else{
        if(undup){
          if(grepl(cval,result,fixed=T)){
            next
          }
        }
        result = paste(result,cval,sep=sep)
      }
    }
  }
  result = trimws(result)
  if(return_as != 'character'){
    func = paste0('as.',return_as)
    result = do.call(func,list(result))
  }
  return(result)
}

####
#Includes: 
#Notes: call concatenate with undup = TRUE. used to simplify calls from aggregate
concatenate_undup <- function(thedata,return_as='character',sep=NA,undup=TRUE)
{
  result = concatenate(thedata,return_as=return_as,sep=sep,undup=undup)
  return(result)
}

####
#Includes: 
#Notes: call concatenate with return_as='integer'. used to simplify calls from aggregate
concatenate_int <- function(thedata,return_as='integer',sep="",undup=FALSE)
{
  result = concatenate(thedata,return_as=return_as,sep=sep,undup=undup)
  return(result)
}

####
#Includes: 
#Notes: call concatenate with return_as='integer'. used to simplify calls from aggregate
concatenate_int_undup <- function(thedata,return_as='integer',sep="",undup=TRUE)
{
  result = concatenate(thedata,return_as=return_as,sep=sep,undup=undup)
  return(result)
}

####
#Includes: 
#Notes: call concatenate with sep=';'. used to simplify calls from aggregate
concatenate_string_array <- function(thedata,return_as='character',sep=";",undup=FALSE)
{
  result = concatenate(thedata,return_as=return_as,sep=sep,undup=undup)
  return(result)
}

####
#Includes: 
#Notes:
is_number <- function(thedata){
  temp <- 0
  if(all(is.na(thedata))){
    return(FALSE)
  }else{
    return(grepl('^(0|[1-9][0-9]*)$',thedata))
  }
}

####
#Includes: 
#Notes: Get the number of digits
number_digits = function(x) {
  result = floor(log10(abs(x))) + 1
  result[!is.finite(result)] = 0
  return(result)
}

####
#Includes: 
#Notes: 'Smart' paste. Removes NA before pasting.
spaste <- function(..., sep=''){
  stra <- unlist(list(...))
  stra = na.omit(stra)
  paste(stra,collapse=sep)
}

####
#Includes: 
#Notes: 'Smart' aggregate. Prevents converting dates to num when there's NAs. Also removes extra columns and puts things back into correct order.
# This works, but I hate doing it this way as aggregate is unpredicatable/volatile
saggregate <- function(x, by, FUN, return_cols = NA, return_by = TRUE){
  temp = 0
  
  # for aggregate, need to convert keys to a list
  if(!is.list(by) && class(by) == 'character'){
    by_bak = by
    for(i in 1:length(by)){
      if(i == 1){
        key_list = x[by[i]]
      }else{
        key_list = c(key_list,x[by[i]])
      }
    } 
  }
  by = key_list
  
  # get class of each column
  date_cols = c()
  for(i in 1:ncol(x)){
    if(class(x[,i]) == 'Date'){
      date_cols = c(date_cols,i)
    }
  }
  
  # if date and there's any na, store the column index so we can fix it later
  na_date_cols = c()
  if(length(date_cols) > 0){
    for(i in 1:length(date_cols)){
      if(any(is.na(x[date_cols[i]]))){
        na_date_cols = c(na_date_cols,date_cols[i])
      }
    }
  }

  result = aggregate(x, by, FUN)
  
  # remove extra columns and put things back in the right order
  unique_names <- unique(colnames(result)) # Find Unique Column Names
  result = result[unique_names] # Keep Only Unique Column Names
  col_order <- colnames(x)
  result <- result[, col_order]
  result = remove_vars_regex(result,"Group.")
  
  # fix the nuked dates
  if(length(na_date_cols) > 0){
    for(i in 1:length(na_date_cols)){
      result[,na_date_cols[i]] = as.Date(result[,na_date_cols[i]], origin = "1970-01-01")
    }
  }
  
  # return specific columns?
  if(!any(is.na(return_cols))){
    result = result[return_cols]
  }
  
  # return keys?
  if(!return_by){
    #temp_remove_cols <- c('rwn_id', 'first_name')
    result = subset(result, select = !(names(result) %in% by)) 
  }
  
  return(result)
}

####
#Includes: 
#Notes: Only do the operation if no nas, optioinally no zeros
paste0_xna <- function(...,xzero=TRUE)
{
  temp = 0
  input = list(...)
  cols = length(input)
  rows = 0
  for(i in 1:cols){
    crows = length(input[[i]])
    if(crows > rows){rows = crows} # find the list element with max rows
    #temp = input[i]
    #temp = unlist(input[i])
  }
  output = c()
  for(i in 1:rows){
    str = ""
    for(j in 1:cols){
      if(length(input[[j]]) == 1){
        val = input[[j]][[1]]
      }else{
        val = input[[j]][[i]]
      }
      if(is.na(val)){
        str = NA
        break
      }
      if(xzero && val == 0){
        str = NA
        break
      }
      str = paste0(str,val)
    }
    output = c(output,str)
  }
  return(output)
}

####
#Includes: 
#Notes: merge, but then mush resulting extra columns back into single column
smerge <- function(...)
{
  temp = 0
  
  result = merge(...)
  
  drop_cols = c()
  for(i in 1:length(result)){
    cn = colnames(result)[i]
    if(cn %in% drop_cols){next}
    is_split = grepl('.x',cn,fixed=T)
    if(is_split){
      drop_cols = c(drop_cols,cn)
      cnr = gsub(".x",".y",cn,fixed=T)
      cnm = gsub(".x","",cn,fixed=T)
      drop_cols = c(drop_cols,cnr)
      result[,cnm] <- ifelse(is.na(result[,cn]), result[,cnr], result[,cn])
    }
  }
  # drop split cols
  #result = subset(result, select())
  result = result[,!(names(result) %in% drop_cols)]
  
  # put back in original order
  #elist = list(...)
  #if(hasArg(x)) {
    #  x = eval.parent(match.call()[["x"]])
    xnames = names(eval.parent(match.call()[["x"]]))
  #}
  #if(hasArg(y)) {
    #  y = eval.parent(match.call()[["y"]])
    ynames = names(eval.parent(match.call()[["y"]]))
  #}
  col_order = union(xnames, ynames)
  col_order = union(col_order, names(result))
  col_order = col_order[col_order %in% names(result)]
  result = result[, col_order]
  ## sort by first value
  result = result[order(result[,1]),]
  
  return(result)
}

####
#Includes: 
#Notes: bind (stack) TWO dataframes even if they don't have the same columns
srbind <- function(thedata1,thedata2)
{
  if(nrow(thedata1) == 0){
    return(thedata2)
  }
  if(nrow(thedata2) == 0){
    return(thedata1)
  }
  if(nrow(thedata1) > 0){
    thedata1[setdiff(names(thedata2), names(thedata1))] <- NA
  }
  if(nrow(thedata2) > 0){
    thedata2[setdiff(names(thedata1), names(thedata2))] <- NA
  }
  
  result = unique(rbind(thedata1,thedata2))
  
  return(result)
}

####
#Includes: 
#Notes: bind n dataframes by columns, remove row labels and duplicate columns
scbind <- function(...)
{
  result = unique(cbind(...))
  # remove row labels
  rownames(result) <- NULL
  
  # merge duplicated columns
  for(i in 1:ncol(result)){
    if(i + 1 <= ncol(result)){
      for(j in (i+1):(ncol(result))){ # for some reason r goes past ncol(result)
        cn = colnames(result)[i]
        ncn = colnames(result)[j]
        if(cn == ncn){
          result[,i] <- ifelse(is.na(result[,i]), result[,j], result[,i])
        }
      }
    }
  }
  
  # remove (last) duplicate columns
  result = result[, !duplicated(colnames(result))]
  
  return(result)
}

####
#Includes: 
#Notes: GREP remove variable from global (or other environment). Can be used inside functions. Also, suppress warnings.
srm <- function(obj = NA, pattern = NA, envir = ".GlobalEnv")
{
  temp = 0
  if(!is.na(pattern)){
    pattern = deparse(substitute(pattern))
    pattern = gsub('\"',"",pattern,fixed=T)
    objs <- ls(pos = envir)
    suppressWarnings(rm(list = objs[grep(pattern, objs)], pos = envir))
  }else{
    obj = deparse(substitute(obj))
    objs <- ls(pos = envir)
    suppressWarnings(rm(list = objs[objs == obj], pos = envir))
  }

  return(TRUE)
}

####
#Includes: 
#Notes: Uppercase a string or array of strings, saving the unique non-uppercase values in a global variable
stoupper <- function(str)
{
  temp = 0
  
  if(typeof(str) != "character"){
    return(str)
  }
  
  str = trimws(str)
  
  #nupper = str
  upper = toupper(str)
  
  strupper = data.frame(str,upper)
  strupper = strupper[strupper$str != strupper$upper,] # remove when both are same
  strupper = unique(strupper)
  #dataset = dataset[dataset$shared_id != 0,]
  
  if(! exists('strupper', envir = dbEnv)){
    dbEnv$strupper = strupper
  }else{
    dbEnv$strupper = unique(srbind(dbEnv$strupper, strupper))
  }

  return(upper)
}

####
#Includes: 
#Notes: Uppercase all strings in array, saving original values
stoupper_df <- function(thedata,include=NA)
{
  temp = 0
  for(i in 1:ncol(thedata)){
    temp = typeof(thedata[,i])
    if(typeof(thedata[,i]) == "character"){
      if(!is.na(include)){
        name = colnames(thedata)[i]
        name_include = grepl(include,name,fixed = T)
      }else{
        name_include = T
      }
      if(name_include){
        thedata[,i] = stoupper(thedata[,i])
      }
    }
  }
  return(thedata)
}

####
#Includes: 
#Notes: Use global variable to revert uppercase to its non-uppercase form
suntoupper <- function(str)
{
  temp = 0
  
  str = trimws(str)
  
  upper = toupper(str)
  
  unupperi = match(upper, dbEnv$strupper$upper)
  
  unupper = ifelse(is.na(unupperi), str, dbEnv$strupper$str[unupperi])
  
  return(unupper)
}

####
#Includes:
#Notes: Similar functionality to agrep, but use adist instead to prevent loose matches on short strings like 'STR' = 'A STRING'
sagrep <- function(find,stra,max.distance=0.1,ignore.case=TRUE,fixed=TRUE,partial=FALSE,costs = list(ins=1, del=1, sub=1),value = FALSE,first_char_cost=0)
{
  #print(find)
  if(find == "FRIENDS SHINE ONLINE"){
    temp = 0
  }
  
  best_index <- 0
  
  find_bak = find
  stra_bak = stra
  
  if(ignore.case){
    find = toupper(find)
    stra = toupper(stra)
  }
  
  # try to find an exact match first
  best_index = match(find,stra,0)
  
  # try to find a match with no spaces
  if(F){
    if(is_empty(best_index)){best_index = 0}
    if(all(best_index == 0)){
      find = gsub(" ","",find)
      stra = gsub(" ","",stra)
      best_index = match(find,stra,0)
      if(is_empty(best_index)){
        best_index = 0
      }
      find = find_bak
      stra = stra_bak
    }
  }
  
  # Use adist
  if(is_empty(best_index)){best_index = 0}
  if(all(best_index == 0)){
    if(max.distance < 1){
      #is a fraction. Calculate based on size of strings.
      #costs = list(ins=max.distance, del=max.distance, sub=max.distance)
      min_size = min(nchar(stra),na.rm=T)
      max_size = max(nchar(stra),na.rm=T)
      avg_size = (min_size + max_size) / 2
      find_size = nchar(find)
      max.distance = (find_size + avg_size) * max.distance
      max.distance = round(max.distance)
    }
    dist <- as.vector(adist(find, stra, ignore.case = ignore.case, fixed = fixed, partial = partial, costs = costs))
    # Add cost if the first character doesn't match
    if(first_char_cost > 0){
      find_first_char = substr(find,1,1)
      stra_first_char = substr(stra,1,1)
      dist = ifelse(stra_first_char != find_first_char, dist + first_char_cost, dist)
    }
    min_dist <- min(dist,na.rm=TRUE)
    if(min_dist > max.distance){min_dist <- NA}
    if(!is.na(min_dist)){best_index <- which(dist == min_dist)}
    
    # Still no match, do a reverse adist
    ## Takes too long
    if(F){
      if(is_empty(best_index)){best_index = 0}
      if(all(best_index == 0)){
        for(i in 1:length(stra)){
          dist <- adist(stra[i], find, ignore.case = ignore.case, fixed = fixed, partial = partial, costs = costs)
          # Add cost if the first character doesn't match
          if(first_char_cost > 0){
            find_first_char = substr(find,1,1)
            stra_first_char = substr(stra[i],1,1)
            dist = ifelse(stra_first_char != find_first_char, dist + first_char_cost, dist)
          }
          min_dist <- dist
          if(min_dist > max.distance){min_dist <- NA}
          if(!is.na(min_dist)){
            best_index <- i
            break
          }
        }
      }
    }
    
    temp = 0
    
    # Use adist, but check if find is in stra or reverse. Match if ???
    if(F){
      if(is_empty(best_index)){best_index = 0}
      if(all(best_index == 0)){ # is find in stra
        dist <- as.vector(adist(find, stra, ignore.case = ignore.case, fixed = fixed, partial = partial, costs = list(ins=1, del=99, sub=99)))
        min_dist <- min(dist,na.rm=TRUE)
        if(min_dist > max.distance){min_dist <- NA}
        if(!is.na(min_dist)){best_index <- which(dist == min_dist)}
      }
      if(is_empty(best_index)){best_index = 0}
      if(all(best_index == 0)){ # is stra in find
        dist <- as.vector(adist(find, stra, ignore.case = ignore.case, fixed = fixed, partial = partial, costs = list(ins=99, del=1, sub=99)))
        min_dist <- min(dist,na.rm=TRUE)
        if(min_dist > max.distance){min_dist <- NA}
        if(!is.na(min_dist)){best_index <- which(dist == min_dist)}
      }
    }
  }
  
  temp = 0
  
  # Still no match, see if entire string is in a row
  ## No good. Cleaned name Child Inc -> Child matches anything with Child in it. 
  if(F){
    if(is_empty(best_index)){best_index = 0}
    if(all(best_index == 0)){
      best_index = which(grepl(find, stra, fixed = T))
    }
  }
  # Still no match, see if entire string is in a row in reverse
  ## No good. Cleaned name Child Inc -> Child matches anything with Child in it. 
  if(F){
    if(is_empty(best_index)){best_index = 0}
    if(all(best_index == 0)){
      for(i in 1:length(stra)){
        rev_grepl = which(grepl(stra[i],find, fixed = T))
        if(!is_empty(rev_grepl)){
          best_index = i
          break
        }
      }
    }
  }
  
  temp = 0
  
  if(is_empty(best_index)){best_index = 0}
  
  if(value){best_index = stra_bak[best_index]}
  
  return(best_index)
}

####
#Includes:
#Notes: Approximate match using adist
amatch <- function(find,stra,no_match=NA_integer_,max.distance=0.1,ignore.case=TRUE,fixed=TRUE,partial=FALSE,costs = list(ins=1, del=1, sub=1),value = FALSE,first_char_cost=0)
{
  
  temp = 0
  
  # try to find an exact match first
  best_index = match(find,stra,0)
  
  # Use adist
  if(all(best_index == 0)){
    if(max.distance < 1){
      #is a fraction. Calculate based on size of strings.
      #costs = list(ins=max.distance, del=max.distance, sub=max.distance)
      min_size = min(nchar(stra))
      max_size = max(nchar(stra))
      avg_size = (min_size + max_size) / 2
      find_size = nchar(find)
      max.distance = (find_size + avg_size) * max.distance
      max.distance = round(max.distance)
    }
    dist <- as.vector(adist(find, stra, ignore.case = ignore.case, fixed = fixed, partial = partial, costs = costs))
    # Add cost if the first character doesn't match
    if(first_char_cost > 0){
      find_first_char = substr(find,1,1)
      stra_first_char = substr(stra,1,1)
      dist = ifelse(stra_first_char != find_first_char, dist + first_char_cost, dist)
    }
    min_dist <- min(dist,na.rm=TRUE)
    if(min_dist > max.distance){min_dist <- NA}
    if(!is.na(min_dist)){best_index <- which(dist == min_dist)}
  }
  
  if(all(best_index == 0)){best_index = no_match}
  
  return(best_index)
}
