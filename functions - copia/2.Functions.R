# aux functions for this script

removeFactor <- function(data, lst_cn, remove){
  for(cn in lst_cn){
    #must be factor
    if(!class(data[,cn])=="factor"){
      data[,cn] <- factor(data[,cn])
    }
    
    #update 2remove for NA
    if(any(levels(data[,cn]) %in% remove)){
      lvl <- levels(data[,cn])[which(levels(data[,cn]) %in% remove)]
      newValue <- rep(NA, length(lvl))
      names(newValue) <- lvl
      data[,cn] <- plyr::revalue(data[,cn], newValue)
    }
  }
  return(data)
}

revalueFactors <- function(data, lst, old, new){
  #check
  if(!length(old) == length(new))
    stop("The length for old and new values have to be the same.")
  
  #get names
  new_value <- new
  names(new_value) <- old
  
  for(cn in lst){
    if(!cn %in% colnames(data))
      next
    
    if(is.factor(data[,cn])){
      aux <- new_value[names(new_value) %in% levels(data[,cn])]
      data[,cn] <- plyr::revalue(data[,cn], aux)
    }else{
      #if not factor, should be
      data[,cn] <- factor(data[,cn])
      aux <- new_value[names(new_value) %in% levels(data[,cn])]
      data[,cn] <- plyr::revalue(data[,cn], aux)
    }
  }
  return(data)
}

#### ###
# Functions used in Script 2
#### ###

updateFactorExcel <- function(data, excel){
  cn = NULL
  for(row in 1:nrow(excel)){
    if(startsWith(x = excel[row,1], prefix = "VAR: ")){
      cn = strsplit(excel[row,1], split = " ")[[1]][2]
      next
    }else{
      old_value <- excel[row,1]
      new_value <- excel[row,2]
      if(is.na(new_value)){
        data <- removeFactor(data, cn, old_value) #remove factor bc does not have an equivalence
      }else if(old_value == new_value){
        next #we will add the factor
      }else{
        data <- revalueFactors(data, cn, old_value, new_value) #refactor
      }
    }
  }
  return(data)
}

addFactors <- function(data, lst_cn, new){
  for(cn in lst_cn){
    levels(data[,cn]) <- unique(c(levels(data[,cn]), new))
  }
  return(data)
}

addVarInfo <- function(VAR_DESCRIPTION, var_name, var_desc, var_cat){
  if(!var_name %in% VAR_DESCRIPTION$Variable){
    if(var_cat %in% unique(VAR_DESCRIPTION$Group)){
      VAR_DESCRIPTION <- rbind(VAR_DESCRIPTION, c(var_name, var_desc, var_cat))
    }else{
      message("var_cat must be one of the following categories:")
      message(paste(unique(VAR_DESCRIPTION$Group), collapse = ", "))
      stop()
    }
  }
  return(VAR_DESCRIPTION)
}

dropLevelsAllColumns <- function(data, keep = NULL){
  aux <- data
  #Drops unused factor levels
  for(cn in colnames(data)){
    if(is.factor(data[,cn])){
      if(cn %in% keep)
        next
      
      aux[,cn] <- droplevels(data[,cn])
    }
  }
  return(aux)
}

info_dimensions <- function(data, names = NULL){
  if(is.null(names)){
    d <- as.data.frame(dim(data))
  }else{
    d <- as.data.frame(dim(data[[names]]))
  }
  
  colnames(d) <- "Dataset statistics"
  rownames(d) <- c("Number of observations", "Number of variables")
  
  return(d)
}
