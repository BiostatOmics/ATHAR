# aux functions for this script
POSIXt2date <- function(data){
  for(cn in colnames(data)){
    if(all(class(data[,cn]) %in% c("POSIXct","POSIXt")))
      data[,cn] = as.Date(data[,cn])
  }
  return(data)
}

getFactorNames <- function(data){
  CN_FACTORS <- colnames(data)[sapply(colnames(data), function(x){is.factor(data[,x])})]
  return(CN_FACTORS)
}

getVarNameFromBinary <- function(cn, VAR_DESCRIPTION){
  aux <- lapply(VAR_DESCRIPTION[which(!is.na(VAR_DESCRIPTION[,1])),1,drop=T], function(x){grep(x, cn)})
  names(aux) = VAR_DESCRIPTION[which(!is.na(VAR_DESCRIPTION[,1])),1,drop=T]
  aux <- names(unlist(aux))
  if(length(aux)==1){
    new_cn <- aux
  }else if(length(aux)>1){
    res <- unlist(lapply(aux, function(x){stringdist::stringdist(x, cn, method = "lv")}))
    new_cn <- aux[which.min(res)]
  }else{
    new_cn <- NA
  }
  return(new_cn)
}

# aux function for third and fourth script

getInfo <- function(var, type="V"){
  FLAG_DESC <- exists("VAR_DESCRIPTION")
  
  if(!FLAG_DESC){
    return(data.frame(Variable = paste0(var), Description = "No description file", Group = "No description file"))
  }
  
  type <- toupper(type)
  if(!var %in% VAR_DESCRIPTION$Variable)
    return(data.frame(Variable = paste0(var, " - Not present"), Description = "Variable not present in the database"))
  
  if(type=="V"){
    return(VAR_DESCRIPTION[VAR_DESCRIPTION$Variable==var,])
  }
  if(type=="D"){
    return(VAR_DESCRIPTION[VAR_DESCRIPTION$Variable==var,]$Description)
  }
}

#### ###
# Functions used in Script 1
#### ###

getUpdatedColnamesDF <- function(data, df.translation, database, keepVariables = F){
  cn_ori <- colnames(data)
  
  subtranslation <- df.translation[,c("Variable", database)]
  subtranslation <- subtranslation[!is.na(subtranslation$`Variable`) & !is.na(subtranslation[,database]),]
  
  colnames_database <- subtranslation[,database,drop=T]
  colnames_database <- colnames_database[colnames_database %in% colnames(data)]
  subtranslation <- subtranslation[subtranslation[,database,drop=T] %in% colnames_database,]
  
  if(!keepVariables){
    subdata <- data[,subtranslation[,database,drop=T]] #just to be sure
    colnames(subdata) <- subtranslation[,"Variable",drop=T]
  }else{
    subdata <- data #just to be sure
    lst_index <- unlist(lapply(subtranslation[,database,drop=T], function(x){which(x == colnames(subdata))}))
    colnames(subdata)[lst_index] <- subtranslation[,"Variable",drop=T]
  }
  return(subdata)
}

# Check if R classes are the sema between template and the datasets
checkRClassesFromFile <- function(data, VAR_TRANSLATION, fix = T){
  subtranslation <- VAR_TRANSLATION[,c("Variable", "Class")]
  subtranslation <- subtranslation[!is.na(subtranslation)[,1],]
  bad_variables <- NULL
  
  for(cn in colnames(data)){
    CLASS_FLAG = T
    
    real_class <- subtranslation[which(subtranslation[,"Variable"]==cn),]$Class
    current_class <- class(data[,cn])
    
    if(real_class == "qualitative"){
      real_class = "factor"
    }else if(real_class == "quantitative"){
      real_class = "numeric"
    }else if(real_class == "ordinal"){
      real_class = "numeric"
    }else if(real_class == "date"){
      real_class = "Date"
    }else if(real_class == "logical"){
      real_class = "logical"
    }else if(real_class == "character"){
      real_class = "character"
    }else{
      message(paste0("Variable class/type must be one of the following: 'character', 'qualitative', 'quantitative', 'ordinal', 'logical' or 'date'. Type ", real_class, " has been found. Fix it."))
      CLASS_FLAG = F
    }
    
    if(real_class != current_class[[1]] & CLASS_FLAG){
      
      #try to fix the class if possible
      fixed = T
      if(fix){
        if(real_class == "numeric"){
          data[,cn] <- tryCatch(expr = {as.numeric(data[,cn])}, error = function(e){data[,cn]})
          if(class(data[,cn])!="numeric"){
            fixed = F
          }
        }
        else if(real_class == "factor"){
          data[,cn] <- tryCatch(expr = {as.factor(data[,cn])}, error = function(e){data[,cn]})
          if(class(data[,cn])!="factor"){
            fixed = F
          }
        }
        else if(real_class == "Date"){
          if(current_class[[1]] == "POSIXct"){
            data[,cn] <- POSIXt2date(data[,cn])
            data[,cn] <- tryCatch(expr = {as.Date(data[,cn])}, error = function(e){data[,cn]})
            if(class(data[,cn])!="Date"){
              fixed = F
            }
          }else{
            data[,cn] <- tryCatch(expr = {as.Date(data[,cn])}, error = function(e){data[,cn]})
            if(class(data[,cn])!="Date"){
              fixed = F
            }
          }
        }
        else if(real_class == "character"){
          data[,cn] <- tryCatch(expr = {as.character(data[,cn])}, error = function(e){data[,cn]})
          if(class(data[,cn])!="character"){
            fixed = F
          }
        }else if(real_class == "logical"){
          data[,cn] <- tryCatch(expr = {as.logical(data[,cn])}, error = function(e){data[,cn]})
          if(class(data[,cn])!="logical"){
            fixed = F
          }
        }
        
        bad_variables <- rbind(bad_variables, c(cn,real_class,current_class[[1]], fixed))
      }else{
        bad_variables <- rbind(bad_variables, c(cn,real_class,current_class[[1]]))
      }
    }
  }
  
  if(fix & length(bad_variables)!=0){
    colnames(bad_variables) <- c("Variable", "Class", "Found", "Fixed")
  }else if(length(bad_variables)!=0){
    colnames(bad_variables) <- c("Variable", "Class", "Found")
  }
  
  problems <- bad_variables[bad_variables[,4]=="FALSE",,drop=F]
  
  if(length(problems)==0){
    problems = NULL
  }
  
  fixed <- bad_variables[bad_variables[,4]=="TRUE",,drop=F]
  
  if(length(fixed)==0){
    fixed = NULL
  }
  
  return(list(data = data, problems = problems, fixed = fixed))
}

checkFactors <- function(data, reference, VAR_TRANSLATION, name, verbose = F){
  
  CN_FACTORS <- getFactorNames(reference)
  lst_variables <- list()
  
  for(cn in CN_FACTORS){
    if(cn %in% colnames(data)){ #colnames should be a factor
      
      if(is.factor(data[,cn])){
        aux <- data[,cn]
      }else{
        aux <- as.factor(data[,cn])
      }
      
      v <- levels(reference[,cn]) #reference levels
      vv <- levels(aux) #new df levels
      
      if(any(!vv %in% v)){
        if(verbose){
          message("Data:")
          message(cn)
          print(vv)
          message("Reference:")
          printLevels(reference, cn)
          message("")
        }
        
        ori_cn <- VAR_TRANSLATION[which(VAR_TRANSLATION[[1]] == cn), name, drop=T]
        title <- c(paste0("VAR: ", cn, " / ", ori_cn), paste0(v, collapse = ", "))
        df <- data.frame(WRONG = vv, NEW = rep(NA, length(vv)))
        
        lst_variables[[cn]] <- list()
        lst_variables[[cn]][[1]] <- title
        lst_variables[[cn]][[2]] <- df
      }
    }
  }
  return(lst_variables)
}

factorToExcel <- function(lst_factor, path){
  if(length(lst_factor)==0){
    return()
  }
  
  df <- NULL
  for(cn in names(lst_factor)){
    df <- as.data.frame(rbind(df, lst_factor[[cn]][[1]]))
    aux <- lst_factor[[cn]][[2]]
    colnames(aux) <- colnames(df)
    df <- rbind(df, aux)
  }
  
  openxlsx::write.xlsx(df, file = paste0(path), overwrite = T, col.names = F)
  return()
}

getVarDescDF <- function(VAR_DESCRIPTION, lst_cn){
  new_cn <- NULL #taking into account binary vars with different termination
  for(cn in lst_cn){
    
    if(cn %in% VAR_DESCRIPTION[which(!is.na(VAR_DESCRIPTION[,1])),1,drop=T]){
      new_cn <- c(new_cn, cn)
    }else{
      new_cn <- c(new_cn, getVarNameFromBinary(cn))
    }
  }
  
  if(length(new_cn)==0){
    return(NULL)
  }else{
    total_vars <- VAR_DESCRIPTION[!is.na(VAR_DESCRIPTION[,1]),]
    tbl <- as.data.frame(total_vars[total_vars[[1]] %in% new_cn,])
    rownames(tbl) <- tbl$Variable
    tbl <- tbl[new_cn,]
    rownames(tbl) <- NULL
    return(tbl)
  }
}
