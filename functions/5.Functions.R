# aux functions for this script

save_ggplot <- function(plot, folder = NULL, name = NULL, wide = T, quality = "4K", dpi = 80, custom = NULL){
  width=NULL
  height=NULL
  
  if(!quality %in% c("HD", "FHD", "2K", "4K", "8K")){
    stop("uality must be one of the following options: 'HD', 'FHD', '2K', '4K', '8K'")
  }
  
  ratios <- c(1.5,1.333333,1.5,2)
  
  if(wide){
    if(quality == "HD"){
      width = 1280/dpi#4.266667
      height = 720/dpi#2.4
    }else if(quality == "FHD"){
      dpi = dpi * ratios[1]
      width = 1920/dpi#6.4
      height = 1080/dpi#3.6
    }else if(quality == "2K"){
      dpi = dpi * ratios[1] * ratios[2]
      width = 2560/dpi#8.533333
      height = 1440/dpi#4.8
    }else if(quality == "4K"){
      dpi = dpi * ratios[1] * ratios[2] * ratios[3]
      width = 3840/dpi#12.8
      height = 2160/dpi#7.2
    }else if(quality == "8K"){
      dpi = dpi * ratios[1] * ratios[2] * ratios[3] * ratios[4]
      width = 7680/dpi#25.6
      height = 4320/dpi#14.4
    }
  }else{
    if(quality == "HD"){
      width = 960/dpi#3.19992
      height = 720/dpi
    }else if(quality == "FHD"){
      dpi = dpi * ratios[1]
      width = 1440/dpi#4.79988
      height = 1080/dpi
    }else if(quality == "2K"){
      dpi = dpi * ratios[1] * ratios[2]
      width = 1920/dpi#6.39984
      height = 1440/dpi
    }else if(quality == "4K"){
      dpi = dpi * ratios[1] * ratios[2] * ratios[3]
      width = 2880/dpi#9.59976
      height = 2160/dpi
    }else if(quality == "8K"){
      dpi = dpi * ratios[1] * ratios[2] * ratios[3] * ratios[4]
      width = 5760/dpi#19.19952
      height = 4320/dpi
    }
  }
  
  if(!is.null(custom)){
    if(length(custom)==2){
      width = custom[1]
      height = custom[2]
    }
  }
  
  if(!endsWith(name,".tiff")){
    name <- paste0(name, ".tiff")
  }
  
  ggsave(plot = plot, filename = paste0(folder,name), width = width, height = height, device='tiff', dpi=dpi)
}

#### ###
# Functions used in Script 5
#### ###

#the structure is a lst of dataframes, we should remove all the patients for each group
deletePatientsInGroupOfVariables <- function(list_variables, patientsToDelete){
  aux <- list_variables
  for(name in names(aux)){
    aux[[name]] <-  deletePatients(data = list_variables[[name]], patientsToDelete = patientsToDelete)
  }
  return(aux)
}

venn_diagram_by_list <- function(dataset_name, lst_names_categories, 
                                 list_venn1, list_venn2, list_venn3 = NULL, list_venn4 = NULL, folder, suffix_text = NULL){ #list of IDS
  
  lst_venn_delete <- list(list_venn1[[dataset_name]],
                          list_venn2[[dataset_name]])
  
  if(!is.null(list_venn3)){
    lst_venn_delete <- append(lst_venn_delete, list_venn3[[dataset_name]])
  }
  
  if(!is.null(list_venn4)){
    lst_venn_delete <- append(lst_venn_delete, list_venn4[[dataset_name]])
  }
  
  names(lst_venn_delete) <- lst_names_categories
  
  lst_null <- NULL
  for(cn in names(lst_venn_delete)){
    if(is.null(lst_venn_delete[[cn]])){
      lst_null <- c(lst_null, cn)
    }
  }
  
  if(length(lst_null)>0){
    lst_venn_delete[lst_null] <- NULL
  }
  
  if(length(lst_venn_delete)<1){
    return(NULL)
  }
  
  colors <- RColorConesa::colorConesa(length(lst_venn_delete))
  alpha <- 0.5
  fill_vector <- NULL
  for(c in colors){
    fill_vector <- c(fill_vector, scales::alpha(c,alpha))
  }
  
  venn.variables <- VennDiagram::venn.diagram(
    x = lst_venn_delete,
    col = colors,
    fill = fill_vector,
    cat.col = colors,
    cat.default.pos = "text",
    cat.pos = rep(0,length(lst_venn_delete)),
    filename = NULL,
    output=TRUE)
  
  save_ggplot(venn.variables, folder, name = paste0(dataset_name,"_venn", ifelse(!is.null(suffix_text),paste0("_", suffix_text),""),".tiff"), wide = T)
  return(venn.variables)
}

# PROVISIONAL !!!
#Read Numerical Ranges
readNumericalRanges <- function(path = NULL){
  library(readxl)
  
  if(is.null(path)){
    if(.Platform$OS.type == "unix") {
      path <- paste0("~/MEGA/Doctorado/Otros proyectos/COVID19/data/numericRanges.xlsx")
    } else {
      path <- paste0("D:/Pedro/Mega/Doctorado/Otros proyectos/COVID19/data/numericRanges.xlsx")
    }
  }
  
  num_ranges <- read_excel(path)
  return(num_ranges)
}

update_lstDF_by_lstVar <- function(lst_all_hospitals, lst_var, col_var = "d.num"){
  lst_all_hospitals_aux <- lst_all_hospitals
  for(h in names(lst_all_hospitals_aux)){
    if(col_var %in% names(lst_var[[h]])){
      cns <- colnames(lst_var[[h]][[col_var]])
    }else if(col_var == "all"){
      cns <- NULL
      for(t in names(lst_var[[h]])){
        cns <- c(cns, colnames(lst_var[[h]][[t]]))
      }
    }else{
      stop("No col_var was found in lst_var object.")
    }
    
    if(all(cns %in% colnames(lst_all_hospitals_aux[[h]]))){
      lst_all_hospitals_aux[[h]][,cns] <- lst_var[[h]][[col_var]][,cns]
    }else{
      stop(paste0("There is a column in lst_var that is not present in lst_all_hospitals for ", h))
    }
  }
  return(lst_all_hospitals_aux)
}

#DELETE VARIABLES
deleteVariables <- function(data = LST_DATA_MERGE[[1]], lst_var = RES_VAR_BIAS[[1]]){
  cn_data <- colnames(data)
  
  for(cn in cn_data){
    if(length(grep(cn, lst_var, fixed = T))>0){
      data[,cn] <- NULL
    }
  }
  
  return(data)
}