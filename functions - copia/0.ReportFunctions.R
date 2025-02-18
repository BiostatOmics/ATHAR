#Funtions to print results into REPOTS

invisibleKable <- function(){
  white_spaces <- data.frame(".")
  colnames(white_spaces) <- NULL
  rownames(white_spaces) <- NULL
  return(knitr::kable(white_spaces, col.names = "") %>% row_spec((white_spaces %>% nrow()), extra_latex_after = "\\arrayrulecolor{white}", color = "white"))
}

#Kable table formatted correctly
#Description will be the last column
kableFormatted <- function(df, colnames=NA, pos="left", MAXLENGTH=50, LongDescription=F, fit=F, scroll=F){
  #knitr and kableExtra
  
  scroll_px <- "450px"
  
  desc <- F
  index <- 0
  if("Desc" %in% colnames(df) | LongDescription){
    desc <- T
    index <- which(colnames(df)=="Desc")
    if(LongDescription)
      index <- ncol(df)
  }
  
  if(scroll){
    if(!sum(is.na(colnames))){
      ifelse(desc,
             return(knitr::kable(df, col.names = colnames, longtable = ifelse(fit, F, T), booktabs = T, escape = F) %>% kableExtra::row_spec(0, bold=T) %>% kableExtra::column_spec(index, width = paste0(MAXLENGTH,"em")) %>% kableExtra::kable_styling(latex_options=ifelse(fit,c("scale_down"),c("basic", "repeat_header")), bootstrap_options = "responsive", full_width = F, position = pos, repeat_header_continued = "\\textit{(Continued on Next Page...)}") %>% scroll_box(width = "100%", height = ifelse(scroll, scroll_px, "100%"))),
             return(knitr::kable(df, col.names = colnames, longtable = ifelse(fit, F, T), booktabs = T, escape = F) %>% kableExtra::row_spec(0, bold=T) %>% kableExtra::kable_styling(latex_options=ifelse(fit,c("scale_down"),c("basic", "repeat_header")), bootstrap_options = "responsive", full_width = F, position = pos, repeat_header_continued = "\\textit{(Continued on Next Page...)}") %>% scroll_box(width = "100%", height = ifelse(scroll, scroll_px, "100%")))
      )
    }else{
      ifelse(desc,
             return(knitr::kable(df, longtable = ifelse(fit, F, T), booktabs = T, escape = F) %>% kableExtra::row_spec(0, bold=T) %>% kableExtra::column_spec(index, width = paste0(MAXLENGTH,"em")) %>% kableExtra::kable_styling(latex_options=ifelse(fit,c("scale_down"),c("basic", "repeat_header")), bootstrap_options = "responsive", full_width = F, position = pos, repeat_header_continued = "\\textit{(Continued on Next Page...)}") %>% scroll_box(width = "100%", height = ifelse(scroll, scroll_px, "100%"))),
             return(knitr::kable(df, longtable = ifelse(fit, F, T), booktabs = T, escape = F) %>% kableExtra::row_spec(0, bold=T) %>% kableExtra::kable_styling(latex_options=ifelse(fit,c("scale_down"),c("basic", "repeat_header")), bootstrap_options = "responsive", full_width = F, position = pos, repeat_header_continued = "\\textit{(Continued on Next Page...)}") %>% scroll_box(width = "100%", height = ifelse(scroll, scroll_px, "100%")))
      )
    }
  }else{
    if(!sum(is.na(colnames))){
      ifelse(desc,
             return(knitr::kable(df, col.names = colnames, longtable = ifelse(fit, F, T), booktabs = T, escape = F) %>% kableExtra::row_spec(0, bold=T) %>% kableExtra::column_spec(index, width = paste0(MAXLENGTH,"em")) %>% kableExtra::kable_styling(latex_options=ifelse(fit,c("scale_down"),c("basic", "repeat_header")), bootstrap_options = "responsive", full_width = F, position = pos, repeat_header_continued = "\\textit{(Continued on Next Page...)}")),
             return(knitr::kable(df, col.names = colnames, longtable = ifelse(fit, F, T), booktabs = T, escape = F) %>% kableExtra::row_spec(0, bold=T) %>% kableExtra::kable_styling(latex_options=ifelse(fit,c("scale_down"),c("basic", "repeat_header")), bootstrap_options = "responsive", full_width = F, position = pos, repeat_header_continued = "\\textit{(Continued on Next Page...)}"))
      )
    }else{
      ifelse(desc,
             return(knitr::kable(df, longtable = ifelse(fit, F, T), booktabs = T, escape = F) %>% kableExtra::row_spec(0, bold=T) %>% kableExtra::column_spec(index, width = paste0(MAXLENGTH,"em")) %>% kableExtra::kable_styling(latex_options=ifelse(fit,c("scale_down"),c("basic", "repeat_header")), bootstrap_options = "responsive", full_width = F, position = pos, repeat_header_continued = "\\textit{(Continued on Next Page...)}")),
             return(knitr::kable(df, longtable = ifelse(fit, F, T), booktabs = T, escape = F) %>% kableExtra::row_spec(0, bold=T) %>% kableExtra::kable_styling(latex_options=ifelse(fit,c("scale_down"),c("basic", "repeat_header")), bootstrap_options = "responsive", full_width = F, position = pos, repeat_header_continued = "\\textit{(Continued on Next Page...)}"))
      )
    }
  }
}

printDFInHTML_ByTabs<- function(lst_names, lst_dataframes, names = NULL, custom_msg = NULL, title_level = 3, multiple_df = F, sig_numbers = 4){
  if(is.character(lst_names)){
    names(lst_names) <- lst_names 
  }
  
  for(h in names(lst_names)){
    
    if(multiple_df){
      cat(paste0("\n"))
      cat(paste0(paste0(rep("#",title_level), collapse = ""), " ", h, " ", "{.tabset .tabset-fade}"))
      cat(paste0("\n"))
    }else{
      cat(paste0("\n"))
      cat(paste0(paste0(rep("#",title_level), collapse = ""), " ", h))
      cat(paste0("\n"))
    }
    
    #NULL or NA
    if(is.null(names)){
      if(!multiple_df){
        if(is.null(lst_dataframes[[h]]) || nrow(lst_dataframes[[h]])==0){
          if(is.null(custom_msg)){
            print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
          }else{
            print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
          }
          next
        }
      }
    }else{
      if(!multiple_df){
        if(is.null(lst_dataframes[[h]][[names]]) || nrow(lst_dataframes[[h]][[names]])==0){
          if(is.null(custom_msg)){
            print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
          }else{
            print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
          }
          next
        }
      }
    }
    
    #WITH VALUE
    if(is.null(names)){
      if(!multiple_df){
        if(all(class(lst_dataframes[[h]])==c("datatables", "htmlwidget"))){
          
          aux <- lst_dataframes[[h]]
          for(c in 1:ncol(aux)){
            aux[,c] <- as.character(aux[,c])
          }
          
          for(r in 1:nrow(aux)){
            for(c in 1:ncol(aux)){
              v = lst_dataframes[[h]][r,c]
              if(is.numeric(v)){
                if(!v %% 1 == 0){
                  aux[r,c] = as.character(signif(v, sig_numbers))
                }else{
                  aux[r,c] = as.character(v)
                }
              }
            }
          }
          
          print(aux)
          
        }else{
          
          scroll <- ifelse(nrow(lst_dataframes[[h]])>15, T, F)
          aux <- lst_dataframes[[h]]
          for(c in 1:ncol(aux)){
            aux[,c] <- as.character(aux[,c])
          }
          
          for(r in 1:nrow(aux)){
            for(c in 1:ncol(aux)){
              v = lst_dataframes[[h]][r,c]
              if(is.numeric(v)){
                if(!v %% 1 == 0){
                  aux[r,c] = as.character(signif(v, sig_numbers))
                }else{
                  aux[r,c] = as.character(v)
                }
              }
            }
          }
          
          print(kableFormatted(aux, scroll = scroll))
          
        }
          
      }else{
        for(var in names(lst_dataframes[[h]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ", var))
          cat(paste0("\n"))
          
          if(is.null(lst_dataframes[[h]][[var]]) || nrow(lst_dataframes[[h]][[var]])==0){
            if(is.null(custom_msg)){
              print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
            }else{
              print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
            }
            next
          }
          
          if(all(class(lst_dataframes[[h]][[var]])==c("datatables", "htmlwidget"))){
            
            aux <- lst_dataframes[[h]][[var]]
            for(c in 1:ncol(aux)){
              aux[,c] <- as.character(aux[,c])
            }
            
            for(r in 1:nrow(aux)){
              for(c in 1:ncol(aux)){
                v = lst_dataframes[[h]][[var]][r,c]
                if(is.numeric(v)){
                  if(!v %% 1 == 0){
                    aux[r,c] = as.character(signif(v, sig_numbers))
                  }else{
                    aux[r,c] = as.character(v)
                  }
                }
              }
            }
            
            print(aux)
            
          }else{
            scroll <- ifelse(nrow(lst_dataframes[[h]][[var]])>15, T, F)
            
            aux <- lst_dataframes[[h]][[var]]
            for(c in 1:ncol(aux)){
              aux[,c] <- as.character(aux[,c])
            }
            
            for(r in 1:nrow(aux)){
              for(c in 1:ncol(aux)){
                v = lst_dataframes[[h]][[var]][r,c]
                if(is.numeric(v)){
                  if(!v %% 1 == 0){
                    aux[r,c] = as.character(signif(v, sig_numbers))
                  }else{
                    aux[r,c] = as.character(v)
                  }
                }
              }
            }
            
            print(kableFormatted(aux, scroll = scroll))
          }
        }
      }
    }else{
      if(!multiple_df){
        scroll <- ifelse(nrow(lst_dataframes[[h]][[names]])>15, T, F)
        
        aux <- lst_dataframes[[h]][[names]]
        for(c in 1:ncol(aux)){
          aux[,c] <- as.character(aux[,c])
        }
        
        for(r in 1:nrow(aux)){
          for(c in 1:ncol(aux)){
            v = lst_dataframes[[h]][[names]][r,c]
            if(is.numeric(v)){
              if(!v %% 1 == 0){
                aux[r,c] = as.character(signif(v, sig_numbers))
              }else{
                aux[r,c] = as.character(v)
              }
            }
          }
        }
        
        print(kableFormatted(aux, scroll = scroll))
      }else{
        for(var in names(lst_dataframes[[h]][[names]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ", var))
          cat(paste0("\n"))
          
          if(is.null(lst_dataframes[[h]][[names]][[var]]) || nrow(lst_dataframes[[h]][[names]][[var]])==0){
            if(is.null(custom_msg)){
              print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
            }else{
              print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
            }
            next
          }
          
          scroll <- ifelse(nrow(lst_dataframes[[h]][[names]][[var]])>15, T, F)
          
          aux <- lst_dataframes[[h]][[names]][[var]]
          for(c in 1:ncol(aux)){
            aux[,c] <- as.character(aux[,c])
          }
          
          for(r in 1:nrow(aux)){
            for(c in 1:ncol(aux)){
              v = lst_dataframes[[h]][[names]][[var]][r,c]
              if(is.numeric(v)){
                if(!v %% 1 == 0){
                  aux[r,c] = as.character(signif(v, sig_numbers))
                }else{
                  aux[r,c] = as.character(v)
                }
              }
            }
          }
          
          print(kableFormatted(aux, scroll = scroll))
        }
      }
    }
    
    cat(paste0("\n"))
  }
}

printFunction_InHTML_ByTabs<- function(lst_names, lst_plots, names = NULL, custom_msg = NULL, multiple_plots = F, title_level = 3, limit_plots = NULL){
  if(is.character(lst_names)){
    names(lst_names) <- lst_names 
  }
  
  for(h in names(lst_names)){
    if(multiple_plots){
      cat(paste0("\n"))
      cat(paste0(paste0(rep("#",title_level), collapse = ""), " ", h, " ", "{.tabset .tabset-fade}"))
      cat(paste0("\n"))
    }else{
      cat(paste0("\n"))
      cat(paste0(paste0(rep("#",title_level), collapse = ""), " ", h))
      cat(paste0("\n"))
    }
    
    #NULL or NA
    if(is.null(names)){
      if(is.null(lst_plots[[h]])){
        if(is.null(custom_msg)){
          print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
        }else{
          print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
        }
        next
      }
    }else{
      if(is.null(lst_plots[[h]][[names]])){
        if(is.null(custom_msg)){
          print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
        }else{
          print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
        }
        next
      }
    }
    
    #WITH VALUE
    cont = 0
    if(is.null(names)){
      if(!multiple_plots){
        lst_plots[[h]]()
      }else{
        for(var in names(lst_plots[[h]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ", var))
          cat(paste0("\n"))
          lst_plots[[h]][[var]]()
          #forzar print df
          print(invisibleKable())
          
          cont = cont+1
          if(!is.null(limit_plots)){
            if(limit_plots == cont){
              cat(paste0("\n"))
              cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ..."))
              cat(paste0("\n"))
              cat(paste0("Only showed first ", limit_plots, " plots of ", length(names(lst_plots[[h]])), " plots."))
              cat(paste0("\n"))
              break
            }
          }
        }
      }
      
    }else{
      if(!multiple_plots){
        lst_plots[[h]][[names]]()
      }else{
        for(var in names(lst_plots[[h]][[names]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ", var))
          cat(paste0("\n"))
          lst_plots[[h]][[names]][[var]]()
          #forzar print df
          print(invisibleKable())
          
          cont = cont+1
          if(!is.null(limit_plots)){
            if(limit_plots == cont){
              cat(paste0("\n"))
              cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ..."))
              cat(paste0("\n"))
              cat(paste0("Only showed first ", limit_plots, " plots of ", length(names(lst_plots[[h]][[names]])), " plots."))
              cat(paste0("\n"))
              break
            }
          }
        }
      }
    }
    
    #forzar print df
    print(invisibleKable())
    
  }
}

printPLOT_byFunction_InHTML_ByTabs<- function(lst_names, lst_plots, names = NULL, custom_msg = NULL, multiple_plots = F, title_level = 3, limit_plots = NULL){
  if(is.character(lst_names)){
    names(lst_names) <- lst_names 
  }
  
  for(h in names(lst_names)){
    if(multiple_plots){
      cat(paste0("\n"))
      cat(paste0(paste0(rep("#",title_level), collapse = ""), " ", h, " ", "{.tabset .tabset-fade}"))
      cat(paste0("\n"))
    }else{
      cat(paste0("\n"))
      cat(paste0(paste0(rep("#",title_level), collapse = ""), " ", h))
      cat(paste0("\n"))
    }
    
    #NULL or NA
    if(is.null(names)){
      if(is.null(lst_plots[[h]])){
        if(is.null(custom_msg)){
          print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
        }else{
          print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
        }
        next
      }
    }else{
      if(is.null(lst_plots[[h]][[names]])){
        if(is.null(custom_msg)){
          print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
        }else{
          print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
        }
        next
      }
    }
    
    #WITH VALUE
    cont = 0
    if(is.null(names)){
      if(!multiple_plots){
        plot(lst_plots[[h]])
      }else{
        for(var in names(lst_plots[[h]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ", var))
          cat(paste0("\n"))
          plot(lst_plots[[h]][[var]])
          #forzar print df
          print(invisibleKable())
          
          cont = cont+1
          if(!is.null(limit_plots)){
            if(limit_plots == cont){
              cat(paste0("\n"))
              cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ..."))
              cat(paste0("\n"))
              cat(paste0("Only showed first ", limit_plots, " plots of ", length(names(lst_plots[[h]])), " plots."))
              cat(paste0("\n"))
              break
            }
          }
        }
      }
      
    }else{
      if(!multiple_plots){
        plot(lst_plots[[h]][[names]])
      }else{
        for(var in names(lst_plots[[h]][[names]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ", var))
          cat(paste0("\n"))
          plot(lst_plots[[h]][[names]][[var]])
          #forzar print df
          print(invisibleKable())
          
          cont = cont+1
          if(!is.null(limit_plots)){
            if(limit_plots == cont){
              cat(paste0("\n"))
              cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ..."))
              cat(paste0("\n"))
              cat(paste0("Only showed first ", limit_plots, " plots of ", length(names(lst_plots[[h]][[names]])), " plots."))
              cat(paste0("\n"))
              break
            }
          }
        }
      }
    }
    
    #forzar print df
    print(invisibleKable())
    
  }
}

printGGPLOTInHTML_ByTabs3LEVELS<- function(lst_names, lst_plots, names = NULL, custom_msg = NULL, title_level = 3, limit_plots = NULL){
  if(is.character(lst_names)){
    names(lst_names) <- lst_names 
  }
  
  for(h in names(lst_names)){
    cat(paste0("\n"))
    cat(paste0(paste0(rep("#",title_level), collapse = ""), " ", h, " ", "{.tabset .tabset-fade}"))
    cat(paste0("\n"))
    
    for(cn in names(lst_names[[h]])){
      cat(paste0("\n"))
      cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ", cn, " ", "{.tabset .tabset-fade}"))
      cat(paste0("\n"))
      
      #NULL or NA
      if(is.null(names)){
        if(is.null(lst_plots[[h]])){
          if(is.null(custom_msg)){
            print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
          }else{
            print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
          }
          next
        }
      }else{
        if(is.null(lst_plots[[h]][[names]])){
          if(is.null(custom_msg)){
            print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
          }else{
            print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
          }
          next
        }
      }
      
      #WITH VALUE
      cont = 0
      if(is.null(names)){
        for(var in names(lst_plots[[h]][[cn]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+2), collapse = ""), " ", var))
          cat(paste0("\n"))
          print(lst_plots[[h]][[cn]][[var]])
          #forzar print df
          print(invisibleKable())
          
          cont = cont+1
          if(!is.null(limit_plots)){
            if(limit_plots == cont){
              cat(paste0("\n"))
              cat(paste0(paste0(rep("#",title_level+2), collapse = ""), " ..."))
              cat(paste0("\n"))
              cat(paste0("Only showed first ", limit_plots, " plots of ", length(names(lst_plots[[h]][[cn]])), " plots."))
              cat(paste0("\n"))
              break
            }
          }
        }
      }else{
        for(var in names(lst_plots[[h]][[cn]][[names]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+2), collapse = ""), " ", var))
          cat(paste0("\n"))
          print(lst_plots[[h]][[cn]][[names]][[var]])
          #forzar print df
          print(invisibleKable())
          
          cont = cont+1
          if(!is.null(limit_plots)){
            if(limit_plots == cont){
              cat(paste0("\n"))
              cat(paste0(paste0(rep("#",title_level+2), collapse = ""), " ..."))
              cat(paste0("\n"))
              cat(paste0("Only showed first ", limit_plots, " plots of ", length(names(lst_plots[[h]][[cn]][[names]])), " plots."))
              cat(paste0("\n"))
              break
            }
          }
        }
      }
    }
    #forzar print df
    print(invisibleKable())
  }
}

#Allows VENN and UPSET
printVENNInHTML_ByTabs<- function(lst_names, lst_plots, names = NULL, custom_msg = NULL, multiple_plots = F, title_level = 3){
  if(is.character(lst_names)){
    names(lst_names) <- lst_names 
  }
  
  for(h in names(lst_names)){
    if(multiple_plots){
      cat(paste0("\n"))
      cat(paste0(paste0(rep("#",title_level), collapse = ""), " ", h, " ", "{.tabset .tabset-fade}"))
      cat(paste0("\n"))
    }else{
      cat(paste0("\n"))
      cat(paste0(paste0(rep("#",title_level), collapse = ""), " ", h))
      cat(paste0("\n"))
    }
    
    grid::grid.newpage()
    
    #NULL or NA
    if(is.null(names)){
      if(is.null(lst_plots[[h]])){
        if(is.null(custom_msg)){
          print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
        }else{
          print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
        }
        next
      }
    }else{
      if(is.null(lst_plots[[h]][[names]])){
        if(is.null(custom_msg)){
          print(kableFormatted(paste0("No data for ", h, " data base."), colnames = "Info"))
        }else{
          print(kableFormatted(paste0(custom_msg, "</br>"), colnames = "Info"))
        }
        next
      }
    }
    
    #WITH VALUE
    if(is.null(names)){
      if(!multiple_plots){
        if(class(lst_plots[[h]])=="gList"){
          grid::grid.draw(lst_plots[[h]], recording = F)
        }else if(class(lst_plots[[h]])=="upset"){
          print(lst_plots[[h]])
        }
        
      }else{
        for(var in names(lst_plots[[h]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ", var))
          cat(paste0("\n"))
          if(class(lst_plots[[h]])=="gList"){
            grid::grid.draw(lst_plots[[h]][[var]], recording = F)
          }else if(class(lst_plots[[h]])=="upset"){
            print(lst_plots[[h]][[var]])
          }
        }
      }
      
    }else{
      if(!multiple_plots){
        if(class(lst_plots[[h]])=="gList"){
          grid::grid.draw(lst_plots[[h]][[names]], recording = F)
        }else if(class(lst_plots[[h]])=="upset"){
          print(lst_plots[[h]][[names]])
        }
        
      }else{
        for(var in names(lst_plots[[h]][[names]])){
          cat(paste0("\n"))
          cat(paste0(paste0(rep("#",title_level+1), collapse = ""), " ", var))
          cat(paste0("\n"))
          if(class(lst_plots[[h]])=="gList"){
            grid::grid.draw(lst_plots[[h]][[names]][[var]], recording = F)
          }else if(class(lst_plots[[h]])=="upset"){
            print(lst_plots[[h]][[names]][[var]])
          }
        }
      }
    }
    print(invisibleKable())
    
  }
}

printVENNInHTML<- function(venn_object){
  grid::grid.newpage()
  grid::grid.draw(venn_object, recording = F)
  print(invisibleKable())
}
