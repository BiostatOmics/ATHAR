# aux functions for this script

getSubColnamesDF <- function(df, remove){
  lst_cn <- colnames(df)[!colnames(df) %in% remove]
  names(lst_cn) <- lst_cn
  return(lst_cn)
}

revalueNAByFactor <- function(data, lst, new, force = F){
  for(cn in lst){
    if(!cn %in% colnames(data)){
      next
    }
    
    if(is.factor(data[,cn])){
      if(new %in% levels(data[,cn])){
        data[is.na(data[,cn]),cn] <- new
      }else if(force){
        l <- levels(data[,cn])
        l <- c(l, new)
        levels(data[,cn]) <- l
        data[is.na(data[,cn]),cn] <- new
      }else{
        message(paste0("Variable not present in levels of variable ", cn))
      }
    }
  }
  
  return(data)
}

isContinuous <- function(vector){
  if(is.numeric(vector)){
    aux <- round(vector)
    res <- vector - aux
    res <- res[!is.na(res)]
    if(all(res==0))
      return(F)
  }
  return(T)
}

#BEST DISTRIBUTION PLOT FOR A NUMERICAL VARIABLE AND OPTIONAL A FACTOR
violin_boxPlot <- function(var_num, group = NULL, x.lab = NULL, y.lab = NULL, outliers = NULL, title = T, subtitle = F, stats = F, wide = T, SIZE_TXT_PVAL = 2){
  
  MAX_POINTS = 1000
  
  if(wide){
    txt_max <- 100
  }else{
    txt_max <- 45
  }
  
  txt_title = paste0("Variable: ", colnames(var_num))
  if(subtitle){
    txt_subtitle = getInfo(colnames(var_num))$Description
    if(nchar(txt_subtitle)>txt_max){
      txt_subtitle <- paste0(substr(txt_subtitle, 1, txt_max), "...")
    }
    if(txt_subtitle == "No description file"){
      txt_subtitle = NULL
    }
  }else{
    txt_subtitle = NULL
  }
  
  if(is.data.frame(group) & is.null(x.lab) & !is.null(group)){
    x.lab <- colnames(group)
  }
  
  var_num_mod <- var_num
  
  if(!is.null(group) & !is.factor(group)){
    group <- group[,1]
    if(!is.factor(group)){
      stop("group variable is not a factor")
    }
  }else{
    var_num_mod$group_mod <- factor("")
    x.lab <- ""
  }
  
  group_mod <- group
  
  ggp <- ggplot(data = var_num_mod, aes(x = group_mod, y = var_num_mod[,1], fill = group_mod)) +
    # The half violins
    geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8)
  
  #IF TOO MANY ROWS, DO NOT COLOR DOTS
  # if(!nrow(var_num_mod) > MAX_POINTS){
  #   ggp <- ggp + geom_point(aes(y = var_num_mod[,1], color = group_mod), position = position_jitter(width = 0.15),
  #                           size = 1, alpha = ifelse(rownames(var_num_mod) %in% rownames(outliers),1,0.2))
  # }
  
  # The points
  ggp <- ggp + geom_point(aes(y = var_num_mod[,1], color = group_mod), position = position_jitter(width = 0.15),
                          size = 1, alpha = ifelse(rownames(var_num_mod) %in% rownames(outliers),1,0.2)) +
    # The boxplots
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
    # Removing legends
    guides(fill = "none", color = "none") +
    # Picking nicer colours
    RColorConesa::scale_fill_conesa() +
    RColorConesa::scale_color_conesa()+
    theme(legend.position="bottom",
          axis.text=element_text(size=5),
          axis.title=element_text(size=5))
  
  if(!is.null(x.lab)){
    ggp <- ggp + xlab(x.lab)
  }
  
  if(!is.null(y.lab)){
    ggp <- ggp + ylab(y.lab)
  }else{
    ggp <- ggp + ylab(colnames(var_num))
  }
  
  if(title){
    ggp <- ggp + ggtitle(label = txt_title, subtitle = txt_subtitle) +
      theme(plot.title = element_text(size=10), plot.subtitle = element_text(size=8))
  }
  
  if(stats & !is.null(group)){
    increment.y <- (max(var_num_mod, na.rm = T) - min(var_num_mod, na.rm = T)) * 0.05
    increment.y <- max(var_num_mod, na.rm = T) + increment.y
    ggp <- ggp + stat_compare_means(method = "kruskal", label.x.npc = 0.5, label.y = increment.y, size = SIZE_TXT_PVAL)
  }
  
  return(ggp)
}

#BEST DISTRIBUTION PLOT FOR A NUMERICAL VARIABLE AND OPTIONAL A FACTOR
distributionPlot.optimal <- function(var_num, group = NULL, title = T, subtitle = F, x.lab = NULL, histogram_mode = NULL, fill_label = NULL, wide = T){
  if(!is.null(group) & !is.factor(group)){
    group <- group[,1]
    if(!is.factor(group)){
      stop("group variable is not a factor")
    }
  }
  
  if(wide){
    max_breaks <- 100
    txt_max <- 100
  }else{
    max_breaks <- 35
    txt_max <- 45
  }
  
  txt_title = paste0("Variable: ", colnames(var_num))
  x.lab <- colnames(var_num)
  if(subtitle){
    txt_subtitle = getInfo(colnames(var_num))$Description
    if(nchar(txt_subtitle)>txt_max){
      txt_subtitle <- paste0(substr(txt_subtitle, 1, txt_max), "...")
    }
    if(txt_subtitle == "No description file"){
      txt_subtitle = NULL
    }
  }else{
    txt_subtitle = NULL
  }
  
  #The Freedman-Diaconis rule
  var.nonas <- var_num[!is.na(var_num),1]
  breaks = 0
  
  bw <- 2 * IQR(var.nonas) / length(var.nonas)^(1/3)
  bw <- ifelse(isContinuous(var.nonas),bw,round(bw))
  if(bw==0){
    bw <- 2 * IQR(var.nonas) / length(var.nonas)^(1/3)
    bw <- ifelse(isContinuous(var.nonas),bw,ceiling(bw))
  }
  if(bw==0){ #if still cero, bw = 5% of mean
    bw <- mean(var.nonas) * 0.05
  }
  breaks <- seq(min(var.nonas), max(var.nonas), )
  
  max_rotate <- ifelse(isContinuous(var.nonas), 10, 20)
  
  while(length(breaks)>max_breaks){
    bw <- bw*1.2 #+20%
    breaks <- seq(min(var.nonas), max(var.nonas), ifelse(isContinuous(var.nonas),bw,round(bw, 2)))
  }
  
  if(isContinuous(var.nonas)){
    breaks <- round(breaks, digits = 2)
  }else{
    breaks <- round(breaks)
  }
  
  rotate.txt <- if(length(breaks)>max_rotate) T else F
  
  # var_num_ori <- var_num
  # var_num <- cut(var_num_ori[,1], breaks)
  # var_num <- as.data.frame(var_num)
  # var_num <- cbind(var_num, group)
  #
  # var_num <- as.data.frame(table(var_num))
  # ggp <- ggplot(var_num, aes(x=var_num[,1], y = Freq, color=group, fill=group)) +
  #   geom_histogram(stat = "identity", alpha=0.5) +
  #   scale_fill_conesa() +
  #   scale_color_conesa() +
  #   ylab("Frequency") +
  #   scale_x_binned(breaks = breaks)
  
  if(is.null(histogram_mode)){
    pos = "identity"
  }else{
    pos = histogram_mode
  }
  
  alpha <- switch (pos,
                   "identity" = 0.5,
                   "stack" = 0.8,
                   "fill" = 0.8,
                   "jitter" = 0.5,
                   "dodge" = 0.8
  )
  
  ggp <- NULL
  if(isContinuous(var_num[,1])){
    if(is.null(group)){
      # Histogram with density plot
      ggp <- ggplot(var_num, aes(x=var_num[,1])) +
        geom_histogram(stat="count", colour=colorConesa(1), fill=colorConesa(1), alpha=alpha, position = pos) +
        RColorConesa::scale_fill_conesa() +
        ylab("Frequency") +
        scale_x_binned(breaks = breaks) +
        theme(legend.position="bottom",
              axis.text=element_text(size=5),
              axis.title=element_text(size=5))
    }else{
      # Color by groups
      ggp <- ggplot(var_num, aes(x=var_num[,1], color=group, fill=group)) +
        geom_histogram(stat = "count", alpha=alpha, position = pos) +
        RColorConesa::scale_fill_conesa() +
        RColorConesa::scale_color_conesa() +
        ylab("Frequency") +
        scale_x_binned(breaks = breaks) +
        theme(legend.position="bottom",
              axis.text=element_text(size=5),
              axis.title=element_text(size=5))
    }
  }else{
    
    if(is.null(group)){
      # Histogram with density plot
      ggp <- ggplot(var_num, aes(x=var_num[,1])) +
        geom_histogram(stat = "count", colour=colorConesa(1), fill=colorConesa(1), alpha=alpha, position = pos) +
        RColorConesa::scale_fill_conesa() +
        ylab("Frequency") +
        scale_x_binned(breaks = breaks) +
        theme(legend.position="bottom",
              axis.text=element_text(size=5),
              axis.title=element_text(size=5))
    }else{
      # Color by groups
      ggp <- ggplot(var_num, aes(x=var_num[,1], color=group, fill=group)) +
        geom_histogram(stat = "count", alpha=alpha, position = pos) +
        RColorConesa::scale_fill_conesa() +
        RColorConesa::scale_color_conesa() +
        ylab("Frequency") +
        scale_x_binned(breaks = breaks) +
        theme(legend.position="bottom",
              axis.text=element_text(size=5),
              axis.title=element_text(size=5))
      
    }
  }
  
  if(!is.null(x.lab)){
    ggp <- ggp + xlab(x.lab)
  }
  
  if(!is.null(group) & !is.null(fill_label)){
    ggp <- ggp + labs(fill=paste0(fill_label, ":  ")) + guides(color="none")
  }
  
  # if(is.data.frame(group)){
  #   ggp <- ggp + guides(fill=guide_legend(title=colnames(group)))
  # }
  
  if(rotate.txt){
    ggp <- ggp + theme(axis.text.x = element_text(angle = ifelse(rotate.txt,90,NULL), vjust = 0.5))
  }
  
  
  #REDUCE SIZE TEXT IF TOO MANY DIVISIONS
  if(length(breaks)>max_rotate*1.4){
    ggp <- ggp + theme(axis.text.x = element_text(size=4))
  }else if(length(breaks)>max_rotate*2){
    ggp <- ggp + theme(axis.text.x = element_text(size=3))
  }
  
  if(title){
    ggp <- ggp + ggtitle(label = txt_title, subtitle = txt_subtitle) +
      theme(plot.title = element_text(size=10), plot.subtitle = element_text(size=8))
  }
  
  if(!is.null(x.lab)){
    ggp <- ggp + xlab(x.lab)
  }
  
  #change fill boxes size
  ggp <- ggp + theme(legend.key.size = unit(0.25, 'cm'))
  
  return(ggp)
}

anovaPval = function (x,y) {
  # 1-suppressWarnings(summary(aov(y ~ factor(x)))[[1]]$`Pr(>F)`[1])
  1-suppressWarnings(kruskal.test(y ~ factor(x))$p.value)
}


#just plot and table
testQualitativeData3.1 <- function(df, cn, group=NULL, pairwise=T, method = "chi", deleteNA=T, SIG=0.05, ROUND_EXP=ROUND_EXP, ROUND_PVAL=ROUND_PVAL, title = T, subtitle = F, length_subtitle = 100){
  
  SIZE_TXT_PVAL = 2
  
  if(method=="chi" & pairwise==T){
    method = "fisher"
  }
  
  if(!all(c(cn, group) %in% colnames(df))){
    stop(paste0("Variables ", cn , " or ", group ," are not present in the data."))
  }
  
  if(!is.factor(df[,cn]) & !is.factor(df[,group])){
    stop(paste0("Variable ", cn, " or group variable (", group ,") are not factors."))
  }
  if(dim(table(df[,cn]))==1){
    stop(paste0("Variable ", cn, " only has one category."))
  }
  
  tbl = table(df[,c(cn,group)], useNA = "i")
  if(is.null(group)){
    tbl_perc = round(prop.table(tbl)*100, 2)
  }else{
    tbl_perc = round(prop.table(tbl, margin = 2)*100, 2)
    colnames(tbl_perc) <- paste0("% ", colnames(tbl))
  }
  
  #delete NA in df
  if(deleteNA){
    tbl <- tbl[!is.na(rownames(tbl)),]
    tbl_perc <- tbl_perc[!is.na(rownames(tbl_perc)),]
  }
  
  if(!is.null(group)){
    #fisher pairwise test
    res.fisher <- NULL
    if(pairwise){
      for(i in 1:(ncol(tbl)-1)){
        for(j in (i+1):ncol(tbl)){
          fisher.pw <- fisher.test(tbl[,c(i,j)], workspace = 2e8, simulate.p.value=T, B=10000)
          res.fisher <- rbind(res.fisher, c(colnames(tbl)[i], colnames(tbl)[j], round(fisher.pw$p.value, ROUND_PVAL), max(tbl[,c(i,j)])+(max(tbl)/10),rownames(tbl)[1]))
        }
      }
      res.fisher <- data.frame(res.fisher, stringsAsFactors = F)
      colnames(res.fisher) <- c("group1", "group2", "p.value", "y.position", "Var1")
      res.fisher$p.value <- as.numeric(res.fisher$p.value)
      res.fisher$y.position <- as.numeric(res.fisher$y.position)
      res.fisher <- res.fisher[order(res.fisher$y.position),]
      
      for(r in 2:nrow(res.fisher)){
        while((res.fisher[(r),]$y.position - res.fisher[(r-1),]$y.position) < (max(tbl)/10)){
          res.fisher[(r),]$y.position <- res.fisher[(r),]$y.position + (max(tbl)/10+1)
        }
      }
    }else{
      if(method=="fisher"){
        res.fisher <- fisher.test(tbl, workspace = 2e8, simulate.p.value=T, B=10000)
      }else{
        res.fisher <- chisq.test(tbl) #chisq.test(tbl, simulate.p.value = T, B = 5000)
      }
      
    }
    
    ############
    # SIG PVAL #
    ############
    
    if(any(is.na(res.fisher$p.value))){
      txt_mess <- paste0("Variable ", cn, " has a factor event with all 0s. We recomend run the function keeping NA values.")
      message(txt_mess)
    }
    
    #info
    dependentVariablesQualitatives <- NULL
    dependentVariablesQualitatives <- rbind(dependentVariablesQualitatives, c(cn, res.fisher$p.value))
    if(pairwise){
      groups <- paste(res.fisher$group1,"vs", res.fisher$group2)
      colnames(dependentVariablesQualitatives) <- c("Variable", groups)
    }else{
      colnames(dependentVariablesQualitatives) <- c("Variable", "p.value")
    }
    
    dependentVariablesQualitatives <- as.data.frame(dependentVariablesQualitatives)
  }
  
  
  #PLOTS
  ggp = NULL
  ggp_fill = NULL
  
  df.tbl <- as.data.frame(tbl)
  if(any(is.na(df.tbl))){
    df.tbl <- revalueNAByFactor(df.tbl, cn, "NA", force = T)
  }
  
  #########
  # PLOTS #
  #########
  FLAG_DESC <- exists("VAR_DESCRIPTION")
  txt_title = paste0("Variable: ", cn)
  if(subtitle){
    txt_subtitle = ifelse(FLAG_DESC, getInfo(cn)$Description, "")
    if(nchar(txt_subtitle)>length_subtitle){
      txt_subtitle <- paste0(substr(txt_subtitle, 1, length_subtitle), "...")
    }
  }else{
    txt_subtitle = NULL
  }
  
  if(!is.null(group)){
    if(pairwise){
      
      txt = NULL
      if(method=="chi"){
        txt = paste0("chisq.test (", res.fisher$parameter, ")=", round(res.fisher$statistic, ROUND_PVAL), " p=", round(res.fisher$p.value, ROUND_PVAL))
      }
      
      if(method=="fisher"){
        if(pairwise==F){
          txt = paste0("fisher.test (", res.fisher$alternative, ")", " p=", round(res.fisher$p.value, ROUND_PVAL))
        }else{
          txt = paste0("fisher.test (", res.fisher$alternative, ")", " p=", round(res.fisher$p.value, ROUND_PVAL)) # CORREGIR !!!
        }
      }
      
      ggp <- ggplot(df.tbl) +
        aes(x = cn.fs, y = Freq, fill = Var1) +
        geom_bar(stat='identity', position='dodge') +
        xlab(label = paste0(cn)) + labs(fill = "Values") +
        scale_fill_manual(values = colorConesa(length(unique(df.tbl$Var1)))) +
        stat_pvalue_manual(res.fisher, label = "p.value", step.increase = 0.0, hide.ns = T, size=SIZE_TXT_PVAL) +
        ggtitle(label = txt_title, subtitle = txt_subtitle) +
        theme(legend.key.size = unit(0.25, 'cm'))
      
      if(title){
        ggp <- ggp + ggtitle(label = txt_title, subtitle = txt_subtitle)
      }
      
      x_pos <- (length(unique(df.tbl[[group]]))+1)/2
      y_pos <- 1.05
      
      ggp_fill <- ggplot(df.tbl) +
        aes(x = df.tbl[,2], y = Freq, fill = df.tbl[,1]) +
        geom_bar(stat='identity', position='fill') +
        xlab(label = paste0(group)) + labs(fill = cn) +
        RColorConesa::scale_fill_conesa() +
        annotate(geom="text", x=x_pos, y=y_pos, label=txt, color="black", size=SIZE_TXT_PVAL) +
        theme(legend.position = "bottom") + ylab("%") +
        ggtitle(label = txt_title, subtitle = txt_subtitle) +
        theme(legend.key.size = unit(0.25, 'cm'))
      
      if(title){
        ggp_fill <- ggp_fill + ggtitle(label = txt_title, subtitle = txt_subtitle)
      }
      
    }else{
      colnames(dependentVariablesQualitatives) <- c("Variable", "p.value")
      
      #f <- as.formula(paste0("~ ", group," + ", cn))
      # ggp_mos <- mosaicplot(f, data = d.cualFiltered, main = paste0(cn, " & ", group), shade = T, na.action = "na.keep")
      # ggp_mos1 <- vcd::mosaic(f, data = d.cualFiltered, direction = "v", shade = T, legend = T, main = paste0(cn, " & ", group), na.action = "na.keep")
      
      txt = NULL
      if(method=="chi"){
        txt = paste0("chisq.test (", res.fisher$parameter, ")=", round(res.fisher$statistic, ROUND_PVAL), " p=", round(res.fisher$p.value, ROUND_PVAL))
      }
      
      if(method=="fisher"){
        if(pairwise==F){
          txt = paste0("fisher.test (", res.fisher$alternative, ")", " p=", round(res.fisher$p.value, ROUND_PVAL))
        }else{
          txt = paste0("fisher.test (", res.fisher$alternative, ")", " p=", round(res.fisher$p.value, ROUND_PVAL)) # CORREGIR !!!
        }
      }
      
      x_pos <- (length(unique(df.tbl[[group]]))+1)/2
      y_pos <- max(df.tbl[,"Freq"])*1.05
      
      ggp <- ggplot(df.tbl) +
        aes(x = df.tbl[,2], y = Freq, fill = df.tbl[,1]) +
        #aes(x = df.tbl[,group], y = Freq, fill = df.tbl[,cn]) +
        geom_bar(stat='identity', position='dodge') +
        xlab(label = paste0(group)) + labs(fill = cn) +
        RColorConesa::scale_fill_conesa() +
        annotate(geom="text", x=x_pos, y=y_pos, label=txt, color="black", size=SIZE_TXT_PVAL) +
        theme(legend.position = "bottom") +
        theme(legend.key.size = unit(0.25, 'cm'))
      
      if(title){
        ggp <- ggp + ggtitle(label = txt_title, subtitle = txt_subtitle)
      }
      
      x_pos <- (length(unique(df.tbl[[group]]))+1)/2
      y_pos <- 1.05
      
      ggp_fill <- ggplot(df.tbl) +
        aes(x = df.tbl[,2], y = Freq, fill = df.tbl[,1]) +
        #aes(x = df.tbl[,group], y = Freq, fill = df.tbl[,cn]) +
        geom_bar(stat='identity', position='fill') +
        xlab(label = paste0(group)) + labs(fill = cn) +
        RColorConesa::scale_fill_conesa() +
        annotate(geom="text", x=x_pos, y=y_pos, label=txt, color="black", size=SIZE_TXT_PVAL) +
        theme(legend.position = "bottom") + ylab("%") +
        theme(legend.key.size = unit(0.25, 'cm'))
      
      if(title){
        ggp_fill <- ggp_fill + ggtitle(label = txt_title, subtitle = txt_subtitle)
      }
      
      if(length(levels(df.tbl[,1]))>3){
        ggp <- ggp + guides(fill=guide_legend(nrow = ceiling(length(levels(df.tbl[,1]))/3), byrow = T))
        ggp_fill <- ggp_fill + guides(fill=guide_legend(nrow = ceiling(length(levels(df.tbl[,1]))/3), byrow = T))
      }
      
      ggp_aux <- ggp + xlab(NULL)
      ggp_fill_aux <- ggp_fill + ggtitle(label = " ", subtitle = " ") + guides(fill = "none", color = "none")
      
      plot_combined <- ggpubr::ggarrange(ggp_aux, ggp_fill_aux)
      
      # library(patchwork)
      # plot_combined <- ggp_aux + ggp_fill_aux
      
    }
  }else{
    ## NOT TEST PERFORMED
    dependentVariablesQualitatives <- NULL
    
    #f <- as.formula(paste0("~ ", group," + ", cn))
    # ggp_mos <- mosaicplot(f, data = d.cualFiltered, main = paste0(cn, " & ", group), shade = T, na.action = "na.keep")
    # ggp_mos1 <- vcd::mosaic(f, data = d.cualFiltered, direction = "v", shade = T, legend = T, main = paste0(cn, " & ", group), na.action = "na.keep")
    
    ggp <- ggplot(df.tbl) +
      aes(x = df.tbl[,1], y = Freq, fill = df.tbl[,1]) +
      #aes(x = df.tbl[,group], y = Freq, fill = df.tbl[,cn]) +
      geom_bar(stat='identity', position='dodge') +
      xlab(label = paste0(group)) + labs(fill = cn) +
      RColorConesa::scale_fill_conesa() +
      theme(legend.position = "bottom") + xlab(cn) +
      theme(legend.key.size = unit(0.25, 'cm'))
    
    if(title){
      ggp <- ggp + ggtitle(label = txt_title, subtitle = txt_subtitle)
    }
    
    ggp_fill <- ggplot(df.tbl) +
      aes(x = "", y = Freq, fill = df.tbl[,1]) +
      #aes(x = df.tbl[,group], y = Freq, fill = df.tbl[,cn]) +
      geom_bar(stat='identity', position='fill') +
      xlab(label = paste0(group)) + labs(fill = cn) +
      RColorConesa::scale_fill_conesa() +
      theme(legend.position = "bottom") + ylab("%") +
      theme(legend.key.size = unit(0.25, 'cm'))
    
    if(title){
      ggp_fill <- ggp_fill + ggtitle(label = txt_title, subtitle = txt_subtitle)
    }
    
    if(length(levels(df.tbl[,1]))>3){
      ggp <- ggp + guides(fill=guide_legend(nrow = ceiling(length(levels(df.tbl[,1]))/3), byrow = T))
      ggp_fill <- ggp_fill + guides(fill=guide_legend(nrow = ceiling(length(levels(df.tbl[,1]))/3), byrow = T))
    }
    
    ggp_aux <- ggp + xlab(NULL)
    ggp_fill_aux <- ggp_fill + ggtitle(label = " ", subtitle = " ") + guides(fill = "none", color = "none")
    
    plot_combined <- ggpubr::ggarrange(ggp_aux, ggp_fill_aux)
    
    # library(patchwork)
    # plot_combined <- ggp_hist_aux + ggp_violin_aux
  }
  
  return(list(df = dependentVariablesQualitatives,
              lst_tables = tbl,
              lst_tables_percentages = tbl_perc,
              plot_bar = ggp,
              plot_fill = ggp_fill,
              plot_combined = plot_combined))#, plots_mosaic = lst_plots_mosaic))
}

testQuantitativeData3.1 <- function(df, cn, group=NULL,
                                    deleteNA = F,
                                    x.lab = NULL,
                                    title = T, subtitle = T, histogram_mode = "identity", stats = T, wide = T){
  
  SIZE_TXT_PVAL = 2
  
  if(!histogram_mode %in% c("identity", "stack", "fill", "dodge", "jitter"))
    stop("Histogram mode must be one of: identity, stack, fill, dodge, jitter")
  
  var_num <- df[,cn,drop=F]
  
  #as.numeric bc Difftime variables can also be treated as numeric
  if(!is.numeric(class(var_num[,cn]))){
    var_num[,cn] <- as.numeric(var_num[,cn])
  }
  
  if(!is.null(group)){
    fill_label <- colnames(group)
  }else{
    fill_label <- NULL
  }
  
  #deleting NAs
  if(deleteNA){
    index_na <- is.na(var_num[,1])
    var_num <- var_num[!index_na,cn,drop=F]
    if(!is.null(group)){
      if(is.data.frame(group)){
        group <- group[!index_na,1,drop=F]
      }else{
        group <- group[!index_na]
      }
    }
  }
  
  sum <- summary(var_num[,cn])
  values <- var_num[,cn]
  
  # Numerical Variables
  q1 <- sum[2][[1]]
  q3 <- sum[5][[1]]
  
  aux.values <- values[!is.na(values)]
  maximum <- aux.values > (q3 + 1.5*(q3-q1)) #maximum below the q3+1.5*IR
  minimum <- aux.values < (q1 - 1.5*(q3-q1)) #minimim above the q3+1.5*IR
  
  aux <- var_num[!is.na(var_num[,1]),,drop=F]
  var_outliers <- aux[(minimum | maximum),cn,drop=F]
  
  #STATS - MERGE
  if(!is.null(group)){
    f <- as.formula(paste0(cn, "~ group"))
    d <- data.frame(var_num[,cn], as.data.frame(group))
    colnames(d) <- c(cn, "group")
    test <- kruskal.test(f, d)
    df <- data.frame(cn, test$p.value)
    colnames(df) <- c("Variable", "p.value")
  }else{
    df <- NULL
  }
  
  # HISTOGRAM
  ggp_hist <- distributionPlot.optimal(var_num = var_num[,cn,drop=F], group = group, x.lab = x.lab, title = title, subtitle = subtitle, histogram_mode = histogram_mode, fill_label = fill_label, wide = wide)
  ## BOXPLOT
  ggp_violin <- violin_boxPlot(var_num = var_num[,cn,drop=F], group = group, x.lab = x.lab, outliers = var_outliers, title = title, subtitle = subtitle, stats = stats, wide = wide, SIZE_TXT_PVAL = SIZE_TXT_PVAL)
  
  ######################################
  # RUN THE ANALYSIS FOR THE UNIC UNIT #
  ######################################
  df.outliers <- data.frame(Patients = rownames(var_outliers), Values = var_outliers[,1])
  df.outliers <- df.outliers[order(df.outliers$Values, decreasing = T),]
  
  if(!is.null(group) & length(levels(group))>3){
    ggp_hist <- ggp_hist + guides(fill=guide_legend(nrow = ceiling(length(levels(df.tbl[,1]))/3), byrow = T))
    ggp_violin <- ggp_violin + guides(fill=guide_legend(nrow = ceiling(length(levels(df.tbl[,1]))/3), byrow = T))
  }
  
  ggp_hist_aux <- ggp_hist + xlab(NULL)
  ggp_violin_aux <- ggp_violin + ggtitle(label = " ", subtitle = " ") + xlab(NULL)
  
  plot_combined <- ggpubr::ggarrange(ggp_hist_aux, ggp_violin_aux)
  
  # library(patchwork)
  # plot_combined <- ggp_hist_aux + ggp_violin_aux
  
  return(list(df = df,
              plot_histogram = ggp_hist,
              plot_violin = ggp_violin,
              plot_combined = plot_combined,
              df.outliers = df.outliers))
}

# somewhat hackish solution to:
# https://twitter.com/EamonCaddigan/status/646759751242620928
# based mostly on copy/pasting from ggplot2 geom_violin source:
# https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r

library(ggplot2)
library(dplyr)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomFlatViolin,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, ...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <- ggproto("GeomFlatViolin", Geom, setup_data = function(data, params){
  
  data$width <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)
  # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
  data %>%
    group_by(group) %>%
    mutate(ymin = min(y),
           ymax = max(y),
           xmin = x,
           xmax = x + width / 2)
  
}, draw_group = function(data, panel_scales, coord) {
  # Find the points for the line to go all the way around
  data <- transform(data, xminv = x,
                    xmaxv = x + violinwidth * (xmax - x))
  
  # Make sure it's sorted properly to draw the outline
  newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                   plyr::arrange(transform(data, x = xmaxv), -y))
  
  # Close the polygon: set first and last point the same
  # Needed for coord_polar and such
  newdata <- rbind(newdata, newdata[1,])
  
  ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
}, draw_key = draw_key_polygon, default_aes = aes(weight = 1, colour = "grey20", fill = "white",
                                                  size = 0.5, alpha = NA, linetype = "solid"), required_aes = c("x", "y"))

norm01 <- function(x){
  if(max(x)-min(x) != 0){
    return((x-min(x))/(max(x)-min(x)))
  }else{
    return(x/length(x))
  }
}

# from function3 - getInfo

#### ###
# Functions used in Script 4
#### ###

#function1 - dataset
QualitativeAnalysis <- function(data, lst_name = NULL, cn_exclude, cn_event, pairwise=F, deleteNA=F, SIG=SIG,
                                ROUND_EXP=ROUND_EXP, ROUND_PVAL=ROUND_PVAL,
                                title = T, subtitle = T){
  if(is.null(lst_name)){
    LST_COL_TEST <- getSubColnamesDF(data, remove = cn_exclude)
    d <- data
  }else{
    LST_COL_TEST <- getSubColnamesDF(data[[lst_name]], remove = cn_exclude)
    d <- data[[lst_name]]
  }

  if(!cn_event %in% colnames(d)){
    group <- NULL
  }else{
    group <- cn_event
  }

  lst_qualitative_results <- purrr::map(LST_COL_TEST, ~testQualitativeData3.1(df = d,
                                                                              cn=., group=group,
                                                                              pairwise=pairwise, deleteNA=deleteNA, SIG=SIG,
                                                                              ROUND_EXP=ROUND_EXP, ROUND_PVAL=ROUND_PVAL,
                                                                              title = title, subtitle = subtitle))
  return(lst_qualitative_results)
}

#function1 - dataset
QuantitativeAnalysis <- function(data, name_num = NULL, name_qual, cn_exclude, cn_event, histogram_mode = "stack", deleteNA=F, SIG=SIG,
                                 stats = T, wide = F,
                                 title = T, subtitle = T){
  if(is.null(name_num)){
    LST_COL_TEST <- getSubColnamesDF(data, remove = cn_exclude)
    d <- data
  }else{
    LST_COL_TEST <- getSubColnamesDF(data[[name_num]], remove = cn_exclude)
    d <- data[[name_num]]
  }

  if(!is.null(cn_event) & cn_event %in% colnames(data[[name_qual]])){
    group <- data[[name_qual]][,cn_event,drop=F]
  }else{
    group <- NULL
  }

  lst_quantitative_results <- purrr::map(LST_COL_TEST, ~testQuantitativeData3.1(df = d,
                                                                                cn=., group = group,
                                                                                histogram_mode = histogram_mode,
                                                                                deleteNA=deleteNA, stats = stats,
                                                                                title = title, subtitle = subtitle,
                                                                                wide = wide, x.lab = NULL))
  return(lst_quantitative_results)
}

combine2plots <- function(a, b, type = "qual"){
  if(!type %in% c("qual", "num")){
    plot_combined <- ggpubr::ggarrange(a, b)
    return(plot_combined)
  }

  if(type == "num"){
    a_aux <- a + xlab(NULL)
    b_aux <- b + ggtitle(label = " ", subtitle = " ") + xlab(NULL)

    plot_combined <- ggpubr::ggarrange(a_aux, b_aux)
    return(plot_combined)
  }

  if(type == "qual"){
    a_aux <- a + xlab(NULL)
    b_aux <- b + ggtitle(label = " ", subtitle = " ") + guides(fill = "none", color = "none")

    plot_combined <- ggpubr::ggarrange(a_aux, b_aux)
    return(plot_combined)
  }
}

# #REMOVE ONLY ONE FACTOR
removeOneFactorColumns <- function(data, keep = NULL){
  aux <- data
  df_cn_deleted <- NULL
  FLAG_DESC <- exists("VAR_DESCRIPTION")

  for(cn in colnames(data)){
    if(is.factor(data[,cn])){
      if(cn %in% keep)
        next

      lvl <- levels(data[,cn])
      if(length(lvl)==0 | length(lvl)==1){
        aux[,cn] <- NULL
        df_cn_deleted <- rbind(df_cn_deleted, c(cn, ifelse(FLAG_DESC, getInfo(cn)$Description, "")))
      }
    }
  }

  if(!is.null(df_cn_deleted)){
    df_cn_deleted <- as.data.frame(df_cn_deleted)
    colnames(df_cn_deleted) <- c("Variables", "Description")
    rownames(df_cn_deleted) <- NULL
  }

  return(list(new_data = aux, cn_deleted = df_cn_deleted))
}

update_lstDFs_by_lstDFs <- function(lst, names = NULL, lst_ori, names.ori = NULL){
  aux <- lst_ori
  for(h in names(aux)){
    if(!h %in% names(lst)){
      message(paste0(h, " not found in individual lists."))
      next
    }

    if(!is.null(names.ori)){
      if(is.null(names)){
        aux[[h]][[names.ori]] <- lst[[h]]
      }else{
        aux[[h]][[names.ori]] <- lst[[h]][[names]]
      }
    }else{
      if(is.null(names)){
        aux[[h]] <- lst[[h]]
      }else{
        aux[[h]] <- lst[[h]][[names]]
      }
    }
  }
  return(aux)
}

combine_lst_info_dimensions <- function(lst_df1, lst_df2, col_name = NULL){
  new_lst <- NULL
  for(h in names(lst_df1)){
    df1 <- lst_df1[[h]]
    df2 <- lst_df2[[h]]
    new_df <- cbind(df1, df2)
    new_df <- as.data.frame(new_df)

    if(is.null(col_name) & ncol(new_df)==2){
      colnames(new_df) <- c("Original Data", "New Data")
    }else if(ncol(new_df)==2){
      colnames(new_df) <- c("Original Data", col_name)
    }else{
      colnames(new_df) <- c(colnames(df1), col_name)
    }

    new_lst[[h]] <- new_df
    rownames(new_lst) <- NULL
  }
  return(new_lst)
}

#Delete variables or patients depending its NA % value.
#IF max NA is in any vairable - delete variable
#ELSE max NA is in patients - check if that patients (without all VAR > VAR_NA_MAX) still have more NA than max NA var
#   if true, then delete PAT, else delete VAR cause the patient will have lesser NAs if all the variables with more than NA_MAX are deleted

delete_PATorVAR <- function(data, VAL_NAMAX = 0.2, VAR_NA_KEEP = NULL, MIN_N_PAT = 50, verbose = F, ONLY_PAT = F, ONLY_VAR = F){

  if(class(data)[[1]]!="data.frame"){
    data <- as.data.frame(data)
  }

  LST_PAT_NA <- data.frame()
  LST_VAR_NA <- data.frame()

  if(ONLY_PAT & ONLY_VAR){
    stop("ONLY_PAT and ONLY_VAR cannot be True at the same time.")
  }

  if(ONLY_PAT){
    LST_VAR_NA <- NULL

    index_cc <- complete.cases(data[,!colnames(data) %in% c(VAR_NA_KEEP)])
    LST_PAT_NA <- data.frame(A = rownames(data[!index_cc,!colnames(data) %in% c(VAR_NA_KEEP)]),
                             B = rowSums(is.na(data[!index_cc,!colnames(data) %in% c(VAR_NA_KEEP)])) / ncol(data[,!colnames(data) %in% c(VAR_NA_KEEP)]))
    colnames(LST_PAT_NA) <- c("Patient", "% Missing Values")
    LST_PAT_NA[,2] <- round(as.numeric(LST_PAT_NA[,2]) * 100, 2)

    data <- data[index_cc,]

    return(list(filtered_data = data, LST_PAT_NA = LST_PAT_NA, LST_VAR_NA = LST_VAR_NA))
  }

  value_var_NA <- colSums(is.na(data[!colnames(data) %in% VAR_NA_KEEP])) / nrow(data[!colnames(data) %in% VAR_NA_KEEP])
  var_NA <- names(value_var_NA)[value_var_NA > VAL_NAMAX]

  if(ONLY_VAR){
    LST_PAT_NA <- NULL
    LST_VAR_NA <- NULL
    if(length(var_NA)>0){
      LST_VAR_NA <- data.frame(A = var_NA,
                               B = value_var_NA[var_NA])
      colnames(LST_VAR_NA) <- c("Variable", "% Missing Values")
      LST_VAR_NA[,2] <- round(as.numeric(LST_VAR_NA[,2]) * 100, 2)
      rownames(LST_VAR_NA) <- NULL

      data <- data[,!colnames(data) %in% var_NA] #faster than data[,var_NA] <- NULL
    }

    return(list(filtered_data = data, LST_PAT_NA = LST_PAT_NA, LST_VAR_NA = LST_VAR_NA))

  }

  value_pat_NA <- rowSums(is.na(data[!colnames(data) %in% c(VAR_NA_KEEP, var_NA)])) / ncol(data[!colnames(data) %in% c(VAR_NA_KEEP, var_NA)])
  pat_NA <- names(value_pat_NA)[value_pat_NA > VAL_NAMAX]

  while(length(var_NA)>0 | length(pat_NA)>0){
    FLAG_VAR = FALSE

    if(max(value_var_NA) >= max(value_pat_NA)){
      FLAG_VAR = TRUE
    }

    #Delete patient
    #Check if we reach the minimum quantity of patients
    if(!FLAG_VAR){
      if(verbose){
        if(all(c("subjid", "SOURCE") %in% colnames(data))){
          print(data[names(which.max(value_pat_NA)),c("subjid", "SOURCE")])
        }else{
          print(data[names(which.max(value_pat_NA)),c(1:5)])
        }

      }

      if((nrow(data)-1) < MIN_N_PAT){
        FLAG_VAR = TRUE #delete variable
      }

    }

    if(FLAG_VAR){
      cn = names(which.max(value_var_NA))
      if(verbose){
        message(cn)
      }
      val = value_var_NA[which.max(value_var_NA)]
      names(val) = NULL
      LST_VAR_NA <- rbind(LST_VAR_NA, c(cn, val))
      data[,cn] <- NULL
    }else{
      cn = names(which.max(value_pat_NA))
      if(verbose){
        message(cn)
      }
      val = value_pat_NA[which.max(value_pat_NA)]
      names(val) = NULL
      LST_PAT_NA <- rbind(LST_PAT_NA, c(cn, val))
      data <- data[!rownames(data) %in% cn,]
    }

    # Comprobar que % de similitud hay entre ellos dos.
    value_var_NA <- colSums(is.na(data[!colnames(data) %in% VAR_NA_KEEP])) / nrow(data[!colnames(data) %in% VAR_NA_KEEP])
    var_NA <- names(value_var_NA)[value_var_NA > VAL_NAMAX]

    value_pat_NA <- rowSums(is.na(data[!colnames(data) %in% c(VAR_NA_KEEP, var_NA)])) / ncol(data[!colnames(data) %in% c(VAR_NA_KEEP, var_NA)])
    pat_NA <- names(value_pat_NA)[value_pat_NA > VAL_NAMAX]
  }

  if(length(LST_VAR_NA)>0){
    LST_VAR_NA <- as.data.frame(LST_VAR_NA)
    colnames(LST_VAR_NA) <- c("Variable", "% Missing Values")
    LST_VAR_NA[,2] <- round(as.numeric(LST_VAR_NA[,2]), 2) * 100
  }

  if(length(LST_PAT_NA)>0){
    LST_PAT_NA <- as.data.frame(LST_PAT_NA)
    colnames(LST_PAT_NA) <- c("Patient", "% Missing Values")
    LST_PAT_NA[,2] <- round(as.numeric(LST_PAT_NA[,2]), 2) * 100
  }

  return(list(filtered_data = data, LST_PAT_NA = LST_PAT_NA, LST_VAR_NA = LST_VAR_NA))
}

deletePatients <- function(data, patientsToDelete){
  if(length(which(rownames(data) %in% patientsToDelete))>0)
    data <- data[-which(rownames(data) %in% patientsToDelete),]
  return(data)
}

filterNAVariables <- function(data, VALNAMAX, keep, var_per_column = 50, max_num_col = 4, separateImages = F){

  if(nrow(data)==0 | is.null(data)){
    return(data)
  }

  count_NA <- colSums(is.na(data))
  perc <- count_NA/nrow(data) #Porcentaje de NAs por columna
  cn.VALNAMAX <- c(colnames(data[,perc<=VALNAMAX]), keep) #Variables with less than VALNAMAX per column and to keep
  cn.VALNAMAX <- cn.VALNAMAX[cn.VALNAMAX %in% colnames(data)] #update bc keep could not be in colnames(data)

  cn.2deleteNA <- colnames(data)[!colnames(data) %in% cn.VALNAMAX]

  percDF <- as.data.frame(round(perc*100,2))
  percDF$ID <- rownames(percDF)
  colnames(percDF) <- c("perc","ID")

  percDF <- percDF[order(percDF$perc, decreasing = T),]
  color <- RColorConesa::colorConesa(2)
  percDF$color <- factor(ifelse(percDF$ID %in% keep, color[2], color[1]))

  block <- ceiling(nrow(percDF)/var_per_column)
  lst_plots <- NULL
  if(block>1){
    for(i in 1:(block-1)){
      a <- ggplot(percDF[(1+((i-1)*var_per_column)):(i*var_per_column),], aes(x = reorder(ID, +perc), y = perc)) +
        geom_bar(stat="identity", fill = percDF$color[(1+((i-1)*var_per_column)):(i*var_per_column)]) +
        RColorConesa::scale_fill_conesa() +
        xlab("Variables") + ylab("% of NAs values") +
        coord_flip() +
        geom_hline(yintercept=VALNAMAX*100, color = "red") +
        theme(axis.text.x = element_text(angle = 0, hjust = 1), axis.text.y = element_text(size = 5))

      lst_plots[[i]] <- a
    }
  }
  #last block
  a <- ggplot(percDF[(1+((block-1)*var_per_column)):nrow(percDF),], aes(x = reorder(ID, +perc), y = perc)) +
    geom_bar(stat="identity", fill = percDF$color[(1+((block-1)*var_per_column)):nrow(percDF)]) +
    RColorConesa::scale_fill_conesa() +
    xlab("Variables") + ylab("% of NAs values") +
    coord_flip() +
    geom_hline(yintercept=VALNAMAX*100, color = "red") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1), axis.text.y = element_text(size = 5))

  lst_plots[[block]] <- a

  names(lst_plots) <- paste0("Variables_",1:length(lst_plots),"/",length(lst_plots))

  ggp <- ggpubr::ggarrange(plotlist = lst_plots, ncol = min(block, max_num_col), nrow = ceiling(block/min(block,max_num_col)))

  #new data
  new_data <- data[,cn.VALNAMAX]

  #porportion columns deleted
  prop_deleted <- length(cn.2deleteNA) / ncol(data)

  #porportion columns keeped
  prop_keeped <- 1-prop_deleted

  #df keep
  df_cn_keep <- NULL
  FLAG_DESC <- exists("VAR_DESCRIPTION")
  if(!length(cn.VALNAMAX)==0){
    for(cn in cn.VALNAMAX){
      df_cn_keep <- rbind(df_cn_keep, c(cn, ifelse(FLAG_DESC, getInfo(cn)$Description, "")))
    }
    if(!is.null(df_cn_keep)){
      df_cn_keep <- as.data.frame(df_cn_keep)
      colnames(df_cn_keep) <- c("Variables", "Description")
      rownames(df_cn_keep) <- NULL
    }
  }

  #df deleted
  df_cn_deleted <- NULL
  if(!length(cn.2deleteNA)==0){
    for(cn in cn.2deleteNA){
      df_cn_deleted <- rbind(df_cn_deleted, c(cn, ifelse(FLAG_DESC, getInfo(cn)$Description, "")))
    }
    if(!is.null(df_cn_deleted)){
      df_cn_deleted <- as.data.frame(df_cn_deleted)
      colnames(df_cn_deleted) <- c("Variables", "Description")
      rownames(df_cn_deleted) <- NULL
    }
  }

  if(separateImages){
    ggp = lst_plots
  }

  return(list(cn_keeped = df_cn_keep, cn_deleted = df_cn_deleted, perc = perc, prop_deleted = prop_deleted, prop_keeped=prop_keeped, new_data = new_data, original_data = data, plot = ggp))
}

#Get Variables by type from FilteredMatrix (just with variables with less than a specific NAs value)
splitVariablesByType <- function(data, d.cual, d.num, d.date, d.unit, UNITS_VAR, lst_except = "subjid"){
  for(cn in colnames(data)){
    c <- class(data[,cn])
    var <- data[,cn]

    #if subjid next because is the ID
    if(cn %in% lst_except)
      next

    if(is.factor(var)){
      if(is.null(d.cual))
        d.cual <- data[,cn, drop=FALSE]
      else
        d.cual <- cbind(d.cual, data[,cn, drop=FALSE])
    }else if(is.numeric(var) | class(var)=="difftime"){
      if(is.null(d.num))
        d.num <- data[,cn, drop=FALSE]
      else
        d.num <- cbind(d.num, data[,cn, drop=FALSE])
    }else if(c=="Date"){
      if(is.null(d.date))
        d.date <- data[,cn, drop=FALSE]
      else
        d.date <- cbind(d.date, data[,cn, drop=FALSE])
    }else if(is.character(var)){
      if(is.null(d.cual))
        d.cual <- data[,cn, drop=FALSE]
      else
        d.cual <- cbind(d.cual, as.factor(data[,cn, drop=FALSE]))
    }else{
      print(paste0("Variable ", cn, " is not a factor, character, numeric or date format."))
    }
  }

  if(!is.null(UNITS_VAR)){
    d.unit <- d.cual[,colnames(d.cual) %in% UNITS_VAR, drop=F]
    d.cual <- d.cual[,!colnames(d.cual) %in% UNITS_VAR, drop=F]
    rownames(d.unit) <- rownames(data)
  }

  #ROWNAMES
  if(!is.null(d.cual)) rownames(d.cual) <- rownames(data)
  if(!is.null(d.num)) rownames(d.num)  <- rownames(data)
  if(!is.null(d.date)) rownames(d.date) <- rownames(data)

  return(list(d.cual = d.cual, d.num = d.num, d.date = d.date, d.unit = d.unit))
}

deleteZeroVarianceVariables <- function(data, mustKeep = NULL, interactions = F, symbol = ":", names = NULL, info=T){

  if(!is.null(names)){
    df <- data[[names]]
  }else{
    df <- data
  }

  if(is.null(df)){return(NULL)}

  #Zero Var
  nZ <- caret::nearZeroVar(df[,!colnames(df) %in% c("time", "event", "status")], saveMetrics = T) #to check if we have to do some changes in the data
  td <- rownames(nZ[nZ$nzv==T,])

  #Do not delete SEX and AGE_ESTIMATEYEARS_CAT
  if(any(mustKeep %in% td)){
    td <- td[-which(td %in% mustKeep)]
  }

  lstDeleted <- td
  df <- df[,!colnames(df) %in% lstDeleted, drop=F]

  #df deleted
  df_cn_deleted <- NULL
  FLAG_DESC <- exists("VAR_DESCRIPTION")
  if(info){
    if(!length(lstDeleted)==0){
      for(cn in lstDeleted){
        df_cn_deleted <- rbind(df_cn_deleted, c(cn, ifelse(FLAG_DESC, getInfo(cn)$Description, "")))
      }
      if(!is.null(df_cn_deleted)){
        df_cn_deleted <- as.data.frame(df_cn_deleted)
        colnames(df_cn_deleted) <- c("Variables", "Description")
        rownames(df_cn_deleted) <- NULL
      }
    }
  }

  return(list(filteredData = df, variablesDeleted = df_cn_deleted))
}

deleteNearZeroCoefficientOfVariation <- function(X, LIMIT = 0.1) {
  # Filtrar solo las columnas numéricas
  numeric_vars <- sapply(X, is.numeric)
  numeric_X <- X[, numeric_vars, drop = FALSE]
  
  # Calcular el coeficiente de variación para cada variable numérica
  cvar <- apply(numeric_X, 2, function(x) {
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    
    # Si la desviación estándar es cero o la media es cero, retornamos NA
    if (sd_x == 0 || mean_x == 0) return(NA)
    
    # Calcular el coeficiente de variación
    sd_x / abs(mean_x)
  })
  
  # Identificar variables con coeficiente de variación menor o igual al límite o NaN
  variables_to_delete <- names(cvar)[cvar <= LIMIT | is.na(cvar)]
  
  # Eliminar las variables identificadas del conjunto original
  newX <- X[, !colnames(X) %in% variables_to_delete, drop = FALSE]
  
  # Preparar la lista de variables eliminadas si existen
  variablesDeleted <- if (length(variables_to_delete) > 0) {
    data.frame(Variables = variables_to_delete)
  } else {
    NULL
  }
  
  # Devolver la lista con los datos filtrados y las variables eliminadas
  return(list(
    X = newX,
    variablesDeleted = variablesDeleted,
    coeff_variation = cvar
  ))
}

detectCorrelation <- function(data, COR_LIMIT = 0.9, method = "pearson", QUAL = F, NUM_QUAL = F, exact = F, rm.na = F, reverse_color = F, palette = "nature"){

  if(!palette %in% names(RColorConesa::getConesaPalettes())){
    stop(paste0("Palette name must be one of the following: ", paste0(names(RColorConesa::getConesaPalettes()), collapse = ", ")))
  }

  if(QUAL & NUM_QUAL){
    stop("Only QUAL or NUM_QUAL could be as TRUE. If BOTH as FALSE, numerical correlation is applied.")
  }

  if(NUM_QUAL & !class(data)=="list"){
    stop("If numerical and qualitative variables analysis, data must be a list with numerical data in first position and qualitative in second.")
  }

  if(class(data)=="list" & length(data)==2 & NUM_QUAL){ #d.cual.binary + d.num
    df_num <- data[[1]]
    df_qual <- data[[2]]
    if(is.null(df_num)){return(NULL)}
    if(is.null(df_qual)){return(NULL)}
    if(ncol(df_num) + ncol(df_qual) == 0){return(NULL)}
  }else if(class(data)=="list" & NUM_QUAL){
    stop("Length of data list must be two. Numerical and Qualitative data.frames in first and second position.")
  }else{
    df <- data
    if(is.null(df) || ncol(df)==1){return(NULL)}
  }

  cor_matrix <- NA
  var_deleted <- NULL

  aux_flag = T #first iteration
  while(any(is.na(cor_matrix)) & aux_flag){

    if(QUAL){
      cor_matrix = diag(1, ncol = ncol(df), nrow = ncol(df))
      colnames(cor_matrix) = rownames(cor_matrix) = colnames(df)
      for (i in 1:(ncol(df)-1)) {
        for (j in (i+1):ncol(df)) {
          myTTT = table(na.omit(df[,c(i,j)]))
          cor_matrix[i,j] = cor_matrix[j,i] = rcompanion::cramerV(myTTT)
        }
      }
    }else if(NUM_QUAL){
      cor_matrix = diag(1, ncol = ncol(df_num), nrow = ncol(df_qual))
      colnames(cor_matrix) = colnames(df_num)
      rownames(cor_matrix) = colnames(df_qual)
      for (i in 1:ncol(df_qual)) {
        for (j in 1:ncol(df_num)) {
          myTTT = na.omit(data.frame(x = df_qual[,i],
                                     y = df_num[,j]))
          if (length(unique(myTTT$x)) > 1) {
            cor_matrix[i,j] = anovaPval(myTTT$x, myTTT$y) #[0,1]
          } else { cor_matrix[i,j] = NA }
        }
      }
    }else{
      cor_matrix <- cor(df, use = "pairwise.complete.obs", method = method)
    }

    if(rm.na){
      max_na <- max(colSums(is.na(cor_matrix)))
      if(!max_na==0){
        cn <- names(colSums(is.na(cor_matrix))[which.max(colSums(is.na(cor_matrix)))])
        if(length(cn)>0){
          var_deleted <- c(var_deleted, cn)
          df <- df[,!colnames(df) %in% cn]
        }
      }
    }else{
      aux_flag = F
    }

  }

  if(any(is.na(cor_matrix))){
    cor_high <- NULL
  }else{
    cor_high <- caret::findCorrelation(x = cor_matrix, cutoff = COR_LIMIT, exact = F)
  }

  if(length(cor_high)>0){
    high_cor_remove <- colnames(cor_matrix)[cor_high]
  }else{
    high_cor_remove = NULL
  }

  testRes = NULL
  if(!NUM_QUAL){
    testRes <- tryCatch(expr = {corrplot::cor.mtest(cor_matrix, conf.level = 0.95)},
                        error = function(e){
                          message(paste0("correlation qualitative: ", e))
                          return(NULL)
                        })
  }

  if(any(is.nan(cor_matrix))){
    cor_matrix[is.nan(cor_matrix)] = NA
  }

  tl.cex	= 0.4
  pch.cex = 0.4
  na.label = " "
  na.cex = 0.4
  cl.cex = 0.3
  number.cex = 0.3

  MIN_SIZE = 30
  MAX_SIZE = 80

  TEXT_MIN_SIZE = 0.1
  TEXT_MAX_SIZE = 0.4

  MAX_NCOL_PVALUE = 25

  # change size depending number of elements in max dim in cor_matrix
  # less than MIN_SIZE -> 0.4
  # max than MAX_SIZE -> 0.1
  # in middle a regression -> x
  if(!max(dim(cor_matrix))<=MIN_SIZE){
    if(max(dim(cor_matrix))>=MAX_SIZE){
      tl.cex = TEXT_MIN_SIZE
    }else{
      tl.cex = (1-(max(dim(cor_matrix))-MIN_SIZE)/(MAX_SIZE-MIN_SIZE)) * (TEXT_MAX_SIZE-TEXT_MIN_SIZE) + TEXT_MIN_SIZE
    }
  }

  col1 <- RColorConesa::getConesaPalettes()[[palette]][[2]] #neg
  col2 <- "white"
  col3 <- RColorConesa::getConesaPalettes()[[palette]][[1]] #pos

  if(reverse_color){
    col4 <- col3
    col3 <- col1
    col1 <- col4
    rm(col4)
  }

  col_vector <- c(col1, col2, col3)
  num_color_sections = 80
  if((QUAL & sum(is.na(cor_matrix))==0) | (NUM_QUAL & sum(is.na(cor_matrix))==0)){
    cl.lim <- c(0,1)
  }else{
    cl.lim <- c(-1,1)
  }

  #For num_qual - sort data and update NAs
  if(NUM_QUAL){
    aux <- cor_matrix
    aux[is.na(aux)] <- -1

    if(nrow(aux)==1 & ncol(aux)>1){
      aux <- t(aux)
      dist_mat <- dist(aux, method = 'euclidean')
      aaa <- hclust(dist_mat, method = "ward.D2")
      aux2 <- t(cor_matrix)
      aux2 <- aux2[aaa$order,,drop=F]
      aux2[is.na(aux2)] <- -1
    }else if(nrow(aux)==1 & ncol(aux)==1){
      dist_mat <- NULL
      aaa <- NULL
      aux2 <- cor_matrix
      aux2[is.na(aux2)] <- -1
    }else{
      dist_mat <- dist(aux, method = 'euclidean')
      aaa <- hclust(dist_mat, method = "ward.D2")
      aux2 <- cor_matrix
      aux2 <- aux2[aaa$order,]
      aux2[is.na(aux2)] <- -1
    }

  #If QUAL and some NAs, change to -1
  }else if(QUAL & any(is.na(cor_matrix))){
    aux2 <- cor_matrix
    aux2[is.na(aux2)] <- -1
    cor_matrix <- aux2
  }

  if(!NUM_QUAL){

    # WITH NAs
    if(any(is.na(cor_matrix))){
      if(max(dim(cor_matrix))<=MAX_NCOL_PVALUE){
        corr_plot <- function(){corrplot::corrplot(cor_matrix, is.corr = TRUE, type = c("lower"), cl.lim = cl.lim, col.lim = cl.lim,
                                                   method = "color", diag = F, addgrid.col = "white", mar = c(0, 0, 0, 0),
                                                   tl.col = "black", tl.cex = tl.cex, cl.cex = cl.cex,
                                                   addCoef.col = "white", number.cex = number.cex, number.digits = 2,
                                                   #p.mat = testRes$p, sig.level = c(0.001, 0.01, 0.05), insig = "blank",# insig = "label_sig",
                                                   pch.cex = pch.cex, pch.col = "white", win.asp = 1, na.label = na.label, na.cex = na.cex,
                                                   col = colorRampPalette(col_vector)(num_color_sections))}
      }else{
        corr_plot <- function(){corrplot::corrplot(cor_matrix, is.corr = TRUE, type = c("lower"), cl.lim = cl.lim, col.lim = cl.lim,
                                                   method = "color", diag = F, addgrid.col = "white", mar = c(0, 0, 0, 0),
                                                   tl.col = "black", tl.cex = tl.cex, cl.cex = cl.cex,
                                                   #addCoef.col = "white", number.cex = number.cex, number.digits = 2,
                                                   #p.mat = testRes$p, sig.level = c(0.001, 0.01, 0.05), insig = "blank",# insig = "label_sig",
                                                   pch.cex = pch.cex, pch.col = "white", win.asp = 1, na.label = na.label, na.cex = na.cex,
                                                   col = colorRampPalette(col_vector)(num_color_sections))}
      }
    }else{

      #NO NAs
      if(max(dim(cor_matrix))<=MAX_NCOL_PVALUE){
        corr_plot <- function(){corrplot::corrplot(cor_matrix, is.corr = TRUE, cl.lim = cl.lim, col.lim = cl.lim,
                                                   order = "hclust", hclust.method = "ward.D2",
                                                   method = "color", diag = T, addgrid.col = "white", mar = c(0, 0, 0, 0),
                                                   tl.col = "black", tl.cex = tl.cex, cl.cex = cl.cex,
                                                   addCoef.col = "white", number.cex = number.cex, number.digits = 2,
                                                   #p.mat = 1-cor_matrix, sig.level = c(0.001, 0.01, 0.05), insig = "blank",# insig = "label_sig",
                                                   pch.cex = pch.cex, pch.col = "white", win.asp = 1, na.label = na.label,
                                                   col = colorRampPalette(col_vector)(num_color_sections))}
      }else{
        corr_plot <- function(){corrplot::corrplot(cor_matrix, is.corr = TRUE, cl.lim = cl.lim, col.lim = cl.lim,
                                                   order = "hclust", hclust.method = "ward.D2",
                                                   method = "color", diag = T, addgrid.col = "white", mar = c(0, 0, 0, 0),
                                                   tl.col = "black", tl.cex = tl.cex, cl.cex = cl.cex,
                                                   #addCoef.col = "white", number.cex = number.cex, number.digits = 2,
                                                   #p.mat = 1-cor_matrix, sig.level = c(0.001, 0.01, 0.05), insig = "blank",# insig = "label_sig",
                                                   pch.cex = pch.cex, pch.col = "white", win.asp = 1, na.label = na.label,
                                                   col = colorRampPalette(col_vector)(num_color_sections))}
      }
    }
  }else{



    if(max(dim(cor_matrix))<=MAX_NCOL_PVALUE){
      corr_plot <- function(){corrplot::corrplot(aux2, is.corr = TRUE, cl.lim = cl.lim, col.lim = cl.lim, #range 0-1
                                                 method = "color", diag = T, addgrid.col = "white", mar = c(0, 0, 0, 0),
                                                 tl.col = "black", tl.cex = tl.cex, cl.cex = cl.cex,
                                                 order = "original",
                                                 addCoef.col = "white", number.cex = number.cex, number.digits = 2,
                                                 #p.mat = 1-cor_matrix, sig.level = c(0.001, 0.01, 0.05), insig = "blank",# insig = "label_sig",
                                                 pch.cex = pch.cex, pch.col = "white", win.asp = 1, na.label = na.label,
                                                 col = colorRampPalette(col_vector)(num_color_sections))}
    }else{
      corr_plot <- function(){corrplot::corrplot(aux2, is.corr = TRUE, cl.lim = cl.lim, col.lim = cl.lim, #range 0-1
                                                 method = "color", diag = T, addgrid.col = "white", mar = c(0, 0, 0, 0),
                                                 tl.col = "black", tl.cex = tl.cex, cl.cex = cl.cex,
                                                 order = "original",
                                                 #addCoef.col = "white", number.cex = number.cex, number.digits = 2,
                                                 #p.mat = 1-cor_matrix, sig.level = c(0.001, 0.01, 0.05), insig = "blank",# insig = "label_sig",
                                                 pch.cex = pch.cex, pch.col = "white", win.asp = 1, na.label = na.label,
                                                 col = colorRampPalette(col_vector)(num_color_sections))}
    }
  }

  return(list(cor_matrix = cor_matrix, corr_plot = corr_plot, columns_to_delete = high_cor_remove, var_with_na = var_deleted))
}

pcaT2 <- function(model, n.comp = 2, conf = c(0.95, 0.99), strict = F){
  misScores = model@scoreMN
  varT = apply(misScores, 2, stats::var)
  miT2 = colSums(t(misScores**2) / varT)
  N = nrow(model@scoreMN)  # N es el número de observaciones
  A = n.comp  # Número de componentes principales seleccionadas

  lst_conf <- list()
  lst_anomal <- list()
  for(val in conf){
    lst_conf[[as.character(val)]] <- A*(N**2 - 1)/(N*(N - A)) * qf(val, A, N-A)
    if(strict){
      lst_anomal[[as.character(val)]] <-  data.frame(ID = names(miT2)[which(miT2 > 2*lst_conf[[as.character(val)]])],
                                                     Value = miT2[which(miT2 > 2*lst_conf[[as.character(val)]])])
    }else{
      lst_anomal[[as.character(val)]] <-  data.frame(ID = names(miT2)[which(miT2 > lst_conf[[as.character(val)]])],
                                                   Value = miT2[which(miT2 > lst_conf[[as.character(val)]])])
    }
  }

  col <- RColorConesa::colorConesa(length(lst_conf)) #colors

  ggp <- ggplot(data = as.data.frame(miT2), aes(x = 1:length(miT2), y = miT2)) +
    geom_line() +
    ylab("T2") + xlab("Observation") + ggtitle("PCA: T2-Hotelling") + xlim(1, length(miT2))

  for(i in 1:length(lst_conf)){
    ggp <- ggp + geom_hline(yintercept = lst_conf[[i]], color = col[[i]], linetype="dashed", size = 1)
  }

  for(i in names(lst_anomal)){
    rownames(lst_anomal[[i]]) <- NULL
  }

  return(list(plot = ggp, lst_outlier = lst_anomal))
}

#contribución de las variables al valor anormal de los sujetos "anómalos"
contribT2 = function (model, observ, cutoff = 2, plots = T) {

  if(nrow(observ)==0){
    return(list(df = NULL, lst_plot = NULL, top5 = NULL))
  }

  misScores = model@scoreMN
  loadings = model@loadingMN
  eigenval = apply(misScores, 2, stats::var)
  X = model@suppLs$xModelMN
  observ = observ[,1]

  # eigenval es la varT anterior (aunque igual lo devuelve ropls)
  # X is the data matrix and must be centered (or centered and scaled if data were scaled)

  misScoresNorm = t(t(misScores**2) / eigenval)
  misContrib = NULL
  for (oo in observ) {
    # print(rownames(misScores)[oo])
    # print(misScores[oo,])
    misPCs = which(as.numeric(misScoresNorm[oo,]) > cutoff)
    lacontri = sapply(misPCs, function (cc) (misScores[oo,cc]/eigenval[cc])*loadings[,cc]*X[oo,])
    lacontri = rowSums((1*(sign(lacontri) == 1))*lacontri)
    misContrib = cbind(misContrib, lacontri)
  }
  colnames(misContrib) = rownames(misScoresNorm[observ,])
  misContrib <- as.data.frame(misContrib)

  lst_ggplots <- list()
  top5 <- NULL

  for(obs in colnames(misContrib)){
    df <- misContrib[,obs,drop=F]
    df$Variable <- rownames(df)
    df <- df[order(df[,1], decreasing = T),]
    df$Variable <- factor(rownames(df), levels = rownames(df))
    colnames(df) <- c("Value", "Variable")

    if(plots){
      ggp <- ggplot(data = df, aes(x = Variable, y = Value)) +
        geom_bar(stat = "identity", fill = RColorConesa::colorConesa(1)) + theme(legend.position = "none") + ggtitle(obs)

      if(nrow(df) <= 20){
        ggp <- ggp + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7.5))
      }else if(nrow(df) > 20 & nrow(df) <= 40){
        ggp <- ggp + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5.5))
      }else if(nrow(df) > 40){
        ggp <- ggp + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 3.5))
      }

      lst_ggplots[[obs]] <- ggp
    }

    top5 <- c(top5, rownames(df)[1:5])
    df_top5 <- data.frame(table(top5))
    df_top5 <- df_top5[order(df_top5$Freq, decreasing = T),]
  }

  colnames(df_top5) <- c("Variable", "Freq in TOP 5")
  rownames(df_top5) <- NULL

  if(length(lst_ggplots)==0){
    lst_ggplots = NULL
  }

  return(list(df = misContrib, lst_plot = lst_ggplots, top5 = df_top5))
}

pcaRSS <- function(model, conf = c(0.95, 0.99), strict = F){
  misLoadings = model@loadingMN
  misScores = model@scoreMN
  X = model@suppLs$xModelMN
  myE = X - misScores %*% t(misLoadings)  # residual matrix

  myE[is.na(myE)] <- 0 #NA by 0
  myRSS = rowSums(myE^2)

  g = stats::var(myRSS)/(2*mean(myRSS))
  h = (2*mean(myRSS)^2)/stats::var(myRSS)

  lst_conf <- list()
  lst_anomal <- list()
  for(val in conf){
    if(strict){
      lst_conf[[as.character(val)]] = g*qchisq(val, df = h)
      lst_anomal[[as.character(val)]] <-  data.frame(ID = names(myRSS)[myRSS > 2*lst_conf[[as.character(val)]]],
                                                     Value = myRSS[which(myRSS > 2*lst_conf[[as.character(val)]])])
    }else{
      lst_conf[[as.character(val)]] = g*qchisq(val, df = h)
      lst_anomal[[as.character(val)]] <-  data.frame(ID = names(myRSS)[myRSS > lst_conf[[as.character(val)]]],
                                                     Value = myRSS[which(myRSS > lst_conf[[as.character(val)]])])
    }
  }

  col <- RColorConesa::colorConesa(length(lst_conf)) #colors

  ggp <- ggplot(data = as.data.frame(myRSS), aes(x = 1:length(myRSS), y = myRSS)) +
    geom_line() +
    ylab("RSS") + xlab("Observation") + ggtitle("PCA: RSS") + xlim(1, length(myRSS))

  for(i in 1:length(lst_conf)){
    ggp <- ggp + geom_hline(yintercept = lst_conf[[i]], color = col[[i]], linetype="dashed", size = 1)
  }

  for(i in names(lst_anomal)){
    rownames(lst_anomal[[i]]) <- NULL
  }

  return(list(plot = ggp, lst_outlier = lst_anomal))
}

#contribución de las variables al valor anormal de los sujetos "anómalos"
contribRSS = function (model, observ, cutoff = 2, plots = T) {

  if(nrow(observ)==0){
    return(list(df = NULL, lst_plot = NULL, top5 = NULL))
  }

  misLoadings = model@loadingMN
  misScores = model@scoreMN
  observ = observ[,1]
  X = model@suppLs$xModelMN
  myE = X - misScores %*% t(misLoadings)  # matriz de residuos

  myE[is.na(myE)] <- 0
  myRSS = rowSums(myE^2)

  misContrib = NULL
  for (j in 1:length(myRSS)){
    eind<-myE[j,]
    signo<-sign(eind)
    contri<-(signo*(eind^2)/myRSS[j])*100
    misContrib<-rbind(misContrib,contri)
  }
  misContrib <- t(misContrib)
  misContrib <- data.frame(misContrib)
  colnames(misContrib) <- rownames(myE)

  lst_ggplots <- list()
  top5 <- NULL
  df_top5 <- NULL

  for(obs in observ){
    df <- misContrib[,obs,drop=F]
    df$Variable <- rownames(df)
    df[is.na(df)] <- 0
    df <- df[order(df[,1], decreasing = T),]
    df$Variable <- factor(rownames(df), levels = rownames(df))
    colnames(df) <- c("Value", "Variable")

    if(plots){
      ggp <- ggplot(data = df, aes(x = Variable, y = Value)) +
        geom_bar(stat = "identity", fill = RColorConesa::colorConesa(1)) + theme(legend.position = "none") + ggtitle(obs)

      if(nrow(df) <= 20){
        ggp <- ggp + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7.5))
      }else if(nrow(df) > 20 & nrow(df) <= 40){
        ggp <- ggp + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5.5))
      }else if(nrow(df) > 40){
        ggp <- ggp + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 3.5))
      }

      lst_ggplots[[obs]] <- ggp
    }

    #can present negative values!!!
    top5 <- c(top5, rownames(df)[1:5])
    df_top5 <- data.frame(table(top5))
    df_top5 <- df_top5[order(df_top5$Freq, decreasing = T),]
  }

  colnames(df_top5) <- c("Variable", "Freq in TOP 5")
  rownames(df_top5) <- NULL

  if(length(lst_ggplots)==0){
    lst_ggplots = NULL
  }

  return(list(df = misContrib, lst_plot = lst_ggplots, top5 = df_top5))
}

factorToBinary <- function(d.cualFiltered, all=T, sep = "_", except = ""){

  if(nrow(d.cualFiltered)==0 | is.null(d.cualFiltered)){
    return(d.cualFiltered)
  }

  binaryMatrix <- NULL
  options(na.action='na.pass')
  for(cn in colnames(d.cualFiltered)){
    if(cn %in% except)
      next
    variable <- d.cualFiltered[, cn, drop=F]
    colnames(variable) <- cn
    if(all){
      form <- as.formula(paste0("~ ", cn, " + 0"))
      binaryVariable <- model.matrix(form, data=variable)[,1:length(levels(variable[,1])), drop=F]
      colnames(binaryVariable) <- paste0(cn, sep, levels(variable[,1]))
    }else{
      form <- as.formula(paste0("~ ", cn))
      binaryVariable <- model.matrix(form, data=variable)[,2:length(levels(variable[,1])), drop=F]
      colnames(binaryVariable) <- paste0(cn, sep, levels(variable[,1])[2:length(levels(variable[,1]))])
    }
    binaryMatrix <- cbind(binaryMatrix, binaryVariable)
  }
  colnames(binaryMatrix) <- sapply(colnames(binaryMatrix), function(x){gsub(" ", "", x)}) #colnames with these symbols has problems in coxSW
  colnames(binaryMatrix) <- sapply(colnames(binaryMatrix), function(x){gsub("/", "_", x)})
  colnames(binaryMatrix) <- sapply(colnames(binaryMatrix), function(x){gsub("-", "_", x)})

  binaryMatrix <- as.data.frame(binaryMatrix)

  #transform to factors
  # for(cn in colnames(binaryMatrix)){
  #   binaryMatrix[,cn] <- as.factor(binaryMatrix[,cn])
  # }
  return(binaryMatrix)
}

bestVarianceComponentOPLS <- function(RES_PCA_RESULTS, LST_VAR, VAR_PCA_STUDY, res_matrix = F){
  varexp_lst <- NULL
  infiniteValue <- 1e-80

  RES_PCA_VARIANCE_COMP <- list()
  for(h in names(RES_PCA_RESULTS)){
    pca.result <- RES_PCA_RESULTS[[h]]
    RES_PCA_VARIANCE_COMP[[h]] <- data.frame()
    for(p in colnames(pca.result@scoreMN)){
      component <- pca.result@scoreMN[,p]
      res.pval <- 1
      varexp <- NULL
      Infinite <- F
      for(cn in VAR_PCA_STUDY){

        if(cn %in% colnames(LST_VAR[[h]]$d.cual)){
          factor <- LST_VAR[[h]]$d.cual[,cn]
        }else if(cn %in% colnames(LST_VAR[[h]]$d.num)){
          factor <- LST_VAR[[h]]$d.num[,cn]

          # create 3 levels
          value_min <- min(factor, na.rm = TRUE)
          value_max <- max(factor, na.rm = TRUE)

          # Calcular los puntos de división para crear tres franjas horarias
          punto_1 <- value_min + (value_max - value_min) / 3
          punto_2 <- value_min + 2 * (value_max - value_min) / 3

          new_factor <- cut(
            factor,
            breaks = c(-Inf, punto_1, punto_2, Inf),
            labels = c("Low", "Medium", "High"),
            include.lowest = TRUE
          )

          factor <- factor(new_factor, levels = c("Low", "Medium", "High"))
        }else{
          next
        }

        df <- as.data.frame(cbind(component, factor))
        df <- df[!is.na(df$factor),]
        res.aov <- aov(factor ~ component, data = df)

        sum_test <- unlist(summary(res.aov))
        p.val <- sum_test["Pr(>F)1"]

        if(!res_matrix){
          if(p.val<infiniteValue){
            Infinite=T
          } #at least one p-val

          if(res.pval>p.val & !Infinite){
            res.pval <- p.val
            varexp <- paste0(cn, " (", format(p.val, scientific = T), ")", collapse = " ")
          }else if(infiniteValue>p.val & Infinite){
            res.pval <- p.val
            varexp <- c(varexp, paste0(cn, " (", format(p.val, scientific = T), ")", collapse = " ")) #add all variables
          }
        }else{
          varexp <- c(varexp, format(p.val, scientific = T))
        }
      }

      if(!res_matrix){
        if(Infinite){
          RES_PCA_VARIANCE_COMP[[h]] <- rbind(RES_PCA_VARIANCE_COMP[[h]], c(p, paste0(varexp, collapse = ", ")))
        }else{
          RES_PCA_VARIANCE_COMP[[h]] <- rbind(RES_PCA_VARIANCE_COMP[[h]], c(p, varexp))
        }
      }else{
        RES_PCA_VARIANCE_COMP[[h]] <- rbind(RES_PCA_VARIANCE_COMP[[h]], varexp)
      }

    }
    if(!res_matrix){
      colnames(RES_PCA_VARIANCE_COMP[[h]]) <- c("Component", "Variables (The lower P-value or all lower than 1e-80)")
    }else{
      RES_PCA_VARIANCE_COMP[[h]] <- cbind(colnames(pca.result@scoreMN), RES_PCA_VARIANCE_COMP[[h]])
      colnames(RES_PCA_VARIANCE_COMP[[h]]) <- c("Component", VAR_PCA_STUDY[VAR_PCA_STUDY %in% c(colnames(LST_VAR[[h]]$d.cual), colnames(LST_VAR[[h]]$d.num))])
    }
    rownames(RES_PCA_VARIANCE_COMP[[h]]) <- NULL
  }

  return(RES_PCA_VARIANCE_COMP)
}

#VAR_DDBB
getHighBiasTest <- function(pca_result, res_qual_results = RES_QUAL_RESULTS$Italian, res_num_results = RES_NUM_RESULTS$Italian, comp_variance, bias_var = VAR_DDBB, VAR_DESCRIPTION = NULL, alpha = 0.05, exclude = NULL){

  #get components with higher bias in bias_var
  #comps <- grep(VAR_DDBB, comp_variance[,2]) #2 columns case

  comps <- comp_variance[as.numeric(comp_variance[,bias_var])<=alpha,1,]

  #get loadings for those variables
  sub_loadings <- pca_result@loadingMN[,comps,drop=F]
  sub_loadings <- abs(sub_loadings)

  #test chi2
  lst_cn <- unlist(purrr::map(rownames(sub_loadings), ~getVarNameFromBinary(., VAR_DESCRIPTION)))
  names(lst_cn) <- rownames(sub_loadings)

  lst_cn_aux <- names(lst_cn)
  names(lst_cn_aux) <- lst_cn

  df_qual <- NULL
  for(cn in lst_cn){
    if(cn %in% names(res_qual_results)){
      df_qual <- rbind(df_qual, res_qual_results[[cn]]$df)
    }
  }
  df_qual$p.value <- as.numeric(df_qual$p.value)
  df_qual$adj.p.value <- p.adjust(df_qual$p.value, method="BH")

  df_num <- NULL
  for(cn in lst_cn){
    if(cn %in% names(res_num_results)){
      df_num <- rbind(df_num, res_num_results[[cn]]$df)
    }
  }
  df_num$p.value <- as.numeric(df_num$p.value)
  df_num$adj.p.value <- p.adjust(df_num$p.value, method="BH")

  df_tests <- rbind(df_qual, df_num)
  df_tests$adj.p.value <- p.adjust(df_tests$p.value, method="BH")

  df_tests <- df_tests[order(df_tests$adj.p.value, decreasing = F),]
  df_num <- df_num[order(df_num$adj.p.value, decreasing = F),]
  df_qual <- df_qual[order(df_qual$adj.p.value, decreasing = F),]

  index = NULL
  index_num = NULL
  index_qual = NULL
  for(cn in exclude){
    if(cn %in% df_tests$Variable){
      index <- c(index, which(df_tests$Variable == cn))
    }
    if(cn %in% df_num$Variable){
      index_num <- c(index_num, which(df_num$Variable == cn))
    }
    if(cn %in% df_qual$Variable){
      index_qual <- c(index_qual, which(df_qual$Variable == cn))
    }
  }

  if(length(index)>0){
    df_tests <- df_tests[-index,]
  }

  if(length(index_num)>0){
    df_num <- df_num[-index,]
  }

  if(length(index_qual)>0){
    df_qual <- df_qual[-index,]
  }

  ####
  df_tests <- cbind(df_tests, sub_loadings[lst_cn_aux[df_tests$Variable],,drop=F])
  df_num <- cbind(df_num, sub_loadings[lst_cn_aux[df_num$Variable],,drop=F])
  df_qual <- cbind(df_qual, sub_loadings[lst_cn_aux[df_qual$Variable],,drop=F])

  df_desc <- getVarDescDF(VAR_DESCRIPTION, df_tests$Variable)
  df_tests$Description <- df_desc$Description

  rownames(df_tests) <- NULL
  rownames(df_num) <- NULL
  rownames(df_qual) <- NULL

  #return(list(numerical = df_num, qualitative = df_qual))
  return(df_tests)
}

plotOPLS <- function(pca = pca.result, comp = c(1,2), mode = "scores", factor = NULL, legend.title = NULL, mahalanovis_limit = 20, top = NULL, radius = 0.2, names = F, allNames = F, colorReverse = F, text.size = 2){
  ggp = NULL
  MAX_POINTS = 1000
  POINT_SIZE = 3
  POINT_SIZE_LOAD = 1.5 #another scale
  POINT_RES = c(1024, 1024)

  modes <- c("scores", "loadings", "biplot")
  if(!mode %in% modes){
    stop(paste0("mode must be one of the following: ", paste0(modes, collapse = ", ")))
  }

  if(class(factor)!="factor" & mode %in% c("scores", "biplot")){
    stop("Factor must be a factor object.")
  }

  if(!class(pca)=="opls"){
    stop("pca must be a opls object.")
  }

  if(mode=="scores"){
    if(ncol(pca@scoreMN)==1){
      message("The PCA has only 1 component")

      pca@scoreMN <- cbind(pca@scoreMN,pca@scoreMN)
      colnames(pca@scoreMN) <- c("p1", "p2")
      df <- as.data.frame(pca@scoreMN)

      comp = c(comp[1], 1)
      subdata <- NULL

      f <- as.factor(factor)
      v.colors <- RColorConesa::colorConesa(length(unique(f)))
      levels(f)[match(levels(f),levels(f))] <- v.colors
      color_values <- as.character(f)

      ggp <- ggplot(df)

      if(nrow(df) > MAX_POINTS){
        ggp <- ggp + scattermore::geom_scattermore(aes(x = df[,comp[1]], y = df[,comp[2]], color = factor), pointsize = POINT_SIZE, pixels = POINT_RES)
      }else{
        ggp <- ggp + geom_point(aes(x = df[,comp[1]], y = df[,comp[2]], color = factor))
      }

      ggp <- ggp +
        # scale_color_manual(values = colorConesa(length(unique(factor)), reverse = colorReverse)) +
        RColorConesa::scale_color_conesa(reverse = colorReverse) +
        ggtitle(label = bquote("Scores (PCA) - "~R^2 == .(sum(pca@modelDF[c(comp[1],comp[2]),1])))) +
        xlab(label = paste0("p",as.character(comp[1]), " (", as.character(pca@modelDF$R2X[comp[1]]*100), " %)")) +
        ylab(label = paste0("p",as.character(comp[2]), " (", as.character(pca@modelDF$R2X[comp[2]]*100), " %)")) +
        labs(color = legend.title) + theme(legend.position="bottom")

    }else{
      df <- as.data.frame(pca@scoreMN)
      mh <- mahalanobis(x = pca@scoreMN[,comp], center = F, cov = cov(pca@scoreMN[,comp]))
      subdata <- as.data.frame(pca@scoreMN)[names(mh)[mh>mahalanovis_limit],]

      ggp <- ggplot(df) + stat_ellipse(aes(x = df[,comp[1]], y = df[,comp[2]], fill = factor), geom = "polygon", alpha = 0.1, show.legend=F)

      if(nrow(df) > MAX_POINTS){
        ggp <- ggp + scattermore::geom_scattermore(aes(x = df[,comp[1]], y = df[,comp[2]], color = factor), pointsize = POINT_SIZE, pixels = POINT_RES)
      }else{
        ggp <- ggp + geom_point(aes(x = df[,comp[1]], y = df[,comp[2]], color = factor))
      }

      ggp <- ggp +
        # scale_color_manual(values = colorConesa(length(unique(factor)), reverse = colorReverse)) +
        # scale_fill_manual(values = colorConesa(length(unique(factor)), reverse = colorReverse)) +
        RColorConesa::scale_color_conesa(reverse = colorReverse) +
        RColorConesa::scale_fill_conesa(reverse = colorReverse) +
        coord_fixed(ratio=1) +
        ggtitle(label = bquote("Scores (PCA) - "~R^2 == .(sum(pca@modelDF[c(comp[1],comp[2]),1])))) +
        xlab(label = paste0("p",as.character(comp[1]), " (", as.character(pca@modelDF$R2X[comp[1]]*100), " %)")) +
        ylab(label = paste0("p",as.character(comp[2]), " (", as.character(pca@modelDF$R2X[comp[2]]*100), " %)")) +
        labs(color = legend.title) + theme(legend.position="bottom")

      if(names){
        ggp <- ggp + ggrepel::geom_text_repel(data = subdata, aes(x = subdata[,comp[1]], y = subdata[,comp[2]]), label = rownames(subdata), size=text.size)
      }else if(allNames){
        ggp <- ggp + ggrepel::geom_text_repel(data = as.data.frame(df), aes(x = as.data.frame(df)[,comp[1]], y = as.data.frame(df)[,comp[2]]), label = rownames(as.data.frame(df)), size=text.size)
      }
    }
  }

  if(mode=="loadings"){

    if(ncol(pca@loadingMN)==1){
      message("The PCA has only 1 component")

      pca@loadingMN <- cbind(pca@loadingMN,pca@loadingMN)
      colnames(pca@loadingMN) <- c("p1", "p2")
      df <- as.data.frame(pca@loadingMN)
      mh <- mahalanobis(x = pca@loadingMN[,comp], center = F, cov = cov(pca@loadingMN[,comp]))
      subdata <- df[apply(df[,comp],1,function(x){sqrt(crossprod(x))>radius}),]

      comp = c(comp[1], 1)
      subdata <- NULL

      ggp <- ggplot(df)

      if(nrow(df) > MAX_POINTS){
        ggp <- ggp + scattermore::geom_scattermore(aes(x = df[,comp[1]], y = df[,comp[2]]), pointsize = POINT_SIZE+1, pixels = POINT_RES)
      }else{
        ggp <- ggp + geom_point(aes(x = df[,comp[1]], y = df[,comp[2]]), size = POINT_SIZE_LOAD)
      }

      ggp <- ggp +
        ggtitle(label = bquote("Loadings (PCA) - "~R^2 == .(max(pca@modelDF[,2])))) +
        xlab(label = paste0("p",as.character(comp[1]), " (", as.character(pca@modelDF$R2X[comp[1]]*100), " %)")) +
        ylab(label = paste0("p",as.character(comp[2]), " (", as.character(pca@modelDF$R2X[comp[2]]*100), " %)"))

    }else{
      df <- as.data.frame(pca@loadingMN)
      subdata <- df[apply(df[,comp],1,function(x){sqrt(crossprod(x))>radius}),]
      for(i in 1:nrow(df)){
        if(rownames(df[i,comp,drop=F]) %in% rownames(subdata)){
          next
        }else{
          #check radius
          aux <- df[i,comp,drop=F]
          y_value <- sqrt(radius^2-aux[,1]^2)
          if(y_value < abs(aux[,2])){
            subdata <- rbind(subdata, df[i,,drop=F])
          }
        }
      }

      ggp <- ggplot(as.data.frame(df))

      if(nrow(df) > MAX_POINTS){
        ggp <- ggp + scattermore::geom_scattermore(aes(x = df[,comp[1]], y = df[,comp[2]]), pointsize = POINT_SIZE, pixels = POINT_RES)
      }else{
        ggp <- ggp + geom_point(aes(x = df[,comp[1]], y = df[,comp[2]]))
      }

      ggp <- ggp +
        coord_fixed(ratio=1) +
        ggtitle(label = bquote("Loadings (PCA) - "~R^2 == .(max(pca@modelDF[,2])))) +
        xlab(label = paste0("p",as.character(comp[1]), " (", as.character(pca@modelDF$R2X[comp[1]]*100), " %)")) +
        ylab(label = paste0("p",as.character(comp[2]), " (", as.character(pca@modelDF$R2X[comp[2]]*100), " %)"))
    }

    if(allNames){
      ggp <- ggp + ggrepel::geom_text_repel(data = as.data.frame(df), aes(x = as.data.frame(df)[,comp[1]], y = as.data.frame(df)[,comp[2]]), label = rownames(as.data.frame(df)), size=text.size)
    }else if(names){
      ggp <- ggp + ggrepel::geom_text_repel(data = subdata, aes(x = subdata[,comp[1]], y = subdata[,comp[2]]), label = rownames(subdata), size=text.size)
    }

    if(!is.null(radius) & nrow(subdata)!=0 & nrow(df) < MAX_POINTS){
      ggp <- ggp + ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = radius))
    }

  }

  if(mode=="biplot"){
    df <- as.data.frame(pca@scoreMN)
    df <- norm01(df[,comp])*2-1 #to -1,1
    mh <- mahalanobis(x = pca@scoreMN[,comp], center = F, cov = cov(pca@scoreMN[,comp]))
    subdata <- as.data.frame(pca@scoreMN)[names(mh)[mh>mahalanovis_limit],]

    ggp <- ggplot(as.data.frame(df)) + stat_ellipse(aes(x = df[,1], y = df[,2], fill = factor), geom = "polygon", alpha = 0.1, show.legend=F)

    if(nrow(df) > MAX_POINTS){
      ggp <- ggp + scattermore::geom_scattermore(aes(x = df[,1], y = df[,2], color = factor), pointsize = POINT_SIZE, pixels = c(1024,1024))
    }else{
      ggp <- ggp + geom_point(aes(x = df[,1], y = df[,2], color = factor))
    }

    ggp <- ggp +
      # scale_color_manual(values = colorConesa(length(unique(factor)), reverse = colorReverse)) +
      # scale_fill_manual(values = colorConesa(length(unique(factor)), reverse = colorReverse)) +
      RColorConesa::scale_color_conesa(reverse = colorReverse) +
      RColorConesa::scale_fill_conesa(reverse = colorReverse) +
      coord_fixed(ratio=1) +
      ggtitle(label = bquote("Biplot (PCA) - "~R^2 == .(sum(pca@modelDF[c(comp[1],comp[2]),1])))) +
      xlab(label = paste0("p",as.character(comp[1]), " (", as.character(pca@modelDF$R2X[comp[1]]*100), " %)")) +
      ylab(label = paste0("p",as.character(comp[2]), " (", as.character(pca@modelDF$R2X[comp[2]]*100), " %)")) +
      labs(color = legend.title) + theme(legend.position="bottom") + guides(shape="none")

    df_loading <- as.data.frame(pca@loadingMN)
    #df_loading <- norm01(df_loading[,comp]) * 2 -1 #loadings already in -1,1 scale
    max.loadings <- apply(abs(df_loading), 2, max)
    max.scores <- apply(abs(df), 2, max)

    # ratio <- max.scores / max.loadings
    # df_loading <- as.data.frame(t(apply(df_loading, 1, function(x){x * ratio})))

    if(nrow(df_loading)<15){
      subdata_loading <- df_loading
    }else if(!is.null(top)){
      aux_loadings <- apply(df_loading,1,function(x){sqrt(crossprod(as.numeric(x[comp])))})
      aux_loadings <- aux_loadings[order(aux_loadings, decreasing = T)]
      subdata_loading <- df_loading[names(aux_loadings)[1:top],]
    }else{
      subdata_loading <- df_loading[apply(df_loading,1,function(x){sqrt(crossprod(as.numeric(x[comp])))>radius}),]
    }

    if(nrow(subdata_loading)==0){
      top = 10 #select top 10

      aux_loadings <- apply(df_loading,1,function(x){sqrt(crossprod(as.numeric(x[comp])))})
      aux_loadings <- aux_loadings[order(aux_loadings, decreasing = T)]
      subdata_loading <- df_loading[names(aux_loadings)[1:top],]
    }


    #depending on DF instead of df_loadings - POINTS
    # if(nrow(df) > MAX_POINTS){
    #   ggp <- ggp + scattermore::geom_scattermore(data = df_loading, aes(x = df_loading[,1], y = df_loading[,2]), pointsize = POINT_SIZE+1.5, pixels = POINT_RES)
    # }else{
    #   ggp <- ggp + geom_point(data = df_loading, aes(x = df_loading[,1], y = df_loading[,2]), size = POINT_SIZE_LOAD)
    # }

    #depending on DF instead of df_loadings - ARROWS
    no_selected_loadings <- df_loading[!rownames(df_loading) %in% rownames(subdata_loading),]
    if(nrow(no_selected_loadings)!=0){
      ggp <- ggp + geom_segment(data = no_selected_loadings, lineend = "butt", linejoin = "mitre", size = 0.2,
                                aes(x = 0, y = 0, xend = no_selected_loadings[,comp[1]], yend = no_selected_loadings[,comp[2]]), arrow = arrow(length = unit(0.1, "cm")))
    }

    ggp <- ggp + geom_segment(data = subdata_loading, lineend = "butt", linejoin = "mitre", size = 0.33,
                                aes(x = 0, y = 0, xend = subdata_loading[,comp[1]], yend = subdata_loading[,comp[2]]), arrow = arrow(length = unit(0.1, "cm")))

    if(allNames){
      ggp <- ggp + ggrepel::geom_text_repel(data = as.data.frame(df_loading), aes(x = as.data.frame(df_loading)[,comp[1]],
                                                                                  y = as.data.frame(df_loading)[,comp[2]]),
                                            label = rownames(as.data.frame(df_loading)), size=text.size, segment.size = 0.2)
    }else if(names){
      ggp <- ggp + ggrepel::geom_text_repel(data = subdata_loading, aes(x = subdata_loading[,comp[1]],
                                                                        y = subdata_loading[,comp[2]]),
                                            label = rownames(subdata_loading), size=text.size, segment.size = 0.2)
    }

    if(is.null(top) &!is.null(radius) & nrow(df) < MAX_POINTS){
      #ggp <- ggp + ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = radius * max(ratio[comp[1]])))
      ggp <- ggp + ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = radius))
    }

  }

  if(!is.null(factor) & length(levels(factor))>3){
    ggp <- ggp + guides(color=guide_legend(nrow = ceiling(length(levels(factor))/3), byrow = T))
  }

  return(list(plot = ggp, outliers = rownames(subdata)))
}