library(dplyr)
library(data.table)
library(tictoc)
library(rtauargus)
library(sdcHierarchies)
options(
  rtauargus.tauargus_exe =
    "Y:/Logiciels/TauArgus/TauArgus4.2.3/TauArgus.exe"
)

compare_suppression <- function(
    nX1 = 2, nX2 = 0, nX3 = 0,
    nY1 = 2, nY2 = 0, nY3 = 0,
    part_prim_theoriq = 0.1,
    dir_name = ""
){
  
  if(!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)
  
  make_X <- function(n1, n2, n3, nom){
    X1 = LETTERS[seq_len(n1)]
    X2 = if(n2 > 0) paste0(sort(rep(X1, n2)), seq_len(n2)) else NULL
    X3 = if(n3 > 0) paste0(sort(rep(X2, n3)), seq_len(n3)) else NULL
    X = c(X1,X2,X3)
    
    hier1 <- sdcHierarchies::hier_create(root = "Total", nodes = X1)
    if(n2 > 0){
      for(r in X1){
        hier1 <- sdcHierarchies::hier_add(hier1, root = r, nodes = X2[substr(X2,1,1) == r])
      }
      if(n3 > 0){
        for(r in X2){
          hier1 <- sdcHierarchies::hier_add(hier1, root = r, nodes = X3[substr(X3,1,2) == r])
        }
      }
    }
    
    hier_convert(hier1, as = "argus") |> 
      slice(-1) |> 
      mutate(level = substring(paste0(level, name),3)) |> 
      select(level) |> 
      write.table(file = file.path(dir_name, paste0(nom,".hrc")), quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    return(list(vec = X, hier = hier1, hrc = file.path(dir_name, paste0(nom,".hrc"))))
  }
  
  X = make_X(nX1, nX2, nX3, "X")
  Y = make_X(nY1, nY2, nY3, "Y")
  min_n_mod <- min(c(length(X), length(Y)))
  
  data1 <- expand.grid(
    V1 = 1:(min_n_mod+1),
    V2 = 1:(min_n_mod+1),
    X = X$vec,
    Y = Y$vec,
    stringsAsFactors = FALSE
  ) %>% 
    as_tibble() %>% 
    mutate(FREQ = ceiling(runif(n(), 1,100)))
  
  data1 <- tabulate_micro_data(
    data1, cat_vars = names(data1)[1:4], resp_var = "FREQ", marge_label = "Total"
  )[,.SD, .SDcols = c(names(data1)[1:4], "FREQ_tot")]
  
  data.table::setnames(data1, "FREQ_tot", "FREQ")
  str(data1)
  
  data1[, is_secret_prim := FREQ <= quantile(data1$FREQ, probs = part_prim_theoriq)]
  count(data1, is_secret_prim)
  
  if(sum(data1$is_secret_prim) == 0){
    print("pas de secret primaire")
    return(NULL)
  }
  
  compute_stats_direct <- function(res){
    
    res1 <- res
    
    setDT(res1)
    res1[
      , `:=`(Status = ifelse(is_secret_prim, "A", Status))
    ]
    
    stats <- res1[
      ,
      .(N = .N, val = sum(FREQ)), 
      by = Status
    ][, `:=`(pc_n = N/sum(N)*100, pc_val = val/sum(val)*100)][]
    
    return(stats)
  }
  
  
  split_and_protect <- function(
    tabular,
    explanatory_vars,
    dir_name,
    totcode,
    hrc = NULL,
    hrc_dir = NULL,
    suppress = "MOD(1,5,1,0,0)"
  ){
    
    list_tables <- reduce_dims(
      dfs = tabular,
      dfs_name = "TAB",
      totcode = totcode,
      hrcfiles = hrc,
      hrc_dir = hrc_dir,
      vars_to_merge = c("X", "Y"),
      over_split = FALSE,
      verbose = TRUE, # to generalize later
      sep_dir = TRUE
    )
    
    params_multi <- formals(fun = "tab_multi_manager")
    params_multi <- params_multi[1:(length(params_multi)-1)]
    # call <- sys.call(); call[[1]] <- as.name('list')
    # new_params <- eval.parent(call)
    
    # for(param in intersect(names(params_multi), names(new_params))){
    #   params_multi[[param]] <- new_params[[param]]
    # }
    
    params_multi$list_tables = list_tables$tabs
    params_multi$list_explanatory_vars = list_tables$vars
    params_multi$hrc = list_tables$hrc
    params_multi$totcode = list_tables$totcode
    params_multi$alt_hrc = list_tables$alt_hrc
    params_multi$alt_totcode = list_tables$alt_totcode
    params_multi$value <- params_multi$freq <- "FREQ"
    params_multi$secret_var <- "is_secret_prim"
    params_multi$suppress <- suppress
    params_multi$dir_name <- dir_name
    
    masq_list <- do.call("tab_multi_manager", params_multi)
    
    result <- restore_format(masq_list, list_tables)
    
    setDT(result)
    result <- result %>% 
      rename(is_secret_final=last_col())
    
    result[
      , `:=`(Status = ifelse(is_secret_prim, "A", ifelse(is_secret_final, "D", "V")))
    ]
    
    stats <- result[
      ,
      .(N = .N, val = sum(FREQ)), 
      by = Status
    ][, `:=`(pc_n = N/sum(N)*100, pc_val = val/sum(val)*100)][]
    
    
    return(list(masq= result, stats = stats, nb_tabs = length(list_tables$tabs)))
  }
  
  
  time_modular_direct <- system.time({
    res_modular_direct <- tab_rtauargus(
      tabular = data1,
      dir_name = file.path(dir_name, "modular_direct"),
      explanatory_vars = c("V1", "V2", "X", "Y"),
      hrc = c(X = X$hrc, Y = Y$hrc),
      totcode = rep("Total", 4),
      secret_var = "is_secret_prim",
      value = "FREQ",
      freq = "FREQ",
      split_tab = FALSE,
      verbose = FALSE
    )
    stats_modular_direct <- compute_stats_direct(res_modular_direct)
  })
  
  
  tic()
  time_modular_split <- system.time({
    res_modular_split <- split_and_protect(
      tabular = data1,
      dir_name = file.path(dir_name, "modular_split"),
      hrc_dir = file.path(dir_name, "modular_split", "hrc"),
      explanatory_vars = c("V1", "V2", "X", "Y"),
      hrc = c(X = X$hrc, Y = Y$hrc),
      totcode = c(V1 = "Total", V2 = "Total", X = "Total", Y = "Total")
    )
  })
  
  
  tic()
  time_hypercube_direct <- system.time({
    res_hypercube_direct <- tab_rtauargus(
      tabular = data1,
      dir_name = file.path(dir_name, "hypercube_direct"),
      explanatory_vars = c("V1", "V2", "X", "Y"),
      hrc = c(X = X$hrc, Y = Y$hrc),
      totcode = rep("Total", 4),
      secret_var = "is_secret_prim",
      value = "FREQ",
      freq = "FREQ",
      split_tab = FALSE,
      suppress = "GH(1,10)"
    )
    stats_hypercube_direct <- compute_stats_direct(res_hypercube_direct)
  })
  
  
  time_hypercube_split <- system.time({
    res_hypercube_split <- split_and_protect(
      tabular = data1,
      dir_name = file.path(dir_name, "hypercube_split"),
      explanatory_vars = c("V1", "V2", "X", "Y"),
      hrc = c(X = X$hrc, Y = Y$hrc),
      hrc_dir = file.path(dir_name, "hypercube_split", "hrc"),
      totcode = c(V1 = "Total", V2 = "Total", X = "Total", Y = "Total"),
      suppress = "GH(1,10)"
    )
  })
  
  nb_nodes_X <- length((X$hier %>% hier_convert(as = "sdc"))$dims) #unique(unlist(lapply((X$hier %>% hier_convert(as = "sdc"))$dims, names))) |> length()
  nb_nodes_Y <- length((Y$hier %>% hier_convert(as = "sdc"))$dims) #unique(unlist(lapply((Y$hier %>% hier_convert(as = "sdc"))$dims, names))) |> length()
  
  res_compute <- data.frame(
    method = c("1-Direct Modular", "2-Split Modular", "3-Direct Hypercube", "4-Split Hypercube"),
    nb_nodes_X = nb_nodes_X,
    nb_nodes_Y = nb_nodes_Y,
    nb_rows = nrow(data1),
    nb_linked_tables = c(1, res_modular_split$nb_tabs, 1,  res_hypercube_split$nb_tabs),
    pc_n_prim = c(stats_modular_direct[Status == "A", pc_n], res_modular_split$stats[Status == "A", pc_n], stats_hypercube_direct[Status == "A", pc_n], res_hypercube_split$stats[Status == "A", pc_n]),
    pc_val_prim = c(stats_modular_direct[Status == "A", pc_val], res_modular_split$stats[Status == "A", pc_val], stats_hypercube_direct[Status == "A", pc_val], res_hypercube_split$stats[Status == "A", pc_val]),
    pc_n_secondary = c(stats_modular_direct[Status == "D", pc_n], res_modular_split$stats[Status == "D", pc_n], stats_hypercube_direct[Status == "D", pc_n], res_hypercube_split$stats[Status == "D", pc_n]),
    pc_val_secondary = c(stats_modular_direct[Status == "D", pc_val], res_modular_split$stats[Status == "D", pc_val], stats_hypercube_direct[Status == "D", pc_val], res_hypercube_split$stats[Status == "D", pc_val]),
    time_computation = c(time_modular_direct[3], time_modular_split[3], time_hypercube_direct[3], time_hypercube_split[3])
  )
  
  return(res_compute)
}


possCompare <- purrr::possibly(.f = compare_suppression, otherwise = NULL)



