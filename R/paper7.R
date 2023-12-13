library(dplyr)
library(data.table)
library(tictoc)
library(rtauargus)
library(sdcHierarchies)
options(
  rtauargus.tauargus_exe =
    "Y:/Logiciels/TauArgus/TauArgus4.2.3/TauArgus.exe"
)

compare_suppression_5vars <- function(
    nX1 = 2, nX2 = 0, nX3 = 0,
    nY1 = 2, nY2 = 0, nY3 = 0,
    nZ1 = 2, nZ2 = 0, nZ3 = 0,
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
  Z = make_X(nZ1, nZ2, nZ3, "Z")
  min_n_mod <- min(c(length(X), length(Y), length(Z)))
  
  data1 <- expand.grid(
    V1 = 1:(min_n_mod+1),
    V2 = 1:(min_n_mod+1),
    X = X$vec,
    Y = Y$vec,
    Z = Z$vec,
    stringsAsFactors = FALSE
  ) %>% 
    as_tibble() %>% 
    mutate(FREQ = ceiling(runif(n(), 1,100)))
  
  data1 <- tabulate_micro_data(
    data1, cat_vars = names(data1)[1:5], resp_var = "FREQ", marge_label = "Total"
  )[,.SD, .SDcols = c(names(data1)[1:5], "FREQ_tot")]
  
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
  
  
 
  time_modular_split <- system.time({
    res_modular_split <- tab_rtauargus(
      tabular = data1,
      dir_name = file.path(dir_name, "modular_split"),
      explanatory_vars = c("V1", "V2", "X", "Y", "Z"),
      hrc = c(X = X$hrc, Y = Y$hrc, Z = Z$hrc),
      totcode = rep("Total", 5),
      secret_var = "is_secret_prim",
      value = "FREQ",
      freq = "FREQ",
      split_tab = TRUE,
      nb_tab_option = "min",
      verbose = FALSE
    ) %>% 
      rename(is_secret_final=last_col()) %>% 
      mutate(Status = ifelse(is_secret_prim, "A", ifelse(is_secret_final, "D", "V")))
    stats_modular_split <- compute_stats_direct(res_modular_split)
  })
  
  tic()
  time_hypercube_direct <- system.time({
    res_hypercube_direct <- tab_rtauargus(
      tabular = data1,
      dir_name = file.path(dir_name, "hypercube_direct"),
      explanatory_vars = c("V1", "V2", "X", "Y", "Z"),
      hrc = c(X = X$hrc, Y = Y$hrc, Z = Z$hrc),
      totcode = rep("Total", 5),
      secret_var = "is_secret_prim",
      value = "FREQ",
      freq = "FREQ",
      split_tab = FALSE,
      suppress = "GH(1,10)"
    )
    stats_hypercube_direct <- compute_stats_direct(res_hypercube_direct)
  })
  
  
  time_hypercube_split <- system.time({
    res_hypercube_split <- tab_rtauargus(
      tabular = data1,
      dir_name = file.path(dir_name, "modular_split"),
      explanatory_vars = c("V1", "V2", "X", "Y", "Z"),
      hrc = c(X = X$hrc, Y = Y$hrc, Z = Z$hrc),
      totcode = rep("Total", 5),
      secret_var = "is_secret_prim",
      value = "FREQ",
      freq = "FREQ",
      split_tab = TRUE,
      nb_tab_option = "min",
      verbose = FALSE,
      suppress = "GH(1,10)"
    ) %>% 
      rename(is_secret_final=last_col()) %>% 
      mutate(Status = ifelse(is_secret_prim, "A", ifelse(is_secret_final, "D", "V")))
    stats_hypercube_split <- compute_stats_direct(res_hypercube_split)
  })
  
  nb_nodes_X <- length((X$hier %>% hier_convert(as = "sdc"))$dims) #unique(unlist(lapply((X$hier %>% hier_convert(as = "sdc"))$dims, names))) |> length()
  nb_nodes_Y <- length((Y$hier %>% hier_convert(as = "sdc"))$dims) #unique(unlist(lapply((Y$hier %>% hier_convert(as = "sdc"))$dims, names))) |> length()
  nb_nodes_Z <- length((Z$hier %>% hier_convert(as = "sdc"))$dims) #unique(unlist(lapply((Y$hier %>% hier_convert(as = "sdc"))$dims, names))) |> length()
  
  res_compute <- data.frame(
    method = c("1-Direct Modular", "2-Split Modular", "3-Direct Hypercube", "4-Split Hypercube"),
    nb_nodes_X = nb_nodes_X,
    nb_nodes_Y = nb_nodes_Y,
    nb_rows = nrow(data1),
    nb_linked_tables = c(1, 4, 1, 4),
    pc_n_prim = c(NA, stats_modular_split[Status == "A", pc_n], stats_hypercube_direct[Status == "A", pc_n], stats_hypercube_split[Status == "A", pc_n]),
    pc_val_prim = c(NA, stats_modular_split[Status == "A", pc_val], stats_hypercube_direct[Status == "A", pc_val], stats_hypercube_split[Status == "A", pc_val]),
    pc_n_secondary = c(NA, stats_modular_split[Status == "D", pc_n], stats_hypercube_direct[Status == "D", pc_n], stats_hypercube_split[Status == "D", pc_n]),
    pc_val_secondary = c(NA, stats_modular_split[Status == "D", pc_val], stats_hypercube_direct[Status == "D", pc_val], stats_hypercube_split[Status == "D", pc_val]),
    time_computation = c(NA, time_modular_split[3], time_hypercube_direct[3], time_hypercube_split[3])
  )
  
  return(res_compute)
}


possCompare_5vars <- purrr::possibly(.f = compare_suppression_5vars, otherwise = NULL)



