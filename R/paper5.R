source(file = "paper3.R")

set.seed(1234)
n_sim = 100

res_simulations <- purrr::map_dfr(
  seq_len(n_sim), 
  \(i){
    possCompare(
      nX1 = 2, nX2 = 2, nX3 = 0,
      nY1 = 2, nY2 = 3, nY3 = 0,
      part_prim_theoriq = 0.1,
      dir_name = "sims_paper5"
    )
  })

res_simulations %>% 
  group_by(method) %>% 
  summarise(
    across(nb_nodes_X:time_computation, mean),
    .groups = "drop"
  )

save(res_simulations, file = "res_simulations_paper5.RData")

lapply(list.files(
  path = "sims_paper5/", 
  pattern = "*", 
  recursive = TRUE,
  full.names = TRUE),
  file.remove
)

res_simulations2 <- purrr::map_dfr(
  seq_len(n_sim), 
  \(i){
    possCompare(
      nX1 = 2, nX2 = 2, nX3 = 0,
      nY1 = 2, nY2 = 3, nY3 = 0,
      part_prim_theoriq = 0.2,
      dir_name = "sims_paper5"
    )
  })

res_simulations2 %>% 
  group_by(method) %>% 
  summarise(
    across(nb_nodes_X:time_computation, mean),
    .groups = "drop"
  )


save(res_simulations2, file = "res_simulations2_paper5.RData")

lapply(list.files(
  path = "sims_paper5/", 
  pattern = "*", 
  recursive = TRUE,
  full.names = TRUE),
  file.remove
)

res_simulations5 <- purrr::map_dfr(
  seq_len(n_sim), 
  \(i){
    possCompare(
      nX1 = 2, nX2 = 2, nX3 = 0,
      nY1 = 2, nY2 = 3, nY3 = 0,
      part_prim_theoriq = 0.5,
      dir_name = "sims_paper5"
    )
  })

res_simulations5 %>% 
  group_by(method) %>% 
  summarise(
    across(nb_nodes_X:time_computation, mean),
    .groups = "drop"
  )


save(res_simulations5, file = "res_simulations5_paper5.RData")
