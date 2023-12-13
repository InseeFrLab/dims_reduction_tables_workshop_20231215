library(dplyr)

load("res_simulations_paper8.RData")
load("res_simulations2_paper8.RData")
load("res_simulations5_paper8.RData")


res_simulations %>% 
  group_by(method) %>% 
  summarise(
    across(nb_nodes_X:time_computation, mean),
    .groups = "drop"
  ) %>% 
  select(1,5:10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    digits = 1
  )

res_simulations %>% 
  filter(method != "1-Direct Modular") %>% 
  group_by(method) %>% 
  summarise(
    across(nb_nodes_X:time_computation, 
           \(x) paste0("[",round(quantile(x, probs = 0.05),1),"; ",
                       round(quantile(x, probs = 0.95),1),"]")),
    .groups = "drop"
  ) %>% 
  select(1,6:10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    digits = 1
  )

res_simulations2 %>% 
  group_by(method) %>% 
  summarise(
    across(nb_nodes_X:time_computation, mean),
    .groups = "drop"
  ) %>% 
  select(1,5:10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    digits = 1
  )

res_simulations2 %>% 
  filter(method != "1-Direct Modular") %>% 
  group_by(method) %>% 
  summarise(
    across(nb_nodes_X:time_computation, 
           \(x) paste0("[",round(quantile(x, probs = 0.05),1),"; ",
                       round(quantile(x, probs = 0.95),1),"]")),
    .groups = "drop"
  ) %>% 
  select(1,6:10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    digits = 1
  )

res_simulations5 %>% 
  group_by(method) %>% 
  summarise(
    across(nb_nodes_X:time_computation, mean),
    .groups = "drop"
  ) %>% 
  select(1,5:10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    digits = 1
  )

res_simulations5 %>% 
  filter(method != "1-Direct Modular") %>% 
  group_by(method) %>% 
  summarise(
    across(nb_nodes_X:time_computation, 
           \(x) paste0("[",round(quantile(x, probs = 0.05),1),"; ",
                       round(quantile(x, probs = 0.95),1),"]")),
    .groups = "drop"
  ) %>% 
  select(1,6:10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    digits = 1
  )


library(ggplot2)

res_simulations %>% select(method, pc_val_secondary) %>% mutate(prim_thq = 10) %>% 
  bind_rows(
    res_simulations2 %>% select(method, pc_val_secondary) %>% mutate(prim_thq = 20)) %>%
  bind_rows(
    res_simulations5 %>% select(method, pc_val_secondary) %>% mutate(prim_thq = 50)) %>% 
  filter(substr(method, 1,1) %in% c(2:4)) %>% 
  ggplot() +
  geom_boxplot(aes(x = pc_val_secondary, y = factor(method), fill = factor(prim_thq)), size = 0.25) +
  labs(y="", x="% of secondary suppression") +
  scale_fill_brewer("% of primary cells", type = "qual", palette = 6) +
  theme_gray(base_size = 8)

ggsave(filename = "boxplot_sims5dims_allmeth.pdf", device = "pdf", units = "cm", width = 20, height = 8)

res_simulations %>% select(method, pc_val_secondary) %>% mutate(prim_thq = 10) %>% 
  bind_rows(
    res_simulations2 %>% select(method, pc_val_secondary) %>% mutate(prim_thq = 20)) %>%
  bind_rows(
    res_simulations5 %>% select(method, pc_val_secondary) %>% mutate(prim_thq = 50)) %>% 
  filter(substr(method, 1,1) %in% c(2,4)) %>% 
  ggplot() +
  geom_boxplot(aes(x = pc_val_secondary, y = factor(method), fill = factor(prim_thq)), size = 0.25) +
  labs(y="", x="% of secondary suppression") +
  scale_fill_brewer("% of primary cells", type = "qual", palette = 6) +
  theme_gray(base_size = 8)

ggsave(filename = "boxplot_sims4dims_nodirecthyper.pdf", device = "pdf", units = "cm", width = 20, height = 8)
