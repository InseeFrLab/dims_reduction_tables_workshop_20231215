
library(dplyr)
library(rtauargus)
# Modify the location
options(
  rtauargus.tauargus_exe =
    "Y:/Logiciels/TauArgus/TauArgus4.2.3/TauArgus.exe"
)
set.seed(1234)

data1 <- expand.grid(
  EDU = c(LETTERS[1:4]),
  OCC = c(LETTERS[1:3]),
  GEN = c("F","M"),
  AGE = c("Child","Adult"),
  stringsAsFactors = FALSE
) %>% 
  as_tibble() %>% 
  mutate(FREQ = ceiling(runif(n(), 1,100)))

data1 <- tabulate_micro_data(
  data1, cat_vars = names(data1)[1:4], resp_var = "FREQ", marge_label = "ALL"
)[,.SD, .SDcols = c(names(data1)[1:4], "FREQ_tot")]

data.table::setnames(data1, "FREQ_tot", "FREQ")
str(data1)

data1[order(EDU, OCC, GEN, AGE), ][1:9,] %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE
  )

data1_step1 <- data1[order(EDU, OCC, GEN, AGE), ][1:9,][, GEN_AGE := paste0(GEN, "_", AGE)][
  , .(EDU, OCC, GEN_AGE, FREQ)
]

data1_step1 %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE
  )

hier1 <- sdcHierarchies::hier_create(root = "ALL_ALL", nodes = c("ALL_Adult", "ALL_Child"))
hier1 <- sdcHierarchies::hier_add(hier1, root = "ALL_Child", nodes = c("F_Child", "M_Child"))
hier1 <- sdcHierarchies::hier_add(hier1, root = "ALL_Adult", nodes = c("F_Adult", "M_Adult"))
sdcHierarchies::hier_display(hier1)

hier2 <- sdcHierarchies::hier_create(root = "ALL_ALL", nodes = c("F_ALL", "M_ALL"))
hier2 <- sdcHierarchies::hier_add(hier2, root = "F_ALL", nodes = c("F_Child", "F_Adult"))
hier2 <- sdcHierarchies::hier_add(hier2, root = "M_ALL", nodes = c("M_Child", "M_Adult"))
sdcHierarchies::hier_display(hier2)

library(ggraph)
library(igraph)

all_modalities <- data.frame(
  name = unique(data1_step1$GEN_AGE)
) %>% 
  mutate(
    commune  = ! name %in% c("ALL_Child","ALL_Adult","F_ALL", "M_ALL")
  )


hier1_graph <- hier1 %>% 
  rename(from = root, to = leaf) %>% 
  graph_from_data_frame(vertices = all_modalities %>% filter(name %in% c(hier1$root, hier1$leaf)))

str(hier1_graph)

hier2_graph <- hier2 %>% 
  rename(from = root, to = leaf) %>% 
  graph_from_data_frame(vertices = all_modalities %>% filter(name %in% c(hier2$root, hier2$leaf)))

ggraph(hier1_graph, 'igraph', algorithm = 'tree') + 
  geom_edge_link(label_parse = TRUE) +
  # geom_node_point(size = 10, colour = 'orangered') +  
  geom_node_label(aes(label = name, fill = commune), color = 'Black', size = 5) +
  guides(fill = "none")
ggsave(filename = "hier1.pdf", device = "pdf", units = "cm", width = 20, height = 15)

ggraph(hier2_graph, 'igraph', algorithm = 'tree') + 
  geom_edge_link(label_parse = TRUE) +
  # geom_node_point(size = 10, colour = 'orangered') +  
  geom_node_label(aes(label = name, fill = commune), color = 'Black', size = 5) +
  guides(fill = "none")
ggsave(filename = "hier2.pdf", device = "pdf", units = "cm", width = 20, height = 15)

data1_step1 %>% 
  filter(GEN_AGE %in% c(hier1$root, hier1$leaf)) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE
  )

data1_step1 %>% 
  filter(GEN_AGE %in% c(hier2$root, hier2$leaf)) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE
  )


library(sdcHierarchies)
h <- hier_create(root = "Total", nodes = c("R1","R2"))
h <- hier_add(h, root = "R1", nodes = c("D11", "D12"))
h <- hier_add(h, root = "R2", nodes = c("D21", "D22"))
hier_display(h)

h_graph <- h %>% 
  rename(from = root, to = leaf) %>% 
  graph_from_data_frame()

ggraph(h_graph, 'igraph', algorithm = 'tree') + 
  geom_edge_link(label_parse = TRUE) +
  # geom_node_point(size = 10, colour = 'orangered') +  
  geom_node_label(aes(label = name), fill = "forestgreen", color = 'Black', size = 5) +
  guides(fill = "none")
ggsave(filename = "geo.pdf", device = "pdf", units = "cm", width = 20, height = 15)


act <- hier_create(root = "Total", nodes = c("A","B","C"))
act <- hier_add(act, root = "A", nodes = c("A1", "A2"))
act <- hier_add(act, root = "B", nodes = c("B1", "B2"))
act <- hier_add(act, root = "C", nodes = c("C1", "C2"))
act <- hier_add(act, root = "C1", nodes = c("C21", "C22"))
hier_display(act)


act_graph <- act %>% 
  rename(from = root, to = leaf) %>% 
  graph_from_data_frame()

ggraph(act_graph, 'igraph', algorithm = 'tree') + 
  geom_edge_link(label_parse = TRUE) +
  # geom_node_point(size = 10, colour = 'orangered') +  
  geom_node_label(aes(label = name), fill = "forestgreen", color = 'Black', size = 5) +
  guides(fill = "none")
ggsave(filename = "act.pdf", device = "pdf", units = "cm", width = 20, height = 15)


hier_convert(act, as = "argus") %>% 
  slice(-1) %>% 
  mutate(level = substring(paste0(level, name),3)) %>% 
  select(level) %>% 
  write.table(file = "act.hrc", quote = FALSE, row.names = FALSE, col.names = FALSE)

hier_convert(h, as = "argus") %>% 
  slice(-1) %>% 
  mutate(level = substring(paste0(level, name),3)) %>% 
  select(level) %>% 
  write.table(file = "geo.hrc", quote = FALSE, row.names = FALSE, col.names = FALSE)


data2 <- expand.grid(V1 = c("Total", 1:5), V2 = c("Total", 1:2), 
                     GEO = unique(c(h$root, h$leaf)), 
                     ACT = unique(c(act$root, act$leaf)),
                     stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% 
  mutate(FREQ = sample(1:1000, n(), replace = TRUE))


res <- from_4_to_3(
    dfs = data2,
    dfs_name = "data2",
    totcode = c(V1 = "Total", V2 = "Total", GEO = "Total", ACT = "Total"),
    hrcfiles = c(GEO = "geo.hrc", ACT = "act.hrc"),
    sep_dir = TRUE,
    hrc_dir = "hrc_data2",
    sep = "_",
    v1 = "GEO",
    v2 = "ACT",
    maximize_nb_tabs = FALSE
)

res$hrcs %>% 
  purrr::iwalk(
    function(file, name){
      hier <- hier_import(inp = file, from = "hrc", root = "Total")
      hier %>%
      ggraph('igraph', algorithm = 'tree') + 
        geom_edge_link(label_parse = TRUE) +
        geom_node_label(aes(label = name), fill = "forestgreen", color = 'Black', size = 5) +
        guides(fill = "none")
      ggsave(filename = paste0("hrc_data2/", name,".pdf"), device = "pdf", units = "cm", width = 20, height = 15)
    }
  )
