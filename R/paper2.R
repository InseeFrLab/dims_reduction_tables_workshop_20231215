
library(dplyr)
library(rtauargus)
library(sdcHierarchies)

# Construction of the fake data
h <- hier_create(root = "Total", nodes = c("R1","R2"))
h <- hier_add(h, root = "R1", nodes = c("D11", "D12"))
h <- hier_add(h, root = "R2", nodes = c("D21", "D22"))
hier_display(h)

h_graph <- h |> rename(from = root, to = leaf) |> graph_from_data_frame()

act <- hier_create(root = "Total", nodes = c("A","B","C"))
act <- hier_add(act, root = "A", nodes = c("A1", "A2"))
act <- hier_add(act, root = "B", nodes = c("B1", "B2"))
act <- hier_add(act, root = "C", nodes = c("C1", "C2"))
act <- hier_add(act, root = "C1", nodes = c("C21", "C22"))

act_graph <- act |> rename(from = root, to = leaf) |> graph_from_data_frame()

data2 <- expand.grid(
  V1 = c("Total", 1:5), V2 = c("Total", 1:2), 
  GEO = unique(c(h$root, h$leaf)), 
  ACT = unique(c(act$root, act$leaf)),
  stringsAsFactors = FALSE) |> 
  as.data.frame() |> 
  mutate(FREQ = sample(1:1000, n(), replace = TRUE))

hier_convert(act, as = "argus") |> 
  slice(-1) |> 
  mutate(level = substring(paste0(level, name),3)) |> 
  select(level) |> 
  write.table(file = "act.hrc", quote = FALSE, row.names = FALSE, col.names = FALSE)

hier_convert(h, as = "argus") |> 
  slice(-1) |> 
  mutate(level = substring(paste0(level, name),3)) |> 
  select(level) |> 
  write.table(file = "geo.hrc", quote = FALSE, row.names = FALSE, col.names = FALSE)

# Call to the function that merge and split the table
res <- from_4_to_3(
  dfs = data2,
  dfs_name = "data2",
  totcode = c(V1 = "Total", V2 = "Total", GEO = "Total", ACT = "Total"),
  hrcfiles = c(GEO = "geo.hrc", ACT = "act.hrc"),
  v1 = "GEO",
  v2 = "ACT"
)