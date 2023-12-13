library(dplyr)
library(rtauargus)
options(
  rtauargus.tauargus_exe =
    "Y:/Logiciels/TauArgus/TauArgus4.2.3/TauArgus.exe"
)
set.seed(1234)

data1 <- expand.grid(
  GEO = paste0("R",1:10),
  EDU = c(LETTERS[1:4]),
  OCC = c(LETTERS[1:3]),
  GEN = c("F","M"),
  AGE = c("Child","Adult"),
  stringsAsFactors = FALSE
) %>% 
  as_tibble() %>% 
  mutate(FREQ = ceiling(runif(n(), 1,100)))

data1 <- tabulate_micro_data(
  data1, cat_vars = names(data1)[1:5], resp_var = "FREQ", marge_label = "ALL"
)[,.SD, .SDcols = c(names(data1)[1:5], "FREQ_tot")]

data.table::setnames(data1, "FREQ_tot", "FREQ")
str(data1)

data1[order(GEO, EDU, OCC, GEN, AGE), ] %>% 
  slice_sample(n=10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE
  )

data1_step1 <- data1[order(GEO, EDU, OCC, GEN, AGE), ][, GEN_AGE := paste0(GEN, "_", AGE)][
  , .(GEO, EDU, OCC, GEN_AGE, FREQ)
]

data1_step1 %>% 
  slice_sample(n=10) %>% 
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


data1_step1_1 <- data1_step1 %>% 
  filter(GEN_AGE %in% c(hier1$root, hier1$leaf)) 
data1_step1_1 %>% 
  slice_sample(n=10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE
  )

data1_step1_2 <- data1_step1 %>% 
  filter(GEN_AGE %in% c(hier2$root, hier2$leaf))

data1_step1_2 %>% 
  slice_sample(n=10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE
  )


data1_step2_1 <- data1_step1_1[order(GEO, EDU, OCC, GEN_AGE), ][, EDU_OCC := paste0(EDU, "_", OCC)][
  , .(GEO, EDU_OCC, GEN_AGE, FREQ)
]


data1_step2_1[order(GEO, GEN_AGE, EDU_OCC),] %>% 
  slice_sample(n=10) %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE
  )


