library(tidyverse)


# Read the data: single cell protein expression (flow cytometry)
# 10,000 cells, 26 proteins
# asinh-transformed and cleaned

df <- read_csv("flow_data.csv")
proteins <- names(df)


#### Histograms ####

df_tall <- df %>%
  pivot_longer(all_of(proteins), names_to="channel", values_to="value")

# variant:
df_tall_better <- df %>%
  mutate(cell_id = c(1:nrow(df))) %>%
  pivot_longer(all_of(proteins), names_to="channel", values_to="value")

ggplot(df_tall, aes(x=value)) +
  geom_histogram() +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()


ggplot(df_tall, aes(x=value)) +
  geom_histogram(bins=100) +
  scale_x_continuous(limits=c(-2,5)) +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()

# equivalent
ggplot(df_tall, aes(x=value)) +
  geom_histogram(bins=100) +
  xlim(c(0,5)) +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()

# not equivalent
ggplot(df_tall, aes(x=value)) +
  geom_histogram(bins=100) +
  coord_cartesian(xlim = c(0,5)) +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()

ggplot(df_tall, aes(x=value)) +
  geom_histogram(bins=100) +
  facet_wrap(~channel, scales="free") +
  theme_bw()



cutoffs <- c("CD3"=1, "CD19"=0.8, "CD4"=0.8, "CD8"=1)

df_cutoffs <- tibble(channel = names(cutoffs),
                     cutoff = unname(cutoffs))

ggplot(df_tall, aes(x=value)) +
  geom_histogram(bins=100) +
  xlim(c(-2,5)) +
  geom_vline(data=df_cutoffs, aes(xintercept=cutoff), 
             linetype="dashed", color="red") +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()


df_tall_cutoffs <- df_tall %>%
  inner_join(df_cutoffs) %>%
  mutate(modality = if_else(value > cutoff, "hi", "lo")) 

ggplot(df_tall_cutoffs, aes(x=value, fill=modality)) +
  geom_histogram(bins=100) +
  xlim(c(-2,5)) +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()


df_cell_type <- df %>%
  mutate(cell_type = case_when(CD3 > cutoffs["CD3"] ~ "Tcell",
                               CD19 > cutoffs["CD19"] ~ "Bcell",
                               .default = "Other"))

df_cell_type_tall <- df_cell_type %>%
  pivot_longer(all_of(proteins), names_to="channel", values_to="value")


ggplot(df_cell_type_tall, aes(x=value, fill=cell_type)) +
  geom_histogram(bins=100) +
  xlim(c(-2,5)) +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()


ggplot(df_cell_type_tall,
       aes(x=value, fill=cell_type)) +
  geom_histogram(bins=100, position="identity") +
  xlim(c(-2,5)) +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()


ggplot(df_cell_type_tall,
       aes(x=value, fill=cell_type)) +
  geom_histogram(bins=100, alpha=0.5, position="identity") +
  xlim(c(-2,5)) +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()



#### graph stuff ####
library(igraph)
library(ggraph)

set.seed(0)
sel_cells <- sample(nrow(df), 3000)
df_sub <- df[sel_cells,]


d <- as.matrix(dist(df_sub))
mask <- apply(d, 1, function(row) {
  smallest <- order(row)[seq(10)]
  result <- rep(0, length(row))
  result[smallest] <- 1
  return(result)
})

g <- graph_from_adjacency_matrix(mask, mode="undirected")

set.seed(0)
ggraph(g, layout = "fr") +
  geom_node_point() +
  geom_edge_link() 

set.seed(0)
ggraph(g, layout = "fr") +
  geom_node_point(aes(color=df_sub$CD19)) +
  scale_color_continuous(low="black", high="red") +
  geom_edge_link(alpha=0.1) +
  theme_graph()


library(leidenAlg)

set.seed(0)
community <- leiden.community(g)$membership


set.seed(0)
ggraph(g, layout = "fr") +
  geom_node_point(aes(color=community)) +
  geom_edge_link(alpha=0.1) +
  theme_graph()


set.seed(0)
df_layout <- create_layout(g, layout="fr")

ggraph(df_layout) +
  geom_node_point(aes(color=community)) +
  geom_edge_link(alpha=0.1) +
  theme_graph()



library(ggiraph)

gir <- ggraph(df_layout) +
  geom_point_interactive(mapping = aes(x = x, y = y,
                                       color = community,
                                       data_id = community,
                                       tooltip = df_sub$CD3)) +
  geom_edge_link(alpha=0.1) +
  theme_graph()

girafe(ggobj = gir)



label <- paste0("CD3 ", if_else(df$CD3 > cutoffs["CD3"], "hi", "lo"), "\n",
                "CD19 ", if_else(df$CD19 > cutoffs["CD19"], "hi", "lo"), "\n",
                "CD4 ", if_else(df$CD4 > cutoffs["CD4"], "hi", "lo"), "\n",
                "CD8 ", if_else(df$CD8 > cutoffs["CD8"], "hi", "lo"), "\n")

gir <- ggraph(df_layout) +
  geom_point_interactive(mapping = aes(x = x, y = y,
                                       color = community,
                                       data_id = community,
                                       tooltip = label[sel_cells])) +
  # geom_edge_link(alpha=0.1) +
  theme_graph()

myplot <- girafe(ggobj = gir)
htmlwidgets::saveWidget(myplot, "cell_graph.html")



myplot <- girafe(ggobj = gir)
myplot <- girafe_options(x=myplot,
                         opts_zoom(min=0.5, max=2),
                         opts_hover(css = "fill:black;stroke:gold;"))
htmlwidgets::saveWidget(myplot, "cell_graph.html")




#### Histograms again ####

df_sub_tall <- df_sub %>%
  pivot_longer(all_of(proteins), names_to="channel", values_to="value")

ggplot(df_sub_tall, aes(x=value)) +
  geom_histogram(bins=100) +
  xlim(c(-2,5)) +
  facet_wrap(~channel, scales="free_y") +
  theme_bw()


# mutate to add community information



# plot histograms by community



#### Explore the data like an immunologist: bivariate plots ####

ggplot(df_sub, aes(x=CD3, y=CD19)) +
  geom_point() +
  theme_bw()

ggplot(df_sub, aes(x=CD3, y=CD19, color = community %in% c(4,14))) +
  geom_point(alpha=0.5, size=0.5) +
  theme_bw()


ggplot(df_sub, aes(x=CD3, y=CD19)) +
  geom_bin2d() +
  scale_fill_viridis_c() +
  theme_bw()

ggplot(df_sub, aes(x=CD3, y=CD19)) +
  geom_bin2d(bins=40) +
  scale_fill_viridis_c() +
  theme_bw()


df_community <- df_sub %>% mutate(community = community)
df_bcell <- df_community %>% filter(community %in% c(4,14))
df_other <- df_community %>% filter(!community %in% c(4,14))

ggplot(df_other, aes(x=CD3, y=CD19)) +
  geom_bin2d(bins=40) +
  scale_fill_viridis_c() +
  ggnewscale::new_scale_fill() +
  geom_bin2d(data=df_bcell, bins=40) +
  scale_fill_viridis_c(option="C") +
  theme_bw()


centroids <- sapply(levels(community), function(lev) {
  cells_in_comm <- which(community == lev)
  apply(df_sub[cells_in_comm,], 2, median)
}) %>% t() %>% as_tibble()


centroids_tall <- centroids %>%
  mutate(community = levels(community)) %>%
  pivot_longer(-community, names_to = "protein", values_to = "expression")


ggplot(centroids_tall, aes(x=protein, y=community, fill=expression)) +
  geom_tile() +
  scale_fill_continuous(low="white", high="red") +
  theme_bw(base_size=16)

#### for some tasks, other packages do better ####
library(pheatmap)

pheatmap(centroids)



