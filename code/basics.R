


library(tidyverse)

############# Reading tabular data ################
df0 <- read.csv("tabular_data.csv") # base R function
print(df0)


df <- read_csv("tabular_data.csv") # function from readr, in tidyverse
print(df)

df

names(df)




############# Basic plots ################

ggplot(df, aes(x=Age, y=Monocyte))


ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point()

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_line()

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_bin_2d()

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_density_2d()

# Multiple layers
ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point() +
  geom_density_2d() +
  geom_line()

# Trend lines
ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point() +
  geom_smooth()

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point() +
  geom_smooth(method="lm")

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_abline(slope=0.001, intercept=0.1)

# More formal: using argument names
ggplot(data=df, mapping=aes(x=Age, y=Monocyte)) +
  geom_point()



# ggplot works with data frames, not matrices
mat <- as.matrix(df)
ggplot(mat, aes(x=Age, y=Monocyte)) +
  geom_point()

df1 <- as.data.frame(mat)
df2 <- as_tibble(mat)

# Temporary solution: cast to numeric only for this plot
ggplot(df1, aes(x=as.numeric(Age), y=as.numeric(Monocyte))) +
  geom_point()

# Permanent solution: cast to numeric in the data itself
df1$Age <- as.numeric(df1$Age)
df1$Monocyte <- as.numeric(df1$Monocyte)

ggplot(df1, aes(x=Age, y=Monocyte)) +
  geom_point()


############# More aesthetics ################

# which layers use which aesthetics?
ggplot(df, aes(x=Age, y=Monocyte, color=Gender)) +
  geom_smooth() +
  geom_point(aes(color = as.character(Age))) 


ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=Gender)) +
  geom_smooth()

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point() +
  geom_smooth(aes(color=Gender))


# each geom has certain aesthetics it can use, and ignores others
ggplot(df, aes(x=Age, y=Monocyte, shape=Gender)) +
  geom_point() +
  geom_smooth(aes(color=Gender))

ggplot(df, aes(x=Age, y=Monocyte, shape=Gender)) +
  geom_point() +
  geom_smooth(aes(color=Gender, linetype=Gender))

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_smooth()


# aesthetics versus fixed values
ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(color="blue") +
  geom_smooth(color="red")

# ineffective
ggplot(df, aes(x=Age, y=Monocyte), color="blue") +
  geom_point() +
  geom_smooth(color="red")

# surprising??
ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color="blue"))

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(color="blue")

# using external vectors
v <- if_else(df$Age < 45, "Young", "Less Young")
df$Age_Status <- v

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=Age_Status)) +
  geom_smooth(color="red")

# using functions of variables
ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=if_else(Age < 45, "Young", "Less Young"))) +
  geom_smooth(color="red")



############# Scales ################

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=if_else(Age < 45, "Young", "Less Young"))) +
  scale_color_discrete(name="Age Group") +
  geom_smooth()


# need to know if variable is discrete or continuous
ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=if_else(Age < 45, "Young", "Less Young"))) +
  scale_color_continuous(name="Age Group") +
  geom_smooth()


ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=Age)) +
  scale_color_continuous(name="Age (yrs)") +
  geom_smooth()

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=Age)) +
  scale_color_continuous(name="Age (yrs)", low="blue", high="red") +
  geom_smooth()

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=Age)) +
  scale_color_continuous(name="Age (yrs)", low="blue", high="red",
                         limits = c(30, 70)) +
  geom_smooth()


ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=Age)) +
  scale_color_gradient2(name="Age (yrs)", low="blue", mid="white", high="red") +
  geom_smooth()

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=Age)) +
  scale_color_gradient2(name="Age (yrs)", low="blue", mid="yellow", high="red",
                        midpoint = 45) +
  geom_smooth()


# viridis scales: friendly for color blindness and grayscale
ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=Age)) +
  scale_color_viridis_c(name="Age (yrs)") +
  geom_smooth()


ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=Age)) +
  scale_color_viridis_b(name="Age (yrs)") +
  geom_smooth()


color_vals <- c("Young" = "#2f4576", "Less Young" = "green")

ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point(aes(color=v)) +
  scale_color_manual(values = color_vals, name="Age group") +
  geom_smooth()



#### Scaling up: iterating through plots ####

cell_types <- c("Monocyte", "T cell CD8 Naive")


for (cell_type in cell_types) {
  print(cell_type)
  ggplot(df, aes(x=Age, y=cell_type)) +
    geom_point()
}


# inside loops or functions, need explicit plot command
for (cell_type in cell_types) {
  p <- ggplot(df, aes(x=Age, y=cell_type)) +
    geom_point()
  
  plot(p)
}

# By default ggplot aesthetics use literals, not variables
# use .data[[]] for names stored in variables
for (cell_type in cell_types) {
  p <- ggplot(df, aes(x=Age, y=.data[[cell_type]])) +
    geom_point()

  plot(p)
}


# save plots to file instead of displaying them in Rstudio
for (cell_type in cell_types) {
  p <- ggplot(df, aes(x=Age, y=.data[[cell_type]])) +
    geom_point()
  
  ggsave(p, filename = paste0("figures/", cell_type, ".pdf"),
         width = 8, height = 6)
}


#### facets, pivots ####

ggplot(df, aes(x=Age, y=Monocyte, color=Gender)) +
  geom_point()


ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point() +
  facet_wrap(~Gender)

## tidyr package in tidyverse
pivot_longer(df, cols = c(4:28))

## if you have column names in an external vector
# pivot_longer(df, cols = all_of(cell_types))

pivot_longer(df, cols = c(4:28), names_to = "Cell_Type", values_to="Fraction")

df_long <- pivot_longer(df, cols = c(4:28), 
                        names_to = "Cell_Type", 
                        values_to="Fraction")

ggplot(df_long, aes(x=Age, y=Fraction)) +
  geom_point() +
  facet_wrap(~Cell_Type)


ggplot(df_long, aes(x=Age, y=Fraction)) +
  geom_point() +
  facet_wrap(~Cell_Type, scales="free_y")


#### pipes ####

df_long <- pivot_longer(df, cols = c(4:28), 
                        names_to = "Cell_Type", 
                        values_to="Fraction")
# df_long$Age_Status = if_else(...)

# mutate function from dplyr package in tidyverse
df_status <- mutate(df_long, 
                    Age_Status = if_else(Age < 45, "Young", "Less Young"))


df_status <- df %>%
  pivot_longer(cols=c(4:28), names_to="Cell_Type", values_to="Fraction") %>%
  mutate(Age_Status = if_else(Age < 45, "Young", "Less Young"))


ggplot(df_status, aes(x=Age_Status, y=Fraction)) +
  geom_boxplot() +
  facet_wrap(~Cell_Type, scales="free_y")

# color vs fill
ggplot(df_status, aes(x=Age_Status, y=Fraction, color=Age_Status)) +
  geom_boxplot() +
  facet_wrap(~Cell_Type, scales="free_y")

ggplot(df_status, aes(x=Age_Status, y=Fraction, fill=Age_Status)) +
  geom_boxplot() +
  facet_wrap(~Cell_Type, scales="free_y")

ggplot(df_status, aes(x=Age_Status, y=Fraction, fill=Gender)) +
  geom_boxplot() +
  facet_wrap(~Cell_Type, scales="free_y")


ggplot(df_status, aes(x=Age_Status, y=Fraction, fill=Age_Status)) +
  geom_boxplot() +
  geom_point(alpha = 0.2) +
  facet_wrap(~Cell_Type, scales="free_y")


ggplot(df_status, aes(x=Age_Status, y=Fraction, fill=Age_Status)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~Cell_Type, scales="free_y")


ggplot(df_status, aes(x=Age_Status, y=Fraction, fill=Age_Status)) +
  geom_violin() +
  facet_wrap(~Cell_Type, scales="free_y")



#### scales, again ####

ggplot(df_status, aes(x=Age_Status, y=log10(Fraction), fill=Age_Status)) +
  geom_violin() +
  facet_wrap(~Cell_Type, scales="free_y")



ggplot(df_status %>% filter(grepl("T cell", Cell_Type)), 
       aes(x=Age_Status, y=Fraction, fill=Age_Status)) +
  geom_violin() +
  scale_y_continuous(trans = "log10") +
  facet_wrap(~Cell_Type, scales="free_y")


#### titles, themes, etc ####

p1 <- ggplot(df, aes(x=Age, y=Monocyte)) +
  geom_point()


p1 + ggtitle("Monocyte proportion by age")

p1 + ggtitle("Monocyte proportion by age") +
  theme_dark()

p1 + ggtitle("Monocyte proportion by age") +
  theme_bw()

p1 + ggtitle("Monocyte proportion by age") +
  theme_void()

p1 + ggtitle("Monocyte proportion by age") +
  theme_bw(base_size=16)

p1 + ggtitle("Monocyte proportion by age") +
  theme_bw(base_size=16) +
  theme(axis.text.x = element_text(size=20))

p1 + ggtitle("Monocyte proportion by age") +
  theme_bw(base_size=16) +
  theme(axis.text.x = element_text(angle=45))


#### patchwork ####

install.packages("patchwork")

library(patchwork)
# cowplot gridextra?

p1 + geom_smooth() | p1 + geom_density_2d()

(p1 + geom_smooth()) / (p1 + geom_density_2d())

p2 <- p1 + geom_smooth()
p3 <- p1 + geom_density_2d()

(p1 | p1) / p2

# get plots from a loop
cell_types <- c("Monocyte", "T cell CD8 Naive", "T cell CD8 Memory")

# or define programatically to avoid typos
cell_types <- names(df)[c(4:28)]
plist <- list()

for (i in c(1:3)) {
  cell_type <- cell_types[i]
  plist[[i]] <- ggplot(df, aes(x=Age, y=.data[[cell_type]])) +
    geom_point()
  
}

plist[[1]] | plist[[2]] / plist[[3]]


# be careful about lazy evaluation
plist <- list()
for (i in c(1:3)) {
  cell_type <- cell_types[i]
  
  # Completely equivalent statements
  values <- df %>% pull(cell_types[i])
  values <- pull(df, cell_types[i])
  
  
  plist[[i]] <- ggplot(df, aes(x = Age, y = values )) +
    geom_point()
}

plist[[1]] | plist[[2]] / plist[[3]]





# to install for after lunch:
install.packages("ggiraph")
install.packages("igraph")
install.packages("ggraph")
install.packages("leidenAlg")
install.packages("ggnewscale")
install.packages("pheatmap")
install.packages("patchwork")




rm(list = ls())




