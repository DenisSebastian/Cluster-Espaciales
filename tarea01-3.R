# TODO: test moran

# conf --------------------------------------------------------------------

pacman::p_load(dplyr, purrr, ggplot2, GGally, 
               factoextra, ggdendro, cluster,
               patchwork, umap, MASS)

options(readr.show_col_types = FALSE)
set.seed(42)
theme_set(theme_bw())

create_graphs = TRUE

# data --------------------------------------------------------------------

df <- 
  readr::read_csv("data/datos_t1_centroides.csv") %>% 
  dplyr::select(-`...1`) %>% 
  janitor::clean_names()

# fns ---------------------------------------------------------------------

source("R/fns.R")

# dim + glimpse -----------------------------------------------------------

# numero observaciones y variables
dim(df)

# muestra de variables
glimpse(df)

# viz 
p1 <-
  gg_plot_points(df, var=NULL,aes(alpha = .3)) +
  labs(title = "Manzanas Las Condes", subtitle = "Info Original") +
  theme(legend.position = "none")

# grupos de variables -----------------------------------------------------
# identificador, valores y caracteristicas

# tipo de variables (identificador, valores y caracteristicas)
cols <- colnames(df)

id_cols <- cols[1:5]
val_cols <- cols[6:28]
car_cols <- cols[29:38]

# NAs ---------------------------------------------------------------------
# NA en indicadores con poblacion relevante cero

map_df(df, ~ sum(is.na(.x))) %>% keep(~.x>0)
NA_rows <- rowSums(is.na(df))>0
df %>% filter(NA_rows) %>% 
  select(all_of(car_cols)) 

my_summarise(df, car_cols)
my_summarise(df, val_cols)
my_summarise(filter(df, NA_rows), car_cols)
  
# omitir NA del analisis
df_na <- na.omit(df)

dim(df)
dim(df_na)

my_summarise(df_na, val_cols)
p2 <-
  gg_plot_points(df_na, var = NULL, aes(alpha = .3)) +
  theme(legend.position = "none") +
  labs(title = "Manzanas, Las Condes", subtitle = "Omite Puntos con NA")
  
p1 + p2

# desc --------------------------------------------------------------------

# histogramas valores
h_val <- gg_hist_group(df_na, val_cols) +
  labs(title = "Distribución de Frecuencia",
       subtitle = "Indicadores de Manzana") +
  theme(legend.position = "none")
ggsave("img/hist_val.png", h_val, units = "in", 
       height = 7, width = 7)

# analisis:
# . escalas difieren
# . variables con forma de camapana, otras sesgadas
# . probar transforamciones logaritmicas a distribuciones sesgadas
# - ojo con normalizacion y estandarizacion: modificar varianza?

# histograma caracteristias
h_car <- gg_hist_group(df_na, car_cols) +
  labs(title = "Distribución de Frecuencia",
       subtitle = "Variables de Caracterización") +
  theme(legend.position = "none")
ggsave("img/hist_car.png", h_car, units = "in",
       height = 7, width = 7)

# analisis:
# . sesgado a la izquierda: aplicar transformacion log

# transformaciones --------------------------------------------------------
# log trans a caracteristicas
# icv, idep, iej, iem, igpe, ilpr, ipj, isal, ise, isv, ivi, iser

log_trans <- function(x, ep = 1) log(x+ep) 

log_vars <- c("icv", "idep", "iej", "iem", "igpe", "ilpr", "ipj", 
              "isal", "ise", "isv", "ivi", "iser")

log_vars <- c(log_vars, car_cols)

df_trans <- df_na

df_trans <- df_trans %>% 
  mutate(across(all_of(log_vars), log_trans))

p <- gg_hist_group(df_trans, log_vars) +
  labs(title = "Efecto Transformación Logarítmica")
ggsave("img/hist_test_log.png", p, units = "in", 
       height = 7, width = 7)
p <- gg_hist_group(df, log_vars) +
  labs(title = "Variables pre Transformación Logarítmica")
ggsave("img/hist_test.png", p, units = "in", 
       height = 7, width = 7)

# gg_plot_points(df_trans, "ibt")
# preproc -----------------------------------------------------------------

df_coords <- select(df_trans, x, y)
df_vars <- select(df_trans, !all_of(id_cols), -c(x,y))
# std
preproc <- caret::preProcess(df_vars)
X_std <- predict(preproc, df_vars)

# outliers ----------------------------------------------------------------

p <- gg_hist_group(X_std, colnames(X_std)) +
  labs(title = "Distribución Variables Estandarizadas")
ggsave("img/X_std_hist.png", units="in", height = 11, width = 11)

my_summarise(X_std, colnames(X_std)) %>% print(n=nrow(.))

# descartar outliers
out_rows <- (apply(X_std, 1, function(x) any(abs(x)>4)))
X <- X_std %>% filter(!out_rows)
coords <- df_coords %>% filter(!out_rows)

p <- gg_hist_group(X, colnames(X)) +
  labs(title = "Distribución Variables Estandarizadas", 
       subtitle = "Descarte de Outliers")
ggsave("img/X_std_hist_out.png", units="in", height = 11, width = 11)

my_summarise(X, colnames(X)) %>% print(n=nrow(.))

# casos notables de outliers en valores sobre o bajo 3 sd del promedio

# corr --------------------------------------------------------------------
# análisis de correlación

corr <- cor(X)
round(corr, 3)

cor_plot <- ggcorrplot::ggcorrplot(corr, hc.order = T, type = "lower") +
  labs(title = "Análisis de Correlación", "Variables Estandarizadas")

ggsave("img/cor_plot.jpg", cor_plot, units = "in", height = 8, width = 8)

# # pairs -------------------------------------------------------------------
# 
# select(X, any_of(val_cols)) %>% select(!matches("ibt","dim"))
# p <- ggpairs(select(X, val_cols, !starts_with("dim")))
# ggsave("img/pairs_val.png", p, unit = "in", height = 11, width = 11)

# # feature sel -------------------------------------------------------------
# 
# library(FSinR)   
# searcher <- searchAlgorithm('geneticAlgorithm')
# evaluator <- filterEvaluator('determinationCoefficient')
# results <- featureSelection(data, 'equipo', searcher, evaluator)

# feature extract ---------------------------------------------------------

# pca
PCA <- princomp(X, cor=TRUE)
barplot(PCA$sdev)
barplot(cumsum(PCA$sdev/sum(PCA$sdev)))

features_pca <- 
  predict(PCA) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(1:2)

# umap
# uniform manifold and projection

data.umap <- umap(X)
features_umap <- data.umap$layout %>% as.data.frame()

# mds

# Multidimensional scaling 
d <- dist(X) # distancias euclidianas entre entidades
data.MDS <- cmdscale(d, eig=TRUE, k = 2) # k es el numero de dimensiones de salida

features_MDS <- 
  data.MDS$points %>% 
  as.data.frame() %>% 
  as_tibble()

# nonparametric MDS
data.nMDS <- isoMDS(d, k=2) 

features_nMDS <- 
  data.nMDS$points %>% 
  as.data.frame() %>% 
  as_tibble()


# cluster var -------------------------------------------------------------
# reduccion de dimensionalidad en espacio de variables

# estadistico de hopkins

h_umap <- get_clust_tendency(features_umap, n = 30, graph = FALSE)$hopkins_stat
h_mds <- get_clust_tendency(features_MDS, n = 30, graph = FALSE)$hopkins_stat
h_nmds <- get_clust_tendency(features_nMDS, n = 30, graph = FALSE)$hopkins_stat
h_pca <-get_clust_tendency(features_pca, n = 30, graph = FALSE)$hopkins_stat

p <-ggplot(features_umap, aes(V1,V2)) + 
  geom_point() +
  labs(title = "Reducción Dimensionalidad",
       subtitle = "UMAP") +
  annotate("text", x = 4, y = -5, label = sprintf("Hopkins = %s",
                                          round(h_umap, 4)))
ggsave("img/umap_hopkins.png", p, units = "in",
       height = 7, width = 7)


p <-
  ggplot(features_MDS, aes(V1,V2)) + 
  geom_point() +
  labs(title = "Reducción Dimensionalidad",
       subtitle = "MDS") +
  annotate("text", x = -8, y = -5, 
           label = sprintf("Hopkins = %s",
                           round(h_mds, 4))
           )

ggsave("img/mds_hopkins.png", p, units = "in",
       height = 7, width = 7)

p <-  
  ggplot(features_nMDS, aes(V1,V2)) +
  geom_point() +
  labs(title = "Reducción Dimensionalidad",
       subtitle = "nMDS") +
  annotate("text", x = -8, y = -5, 
           label = sprintf("Hopkins = %s",
                           round(h_nmds, 4))
  )
ggsave("img/nmds_hopkins.png", p, units = "in",
       height = 7, width = 7)
ggsave("img/nmds_hopkins.png", p, units = "in",
       height = 7, width = 7)

p <-
  ggplot(features_pca, aes(comp_1, comp_2)) + 
  geom_point() +
  labs(title = "Reducción Dimensionalidad",
       subtitle = "PCA") +
  annotate("text", x = -8, y = -5, 
           label = sprintf("Hopkins = %s",
                           round(h_pca, 4))
  )

ggsave("img/pca_hopkins.png", p, units = "in",
       height = 7, width = 7)



# . umap revela mayor cantidad de grupos en espacio de variables


# cluster espacio variables -----------------------------------------------

# jerarquico

# dist
d0 <- dist(features_umap)

# analizo graficamente la distribucion de las distancias entre puntos
hist(d0, main = "Distancias en espacio de features")

# hacemos un modelo jerarquico con distancia completa
model_complete <- hclust(d0)
hgroups <- cutree(model_complete, k = 13)  

# kmeans
kmod <- kmeans(features_umap, 13)
kgroups <- kmod$cluster


# visualizamos los grupos resultantes
p1 <-
  ggplot(features_umap) +
  geom_point(aes(V1,V2, col=factor(hgroups))) +
  theme(legend.position = "none") +
  labs(title = "Hierarchical Cluster Feature Space",
       subtitle = "Feature Space Results")

# visualizamos los grupos resultantes
p2 <-
  ggplot(features_umap) +
  geom_point(aes(V1,V2, col=factor(kgroups))) +
  theme(legend.position = "none") +
  labs(title = "K-Means Cluster Feature Space",
       subtitle = "Feature Space Results")

p <- p1 + p2

ggsave("img/feature_cluster_compare.png", p,
       units="in",height = 7,width = 7)

p1 <-
  ggplot(coords) +
  geom_hex(aes(x=x,y=y, 
               fill = factor(hgroups)),
           alpha = .5) +
  geom_point(aes(x,y, col=factor(hgroups))) +
  theme(legend.position = "none") +
  labs(title = "Hierarchical Cluster Feature Space",
       subtitle = "Geography Results")+
  xlab("") + ylab("")
p2 <-
  ggplot(coords) +
  geom_hex(aes(x=x,y=y, 
               fill = factor(kgroups)),
           alpha = .5) +
  geom_point(aes(x,y, col=factor(kgroups))) +
  theme(legend.position = "none")+
  labs(title = "K-Means Cluster Feature Space",
       subtitle = "Geography Results")+
  xlab("") + ylab("")

p <- p1 + p2

ggsave("img/feature_cluster_compare_geo.png", p,
       units="in",height = 7,width = 10)


# TODO:  evaluacion de cluster {metricas}

# cluster sp --------------------------------------------------------------

d1 <- dist(coords)
hist(d1, main="Geo Distance Histogram", xlab = "m")
hgeo <- hclust(d1)
hgeo_groups <- cutree(hgeo, k = 13)

kgeo <- kmeans(coords, 13)
kgeo_groups <- kgeo$cluster

p1 <-  
  ggplot(coords) +
  geom_hex(aes(x=x,y=y, 
               fill = factor(hgeo_groups)),
           alpha = .5) +
  geom_point(aes(x,y, col=factor(hgeo_groups))) +
  theme(legend.position = "none") +
  labs(title = "Hierarchical Cluster Geography Space",
       subtitle = "Geography Results")+
  xlab("") + ylab("")


p2 <-
  ggplot(coords) +
  geom_hex(aes(x=x,y=y, 
               fill = factor(kgeo_groups)),
           alpha = .5) +
  geom_point(aes(x,y, col=factor(kgeo_groups))) +
  theme(legend.position = "none") +
  labs(title = "K-Means Cluster Geography Space",
       subtitle = "Geography Results") +
  xlab("") + ylab("")

p <- p1 + p2

ggsave("img/geography_cluster_compare_geo.png", p,
       units="in",height = 7,width = 10)


# TODO: 2000 metros (EVALUAR CLUSTER)

# nb 

# moran

# affinity prop

# density: dbscan

# cluster var + sp --------------------------------------------------------

library(ClustGeo)

range.alpha <- seq(0,1,0.1)
K <- 13
cr <- choicealpha(d0, d1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
# alpha que compensa perdida en homogeneidad en features (D0)
# con ganancia en homogeneidad espacial (D1)
alpha_geo = 0.9
tree <- hclustgeo(d0,d1,alpha = alpha_geo)
geoclust <- cutree(tree,13)


# TODO: nb constraint
library(sf)
library(spdep)

pts <- st_as_sf(coords, coords = c("x","y"), crs = 3857)
nb <- dnearneigh(pts, d1 = 0, d2 = 2000)
A <- nb2mat(nb,style="B")
diag(A) <- 1

D1 <- as.dist(1-A)

range.alpha <- seq(0,1,0.1)
K <- 13
cr <- choicealpha(d0, D1, range.alpha,
                  K, graph=FALSE)
plot(cr) # alpha buen compromiso0.5

alpha_nb = 0.5
tree_nb <- hclustgeo(d0,D1,alpha = alpha_nb)
geoclust_nb <- cutree(tree_nb,13)



p1 <-
  ggplot(coords) +
  geom_hex(aes(x=x,y=y, 
               fill = factor(geoclust)),
           alpha = .5) +
  geom_point(aes(x,y, col = factor(geoclust))) +
  theme(legend.position = "none") +
  xlab("") + ylab("") +
  labs(title = "Cluster Regional",
       subtitle = "Restricción Distancia en Metros") +
  annotate("text", x = 352500, y = 6306000, 
           label = sprintf("alpha = %s",
                           alpha_geo)
  )

p2 <-
  ggplot(coords) +
  geom_hex(aes(x=x,y=y, 
                 fill = factor(geoclust_nb)),
           alpha = .5) +
  geom_point(aes(x,y,
                 col = factor(geoclust_nb))) +
  theme(legend.position = "none") +
  xlab("") + ylab("") +
  labs(title = "Cluster Regional", 
       subtitle = "Restricción Vecindad < 2000 metros") +
  annotate("text", x = 352500, y = 6306000,
           label = sprintf("alpha = %s",
                           alpha_nb)
  )


p <- p1 + p2

ggsave("img/cluster_regional_compare.png", p,
       units="in", height = 9, width = 11)

