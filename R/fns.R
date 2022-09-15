pacman::p_load(mclust, e1071, cluster, flexclust, factoextra, magrittr)

gg_hist_group <- function(df, cols) {
  df %>% 
    select(all_of(cols)) %>% 
    tidyr::pivot_longer(cols = everything(),
                        names_to = "var") %>% 
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(~var, scales = "free")
}

gg_plot_points <- function(df, var = NULL, ...) {
  if (is.null(var)) {
    ggplot(df, aes(x = x, y = y)) +
      geom_point(...)
  } else {
    ggplot(df, aes(x = x, y = y, colour = !!sym(var))) +
      geom_point(...)  
  }
}

my_summarise <- function(.data, vars, ...) {
  .data %>%
    group_by(...) %>%
    summarise(
      across(
        .cols = all_of(vars),
        .fns = list(
          min = ~ min(.x, na.rm = TRUE),
          q25 = ~ quantile(.x, .25,na.rm = TRUE),
          mean = ~mean(.x, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          q75 = ~ quantile(.x,.75,na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          # iqr = ~ IQR(.x, na.rm = TRUE),
          NAs = ~ sum(is.na(.x)),
          N = ~ n()
        ),
        .names = "{col}__{.fn}"
      )
    ) %>%
    tidyr::pivot_longer(cols = matches("__"),names_to = c("var", "fun"), names_sep = "__") %>%
    tidyr::pivot_wider(names_from = fun, values_from = value) %>% 
    arrange(var)
}


clus_cor <- function(data, clusters){
  
  k <- length(unique(clusters))
  
  mat_dis <- 
    data %>% 
    arrange(clusters) %>% 
    dist() %>% 
    as.matrix() 
  
  # construimos matriz de similaridad inicial
  mat_simil <- 1 / (1 + mat_dis)
  
  ideal_mat <- 
    matrix(0, 
           nrow = nrow(data), 
           ncol = nrow(data))
  
  walk(1:k, 
       function(x) {
         ideal_mat[which(clusters==x),which(clusters==x)] <<- 1  
       })
  
  
  # calculamos la correlacion 
  cor <-    cor(ideal_mat[upper.tri(ideal_mat)],
                mat_simil[upper.tri(mat_simil)])
  
  return(cor)
}


inspeccion_visual <- function(data, clusters){
  mat_dis <- 
    data %>% 
    arrange(clusters) %>% 
    dist() %>% 
    as.matrix() 
  
  image(mat_dis)
}


# funcion que evalua cohesion de cada cluster
cohesion <- function (x, data, clusters) {
  tempData <- data[which(clusters==x),]
  coh <- sum(dist2(tempData, colMeans(tempData))^2)
  
  return(coh)
}


# funcion que evalua cohesion del proceso de clustering
clus_cohes <- function(data, clusters){
  k <- length(unique(clusters))
  vec_coh <- map_dbl(unique(clusters), function(x) cohesion(x=x, data=data, clusters=clusters))
  return(vec_coh)
}


# funcion que evalua separacion de cada cluster
separacion <- function (x, data, clusters) {
  meanData <- colMeans(data)
  
  tempData <- data[which(clusters==x),]
  sep <- nrow(tempData)*sum((meanData-colMeans(tempData))^2)
  
  return(sep)
}

# funcion que evalua separacion del proceso de clustering
clus_sep <- function(data, clusters){
  k <- length(unique(clusters))
  
  
  vec_sep <- map_dbl(unique(clusters), function(x) separacion(x=x, data=data, clusters = clusters))
  return(vec_sep)
}
evaluacion_cluster <- function(data, clusters){
  coefSil <- silhouette(clusters,dist(data))
  cohesiones <- clus_cohes(data, clusters)
  separaciones <- clus_sep(data, clusters)
  correlacion <- clus_cor(data, clusters)
  
  
  
  return(list(correlacion = correlacion, 
              coefSil = coefSil, 
              cohesiones = cohesiones, 
              separaciones = separaciones))
}
