plot_movements <- function(
  cl1,
  cl2,
  data1,
  data2,
  matcher,
  var1,
  var2,
  clusters_included = NULL
){
matcher <- joinD_ordered$id.x
old_cluster <- cl1[matcher]

old_pc_value1 <- data1[matcher,][[var1]]
old_pc_value2 <- data1[matcher,][[var2]]
new_cluster <- cl2
new_pc_value1 <- data2[[var1]]
new_pc_value2 <- data2[[var2]]
plot_data_1 <- tibble(
  id = 1:length(old_cluster),
  year = 2017,
  cluster = old_cluster,
  value = c(scale(old_pc_value1)),
  value2 = c(scale(old_pc_value2))
)
plot_data_2 <- tibble(
  id = 1:length(new_cluster),
  year = 2018,
  cluster = new_cluster,
  value = c(scale(new_pc_value1)),
  value2 = c(scale(new_pc_value2))
)

plot_data_1$change <- plot_data_2$change <- plot_data_1$cluster != plot_data_2$cluster

plot_segment_data <- left_join(plot_data_1,plot_data_2, by= c("id","change"))
plot_segment_data <- plot_segment_data %>% filter(complete.cases(plot_segment_data))
plot_data <- rbind(plot_data_1,plot_data_2)
plot_data <- plot_data %>% filter(complete.cases(plot_data))

if(!is.null(clusters_included)){
  plot_segment_data <- plot_segment_data %>% filter(cluster.x %in% clusters_included,cluster.y %in% clusters_included)
  plot_data <- plot_data %>% filter(cluster %in% clusters_included)
}

p1 <- ggplot() + geom_point(
  data = plot_data, aes(x = value, y = value2,colour = factor(cluster))) +
  geom_text(
    data = plot_data %>% filter(change), aes(x = value, y = value2, label = id)
  ) + 
  geom_segment(data = plot_segment_data,
               aes(x  = value.x, y = value2.x, xend = value.y, yend = value2.y,alpha = change),arrow = arrow(length = unit(0.01, "npc")))
return(p1)
}

