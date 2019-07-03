library(readr)
library(grid)
library(gridExtra)
library(ggforce)
fineroot2leaf_backup <-
  read.csv("~/ecowdery/ED.Hydro/parameters/constraint_calculations/constraint_data/FRED/fineroot2leaf_backup.csv", stringsAsFactors = FALSE) %>%
  select(one_of("id","site_id","specie_id", "citation_id", "mean")) %>%
  mutate(site_id = paste0(site_id %/% 1e+09, "-", site_id %% 1e+09)) %>%
  mutate(citation_id = paste0(citation_id %/% 1e+09, "-", citation_id %% 1e+09))


fineroot2leaf_backup$citation_id %>% unique()

upper_cutoff <- 1.56

give.n <- function(x){
  return(data.frame(y = min(x), label = paste0("\n", paste0("n = ",length(x)))))
}


# give.cutoff <- function(x){
#   n <- length(x)
#   return(data.frame(y = upper_cutoff,
#          label = paste0("\n", paste0(max(x) , " point(s) cutoff"))))
# }

p <- ggplot(fineroot2leaf_backup)

p1 <- p +
  geom_density(aes(x = mean)) +
  coord_flip()  +
  scale_y_reverse() +
  xlab("fineroot2leaf") +
  geom_vline(aes(xintercept = mean(mean)), size = 1, color = "gray") + xlim(0,upper_cutoff)

p2 <- p +
  geom_boxplot(aes(x = as.factor(site_id), y = mean)) +
  facet_zoom(ylim = c(0,upper_cutoff)) +
  geom_jitter(aes(x = as.factor(site_id), y = mean, color = as.factor(citation_id)), width = .05, size = 3, alpha = .4) +
  xlab("Site id") + ylab("fineroot2leaf") +
  theme(legend.title = element_blank())

# +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


# +
  # stat_summary(aes(x = as.factor(site_id), y = mean), fun.data = give.n, geom = "text", size = 6)


lay <- rbind(c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2),
             c(1,1,2,2,2,2,2,2,2,2,2,2,2,2,2))

# png(filename = "fineroot2leaf_backup_plot.png")

grid.arrange(p1,p2, layout_matrix = lay, top = textGrob("Original Subset of BAAD database", gp=gpar(fontsize=20)))

# dev.off()

