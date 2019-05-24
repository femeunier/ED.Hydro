FRED <- read.csv("/fs/data3/ecowdery/FRED/FRED_sub.csv")

glimpse(FRED)

FRED2 <- FRED %>% select(one_of(traits)) %>% gather(key = "trait")
names(FRED2)
FRED2$value <- as.numeric(FRED2$value)
unique(FRED2$trait)
ggplot(FRED2) + geom_density(aes(x= value), fill = "gray") + facet_wrap(vars(trait), scales = "free")
ggplot(FRED2) + geom_histogram(aes(x= value), fill = "gray") + facet_wrap(vars(trait), scales = "free")
