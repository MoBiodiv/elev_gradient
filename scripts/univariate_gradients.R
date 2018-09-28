library(tidyverse)
library(mobr)

# read in data files. 
dat <- as_tibble(read.csv("data/Species_counts.csv"))
summary(dat)
str(dat)
dat$Site <- as.character(dat$Site)
dat$Site <- ifelse(dat$Site == "CATALOOCHEE", "CATA",
                   dat$Site)

sites <- as_tibble(read.csv("data/sites.csv"))
plot_attr <- merge(dat[ , 1:4], sites, by.x = "Site",
                   by.y = "Site.Code", all.x = T)


all(plot_attr$Site == dat$Site)

# drop site id information
dat <- dat[ , -c(1:4)]

# use UTM coordinates so that Euclidean distances
# can be sued. 
mob_in <- make_mob_in(comm = dat[ , -c(1:4)],
                      plot_attr = plot_attr,
                      coord_names = c("UTM.E", "UTM.N"))

stats <- get_mob_stats(mob_in, group_var = "Site")
plot(stats)

alphas <- stats$samples_stats
alphas$Elevation <- plot_attr$Elevation..m.[match(alphas$group, plot_attr$Site)]

alphas %>%
  filter(abs(value) < 1000) %>% #some of the S_PIE blow up because of low numbers of individuals
  ggplot(aes(Elevation, value) ) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(. ~ index,scales = "free")
ggsave("alphas.pdf", path = "./figs")

gammas <- stats$groups_stats  
gammas$Elevation <- plot_attr$Elevation..m.[match(gammas$group, plot_attr$Site)]

gammas %>%
  filter(abs(value) < 1000) %>%
  ggplot(aes(Elevation, value)) +
    geom_point() +
    geom_smooth(method="lm") +
    facet_wrap( . ~ index,scales = "free")
ggsave("gammas.pdf", path = "./figs")
