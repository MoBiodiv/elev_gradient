library(mobr)

# read in data files. 
ant_comm <- read.csv("data/comm_dat.csv", row.names = 1)
ant_plot_attr <- read.csv("data/site_dat.csv", row.names = 1)

# slightly change elev of RAMY b/c of delta_stats issue
ant_plot_attr$Elevation_m[ant_plot_attr$Site == 'RAMY'] = 942

ant_mob_in <- make_mob_in(ant_comm, ant_plot_attr)

stats <- get_mob_stats(ant_mob_in, group_var = "Site")
plot(stats)

alphas <- stats$samples_stats
alphas$Elevation <- ant_plot_attr$Elevation_m[match(alphas$group, plot_attr$Site)]

alphas %>%
  filter(abs(value) < 1000) %>% #some of the S_PIE blow up because of low numbers of individuals
  ggplot(aes(Elevation, value) ) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(. ~ index,scales = "free")
ggsave("alphas.pdf", path = "./figs")

gammas <- stats$groups_stats  
gammas$Elevation <- ant_plot_attr$Elevation_m[match(gammas$group, plot_attr$Site)]

gammas %>%
  filter(abs(value) < 1000) %>%
  ggplot(aes(Elevation, value)) +
    geom_point() +
    geom_smooth(method="lm") +
    facet_wrap( . ~ index,scales = "free")
ggsave("gammas.pdf", path = "./figs")

## continuous analysis

deltas = get_delta_stats(ant_mob_in, group_var = 'Elevation_m',
                         type = 'continuous', n_perm = 199)    

save(deltas, file = './results/deltas.Rdata')

pdf('./figs/deltas_b1.pdf')
plot(deltas, stat = 'b1')
dev.off()

# drop the NODI site which only has 6 individuals
ant_mob_in = subset(ant_mob_in, Site != 'NODI')

deltas_noNODI = get_delta_stats(ant_mob_in, group_var = 'Elevation_m',
                         type = 'continuous', n_perm = 199)    

save(deltas_noNODI, file = './results/deltas_noNODI.Rdata')

pdf('./figs/deltas_noNODI_b1.pdf')
plot(deltas_noNODI, stat = 'b1')
dev.off()