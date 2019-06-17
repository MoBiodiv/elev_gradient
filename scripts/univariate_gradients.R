library(mobr)
library(vegan)
library(dplyr)
library(purrr)
library(ggplot2)
library(egg)

# read in data files. 
ant_comm <- read.csv("data/comm_dat.csv", row.names = 1)
ant_plot_attr <- read.csv("data/site_dat.csv", row.names = 1)

# slightly change elev of RAMY b/c of delta_stats issue
ant_plot_attr$elevation_m[ant_plot_attr$site == 'RAMY'] = 942

ant_mob_in <- make_mob_in(ant_comm, ant_plot_attr)

## compute mob stats-------------------

plot(ant_plot_attr[ , c('elevation', 'log_npp', 'npp_g_c_m2', "mean_monthly_temperature", "ndvi_evi")])

stats <- get_mob_stats(ant_mob_in, group_var = "site")
plot(stats)

alphas <- stats$samples_stats
alphas$elevation <- ant_plot_attr$elevation_m[match(alphas$group, ant_plot_attr$site)]

S = rowSums(ant_comm > 0)
full_mod = lm(S ~ elevation + log_npp + ndvi_evi, 
              data = ant_plot_attr)
red_mod = lm(S ~ elevation, data = ant_plot_attr)
summary(full_mod)
summary(red_mod)
anova(full_mod, red_mod)

#get Rsq values
lm_alpha <- alphas  %>%
            split(alphas$index) %>%
            map(filter, abs(value) < 1000) %>%
            map(function(x) lm(value ~ elevation, data = x))

r2_alpha <- lm_alpha %>%
            map_dbl(function(x) summary(x)$r.squared) 

alphas %>%
  ggplot(aes(x = elevation, y = value)) +
  #geom_point(aes(col = group)) +
  geom_smooth(method = "lm", se = T, col = "black") +
  facet_wrap( . ~ index, scales = "free", 
             labeller = labeller(
               index = function(x) {
                 r2 = round(r2_alpha[x], 2)
                 return(paste(x, "; R2=", r2, sep = ""))
               }))

ggsave("alphas.pdf", path = "./figs", width = 20, height = 20, units = "cm")


gammas <- stats$groups_stats  
gammas$elevation <- ant_plot_attr$elevation_m[match(gammas$group, ant_plot_attr$site)]

#get Rsq values
lm_gamma <- gammas %>% 
            split(gammas$index) %>%
            map(filter, abs(value) < 1000) %>%
            map(function(x) lm(value ~ elevation, data=x))

r2_gamma <- lm_gamma %>%
            map_dbl(function(x) summary(x)$r.squared) 

gammas %>%
  subset(abs(value) < 1000) %>% #some of the S_PIE blow up because of low numbers of individuals
  ggplot(aes(x = elevation, y = value)) +
  #geom_point(aes(col = group)) +
  geom_smooth(method = "lm", se = T, col = "black") +
  facet_wrap( . ~ index, scales = "free", 
             labeller = labeller(
               index = function(x) {
                 r2 = round(r2_gamma[x], 2)
                 return(paste(x, "; R2=", r2, sep = ""))
               }))

ggsave("gammas.pdf", path = "./figs", width = 20, height = 20, units = "cm")


mob_met = rbind(data.frame(scale = 'alpha', alphas),
                data.frame(scale = 'gamma', gammas))

mob_met %>%
  subset(index == "N") %>% 
  #subset(scale == "alpha") %>%
    ggplot(aes(x = elevation, y = value, col = scale)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = T)

ggsave("N.pdf", path = "./figs", width = 12, height = 8, units = "cm")

p1 = mob_met %>% 
  subset(abs(value) < 1000) %>%
  subset(index %in% c('S', 'S_n', 'S_PIE')) %>% 
  ggplot(aes(x = elevation, y = value, col = scale)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = T) +
    facet_wrap(. ~ index, scales = "free")


p2 = mob_met %>% 
    subset(abs(value) < 1000) %>%
    subset(index %in% c('beta_S', 'beta_S_n', 'beta_S_PIE')) %>% 
    ggplot(aes(x = elevation, y = value)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = T) +
    facet_wrap(. ~ index, scales = "free")

g = ggarrange(p1, p2)
ggsave("ENS.pdf", plot = g, path = "./figs", width = 20, height = 15, units = "cm")

## continuous analysis ------------------

deltas = get_delta_stats(ant_mob_in, group_var = 'elevation_m',
                         type = 'continuous', n_perm = 199)    

save(deltas, file = './results/deltas.Rdata')
#load('./results/deltas.Rdata')

pdf('./figs/deltas_b1.pdf')
plot(deltas, stat = 'b1', scale_by = 'indiv')
dev.off()

# drop the NODI site which only has 6 individuals
ant_mob_in = subset(ant_mob_in, Site != 'NODI')

deltas_noNODI = get_delta_stats(ant_mob_in, group_var = 'elevation_m',
                         type = 'continuous', n_perm = 199)    

save(deltas_noNODI, file = './results/deltas_noNODI.Rdata')
#load('./results/deltas_noNODI.Rdata')

pdf('./figs/deltas_noNODI_b1.pdf')
plot(deltas_noNODI, stat = 'b1', scale_by = 'indiv')
dev.off()

## analysis of composition change ------------

# indirect gradient analysis -----------
empty_sites = rowSums(ant_comm) == 0
ant_comm_no0 = ant_comm[!empty_sites, ]

ant_mds = metaMDS(ant_comm_no0)
stressplot(ant_mds)

plot(ant_mds, display='sp', type='n')
orditorp(ant_mds, display='sp', priority = colSums(ant_comm_no0))
fit = envfit(ant_mds, ant_plot_attr[!empty_sites, c('elevation_m', 'utm_n', 'utm_e')])
plot(fit)

# at the site rather than trap scale
ant_comm_agg = aggregate(ant_comm, by = list(ant_plot_attr$site), sum)[ , -1]
ant_plot_attr_agg = aggregate(ant_plot_attr, by = list(ant_plot_attr$site),
                              function(x) x[1])[ , -1]

ant_mds = metaMDS(ant_comm_agg)

pdf('./figs/ant_mds.pdf')
plot(ant_mds, display='sp', type='n')
orditorp(ant_mds, display='sp', priority = colSums(ant_comm_no0))
fit = envfit(ant_mds, ant_plot_attr_agg[, c('elevation_m', 'utm_n', 'utm_e')])
plot(fit)
stressplot(ant_mds)
dev.off()


ant_rda = rda(sqrt(ant_comm_agg) ~ elevation_m, data = ant_plot_attr_agg)
ant_rda
RsquareAdj(ant_rda)
anova(ant_rda, by = 'terms', permutations = 2e3)
ant_mso = mso(ant_rda, ant_plot_attr_agg[ , c('utm_e', 'utm_n')], grain = 5000, 
              permutations = 1000)

pdf('./figs/rda_results.pdf')
plot(ant_rda, display = c('sp', 'bp'), type='n')
orditorp(ant_rda, display = 'sp')
text(ant_rda, display = 'bp', col='red')
msoplot(ant_mso)
dev.off()

#elev = ant_plot_attr_agg$elevation_m[rowSums(ant_comm) > 0]
#xy = ant_plot_attr[rowSums(ant_comm) > 0, c('UTM_E', 'UTM_N')]
#ants = ant_comm[rowSums(ant_comm) > 0, ]
ant_cca = cca(sqrt(ant_comm_agg) ~ elevation_m, data = ant_plot_attr_agg)
RsquareAdj(ant_cca)
anova(ant_cca, by = 'terms', permutations = 2e3)
ant_mso_cca = mso(ant_cca, ant_plot_attr_agg[ , c('utm_e', 'utm_n')], grain = 5000, permutations = 1000)

pdf('./figs/cca_results.pdf')
plot(ant_cca, display = c('sp', 'bp'), type='n')
orditorp(ant_cca, display = 'sp')
text(ant_cca, display = 'bp', col='red')
msoplot(ant_mso_cca)
dev.off()
