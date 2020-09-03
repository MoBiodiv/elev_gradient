install.packages(c('devtools', 'mobr', 'leaflet', 'mapview', 'tidyr',
                   'vegan', 'dplyr', 'ggplot2', 'egg'))

library(mobr)
library(vegan)
library(dplyr)
library(ggplot2)
library(egg)
devtools::install_version("broom", version = "0.5.4", repos = "http://cran.us.r-project.org")
library(broom)

# read in data file
ant_dat <- read.csv('./data/dryad/smokies_all.csv')
names(ant_dat)
ant_mob_in <- make_mob_in(ant_dat[ , 5:42], ant_dat[ c(1:4, 43:50)],
                          coord_names = c('sample_x', 'sample_y'))

# drop site with low number of individuals
ant_mob_in = subset(ant_mob_in, site != 'NODI', drop_levels = T)


## compute mob stats-------------------

stats <- get_mob_stats(ant_mob_in, group_var = "site", 
                       effort_samples = c(15, 80))

plot(stats)

alphas <- stats$samples_stats
alphas$elevation <- ant_dat$elevation_m[match(alphas$group, ant_dat$site)]

gammas <- stats$groups_stats  
gammas$elevation <- ant_dat$elevation_m[match(gammas$group, ant_dat$site)]

# fit linear models
lm_alpha = alphas %>% group_by(index, effort) %>%
    do(mod = lm(value ~ elevation, data = .))
lm_gamma = gammas %>% group_by(index, effort) %>%
    do(mod = lm(value ~ elevation, data = .))

# get model coefs

mod_coef_alpha = broom::tidy(lm_alpha, mod)
mod_coef_gamma = broom::tidy(lm_gamma, mod)

# get model summary
mod_sum_alpha = broom::glance(lm_alpha, mod)
mod_sum_gamma = broom::glance(lm_gamma, mod)

mob_met = rbind(data.frame(scale = 'alpha', alphas),
                data.frame(scale = 'gamma', gammas))
mob_met$index = factor(mob_met$index, 
                       levels = levels(mob_met$index)[c(2:1, 3:7)])

N1 = mob_met %>%
     subset(index %in% 'N') %>%
     ggplot(aes(x = elevation, y = value)) + 
         geom_point(aes(color = scale)) +
         geom_smooth(aes(color = scale), method = 'lm', se = F) +
         labs(x = "Elevation (m)", y = "Total abundance (N)")
ggsave("grad_vs_N.pdf", plot = N1, path = "./figs", width = 15, height = 12, 
       units = "cm")

       
       
SN1 = mob_met %>% 
     subset(index %in% c('S', 'N')) %>%
     subset(scale == 'alpha') %>%
     ggplot(aes(x = elevation, y = value)) + 
         geom_point() +
         geom_smooth(method = 'lm', se = T) +
         facet_wrap(~ index, scales = "free",  
                    labeller = as_labeller(c(S = "Species richness (S)",
                                             N = "Total abundance (N)"))) +
         labs(x = "Elevation (m)")

ggsave('./figs/grad_vs_S&N.pdf', SN1)


p1 = mob_met %>% 
  subset(abs(value) < 1000) %>%
  subset(index %in% c('S', 'S_n', 'S_PIE')) %>% 
  ggplot(aes(x = elevation, y = value, col = scale)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = F) +
    facet_wrap(. ~ index, scales = "free")


p2 = mob_met %>% 
    subset(abs(value) < 1000) %>%
    subset(index %in% c('beta_S', 'beta_S_n', 'beta_S_PIE')) %>% 
    ggplot(aes(x = elevation, y = value)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = F) +
    facet_wrap(. ~ index, scales = "free")

g = ggarrange(p1, p2)
ggsave("ENS.pdf", plot = g, path = "./figs", width = 20, height = 15, units = "cm")


## continuous analysis ------------------

deltas = get_delta_stats(ant_mob_in, env_var = 'elevation_m',
                         group_var = 'site', stats = c('betas', 'r'),
                         type = 'continuous', spat_algo = 'kNCN',
                         n_perm = 199, overall_p = TRUE)

save(deltas, file = './results/deltas.Rdata')
#load('./results/deltas.Rdata')

deltas$env_var = 'elevation (m)'

plot(deltas, stat = 'b1')

pdf('./figs/deltas_b1.pdf')
plot(deltas, stat = 'b1', scale_by = 'indiv',
     eff_sub_effort = F, eff_log_base = 2,
     eff_disp_pts = F,
     eff_disp_smooth = T)
dev.off()


pdf('./figs/deltas_r.pdf')
plot(deltas, stat = 'r', scale_by = 'indiv',
     eff_sub_effort = T, eff_log_base = 2,
     eff_disp_pts = T,
     eff_disp_smooth = F)
dev.off()

# exploration of centering effect size
deltas$S_df <- deltas$S_df %>%
    group_by(test, effort) %>%
    mutate(effect = effect - mean(effect)) %>%
    ungroup()

deltas$S_df[deltas$S_df$test == 'SAD', ] <-
    deltas$S_df %>%
    subset(test == 'SAD') %>%
    group_by(effort) %>%
    mutate(effect = effect - mean(effect))

## non-published supplemental analysis of compositional change ------------

# indirect gradient analysis -----------
empty_sites = rowSums(ant_mob_in$comm) == 0
ant_comm_no0 = ant_mob_in$comm[!empty_sites, ]

ant_mds = metaMDS(ant_comm_no0)
stressplot(ant_mds)

plot(ant_mds, display='sp', type='n')
orditorp(ant_mds, display='sp', priority = colSums(ant_comm_no0))
fit = envfit(ant_mds, ant_mob_in$env[!empty_sites, c('elevation_m', 'utm_n', 'utm_e')])
plot(fit)

# at the site rather than trap scale
ant_comm_agg = aggregate(ant_mob_in$comm, by = list(ant_mob_in$env$site), sum)[ , -1]
ant_dat_agg = aggregate(ant_mob_in$env, by = list(ant_mob_in$env$site),
                              function(x) x[1])[ , -1]

ant_mds = metaMDS(ant_comm_agg)

pdf('./figs/ant_mds.pdf')
plot(ant_mds, display='sp', type='n')
orditorp(ant_mds, display='sp', priority = colSums(ant_comm_no0))
fit = envfit(ant_mds, ant_dat_agg[, c('elevation_m', 'utm_n', 'utm_e')])
plot(fit)
stressplot(ant_mds)
dev.off()


ant_rda = rda(sqrt(ant_comm_agg) ~ elevation_m, data = ant_dat_agg)
ant_rda
RsquareAdj(ant_rda)
anova(ant_rda, by = 'terms', permutations = 2e3)
ant_mso = mso(ant_rda, ant_dat_agg[ , c('utm_e', 'utm_n')], grain = 5000, 
              permutations = 1000)

pdf('./figs/rda_results.pdf')
plot(ant_rda, display = c('sp', 'bp'), type='n')
orditorp(ant_rda, display = 'sp')
text(ant_rda, display = 'bp', col='red')
msoplot(ant_mso)
dev.off()

#elev = ant_dat_agg$elevation_m[rowSums(ant_comm) > 0]
#xy = ant_dat[rowSums(ant_comm) > 0, c('UTM_E', 'UTM_N')]
#ants = ant_comm[rowSums(ant_comm) > 0, ]
ant_cca = cca(sqrt(ant_comm_agg) ~ elevation_m, data = ant_dat_agg)
RsquareAdj(ant_cca)
anova(ant_cca, by = 'terms', permutations = 2e3)
ant_mso_cca = mso(ant_cca, ant_dat_agg[ , c('utm_e', 'utm_n')], grain = 5000, permutations = 1000)

pdf('./figs/cca_results.pdf')
plot(ant_cca, display = c('sp', 'bp'), type='n')
orditorp(ant_cca, display = 'sp')
text(ant_cca, display = 'bp', col='red')
msoplot(ant_mso_cca)
dev.off()
