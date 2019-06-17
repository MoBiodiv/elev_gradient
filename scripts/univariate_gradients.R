library(mobr)
library(vegan)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(egg)

# read in data files. 
ant_comm <- read.csv("data/comm_dat.csv", row.names = 1)
ant_plot_attr <- read.csv("data/site_dat.csv", row.names = 1)

# quick and dirty fix so elev not exactly matching
table(ant_plot_attr$elevation_m)
which(ant_plot_attr$elevation_m == 941)
ant_plot_attr$elevation_m[273:288] = 941.01
    
ant_mob_in <- make_mob_in(ant_comm, ant_plot_attr, coord_names = c('x', 'y'))

## compute mob stats-------------------

plot(ant_plot_attr[ , c('elevation_m', 'log_npp', 'npp_g_c_m2', "mean_monthly_temperature", "ndvi_evi")])

a = aggregate(ant_comm, list(ant_plot_attr$site), sum)[ ,-1]
Sa = rowSums(a> 0)
Na = rowSums(a)
ev = tapply(ant_plot_attr$elevation_m, list(ant_plot_attr$site), function(x) x[1])
summary(lm(Na ~ ev))

S = rowSums(ant_comm > 0)
N = rowSums
summary(lm(N ~ ant_plot_attr$elevation_m))
summary(glm(N ~ ant_plot_attr$elevation_m, family = 'poisson'))
pseudo_r2 = function(glm_mod) {
    1 -  glm_mod$deviance / glm_mod$null.deviance
}
pseudo_r2(glm(Na ~ ant_plot_attr$elevation_m, family = 'poisson'))

summary(lm(S ~ N))
mod = lm(S ~ elevation_m + log_npp + ndvi_evi, data = ant_plot_attr)
par(mfrow=c(1,2))
termplot(mod, se = T, partial.resid = T)
cor(ant_plot_attr$elevation_m, ant_plot_attr$npp_g_c_m2, use = "complete.obs")
# [1] 0.3151249
cor(S, ant_plot_attr$elevation_m)
# [1] -0.558464
Sg = rowSums((aggregate(ant_comm, by = list(as.character(ant_plot_attr$site)), sum)[ , -1] > 0))
en = aggregate(ant_plot_attr, by = list(as.character(ant_plot_attr$site)), function(x) x[1])[, -1]
cor(Sg, en$elevation_m)
#[1] -0.7849327
cor(Sg[!is.na(en$log_npp)], en$elevation_m[!is.na(en$log_npp)])

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

p_gamma <- lm_gamma %>%
           map_dbl(function(x) summary(lm_gamma$N)$coeff[2,4])

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
mob_met$index = factor(mob_met$index, 
                       levels = levels(mob_met$index)[c(2:1, 3:7)])

N = with(mob_met, value[index == "N"]) 
S = with(mob_met, value[index == "S"])
scale = with(mob_met, scale[index == "N"])
dat = data.frame(N, S, scale)

gSN = dat %>%
      ggplot(aes(x = N, y = S, col = scale)) + 
      geom_point() +
      geom_smooth(method = 'lm', se = T) + 
      guides(colour='none')

gSE = mob_met %>%
      subset(index == "S") %>%
      ggplot(aes(x = elevation, y = value, col = scale)) +
      geom_point() +
      labs(y = "S") + 
      guides(colour='none')


gNE = mob_met %>%
      subset(index == "N" & scale == 'gamma') %>% 
      ggplot(aes(x = elevation, y = value)) + 
      geom_point() +
      geom_smooth(method = 'lm', se = T) + 
      labs(y = "N") + 
      guides(colour='none')

summary(lm(value ~ elevation, data = mob_met,
           subset = scale == 'gamma' & index == 'N')) 


gSnE = mob_met %>%
       subset(index == "S_n") %>%
       ggplot(aes(x = elevation, y = value, col = scale)) +
       geom_point() +
       geom_smooth(method = 'lm', se = T) + 
       labs(y = expression(S[n]))
    
g = ggarrange(gSE, gNE, gSnE, nrow = 1)

p1 = mob_met %>% 
    subset(abs(value) < 1000) %>%
    subset(index %in% c('S', 'N', 'S_n', 'S_PIE')) %>% 
    ggplot(aes(x = elevation, y = value, col = scale)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = T) +
    facet_wrap(. ~ index, scales = "free", nrow = 1)



ggsave("N.pdf", plot = gNE, path = "./figs", width = 12, height = 8, units = "cm")

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

# alpha, beta, gamma on the same 3 panels 

new_index = sub('beta_', '', mob_met$index) 
new_scale = ifelse(grepl('beta_', mob_met$index),
                   'beta',
                   as.character(mob_met$scale))

mob_met$index = new_index
mob_met$scale = new_scale

mob_met %>% 
    subset(abs(value) < 1000) %>%
    subset(index %in% c('S', 'S_n', 'S_PIE')) %>% 
    ggplot(aes(x = elevation, y = value, col = scale)) + 
    #geom_point() +
    geom_smooth(method = 'lm', se = T) +
    facet_wrap(. ~ index, scales = "fixed")  

ggsave("div_grad.pdf", path = "./figs", width = 20, height = 8, units = "cm")




## continuous analysis ------------------
deltas = get_delta_stats(ant_mob_in, stat = 'r', 
                         group_var = 'elevation_m',
                         type = 'continuous', n_perm = 19)
plot(deltas, 'r', scale_by = 'indiv')


save(deltas, file = './results/deltas.Rdata')
#load('./results/deltas.Rdata')
deltas$group_var = 'elevation(m)'

pdf('./figs/deltas_b1.pdf')
plot(deltas, 'elevation_m', stat = 'b1', scale_by = 'indiv')
dev.off()

# drop the NODI site which only has 6 individuals
ant_mob_in = subset(ant_mob_in, site != 'NODI', drop_levels = T)

deltas_noNODI = get_delta_stats(ant_mob_in, group_var = 'elevation_m',
                                stat = c('betas', 'r'), type = 'continuous', n_perm = 49,
                                overall_p = TRUE)    

save(deltas_noNODI, file = './results/deltas_noNODI.Rdata')
#load('./results/deltas_noNODI.Rdata')

pdf('./figs/deltas_noNODI_b1.pdf')
plot(deltas_noNODI, stat = 'b1', scale_by = 'indiv',
     eff_sub_effort = TRUE, eff_log_base = 2.8,
     eff_disp_pts = T,
     eff_disp_smooth = F)
dev.off()

pdf('./figs/deltas_noNODI_r.pdf')
plot(deltas_noNODI, stat = 'r', scale_by = 'indiv',
     eff_sub_effort = TRUE, eff_log_base = 2.8,
     eff_disp_pts = T,
     eff_disp_smooth = F)
dev.off()

# drop the next least populated site: DBSP & TRPA
ant_mob_in = subset(ant_mob_in, site != 'DBSP' & site != 'TRPA',
                    drop_levels = T)

deltas_highN = get_delta_stats(ant_mob_in, group_var = 'site', env_var = 'elevation_m',
                                type = 'continuous', n_perm = 19)    

g = plot(deltas_highN, 'elevation_m', stat = 'b1', scale_by = 'indiv')
ggsave('deltas_highN_b1.pdf', g, path = './figs', width = 20, height = 16,
       units = 'cm' )

# drop down to the 23 sites with more than 40 individuals
sub = tapply(rowSums(ant_mob_in$comm), list(ant_mob_in$env$site), sum)
sub = names(sub)[sub > 40]
ant_mob_in = subset(ant_mob_in, site %in% sub, drop_levels = T)

deltas_higherN = get_delta_stats(ant_mob_in, group_var = 'site', env_var = 'elevation_m',
                               type = 'continuous', n_perm = 199,
                               log_scale = T)

save(deltas_higherN, file = './results/deltas_higherN.Rdata')
#load('./results/deltas_higherN.Rdata')

g = plot(deltas_higherN, 'elevation_m', stat = 'b1', scale_by = 'indiv')
ggsave('deltas_higherN_b1.pdf', g, path = './figs', width = 20, height = 16,
       units = 'cm' )


# examine different explanatory variable npp
ant_mob_in = subset(ant_mob_in, !is.na(npp_g_c_m2), drop_levels = T)

deltas_npp = get_delta_stats(ant_mob_in, group_var = "site",
                             env_var = "npp_g_c_m2",
                             type = 'continuous', n_perm = 19, 
                             inds = )

deltas_lognpp = get_delta_stats(ant_mob_in, group_var = 'site', 
                                env_var = "log_npp",
                             type = 'continuous', n_perm = 199)

save(deltas_lognpp, file = './results/deltas_lognpp.Rdata')
#load('./results/deltas_lognpp.Rdata')

plot(deltas_npp, "npp_g_c_m2", stat = 'b1', scale_by = 'indiv') 
plot(deltas_lognpp, "log_npp", stat = 'b1', scale_by = 'indiv', log2 = 'x')

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
