library(mobr)
library(vegan)

# read in data files. 
ant_comm <- read.csv("data/comm_dat.csv", row.names = 1)
ant_plot_attr <- read.csv("data/site_dat.csv", row.names = 1)

# slightly change elev of RAMY b/c of delta_stats issue
ant_plot_attr$Elevation_m[ant_plot_attr$Site == 'RAMY'] = 942

ant_mob_in <- make_mob_in(ant_comm, ant_plot_attr)

stats <- get_mob_stats(ant_mob_in, group_var = "Site")
plot(stats)

alphas <- stats$samples_stats
alphas$Elevation <- ant_plot_attr$Elevation_m[match(alphas$group, ant_plot_attr$Site)]

#get Rsq values
lm_alpha<-alphas  %>% split(alphas$index) %>% map(filter,abs(value) < 1000) %>%  map(function(x) lm(value~Elevation,data=x))
Rsq_alpha<-lm_alpha %>% map_dbl(function(x)return(summary(x)$r.squared)) 

alphas %>%
  filter(abs(value) < 1000) %>%#some of the S_PIE blow up because of low numbers of individuals
  ggplot(aes(x=Elevation, y=value)) +
  geom_point(aes(col= group))+
  geom_smooth(method="lm", se = F, col= "black")+
  facet_wrap( . ~ index,scales = "free", labeller = labeller(index= function(x) {
    rsq=round(Rsq_alpha[x],4)
    return(paste(x,"; Rsq=",rsq, sep=""))}))
ggsave("alphas.pdf", path = "./figs", width = 20,height = 20, units = "cm")


gammas <- stats$groups_stats  
gammas$Elevation <- ant_plot_attr$Elevation_m[match(gammas$group, ant_plot_attr$Site)]

#get Rsq values
lm_gamma<-gammas  %>% split(gammas$index) %>% map(filter,abs(value) < 1000) %>%  map(function(x) lm(value~Elevation,data=x))
Rsq_gamma<-lm_gamma %>% map_dbl(function(x)return(summary(x)$r.squared)) 

gammas %>%
  filter(abs(value) < 1000) %>%
  ggplot(aes(Elevation, value)) +
    geom_point(aes(col= group)) +
    geom_smooth(method="lm", se= F, col= "black") +
  facet_wrap( . ~ index,scales = "free", labeller = labeller(index= function(x) {
    rsq=round(Rsq_gamma[x],4)
    return(paste(x,"; Rsq=",rsq, sep=""))}))
ggsave("gammas.pdf", path = "./figs", width = 20,height = 20, units = "cm")

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

# analysis of composition change
pdf('./figs/rda_results.pdf')
ant_rda = rda(sqrt(ant_comm) ~ Elevation_m, data = ant_plot_attr)
ant_rda
RsquareAdj(ant_rda)
anova(ant_rda, by = 'terms', permutations = 2e3)
plot(ant_rda, display = c('sp', 'bp'))
ant_mso = mso(ant_rda, ant_plot_attr[ , c('UTM_E'< 'UTM_N')], grain = 5000, 
              permutations = 1000)
msoplot(ant_mso)
dev.off()

pdf('./figs/cca_results.pdf')
elev = ant_plot_attr$Elevation_m[rowSums(ant_comm) > 0]
xy = ant_plot_attr[rowSums(ant_comm) > 0, c('UTM_E', 'UTM_N')]
ants = ant_comm[rowSums(ant_comm) > 0, ]
ant_cca = cca(sqrt(ants) ~ elev)
RsquareAdj(ant_cca)
anova(ant_cca, by = 'terms', permutations = 2e3)
plot(ant_cca, display = c('sp', 'bp'))
ant_mso_cca = mso(ant_cca, xy, grain = 5000, permutations = 1000)
msoplot(ant_mso_cca)
dev.off()
