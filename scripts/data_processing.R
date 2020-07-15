library(readxl)
library(janitor)
library(leaflet)
library(mapview)
library(tidyr)

# Objective: 
# to process the raw excel files provided by Nathan Sanders so that they can 
# be analyzed and shared on Dryad data repository. 

# Note: the following two data files have data on 7 more sites than 
# analyzed in Sanders et al. 2007 GEB
dat_comm <- read_xlsx('./data/180926-AllRawData.xlsx',
                     sheet = 'All')
dat_sites <- read_xlsx('./data/180926-AllRawData.xlsx',
                     sheet = 'SItes')
dat_splist <- read_xlsx('./data/smokies_species_list.xlsx')

# fix species names in dat_comm
sp_code <- dat_splist$sp_code[match(names(dat_comm)[-(1:4)], dat_splist$raw_name)]
names(dat_comm) <- c(names(dat_comm)[1:4], sp_code)

# clean and standardize column names 
dat_comm <- clean_names(dat_comm)
dat_sites <- clean_names(dat_sites)

# rename site to site_name so that a conflict does not arise
names(dat_sites)[1:2] <- c("site_name", "site")

# fix site variable in dat_comm
dat_comm$site <- ifelse(dat_comm$site == "CATALOOCHEE", "CATA",
                       dat_comm$site)

# fix typo in field called code in which one row was recorded as being sampled
# in 2007 (per email with Nathan Sanders)

dat_comm$code[grep('LS07', dat_comm$code)] 
dat_comm$code[grep('RAMY', dat_comm$code)] 
# only the A2 sample is listed with the 07 designation so change that to 06
dat_comm$code[grep('LS07', dat_comm$code)] <- "RAMYLS06A2"

# remove species that never occurs, Pheidole bicarinata
dat_comm <- subset(dat_comm, select = -pheibica)

# merge in env and coords from dat_sites
dat_all <- merge(dat_comm, dat_sites)

# compute new coordinates based upon sampling design---------
# see ./figs/plot_design.pdf

calc_coord <- function(x, y, subplot, sample) {
    for(i in seq_along(x)) {
        if (subplot[i] == "A" | subplot[i] == "C")
            x[i] = x[i] - 20
        else 
            x[i] = x[i] + 20
        if (subplot[i] == "A" | subplot[i] == "B")
            y[i] = y[i] + 20
        else
            y[i] = y[i] - 20
        if (sample[i] == 1 | sample[i] == 3) 
            x[i] = x[i] - 4.5
        else
            x[i] = x[i] + 4.5
        if (sample[i] == 1 | sample[i] == 2)
            y[i] = y[i] + 4.5
        else
            y[i] = y[i] - 4.5
    }
    return(data.frame(x, y))
}
    
tst_coords <- data.frame(x = rep(25,16),
                         y = rep(25, 16),
                         subplot = rep(c("A","B","C","D"), each=4),
                         sample = rep(1:4, 4))

new_coords <- with(tst_coords, calc_coord(x, y, subplot, sample))
centroids <- aggregate(new_coords, list(tst_coords$subplot), mean)

pdf('./figs/coord_convert_test.pdf')
plot(new_coords)
text(centroids[1, c('x', 'y')], labels = "A")
text(centroids[2, c('x', 'y')], labels = "B")
text(centroids[3, c('x', 'y')], labels = "C")
text(centroids[4, c('x', 'y')], labels = "D")
dev.off()

# inspect distribution of spatial distances of the plot design--------
dists <- dist(new_coords, method = "euclidean")

hist(dists)

par(mfrow = c(1,1))
dist.distr <- as.data.frame(table(dists))
dist.distr$dists <- as.character(dist.distr$dists)
dist.distr$dists <- as.numeric(dist.distr$dists)
plot(dist.distr)
pdf('./figs/distance_distribution.pdf')
plot(dist.distr)
dev.off()

# compute spatial UTM coordinates of the samples
sample_coords <- with(dat_all, calc_coord(utm_e, utm_n, subplot, sample))
names(sample_coords) <- paste('sample_', names(sample_coords), sep='')
dat_all <- cbind(dat_all, sample_coords)

# export cleaned merged data file for Dryad repository ---------------
write.csv(dat_all, file='./data/dryad/smokies_all.csv', row.names=F)

# plot site coordinates on map ------------------
m <- leaflet(data = site_dat) %>% 
       addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
       addMarkers(~e, ~n)
mapshot(m, file = paste0(getwd(), "/figs/sample_map.pdf"))

# there appears to be considerable heterogenity in the aspect, slope,
# and other topoedaphic features of these sites beyond just elevation

