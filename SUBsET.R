## Analysis code for SUBsET PAK by Schwalb et al. 2023
## Distributed under CC BY 4.0
## RScript: SUBsET.R

# Packages ==========
library(data.table) # Faster than data.frame
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
library(ggpubr) # Publication ready plots
library(skimr) # Skim through database
library(sf) # Support for simple features
library(patchwork) # Simpler plot composition 
library(RColorBrewer) # Colour palletes

# Load data ==========
PAK <- as.data.table(import(here("tool","SUBsET Pakistan.xlsx"), which = "Input 1", skip = 1)) # Initial data
PAKdb <- as.data.table(import(here("tool","SUBsET Pakistan.xlsx"), which = "Analysis")) # SUBsET outputs
PAKshp <- st_read("shp/PAK.shp") # District level
PAKreg <- st_read("shp/PAKreg.shp") # Regional level

# Data summary ==========
unique(PAK$district) # Districts = 150
skim(PAK) # Skim for missing and completeness rate
skim(PAK[PAK$region_id == "PK1",]) # Azad Jammu and Kashmir: No variable information
skim(PAK[PAK$region_id == "PK2",]) # Balochistan: 100% complete
skim(PAK[PAK$region_id == "PK3",]) # Gilgit/Baltistan: No variable information
skim(PAK[PAK$region_id == "PK4",]) # Islamabad: No info on room or food insecurity
skim(PAK[PAK$region_id == "PK5",]) # Khyber Pakhtun Khwa: 100% complete
skim(PAK[PAK$region_id == "PK6",]) # Punjab: 100% complete
skim(PAK[PAK$region_id == "PK7",]) # Sindh: 100% complete

summary(PAK)
summary(PAKdb)

# Shapefiles ==========
# a. Data curation
PAKshp <- PAKshp %>%
  select(ADM2_PCODE, geometry, Shape_Leng, Shape_Area) %>% 
  rename(district_id = ADM2_PCODE) %>% 
  inner_join(PAKdb, by = "district_id")

PAKshp$cdrcat <- cut(PAKshp$cdr, breaks=c(0, 0.25, 0.7, 1, Inf), 
                   labels=c("<25%","25-70%","70-100%",">100%"))
PAKshp$cdrregcat <- cut(PAKshp$cdr_reg, breaks=c(0, 0.25, 0.7, 1, Inf), 
                     labels=c("<25%","25-70%","70-100%",">100%"))

PAKreg <- PAKreg %>%
  select(ADM1_PCODE, geometry, Shape_Leng, Shape_Area) %>% 
  rename(region_id = ADM1_PCODE) %>% 
  inner_join(PAKdb, by = "region_id")

# b. Plots
PAKplot <- ggplot(data = PAKshp) +
  geom_sf(aes(fill = region), colour = "white") + 
  geom_sf(data = PAKreg, colour = '#3B3B3B', fill = NA, linewidth = 0.2) +
  scale_fill_manual(values = c(
    'Azad Jammu and Kashmir' = '#E78AC3',
    'Balochistan' = '#8DA0CB',
    'Gilgit-Baltistan' = '#66C2A5',
    'Islamabad' = '#F06060', 
    'Khyber Pakhtun Khwa' = '#FFD92F', 
    'Punjab' = '#A6D854', 
    'Sindh' = '#FC8D62'), 
    name = 'Region') +
  theme_void() +
  theme(legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0), 
        legend.margin = margin(t = 0, r = 10, b = 10, l = 10, unit = "pt"), 
        legend.background = element_rect(fill = "white", colour = NA)) 
tiff(here("Plots","01_regions.tiff"), width = 8, height = 8, units = 'in', res = 200)
print(PAKplot)
dev.off()

PAKinc <- ggplot(data = PAKshp) +
  geom_sf(aes(fill = inc_100k), colour = "white", linewidth = 0.1) + 
  geom_sf(data = PAKreg, colour = '#3B3B3B', fill = NA, linewidth = 0.2) +
  scale_fill_gradientn(name = "TB incidence per 100k", 
                       colours = c("#f7f3f7","#f4dede","#f2b1b1","#c38ec3","#5e4fa2","#3d2147"), 
                       limits = c(0, 500), 
                       breaks = c(0, 100, 200, 300, 400, 500)) +
  theme_void() +
  theme(legend.position = 'bottom', legend.title = element_text(size = 12),
        legend.text = element_text(size = 10), legend.key.width = unit(2, 'cm'),
        legend.key.height = unit(0.5, 'cm'), legend.spacing.x = unit(0.5, 'cm'),
        legend.box.margin = margin(t = 0, r = 10, b = 0, l = 0),
        legend.margin = margin(t = 5, r = 10, b = 5, l = 10))
tiff(here("plots","02_incidence_dist.tiff"), width = 8, height = 8, units = 'in', res = 200)
print(PAKinc)
dev.off()

PAKcdr <- ggplot(data = PAKshp) +
  geom_sf(aes(fill = cdrcat), colour = "white", linewidth = 0.1) +
  geom_sf(data = PAKreg, colour = '#3B3B3B', fill = NA, linewidth = 0.2) +
  scale_fill_manual(name = "Case detection rate", 
                    values = c("<25%"="#f2b1b1", "25-70%"="#b358b2", 
                               "70-100%"="#5e4fa2", ">100%"="#3d2147")) +
  theme_void() +
  theme(legend.position = c(0.90, 0.10),
        legend.justification = c(1, 0), 
        legend.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"), 
        legend.background = element_rect(fill = "white", colour = NA)) 
tiff(here("plots","03_cdr_dist.tiff"), width = 8, height = 8, units = 'in', res = 200)
print(PAKcdr)
dev.off()

# c. Region-specific plots
prov <- sort(unique(PAKshp$region)) # 7 provinces

for(i in 1:length(prov)){
  print(prov[i])
  
  pak <- PAKshp %>%
    filter(region == prov[i])
  
  pakreg <- PAKreg %>% 
    filter(region == prov[i])
  
  PKregplot <- ggplot(data = pak) +
    geom_sf(aes(fill = inc_100k), colour = "white", linewidth = 0.1) + 
    geom_sf(data = pakreg, colour = '#3B3B3B', fill = NA, linewidth = 0.2) +
    scale_fill_gradientn(name = "TB incidence per 100k", 
                         colours = c("#f7f3f7","#f4dede","#f2b1b1","#c38ec3","#5e4fa2","#3d2147"), 
                         limits = c(0, 500), 
                         breaks = c(0, 100, 200, 300, 400, 500)) +
    theme_void() +
    theme(legend.position = 'bottom', legend.title = element_text(size = 12),
          legend.text = element_text(size = 10), legend.key.width = unit(2, 'cm'),
          legend.key.height = unit(0.5, 'cm'), legend.spacing.x = unit(0.5, 'cm'),
          legend.box.margin = margin(t = 0, r = 10, b = 0, l = 0),
          legend.margin = margin(t = 5, r = 10, b = 5, l = 10))
  
  tiff(here("plots", "region", paste("incidence_", prov[i], ".tiff", sep = "")),  width = 7, height = 7, units = 'in', res = 100) 
  print(PKregplot)
  dev.off()
  
  PKcdrregplot <- ggplot(data = pak) +
    geom_sf(aes(fill = cdrcat), colour = "white", linewidth = 0.1) +
    geom_sf(data = pakreg, colour = '#3B3B3B', fill = NA, linewidth = 0.2) +
    scale_fill_manual(name = "Case detection rate", 
                      values = c("<25%"="#f2b1b1", "25-70%"="#b358b2", 
                                 "70-100%"="#5e4fa2", ">100%"="#3d2147")) +
    theme_void() +
    theme(legend.position = 'bottom',
          legend.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"), 
          legend.background = element_rect(fill = "white", colour = NA)) 
  
  tiff(here("plots", "region", paste("cdr_", prov[i], ".tiff", sep = "")),  width = 7, height = 7, units = 'in', res = 100) 
  print(PKcdrregplot)
  dev.off()
}

# d. Healthcare facilities
HCinc <- ggplot(PAKdb, aes(x = hc_fac_100k, y = inc_100k)) +
  geom_point(aes(colour = region)) +
  scale_colour_manual(values = c(
    'Azad Jammu and Kashmir' = '#E78AC3', 
    'Balochistan' = '#8DA0CB',  
    'Gilgit-Baltistan' = '#66C2A5', 
    'Islamabad' = '#F06060', 
    'Khyber Pakhtun Khwa' = '#FFD92F', 
    'Punjab' = '#A6D854', 
    'Sindh' = '#FC8D62')) +
  stat_smooth(method = "lm", col = "darkgrey", se = TRUE, linewidth = 0.8) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, 
           label.x = 3.75, label.y = 260) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, 1), expand = c(0,0)) +
  labs(x = "Number of healthcare facilities per 100k",
       y = "TB incidence per per 100k", colour = "Region") +
  theme_bw()
tiff(here("plots", "04_hcfacilities_incidence.tiff"), width = 8, height = 8, units = 'in', res = 200)
print(HCinc)
dev.off()

HCcdr <- ggplot(PAKdb, aes(x=hc_fac_100k, y=cdr)) +
  geom_point(aes(colour = region)) +
  scale_colour_manual(values = c(
    'Azad Jammu and Kashmir' = '#E78AC3', 
    'Balochistan' = '#8DA0CB',  
    'Gilgit-Baltistan' = '#66C2A5', 
    'Islamabad' = '#F06060', 
    'Khyber Pakhtun Khwa' = '#FFD92F', 
    'Punjab' = '#A6D854', 
    'Sindh' = '#FC8D62')) +
  stat_smooth(method = "lm", col = "darkgrey", se = TRUE, linewidth = 0.8) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, 
           label.x = 3.75, label.y = 1) +
  scale_y_continuous(limits = c(0, 3), labels = scales::percent, expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, 1), expand = c(0,0)) +
  labs(x = "Number of healthcare facilities per 100k",
       y = "Case detection rate", colour = "Region") +
  theme_bw()
tiff(here("plots", "04_hcfacilities_cdr.tiff"), width = 8, height = 8, units = 'in', res = 200)
print(HCcdr)
dev.off()

HC <- (HCinc + HCcdr) + 
  plot_layout(guides = 'collect')
tiff(here("plots", "04_hcfacilities.tiff"), width = 12, height = 8, units = 'in', res = 200)
print(HC)
dev.off()

# Sensitivity analysis
PAKdbSA <- as.data.table(import(here("tool","SUBsET Pakistan (SA).xlsx"), which = "Analysis")) %>% # SUBsET SA outputs
  mutate(type = 'sens')

SA <- PAKdb %>% 
  mutate(type = 'main') %>% 
  rbind(PAKdbSA) %>% 
  select(district_id, region_id, pop_1k, inc_100k, inc_100k_lo, inc_100k_hi, type) %>% 
  mutate(pop = pop_1k * 1e3) %>% 
  mutate(inc = inc_100k * pop / 1e5) %>% 
  pivot_wider(names_from = type, values_from = starts_with("inc")) %>% 
  mutate(inc_lo = inc_100k_lo_main * pop / 1e5, inc_hi = inc_100k_hi_main * pop / 1e5) %>% 
  select(district_id, region_id, pop, inc_main, inc_lo, inc_hi, inc_sens)

SAfig <- ggplot(SA) +
  geom_point(aes(x = inc_sens, y = inc_main), colour = "#0E4C92") +
  geom_errorbar(aes(x = inc_sens, ymin = inc_lo, ymax = inc_hi), colour = "#0E4C92", linewidth = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "#FF0000", linetype = "solid") +
  geom_abline(intercept = 0, slope = 1.1, color = "#FF0000", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 0.9, color = "#FF0000", linetype = "dashed") +
  scale_x_continuous(labels = scales::label_number(scale = 1e-3, suffix = 'K')) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = 'K')) +
  labs(x = "TB incidence allocated by population size",
       y = "TB incidence based on full model") +
  coord_cartesian(xlim = c(0, 35e3), ylim = c(0, 35e3), expand = FALSE) +
  theme_bw()
tiff(here("plots","05_sensanalysis.tiff"), width = 8, height = 8, units = 'in', res = 200)
print(SAfig)
dev.off()  

SAs <- SA %>% 
  mutate(above = inc_lo > 1.1 * inc_sens,
         below = inc_hi < 0.9 * inc_sens)
sum(SAs$above)
sum(SAs$below)
