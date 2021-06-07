### Master thesis (master thesis manuscript) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Date created:     2021-05-03
# Location created: Jurastrasse, Bern
# Author:           Raphael S. von Bueren (GitHub: vonbueren)
# Last Entry:       2021-06-07
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Description ----
# Figures and tables used in the thesis are created in this R-script.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Get started ----
rm(list = ls())
Sys.setenv(TZ = "GMT")
#graphics.off()                    # Clear Graphic Window
#cat( "\014" )                     # Clear Console ( =  CTRL L)

source( "R_SCRIPTS/1_CarexNardus2021_myfunctions.R" )
library(tidyverse)
library(reshape2)
library(Rmisc)
library(ggthemes)
library(lubridate)
library(vegan)
#devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)

theme_set(
  theme_clean() +
    theme(
      legend.title = element_blank(), 
      legend.position = "top", 
      legend.background = element_rect(colour = "white"))
)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Import data and adjustments ----
# Leakage data
excel_files         <- list.files(path = "DATA/raw_leakage", pattern = "*.xlsx", full.names = TRUE) 
leakage_list        <- sapply( excel_files, readxl::read_excel, na = "NA", simplify = FALSE, USE.NAMES = TRUE)  
names(leakage_list) <- gsub("DATA/raw_leakage/", "", names(leakage_list) ) # remove path from file names
eval.10             <- leakage_list$`Frost_Leakage_2020_08_17_Site9_11_12_Temp_8_11_13_15_18_Roehrchen_24h.xlsx`
eval.12             <- leakage_list$`Frost_Leakage_2020_08_25_Site8_10_11_Monol_MonolOrig_Temp_11_14_16_18_21_Roehrchen_24h.xlsx`

# R-created data frames (R_xxx_xxx.csv)
logger <- read_csv("DATA/R_GMT_iButtons.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), Time_GMT = col_datetime() ),
                   na = "NA")
therm  <- read_csv("DATA/R_pos_temperature.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), wintersnow19_GMT = col_datetime(), wintersnow20_GMT = col_datetime(), snowfree20_GMT = col_datetime(), snowmelt20_GMT = col_datetime()),
                   na = "NA")
ALPFOR <- read_csv("DATA/R_GMT_ALPFORclimate.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), Time_GMT = col_datetime() ),
                   na = "NA")
obs    <-  read_csv("DATA/R_pos_characteristics.csv",
                    col_names = T,
                    col_types = cols(.default = col_double(),
                                     macroexposure = col_factor(), exposure = col_factor(), topography = col_factor(), wind_edge = col_logical(),
                                     species = col_factor(), control_spec_1 = col_factor(), control_spec_2 = col_factor(), control_spec_3 = col_factor(),
                                     position_description = col_character()),
                    na = "NA")
soil   <- read_csv("DATA/R_pos_soil.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), sample_GMT = col_datetime() ),
                   na = "NA")
spec   <- read_csv("DATA/R_species_information.csv",
                   col_names = T,
                   col_types = cols(.default = col_factor(),
                                    "ID_species" = col_double(), "aID_SP" = col_double(), "InfoFlora" = col_double(),
                                    "T" = col_double(), "N" = col_double(), "L" = col_double(), "F" = col_double(),
                                    "H" = col_double(), "EG" = col_double(), "W" = col_double(), "R" = col_double(),
                                    "MV" = col_double(), "KL" = col_double(), "Best_Bu" = col_logical(), "FrSV" = col_double(),
                                    "WV" = col_double(), "TV" = col_double(), "FW" = col_double(), "sla" = col_double(),
                                    "ch" = col_double(), "sm" = col_double()),
                   na = "NA")
veg    <- read_csv("DATA/raw_pos_vegetation.csv",
                   col_names = T,
                   col_types = cols(.default = col_double()),
                   na = "NA")
EIV  <- read_csv("DATA/R_pos_EIV.csv",
                 col_names = T,
                 col_types = cols(.default = col_double()),
                 na = "NA")
frost         <- read_csv("DATA/R_frostmeasure_freezingresistance.csv",
                          col_names = T,
                          col_types = cols(.default = col_double(), type = col_factor(), sample = col_factor(),
                                           tissue = col_factor(), age = col_factor(), date = col_datetime(), Date = col_date()),
                          na = "NA")
frost$sample  <- frost$sample %>% fct_recode(Carex = "Cc", Nardus = "Ns")
vigfrost <- read_csv("DATA/raw_vigmeasurefrost_vigour.csv",
                     col_names = T,
                     col_types = cols(.default = col_double(), species = col_factor(), pot_description = col_factor(), 
                                      date_timeGMT = col_datetime(format = "%d.%m.%y %H:%M"), fungi_visual = col_factor(),
                                      new_shoot_within_dead_tussock = col_logical(), notes = col_character()),
                     na = "NA")
hair.harvest <- read_csv("DATA/raw_pos_hairdresser.csv",
                         col_names = T,
                         col_types = cols(.default = col_double(), Harvest_GMT = col_datetime(format = "%d.%m.%y %H:%M") ),
                         na = "NA")

# All observation data
obs.all <- obs %>%
  left_join(therm, by = "ID_position") %>% 
  left_join(EIV,   by = "ID_position") %>% 
  left_join(soil,  by = "ID_position") %>% 
  left_join(veg,   by = "ID_position") 
obs.all <- obs.all %>% dplyr::rename(Carex = Carexcurvula, Nardus = Nardusstricta)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Material & Methods: Numbers ----
as.factor(frost$ID_position) %>% levels %>% length                   # number of microsites with frost samples (incl. monoliths and origin)
frost %>% filter(sample == "Carex" | sample == "Nardus") %>%  nrow   # number of leakage curves (only Carex and Nardus)

# information for leakage curves (Carex_Orig2_ALT, eval.12 // Nardus_110, eval.10)
obs %>% filter(ID_position == 20)
frost %>% filter(Date == "2020-08-25") %>% filter(type == "origin") %>% filter(replic == 2) %>% filter(age == "old")
obs %>% filter(ID_position == 110)       
frost %>% filter(Date == "2020-08-17") %>% filter(type == "logger") %>% filter(ID_position == 110)
# names(leakage_list) <- gsub("DATA/raw_leakage/", "", names(leakage_list) ) # remove path from file names
# eval.10             <- leakage_list$`Frost_Leakage_2020_08_17_Site9_11_12_Temp_8_11_13_15_18_Roehrchen_24h.xlsx`
# eval.12             <- leakage_list$`Frost_Leakage_2020_08_25_Site8_10_11_Monol_MonolOrig_Temp_11_14_16_18_21_Roehrchen_24h.xlsx`

## Material & Methods: Leakage curves (one for Carex, one for Nardus) -> maybe add initial starting parameter and model output to plot ----
Nardus <- eval.10 %>% select(Temperature, c("Nardus_110_JUNG_1", "Nardus_110_JUNG_2", "Nardus_110_JUNG_3")) # k = -0.5, beta = 500
Carex  <- eval.12 %>% select(Temperature, c("Carex_Orig2_ALT_1", "Carex_Orig2_ALT_2"))                      # k = -0.5, beta = 500

# Choose data frame of interest (Carex or Nardus)
dat.1 <-  Carex
name  <- "Carex"

# Choose initial starting parameters
k     <- -0.5 # Gompertz
beta  <- 500  # Gompertz
x0    <- -10  # Boltzmann
dx    <- -1   # Boltzmann

# Create data frame
dat.2           <- reshape2::melt(dat.1, id = "Temperature")                                  # make long format
zero.leakage    <- mean(dat.2$value[dat.2$Temperature == max(dat.2$Temperature)], na.rm = T)
max.leakage     <- mean(dat.2$value[dat.2$Temperature == -30], na.rm = T)
percent.damage  <- (dat.2$value - zero.leakage)/(max.leakage - zero.leakage)*100              # calculate percent leakage
dat.3           <- na.omit(cbind(dat.2, percent.damage))

# GOMPERTZ: Estimate parameters "beta" and "k" (alpha = 100 --> maximum value)
nls.gompertz    <- minpack.lm::nlsLM( percent.damage ~ 100 * exp( -beta * exp(-k * Temperature )),
                                      data    = dat.3,
                                      start   = list(beta = beta, k = k),
                                      control = list(maxiter = 1000))

# BOLTZMANN: Estimate parameters "x0" and "dx"
maximum <- 100 # 100% is maximum leakage
minimum <- 0   # 0% is minimum leakage
nls.boltzmann <- minpack.lm::nlsLM(percent.damage ~ (maximum + (minimum - maximum)/(1 + exp((Temperature - x0) / dx ))),
                                   start   = list(x0 = x0, dx = dx), 
                                   data    = dat.3,
                                   control = list(maxiter = 1000))

# Create functions and LT50-values
beta   <-  coef(nls.gompertz)[["beta"]]
k      <-  coef(nls.gompertz)[["k"]]
x0     <-  coef(nls.boltzmann)[[1]]
dx     <-  coef(nls.boltzmann)[[2]]

fun.gompertz <- function(x){
  y =  100 * exp( -beta * exp(-k * x ))
  return(y)
}

fun.boltz <- function(x){
  y = maximum + (minimum - maximum) / (1 + exp( (x - x0) / dx )) 
  return(y)
}

boltz.LT50 <- x0 
gomp.LT50  <- (-log( (log(100) - log(50) ) / coef(nls.gompertz)[["beta"]]) / coef(nls.gompertz)[["k"]])  # see: Lim, Arora and Townsed 1998

# ggplot
color  <- ifelse(name == "Carex", "darkorange2", "olivedrab4")
letter <- ifelse(name == "Carex", "A", "B")
p <- ggplot2::ggplot() + 
  stat_function(fun = fun.gompertz, aes(colour = "Gompertz"),
                size = 3, alpha = 0.5, xlim = c(-5, 35)) +
  stat_function(fun = fun.boltz,    aes(colour = "Boltzmann"),
                size = 3, alpha = 0.5, xlim = c(-5, 35)) +
  geom_point(data = dat.3, aes(x = Temperature, y = percent.damage),
             size = 12, alpha = 0.5, col = color) +
  scale_colour_manual(values = c("blue", "red")) +
  geom_segment(aes(x = gomp.LT50,  xend = gomp.LT50,  y = 50, yend = -15), col = "red",   size = 1) +
  geom_segment(aes(x = boltz.LT50, xend = boltz.LT50, y = 50, yend = -15), col = "blue",  size = 1) +
  geom_segment(aes(x = 10, xend = min(gomp.LT50, boltz.LT50), y = 50, yend = 50), col = "black", size = 1) +
  geom_segment(aes(x = -20, xend = -16.3, y = 5, yend = -12), col = "black", size = 1, arrow = arrow(angle = 20, type = "closed")) + # only for Carex
  scale_x_reverse(   limits = c( 10, -40), breaks = seq(0,-30,-10)) +
  scale_y_continuous(limits = c(-15, 120), breaks = seq(0,100, 25)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Temperature (°C)", y = "Percent injury (%)") +
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 50)) +
  annotate("text", x = 7, y = 110, angle = 0,
                        label = name, fontface = "italic",
                        hjust = 0, parse = F, size = 20, col = color) +
  annotate("text", x = 10,  y = 120, hjust = 1.5, vjust = 0.5, label = letter, size = 20) +
  annotate("text", x = -26, y = 8.5,   label = "LT50", size = 15) + # only for Carex
  theme(plot.title        = element_text(size = 35, face = "italic", colour=c("black"), vjust = 0),
        axis.title.x      = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y      = element_text(size = 35), 
        axis.text.x       = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y       = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank())+
  theme(legend.position   = c(0.8, 0.4),
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 35, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) # +
  # theme(legend.position = "none") # only for Nardus
p
# ggsave(p, file = paste("LeakagePlot_", name, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 25, units = "cm")

# Model output
summary(nls.boltzmann)
summary(nls.gompertz)

boltz.LT50
gomp.LT50

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results (microsites and micro-climate): Table - descriptive (general, climate, soil, vegetation) and temperature records----
# general
microsites <- obs %>% filter(ID_position > 40)                    # only choose microsites of observational study
microsites %>% nrow                                               # number of microsites
microsites %>% select(elevation) %>% sapply(quantile, na.rm = T)
microsites %>% select(exposure_d) %>% sapply(quantile, na.rm = T)
microsites %>% select(slope) %>% sapply(quantile, na.rm = T)
microsites %>% select(soil_depth) %>% sapply(quantile, na.rm = T)
microsites %>% select(soil_moist) %>% sapply(quantile, na.rm = T)
microsites %>% select(wind_edge) %>% sapply(sum)                  # number of wind-edge positions

# micro-climate
microclimate <- therm %>% filter(ID_position > 40)                # only choose microsites of observational study
microclimate %>% select(Temp_min) %>% sapply(quantile, na.rm = T)
microclimate %>% select(Temp_max) %>% sapply(quantile, na.rm = T)
microclimate %>% select(GDH.0_MA) %>% sapply(quantile, na.rm = T)
microclimate %>% select(GDH.5_MA) %>% sapply(quantile, na.rm = T)
microclimate %>% select(FDH.0_OA) %>% sapply(quantile, na.rm = T)
microclimate %>% select(snowfree20_day) %>% sapply(quantile, na.rm = T)
microclimate %>% select(snowfree20_day, snowfree20_GMT) %>% filter(snowfree20_day == 316) # use days here from line before to know the date
microclimate %>% select(snowmelt20_day) %>% sapply(quantile, na.rm = T)
microclimate %>% select(snowmelt20_day, snowmelt20_GMT) %>% filter(snowmelt20_day == 220) # use days here from line before to know the date
microclimate %>% select(days_snow19_20) %>% sapply(quantile, na.rm = T)
microclimate %>% select(days_frozen) %>% sapply(quantile, na.rm = T)
microclimate %>% select(days_growper20.1) %>% sapply(quantile, na.rm = T)
microclimate %>% select(days_growper20.5) %>% sapply(quantile, na.rm = T)

# soil
micro.soil <- soil %>% filter(ID_position > 40)                   # only choose microsites of observational study
micro.soil %>% select(pH) %>% sapply(quantile, na.rm = T)
micro.soil %>% select(citricacid_soluble_P_mg_per_kg) %>% sapply(quantile, na.rm = T)
micro.soil %>% select(water_soluble_P_mg_per_kg) %>% sapply(quantile, na.rm = T)
micro.soil %>% select(C_mg_per_g) %>% sapply(quantile, na.rm = T)
micro.soil %>% select(N_mg_per_g) %>% sapply(quantile, na.rm = T)
micro.soil %>% select(C_N_ratio) %>% sapply(quantile, na.rm = T)
micro.soil %>% select(d13C) %>% sapply(quantile, na.rm = T)
micro.soil %>% select(d15N) %>% sapply(quantile, na.rm = T)

# vegetation
micro.veg <- replace(veg, veg > 0 & veg <= 1, 1)
micro.veg$spec.nr <- rowSums(micro.veg %>% select(-ID_position))
micro.veg %>% select(-ID_position, -spec.nr) %>% ncol             # total number of species
micro.veg %>% select(ID_position, spec.nr)                        # species per microsite
micro.veg %>% select(spec.nr) %>% sapply(quantile, na.rm = T)
nlevels(spec$family)
spec$functional_group_3 %>% table
micro.veg %>% filter(Carexcurvula == 1 & Nardusstricta == 1) %>% nrow
micro.veg %>% filter(Carexcurvula == 1 & Nardusstricta == 0) %>% nrow
micro.veg %>% filter(Carexcurvula == 0 & Nardusstricta == 1) %>% nrow
micro.veg %>% filter(Carexcurvula == 0 & Nardusstricta == 0) %>% nrow

# lowest temperatures
microclimate %>% select(Temp_min) %>% sapply(quantile, na.rm = T)
logger %>% select(Time_GMT, Temp41) %>% filter(Temp41 < -13) %>% data.frame
obs %>% filter(ID_position == 41)

ALPFOR %>% select(Time_GMT, TAIR1) %>%
  filter(Time_GMT > "2019-08-30") %>%
  filter(Time_GMT < "2020-10-10") %>% 
  select(Time_GMT, TAIR1) %>% filter(TAIR1 < - 16.1) %>% 
  data.frame

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results (microsites and micro-climate): Figure - three temperature curves (air weather station, 111, 119) ----
# Use these 4 lines for air temperature plot (weather station ALPFOR) --> but not for the two soil temperature plots
data        <- ALPFOR %>% select(Time_GMT, TempAir = TAIR1) %>%
  filter(Time_GMT > "2019-08-30") %>%
  filter(Time_GMT < "2020-10-10") %>% 
  data.frame
description <- "air (2 m)"
color       <- "darkblue"
i           <- 1 # Dirty: Use logger 1 for weather station because there only NAs for snow...

# Use these 8 lines for the ptwo plots for logger 111 and 119 (soil temperatures) --> but not for the air temperature plot
position.logger <- 111 # write logger number here (111 and 119)

description     <- ifelse(position.logger == 111, "wind-edge (-3 cm)", "snow-bed (-3 cm)")
color           <- "darkred"
i               <- 17-41 + position.logger
logger.d        <- as.data.frame(logger)
no.na           <- logger.d[!is.na(logger.d[ , (i+1) ]), ]    # remove NA's to draw nice line plot
data            <- no.na[ , c(1, (i+1)) ]

# Plot for each separately (air,111,119)
data$year <- year(data[ ,1])
year.start <- as.POSIXct("2019-08-30", format = "%Y-%m-%d", tz = "GMT")
year.end <- as.POSIXct("2020-10-10", format = "%Y-%m-%d", tz = "GMT")
min.tem  <- subset(data, data[ , 2] == min(data[ , 2]))
min.temp <- min.tem[1,] # take only first row (because sometimes, the same minimum value is reached multiple times)
p     <- ggplot(data, aes(x = data[ ,1], y = data[ , 2])) +
  # ggtitle(paste("Temperatures at logger position", substring(colnames(data)[2], 5,7))) +
  ggtitle("")+
  theme(plot.title = element_text(size = 30, face = "bold")) +
  ylim(-20,25) +
  labs ( x = "", y = paste("Temperature (°C)") ) +
  theme(axis.title.y   = element_text(size = 35, margin = margin(t = 0, r = 25, b = 0, l = 0)),
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 25, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  scale_x_datetime(date_breaks = "1 month",
                   limits = lubridate::ymd_h(c("2019-08-30 00", "2020-10-10 00")),
                   labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x),
                                                paste(lubridate::month(x, label = TRUE), "\n", year(x)),
                                                paste(lubridate::month(x, label = TRUE)))) +
  theme(panel.grid.major.x = element_line(colour = "grey", linetype = "dotted")) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_vline(xintercept = therm$snowfree20_GMT[i], colour = "lightblue", size = 1) +
  geom_vline(xintercept = therm$snowmelt20_GMT[i], colour = "purple", size = 1) +
  annotate("text", x = therm$snowfree20_GMT[i], y = 20.8, angle = 0,
           label = paste(" snow", "\n", "disappearance", sep = ""),
           hjust = -0.05, parse = F, size = 12, col = "lightblue") + # only for 111
  annotate("text", x = therm$snowmelt20_GMT[i], y = 25, angle = 0,
           label = paste("season start"),
           hjust = -0.05, parse = F, size = 12, col = "purple") +
  annotate("rect",
           xmin = therm$wintersnow19_GMT[i], xmax = as.POSIXct(therm$snowfree20_GMT[i]),
           ymin = -Inf, ymax = 0,  fill = "grey", alpha=.5) +
  annotate("text", x = as.POSIXct(therm$snowfree20_GMT[i]-((therm$snowfree20_GMT[i]-therm$wintersnow19_GMT[i])/2)),
           y = -15, size = 12, hjust = 0.5, color = "white",
           label = "snow") +
  geom_line(col = color, size = 0.7) +
  geom_point(aes(x = min.temp[ ,1], y = min.temp[ , 2]), color="blue", size = 12, shape = 1, stroke = 3) +
  annotate("text", x = as.POSIXct(min.temp[,1]), y = min.temp[,2],
           label = paste(round(min.temp[ , 2],1), "°C" ),
           hjust=ifelse(description == "air (2 m)", -0.15, 0), vjust=ifelse(description == "air (2 m)", 1, 2), size = 12, col = "blue") +
  annotate("text", x = year.start + 60*60*24*30*12.2, y = -15, label = description, size = 12, fontface = "bold.italic") +
  annotate("text", x = year.start, y = 25, hjust = 3.3, vjust = -0.2, 
           label = ifelse(description == "air (2 m)", "A", ifelse(description == "wind-edge (-3 cm)", "B", "C")),
           size = 15) +
  theme(panel.grid.major.x = element_line(colour = "transparent"),
        panel.grid.major.y = element_line(colour = "transparent")) +
  coord_cartesian(clip = "off")
p
# ggsave(p, file = paste0( colnames(data)[2], ".png"),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 46, height = 16, units = "cm" )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results (observational study): Figure - CCA ----
# Change column names of data frame *veg* from "genusspecies" to "genspec" (3+3 letters each)
fullname            <- colnames(veg) %>% data.frame %>% filter(. != "ID_position") %>% dplyr::rename("genusspecies" = ".")
allname             <- fullname %>% left_join(spec %>% select(genusspecies, genus, species, subspecies), by = "genusspecies")
allname$genspec     <- paste(substring(allname$genus, 1, 3), substring(allname$species, 1, 3), sep = "")
allname[allname$genus == "Ligusticum" & allname$species == "mutellinoides", "genspec"] <- "Ligmuo"
colnames(veg)[2:87] <- allname$genspec
veg.0.1             <- replace(veg, veg > 0 & veg <= 1, 1)   # change cover data to presence/absence data

# Preparation vegetation data
plants           <- veg %>% data.frame                # here, you could also try with veg.0.1
rownames(plants) <- plants$ID_position                # change column ID_positions to row names
plants           <- plants %>% select(-ID_position) 

# Preparation environmental data
env.all <- obs %>%
  left_join(therm, by = "ID_position") %>% 
  left_join(EIV,   by = "ID_position") %>% 
  left_join(soil,  by = "ID_position") %>% 
  filter(ID_position > 40) %>%
  data.frame
rownames(env.all) <- env.all$ID_position              # change column ID_positions to row names
env.all           <- env.all %>% select(-ID_position) 
str(env.all)
env.all$Temp_min  <- env.all$Temp_min*-1
env               <- env.all %>% select("Soil minimum temperature" = Temp_min,  "Growing degree hours (≥ 0 °C)" = GDH.0_MA,    # select variables of concern
                                        "Soil moisture" = soil_moist,
                                        "pH-value" = pH, "C/N-ratio" = C_N_ratio, "Phosphorus " = citricacid_soluble_P_mg_per_100g)     

ccamodel   <- vegan::cca(plants~., env, na.action = na.exclude ) %>% print
finalmodel <- vegan::ordistep(ccamodel, scope=formula(ccamodel)) %>% print

vegan::anova.cca(finalmodel)
vegan::anova.cca(finalmodel, by="terms")
vegan::anova.cca(finalmodel, by="axis")

# finalmodel  # proportion of constrained on total inertia -> variability explained by my constraining variables (environmental factors) --> 16.9%

axis1 <- round(finalmodel$CCA$eig[1]*100,1)
axis2 <- round(finalmodel$CCA$eig[2]*100,1)
p.CCA <- ggplot2::autoplot(finalmodel,
                           arrows = T,
                           loadings = T, loadings.colour = "green",
                  geom = "text", 
                  layers = c("biplot", "species")) +
  theme(legend.position = "none") +
  xlim(c(-3,2.3)) +
  scale_colour_manual(values = c("darkred")) +
  theme(axis.title.x   = element_text(size = 20, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 20), 
        axis.text.x    = element_text(size = 20, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 20, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) +
  xlab(paste("CCA1 (", axis1, " %)", sep = "")) +
  ylab(paste("CCA2 (", axis2, " %)", sep = ""))
p.CCA

# ggsave(p.CCA, file = "Figure4_CCA.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 31, height = 21, units = "cm")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results (observational study): Figure - Cover vs. Mintemp, Indval, Elevation ----
# Mintemp
obs.all.2 <- obs.all %>% filter(ID_position > 40)
df        <- reshape2::melt(obs.all.2[,c("Temp_min", "Carex", "Nardus")], id.vars = 1) %>%
  dplyr::rename(cover = value, species = variable)
p.min     <- ggplot(df, aes(x = Temp_min, y = cover,
                            color = factor(species),
                            shape = factor(species))) + 
  geom_point(size = 6, alpha = 0.5) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1.15),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = c(0.9, 0.9),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", linetype = "solid", colour = "grey"),
        legend.text = element_text(size = 30, face = "italic", margin = margin(r = 20)),
        legend.spacing.x = unit(0.3, 'cm')) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "Soil minimum temperature (°C)",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  scale_x_reverse(limits = c(1, -14), breaks = seq(-14,1,2)) +
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 0,  y = 0.9, hjust = 3.5, vjust = -1.7, label = "A", size = 15) +
  theme(legend.position = "none")
p.min

# ggsave(p.min, file = "Cover_Mintemp.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm")

# Indval
n.mean      <- c("T.0.1.mean", "N.0.1.mean", "L.0.1.mean", "F.0.1.mean", "H.0.1.mean", "R.0.1.mean")               # , "W.0.1.mean")
n.median    <- c("T.0.1.median", "N.0.1.median", "L.0.1.median", "F.0.1.median", "H.0.1.median", "R.0.1.median")   # , "W.0.1.median")
title       <- c("temperature", "nitrogen", "light", "moisture", "organic material", "acidity")                    # , "moisture variability")
EIV_CN      <- left_join(EIV, veg %>% select("ID_position", "Carcur", "Narstr")) %>% data.frame
EIV_CN_long <- reshape2::melt(EIV_CN[,c("ID_position", n.mean)], id.vars = 1) %>%   # HERE SWITCH BETWEEN MEAN AND MEDIAN (Median not good because e.g. mostly 1, 1.5 or 2 for Temp)
  dplyr::rename(EIV_value = value, EIV_type = variable) %>%  
  left_join(EIV_CN %>%
              select(ID_position, Carcur, Narstr) %>%
              dplyr::rename(Carex = Carcur, Nardus = Narstr)) %>% 
  gather("Carex", "Nardus",key = "species", value = "cover") %>% 
  tibble
p.ind <- ggplot(EIV_CN_long %>% filter(EIV_type == "T.0.1.mean"), aes(x = EIV_value, y = cover,
                                                                  col = species, shape = species)) +
  geom_point(size = 6, alpha = 0.5) +
  stat_smooth(method = "lm", size = 2, alpha = 1, se = F, show.legend = F, fullrange = F) +
  stat_smooth(method = "lm", size = 0.2, alpha = 0.25, se = T, show.legend = F,
              aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_x_continuous(limits = c(1,2), breaks = seq(1,2,0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(0,1.15),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = c(0.8,0.95),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", linetype = "solid", colour = "grey"),
        legend.text = element_text(size = 20, face = "italic", margin = margin(r = 10)),
        legend.spacing.x = unit(0.2, 'cm'))+
  theme(legend.key.height=unit(1.1,"cm")) +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(x = "Temperature indicator value",
       y = "Cover (%)") +
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 1,  y = 0.9, hjust = 2.25, vjust = -1.7, label = "B", size = 15) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(-0.05,1.15), clip = "off")
p.ind

# ggsave(p.ind, file = "Cover_Indval.Temp.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm")

EIV.T <- EIV_CN_long %>% filter(EIV_type == "T.0.1.mean")
EIV.T
fit <- with(EIV.T, lm(cover~EIV_value*species))
summary(fit)
confint(fit)

fitCarex <- with(EIV.T %>% filter(species == "Carex"), lm(cover~EIV_value))
summary(fitCarex)
confint(fitCarex)
EIV.T %>% filter(species == "Carex") %>% filter(!is.na(EIV_value)) %>% nrow
par(mfrow = c(2,2)); plot(fitCarex)

fitNardus <- with(EIV.T %>% filter(species == "Nardus"), lm(cover~EIV_value))
summary(fitNardus)
confint(fitNardus)
EIV.T %>% filter(species == "Nardus") %>% filter(!is.na(EIV_value)) %>% nrow
par(mfrow = c(2,2)); plot(fitNardus)

# Elevation
df <- reshape2::melt(obs.all.2[,c("elevation", "Carex", "Nardus")], id.vars = 1) %>%
  dplyr::rename(cover = value, species = variable)
p.ele <- ggplot(df,aes(x = elevation, y = cover,
                   color = factor(species),
                   shape = factor(species))) + 
  geom_point(size = 6, alpha = 0.5) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  stat_smooth(method = "loess", size = 2, alpha = 1, se = F, show.legend = F, span = 0.75) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F,
              aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_x_continuous(limits = c(2150, 2850),
                     breaks = seq(2200,2800,200)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1.15),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = c(0.27, 0.95), 
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", linetype = "solid", colour = "grey"),
        legend.text = element_text(size = 30, face = "italic", margin = margin(r = 20)),
        legend.spacing.x = unit(0.3, 'cm')) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "Elevation (m)",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 2200,  y = 0.9, hjust = 3.5, vjust = -1.7, label = "C", size = 15) +
  theme(legend.position = "none")
p.ele

# ggsave(p.ele, file = "Cover_Elevation.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results (freezing resistance analysis): Table - Leakage data descriptive (number of...) ----
frost.all <- frost %>%
  left_join(obs, by = "ID_position")
CCNS <- frost.all %>% filter(sample == "Carex" | sample == "Nardus")
frost.all %>% summary
frost.all %>% filter(sample == "Carex") %>% select(age) %>% summary
frost.all %>% filter(sample == "Carex" & tissue == "leaf") %>% select(LT50.bolt, LT50.gomp) %>% summary
frost.all %>% filter(sample == "Nardus"& tissue == "leaf") %>% select(LT50.bolt, LT50.gomp) %>% summary

CCNS.lom.oy <- frost.all %>%    # Carex (CC) and Nardus (NS) --> NO other species // logger (l), origin (o) and monoliths (m) // old (o) and young (y)
  filter(tissue == "leaf") %>% 
  filter(sample == "Carex" | sample == "Nardus") %>% 
  filter(age == "young" | age == "old") %>% 
  droplevels

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results (freezing resistance analysis): Models: Tissue type (flower, leaf) and tissue age (Carex old, young) ----
# Tissue type
CCNS <- frost.all %>% filter(sample == "Carex" | sample == "Nardus")
CCNS %>% select(tissue, sample) %>% table
df <- CCNS
df_CI <- df %>%
  group_by(tissue, sample) %>%
  dplyr::summarise(
    LT50 = mean(LT50.gomp),
    CI = Rmisc::CI(LT50.gomp)[1]-Rmisc::CI(LT50.gomp)[2])

p <- ggplot(df_CI, aes(x = tissue, y = LT50,
                       color = factor(sample))) +
  geom_pointrange( aes(ymin = LT50-CI, ymax = LT50+CI),
                   position = position_dodge(0.2),
                   size = 1, alpha = 1, show.legend = F) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_y_continuous(limits = c(-18, -6), breaks = seq(-8, -17, -3)) +
  labs(x = "",
       y = "LT50 (°C)") +
  coord_cartesian(clip = "off") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 2.4,  y = -7, hjust = 0, vjust = 0, label = "A", size = 20) # +
# annotate("text", x = 0.88,  y = -6, hjust = 0, vjust = 0, label = "n = 135", size = 8) +
# annotate("text", x = 1.88,  y = -6, hjust = 0, vjust = 0, label = "n = 5", size = 8) 
p

# ggsave(p, file = "LT50_tissue.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 15, height = 20, units = "cm")

fit <- with(df, t.test(LT50.gomp~tissue, alternative = "two.sided", var.equal = F))
fit
(fit$estimate[2]-fit$estimate[1])
df$tissue %>% table

# Carex age
compareAGE <- CCNS %>%
  select(age, age_compare, LT50.gomp) %>%
  # filter(age_compare != 26) %>% 
  filter(!is.na(age_compare)) %>% data.frame
compareAGE$age_compare <- as.integer(compareAGE$age_compare)
compareAGE$age         <- as.character(compareAGE$age)
compareAGE             <- compareAGE %>% arrange(desc(age))
wide <- compareAGE %>% select(age_compare, age, LT50.gomp) %>% spread(key = age, value = LT50.gomp)
test <- with(wide, t.test(young, old, paired = TRUE))
main <- paste("leaves__LT50_vs_Age_Carex__", title, sep = "")
p <- ggpubr::ggpaired(compareAGE, x = "age", y = "LT50.gomp",
                      color = "age", line.color = "gray", line.size = 0.4) +
  scale_color_manual(breaks = c("young", "old"),
                     values = c("grey40", "grey40")) +
  # ggpubr::stat_compare_means(paired = TRUE, method = "t.test") +
  theme(legend.position = "none") +
  xlab("") +
  ylab("LT50 (°C)") +
  ggtitle(main) +
  labs(subtitle = paste("difference: ",
                        round(test$estimate,1),
                        "K  //  ", "95%-CI: [",
                        round(test$conf.int[1],1),
                        ",",
                        round(test$conf.int[2],1),
                        "] // n = ",
                        nrow(compareAGE)/2,
                        " // p = ",
                        round(test$p.value, 4),
                        " (pairwise t-test)",
                        sep = ""))
p
# ggsave(p, file = "LT50_CAREX_age.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 15, height = 20, units = "cm")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results (freezing resistance analysis): Figure - LT50 vs. species (confidence interval), LT50 vs. GDH10 and vs. DSS ----
# Only Carex and Nardus leaves
CCNS <- CCNS.lom.oy
CCNS %>% select(tissue, sample) %>% table
df <- CCNS
df_CI <- df %>%
  group_by(tissue, sample) %>%
  dplyr::summarise(
    LT50 = mean(LT50.gomp),
    CI = Rmisc::CI(LT50.gomp)[1]-Rmisc::CI(LT50.gomp)[2])
df_CI <- df_CI %>% filter(tissue == "leaf")
p <- ggplot(df_CI, aes(x = sample, y = LT50,
                       color = factor(sample))) +
  geom_pointrange( aes(ymin = LT50-CI, ymax = LT50+CI),
                   position = position_dodge(0.2),
                   size = 2, alpha = 1, show.legend = F) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_x_discrete(expand = c(0.9, 0.1)) +
  scale_y_continuous(limits = c(-17, -12.4), breaks = seq(-13, -16, -1)) +
  theme(panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "LT50 (°C)") +
  coord_cartesian(clip = "off") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0), face = "italic"),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(plot.margin = margin(t = 50, r = 50, b = 20, l = 20)) +
  annotate("text", x = 0,  y = -12.8, hjust = 1.5, vjust = -1, label = "A", size = 17)
p
# ggsave(p, file = "LT50_sample.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 20, height = 20, units = "cm")

leaves <- CCNS
table(leaves$sample)
fit <- with(leaves, t.test(LT50.gomp~sample))
fit
df_CI$LT50-df_CI$CI
df_CI$LT50+df_CI$CI
fit$estimate[2]-fit$estimate[1]

# LT50 vs. GDH.0_10d (Carex)
CCNS.sub <- CCNS.lom.oy %>% filter(DSS > 10) %>% filter(sample == "Carex") # below snow GDH = 0 -> just use data collected at least 10 days after snow melt
fit <- lm(LT50.gomp~GDH.0_10d, data = CCNS.sub)
par(mfrow = c(2,2)); plot(fit)
label <-  bquote(italic(adj.R)^2 == .(format(summary(fit)$adj.r.squared, digits = 2)))
table(CCNS.sub$sample, !is.na(CCNS.sub$GDH.0_10d))
p <- ggplot(CCNS.sub, aes(x = GDH.0_10d, y = LT50.gomp, col = sample, shape = sample))+
  geom_point(size = 6, alpha = 0.5) +
  stat_smooth(method = "lm", size = 2, alpha = 1, se = F, show.legend = F, fullrange = F) +
  stat_smooth(method = "lm", size = 0.2, alpha = 0.25, se = T, show.legend = F) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("Carex", "Nardus"),
                     values = c(16,17)) +
  scale_x_continuous(limits = c(1900, 4000),
                     breaks = seq(2000,4000,1000)) +
  scale_y_continuous(limits = c(-23, -6),
                     breaks = seq(-10, -25, -5)) +
  coord_cartesian(clip = "off") +
  labs(x = paste("GDH (≥ 0 °C) of 10 days"),       # \n within 10 days before sampling"),
       y = "LT50 (°C)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(plot.margin = margin(t = 50, r = 50, b = 20, l = 20)) +
  annotate("text", x = 2000,  y = -7, hjust = 2.5, vjust = -0.5, label = "B", size = 17) +
  annotate("text", x = 3200,  y = -7, hjust = 0, vjust = 0,
           label =  label,
           size = 8, fontface = "italic") +
  annotate("text", x = 3500,  y = -22.5, hjust = 0, vjust = 0, label = "Carex", size = 12, fontface = "italic", color = "darkorange2") +
  theme(legend.position = "none")
p
# ggsave(p, file = "LT50_GDH0.10.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 20, height = 20, units = "cm" )


# LT50 vs. DSS (Nardus)
CCNS.sub <- CCNS.lom.oy %>% filter(sample == "Nardus")
fit <- lm(LT50.gomp~DSS, data = CCNS.sub)
par(mfrow = c(2,2)); plot(fit)
label <-  bquote(italic(adj.R)^2 == .(format(summary(fit)$adj.r.squared, digits = 2,nsmall = 2)))
table(CCNS.sub$sample, !is.na(CCNS.sub$DSS))
p <- ggplot(CCNS.sub, aes(x = DSS, y = LT50.gomp, col = sample, shape = sample))+
  geom_point(size = 6, alpha = 0.5) +
  stat_smooth(method = "lm", size = 2, alpha = 1, se = F, show.legend = F, fullrange = F) +
  stat_smooth(method = "lm", size = 0.2, alpha = 0.25, se = T, show.legend = F) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("Carex", "Nardus"),
                     values = c(16,17)) +
  scale_x_continuous(limits = c(0, 200),
                     breaks = seq(30,180,60)) +
  scale_y_continuous(limits = c(-23, -6),
                     breaks = seq(-10, -25, -5)) +
  coord_cartesian(clip = "off") +
  labs(x = paste("DSS"),       # \n within 10 days before sampling"),
       y = "LT50 (°C)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(plot.margin = margin(t = 50, r = 50, b = 20, l = 20)) +
  annotate("text", x = 0,  y = -7, hjust = 2, vjust = -0.5, label = "C", size = 17) +
  annotate("text", x = 130,  y = -7, hjust = 0, vjust = 0,
           label =  label,
           size = 8, fontface = "italic") +
  annotate("text", x = 140,  y = -22.5, hjust = 0, vjust = 0, label = "Nardus", size = 12, fontface = "italic", color = "olivedrab4") +
  theme(legend.position = "none")
p
# ggsave(p, file = "LT50_DSS.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 20, height = 20, units = "cm" )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results: Table (and some figures) - frost model GDH and min temp (different intervals) ----
# Overview plots including model output
CCNS <- CCNS.lom.oy
c <- c(1,3,5,7,10,14,20)
vector <- c(paste("GDH.0_", c, "d", sep = ""), paste("min_", c, "d", sep = ""), "DOY", "DSS")
dat <- data.frame()
for (i in 1: length(vector)){
  CCNS.sub <- if(i >= 15) {
    CCNS
  } else {
    CCNS %>% filter(DSS > rep(c, 2)[i])
  }
  main     <- paste("leaves__LT50_vs_", vector[i], "__CCNS.lom.oy", sep = "")
  formula <- paste("LT50.gomp ~ ", vector[i], sep = "")
  fitCC <- with(CCNS.sub %>% filter(sample == "Carex"), lm(as.formula(formula)))
  fitNS <- with(CCNS.sub %>% filter(sample == "Nardus"), lm(as.formula(formula)))
  p        <- ggplot(CCNS.sub, aes_string(x = vector[i], y = "LT50.gomp", col = "sample", shape = "sample"))+
    geom_point(size = 6, alpha = 0.5) +
    stat_smooth(method = "lm", size = 2, alpha = 1, se = F, show.legend = F, fullrange = F) +
    stat_smooth(method = "lm", size = 0.2, alpha = 0.25, se = T, show.legend = F) +
    scale_color_manual(breaks = c("Carex", "Nardus"),
                       values = c("darkorange2", "olivedrab4")) +
    scale_y_continuous(limits = c(-23, -6),
                       breaks = seq(-10, -25, -5)) +
    coord_cartesian(clip = "off") +
    labs(x = if(i <= 7) {
      paste("Growing degree hours (≥ 0 °C)")
    } else if(i <= 14) {
      paste("Minimum temperature (°C)")
    } else if(i <= 15) {
      paste("DOY")
    } else {
      paste("DSS")
    },
    y = "LT50 (°C)") +
    theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
          axis.title.y   = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)), 
          axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.ticks.length = unit(10, "pt")) +
    theme(panel.grid.major.y = element_blank()) +
    theme(plot.margin = margin(t = 50, r = 20, b = 20, l = 20)) +
    theme(legend.position = "none") +
    ggtitle(label = main,
            subtitle = paste("Carex: adj R^2 = ", round(summary(fitCC)$adj.r.squared, 2), " / ",
                             "slope = ", round(dummy.coef(fitCC)[[2]], 3), " / ",
                             "CI = " , round(confint(fitCC)[[2,1]], 3), ", ", round(confint(fitCC)[[2,2]], 3), " / ",
                             "p = ", round(summary(fitCC)$coefficients[2,4], 7), " / ",
                             "BIC = ", round(BIC(fitCC), 1), "\n",
                             "Nardus: adj R^2 = ", round(summary(fitNS)$adj.r.squared, 2), " / ",
                             "slope = ", round(dummy.coef(fitNS)[[2]], 3), " / ",
                             "CI = " , round(confint(fitNS)[[2,1]], 3), ", ", round(confint(fitNS)[[2,2]], 3), " / ",
                             "p = ", round(summary(fitNS)$coefficients[2,4], 7), " / ",
                             "BIC = ", round(BIC(fitNS), 1),
                             sep = ""))
  p
  dat[i, "predictor"] <- vector[i]
  dat[i, "CC_slope"]  <- round(dummy.coef(fitCC)[[2]], 4)
  dat[i, "CC_CI_low"] <- round(confint(fitCC)[[2,1]], 4)
  dat[i, "CC_CI_up"]  <- round(confint(fitCC)[[2,2]], 4)
  dat[i, "CC_p"]      <- round(summary(fitCC)$coefficients[2,4], 7)
  dat[i, "CC_n"]      <- CCNS.sub %>% filter(sample == "Carex") %>% nrow
  dat[i, "CC_adjR2"]  <- round(summary(fitCC)$adj.r.squared, 2)
  dat[i, "NS_slope"]  <- round(dummy.coef(fitNS)[[2]], 4)
  dat[i, "NS_CI_low"] <- round(confint(fitNS)[[2,1]], 4)
  dat[i, "NS_CI_up"]  <- round(confint(fitNS)[[2,2]], 4)
  dat[i, "NS_p"]      <- round(summary(fitNS)$coefficients[2,4], 7)
  dat[i, "NS_adjR2"]  <- round(summary(fitNS)$adj.r.squared, 2)
  dat[i, "NS_n"]      <- CCNS.sub %>% filter(sample == "Nardus") %>% nrow
  # ggsave(p, file = paste("Overview__", main, ".png", sep = ""),
  #        path = "R_PLOTS/_FIGURES_THESIS/GDH_and_min",
  #        width = 25, height = 20, units = "cm" )
}
dat
# write.csv(dat, file = "R_PLOTS/_FIGURES_THESIS/GDH_and_min/LT50_predictors.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results (freezing resistance analysis): Model - freezing resistance model (including BIC model comparison)----
# Carex
CC <- CCNS %>% filter(sample == "Carex")
data <- CC %>% filter(DSS > 10)
fit1 <- lmerTest::lmer(LT50.gomp ~                         (1|elevation),                          data = data)
fit2 <- lmerTest::lmer(LT50.gomp ~ DOY +                   (1|elevation),                          data = data)
fit3 <- lmerTest::lmer(LT50.gomp ~ DSS +                   (1|elevation),                          data = data)
fit4 <- lmerTest::lmer(LT50.gomp ~ GDH.0_10d +             (1|elevation),                          data = data)
fit5 <- lmerTest::lmer(LT50.gomp ~ DSS + GDH.0_10d +       (1|elevation),                          data = data)
fit6 <- lmerTest::lmer(LT50.gomp ~ DOY + GDH.0_10d +       (1|elevation),                          data = data)
fit7 <- lmerTest::lmer(LT50.gomp ~ DOY + DSS +             (1|elevation),                          data = data)
fit8 <- lmerTest::lmer(LT50.gomp ~ DOY + DSS + GDH.0_10d + (1|elevation),                          data = data)
BIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)
summary(fit4)
confint(fit4, oldNames = F)
confint(fit4, oldNames = F)*1000 # for 1000 GDH
plot(fit4) # TA-plot
par(mfrow = c(1,2))
qqnorm(ranef(fit4)$elevation[, 1], main = "Random effects of site")
qqnorm(resid(fit4), main = "Residuals")
par(mfrow = c(1,1))

# Nardus
NS <- CCNS %>% filter(sample == "Nardus")
data <- NS %>% filter(DSS > 1)
fit1 <- lmerTest::lmer(LT50.gomp ~                         (1|elevation),                          data = data)
fit2 <- lmerTest::lmer(LT50.gomp ~ DOY +                   (1|elevation),                          data = data)
fit3 <- lmerTest::lmer(LT50.gomp ~ DSS +                   (1|elevation),                          data = data)
fit4 <- lmerTest::lmer(LT50.gomp ~ GDH.0_1d +              (1|elevation),                          data = data)
fit5 <- lmerTest::lmer(LT50.gomp ~ DSS + GDH.0_1d +        (1|elevation),                          data = data)
fit6 <- lmerTest::lmer(LT50.gomp ~ DOY + GDH.0_1d +        (1|elevation),                          data = data)
fit7 <- lmerTest::lmer(LT50.gomp ~ DOY + DSS +             (1|elevation),                          data = data)
fit8 <- lmerTest::lmer(LT50.gomp ~ DOY + DSS + GDH.0_1d +  (1|elevation),                          data = data)
fit9 <- lmerTest::lmer(LT50.gomp ~ DSS * GDH.0_1d +        (1|elevation),                          data = data)
BIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9)
summary(fit5)
confint(fit5, oldNames = F)*100 # for 100 GDH
confint(fit5, oldNames = F)*30  # for 30 days 
plot(fit5) # TA-plot
par(mfrow = c(1,2))
qqnorm(ranef(fit5)$elevation[, 1], main = "Random effects of site")
qqnorm(resid(fit5), main = "Residuals")
par(mfrow = c(1,1))

# Package citation
citation("lmerTest")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Results (freezing resistance analysis): Figure - Survival experiment (growth rate vs. freezing temperature) ----
treatment <- data.frame(ID_Temp        = c(2,  -12,   -15,   -18,   -21,   -24,   -27, -30),
                        treatment_temp = c(2, -7.8, -12.3, -18.2, -21.1, -23.5, -26.9, -30))
vigfrost$treatment_temp <- treatment$treatment_temp[match(vigfrost$ID_Temp, treatment$ID_Temp)]   

# Add date as "day after snow removal"
vigfrost$day <- as.double(difftime(vigfrost$date_timeGMT, as.POSIXct("2021-02-18 10:00"), units = "days"))

# Extract growth rate as linear function
growth <- function(x, y){
  coef(glm(y ~ x, family = "gaussian"))[2]
}
pots <- vigfrost %>% dplyr::filter(ID_pot <= 130) %>% 
  group_by(ID_pot, treatment_temp, species, replicate, pot_description, palette) %>%
  dplyr::summarize(growth = growth(x = day, y = leaf_g_mean)) 
pots$species <- pots$species %>% recode_factor(Cc = "Carex", Ns = "Nardus")

# Plot growth rates without negative growth (boxplots)
pots$growth_no_neg <- as.numeric(ifelse(pots$growth > 0.02, pots$growth, 0))
p <- ggplot(pots, aes(x = treatment_temp,
                      y = growth_no_neg*10, # conversion from cm to mm
                      color = factor(species),
                      shape = factor(species)))+
  geom_point(size = 6, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", aes(group = species), size = 2, show.legend = FALSE) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_x_reverse(limits = c(5, -33),
                  breaks = seq(-30,0,10))+
  scale_y_continuous(limits = c(-0.1, 2.2),
                     breaks = seq(0, 2, 1)) +
  theme(legend.position = c(0.85, 0.85),
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 30, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.5,"cm")) +
  coord_cartesian(clip = "off") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(plot.margin = margin(t = 50, r = 20, b = 20, l = 20)) +
  labs(x = "Frost treatment (°C)",
       y = expression("Leaf growth (mm day"^-1*")")) +
  annotate("text", x = 2,  y = 2, hjust = 5, vjust = -1.7, label = "C", size = 15) 
p
# ggsave(p, file = "Survival_experiment.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 35, height = 25, units = "cm" )
 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Supplementary material: Table - Vegetation (species) ----
veg    <- read_csv("DATA/raw_pos_vegetation.csv",
                   col_names = T,
                   col_types = cols(.default = col_double()),
                   na = "NA")
veg.0.1 <- replace(veg, veg > 0 & veg <= 1, 1)   # change cover data to presence/absence data
abundance <- veg.0.1 %>%
  select(-ID_position) %>% 
  colSums() %>%
  data.frame %>% 
  tibble::rownames_to_column("row_names")
colnames(abundance) <- c("genusspecies", "microsites")
abundance$microsites_percent <- round((abundance$microsites/nrow(veg.0.1))*100,0)
abundance$cover <- round((veg %>% select(-ID_position) %>% colSums)/115*100, 2)
abundance <-  abundance %>% arrange(-cover)
veg.survey <- abundance %>% left_join(spec, by = "genusspecies")
veg.survey$name <- with(veg.survey, (paste(genus,
                                           paste("",species),
                                           ifelse(is.na(subspecies), "", paste(" ssp.", subspecies)),
                                           ifelse(is.na(author_citation), "", paste("", author_citation)),
                                           sep = "")))
veg.survey$abundance <- with(veg.survey, paste(microsites, " (", microsites_percent, " %)", sep = ""))
veg.survey$cover <- with(veg.survey, paste(cover, " %", sep = ""))
veg.survey <- veg.survey %>% select(name, family, functional_group = functional_group_3, cover, abundance) %>% print

# write.csv(veg.survey, "R_PLOTS/_FIGURES_THESIS/abundance_table.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Supplementary material: Figure - Indicator values (mean) ----
levels(EIV_CN_long$EIV_type)       <- title
EIV_CN_long                        <- EIV_CN_long %>% left_join(obs %>% select(ID_position, wind_edge), by = "ID_position")
EIV_CN_long$wind_edge              <- as.factor(EIV_CN_long$wind_edge)
EIV_CN_long$wind_edge <- relevel(EIV_CN_long$wind_edge,  "TRUE")
levels(EIV_CN_long$wind_edge) <- c("wind-edge", "not wind-edge")

all_data     <- EIV_CN_long
no_windedges <- EIV_CN_long %>% filter(wind_edge == "not wind-edge")

# make ggplot with both data frames: "all_data" and "no_windedges"
plot    <-  all_data     # choose one of the two
header  <- "all_data"    # choose one of the two

# labels
left           <- data.frame( label = c("cold", "infertile", "shade", "dry", "bare soil", "acidic"), EIV_type = title)
right          <- data.frame( label = c("warm", "fertile", "full light", "wet", "raw humus", "alkaline"), EIV_type = title)

p <- ggplot(plot, aes(x = EIV_value, y = cover)) +
  facet_grid(rows = vars(factor(EIV_type))) +  # trick here with factor - otherwise sorted alphabetically
  geom_point(aes(col = species, shape = wind_edge), size = 4, alpha = 0.5) +
  stat_smooth(aes(col = species), method = "loess", size = 1.5, alpha = 1, se = F, show.legend = F, span = 1) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F, span = 1,
              aes(col = species, ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("wind-edge", "not wind-edge"),
                     values = c(8, 18)) +
  scale_x_continuous(limits = c(1,5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1),
                     breaks = seq(0,1,0.5)) +
  geom_text(data = left,  mapping = aes(x = 1.3, y = 0.92, label = label), size = 8, fontface = "italic") +
  geom_text(data = right, mapping = aes(x = 4.7, y = 0.92, label = label), size = 8, fontface = "italic") +
  theme(legend.position = "top",
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 20, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.1,"cm")) +
  theme(axis.title.x   = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 25, margin = margin(t = 0, r = 18, b = 0, l = 0)), 
        axis.text.x    = element_text(size = 25, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 25, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt"),
        strip.text.y = element_text(size = 25, colour = "black", face = "bold")) +
  theme(panel.grid.major.y = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA, size = 1.5)) +
  labs(x = "Indicator value (Landolt)",
       y = "Cover (%)")
p
# ggsave(p, file = paste("FigureS1_Cover_Indval.All_mean_", header, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 30, height = 50, units = "cm" )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Supplementary material: Figure - Cover vs. Growing season (≥ 5°C), GDH (≥ 0°C), soil moisture, soil depth, pH, C/N, phosphorus ----
# Growing conditions (≥ 5 °C)
obs.all <- obs.all %>% filter(ID_position > 30) # Friseurexperiment und Monolithenexperiment nicht nehmen
df      <- reshape2::melt(obs.all[,c("ID_position", "days_growper20.5")], id.vars = 1) %>%   # HERE SWITCH BETWEEN MEAN AND MEDIAN (Median not good because e.g. mostly 1, 1.5 or 2 for Temp)
  dplyr::rename(days_growing_conditions = value, type = variable) %>%  
  left_join(obs.all %>%
              select(ID_position, Carex, Nardus) %>%
              dplyr::rename(Carex = Carex, Nardus = Nardus)) %>% 
  gather("Carex", "Nardus",key = "species", value = "cover") %>% 
  tibble %>% 
  left_join(obs %>% select(ID_position, wind_edge), by = "ID_position")
df$wind_edge              <- as.factor(df$wind_edge)
df$wind_edge <- relevel(df$wind_edge,  "TRUE")
levels(df$wind_edge) <- c("wind-edge", "not wind-edge")

all_data     <- df
no_windedges <- df %>% filter(wind_edge == "not wind-edge")

# make ggplot with both data frames: "all_data" and "no_windedges"
plot    <-  all_data     # choose one of the two
header  <- "all_data"    # choose one of the two

p.grow <- ggplot(plot, aes(x = days_growing_conditions, y = cover)) + 
  geom_point(aes(col = species, shape = wind_edge), size = 8, alpha = 0.5) +
  stat_smooth(aes(col = species), method = "loess", size = 2, alpha = 1, se = F, show.legend = F, span = 1) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F, span = 1,
              aes(col = species, ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("wind-edge", "not wind-edge"),
                     values = c(8, 18)) +
  scale_x_continuous(limits = c(70, 200), breaks = seq(60,180,30)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = "top",
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 20, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "Days with growing conditions (≥ 5 °C)",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 195,  y = 0.9, hjust = 0, vjust = 0, label = "A", size = 20) +
  theme(legend.position = "none")
p.grow
# ggsave(p.grow, file = paste("Cover_GrowCond5_", header, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm" )

# Growing degree hours (≥ 0 °C)
obs.all <- obs.all %>% filter(ID_position > 30) # Friseurexperiment und Monolithenexperiment nicht nehmen
df      <- reshape2::melt(obs.all[,c("ID_position", "GDH.0_MA")], id.vars = 1) %>%   # HERE SWITCH BETWEEN MEAN AND MEDIAN (Median not good because e.g. mostly 1, 1.5 or 2 for Temp)
  dplyr::rename(GDH = value, type = variable) %>%  
  left_join(obs.all %>%
              select(ID_position, Carex, Nardus) %>%
              dplyr::rename(Carex = Carex, Nardus = Nardus)) %>% 
  gather("Carex", "Nardus",key = "species", value = "cover") %>% 
  tibble %>% 
  left_join(obs %>% select(ID_position, wind_edge), by = "ID_position")
df$wind_edge              <- as.factor(df$wind_edge)
df$wind_edge <- relevel(df$wind_edge,  "TRUE")
levels(df$wind_edge) <- c("wind-edge", "not wind-edge")

all_data     <- df
no_windedges <- df %>% filter(wind_edge == "not wind-edge")

# make ggplot with both data frames: "all_data" and "no_windedges"
plot    <-  all_data      # choose one of the two
header  <- "all_data"     # choose one of the two

p.gdh<- ggplot(plot, aes(x = GDH, y = cover)) + 
  geom_point(aes(col = species, shape = wind_edge), size = 8, alpha = 0.5) +
  stat_smooth(aes(col = species), method = "loess", size = 2, alpha = 1, se = F, show.legend = F, span = 1) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F, span = 1,
              aes(col = species, ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("wind-edge", "not wind-edge"),
                     values = c(8, 18)) +
  scale_x_continuous(limits = c(9000, 40000), breaks = seq(10000, 30000, 10000)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = "top",
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 20, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "Growing degree hours (≥ 0 °C)",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 37500,  y = 0.9, hjust = 0, vjust = 0, label = "B", size = 20) +
  theme(legend.position = "none")
p.gdh
# ggsave(p.gdh, file = paste("Cover_GDH0_", header, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm" )

# Soil moisture
obs.all <- obs.all %>% filter(ID_position > 30) # Friseurexperiment und Monolithenexperiment nicht nehmen
df      <- reshape2::melt(obs.all[,c("ID_position", "soil_moist")], id.vars = 1) %>%   # HERE SWITCH BETWEEN MEAN AND MEDIAN (Median not good because e.g. mostly 1, 1.5 or 2 for Temp)
  dplyr::rename(moisture = value, type = variable) %>%  
  left_join(obs.all %>%
              select(ID_position, Carex, Nardus) %>%
              dplyr::rename(Carex = Carex, Nardus = Nardus)) %>% 
  gather("Carex", "Nardus",key = "species", value = "cover") %>% 
  tibble %>% 
  left_join(obs %>% select(ID_position, wind_edge), by = "ID_position")
df$wind_edge              <- as.factor(df$wind_edge)
df$wind_edge <- relevel(df$wind_edge,  "TRUE")
levels(df$wind_edge) <- c("wind-edge", "not wind-edge")

all_data     <- df
no_windedges <- df %>% filter(wind_edge == "not wind-edge")

# make ggplot with both data frames: "all_data" and "no_windedges"
plot    <-  all_data      # choose one of the two
header  <- "all_data"    # choose one of the two

p.moist <- ggplot(plot, aes(x = moisture, y = cover)) + 
  geom_point(aes(col = species, shape = wind_edge), size = 8, alpha = 0.5) +
  stat_smooth(aes(col = species), method = "loess", size = 2, alpha = 1, se = F, show.legend = F, span = 1) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F, span = 1,
              aes(col = species, ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("wind-edge", "not wind-edge"),
                     values = c(8, 18)) +
  scale_x_continuous(limits = c(10, 90), breaks = seq(20, 80, 20)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = "top",
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 20, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "Soil moisture (%vol)",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 85,  y = 0.9, hjust = 0, vjust = 0, label = "C", size = 20) +
  theme(legend.position = "none")
p.moist
# ggsave(p.moist, file = paste("Cover_moisture_", header, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm" )

# Soil depth
obs.all <- obs.all %>% filter(ID_position > 30) # Friseurexperiment und Monolithenexperiment nicht nehmen
df      <- reshape2::melt(obs.all[,c("ID_position", "soil_depth")], id.vars = 1) %>%   # HERE SWITCH BETWEEN MEAN AND MEDIAN (Median not good because e.g. mostly 1, 1.5 or 2 for Temp)
  dplyr::rename(depth = value, type = variable) %>%  
  left_join(obs.all %>%
              select(ID_position, Carex, Nardus) %>%
              dplyr::rename(Carex = Carex, Nardus = Nardus)) %>% 
  gather("Carex", "Nardus",key = "species", value = "cover") %>% 
  tibble %>% 
  left_join(obs %>% select(ID_position, wind_edge), by = "ID_position")
df$wind_edge              <- as.factor(df$wind_edge)
df$wind_edge <- relevel(df$wind_edge,  "TRUE")
levels(df$wind_edge) <- c("wind-edge", "not wind-edge")

all_data     <- df
no_windedges <- df %>% filter(wind_edge == "not wind-edge")

# make ggplot with both data frames: "all_data" and "no_windedges"
plot    <-  all_data      # choose one of the two
header  <- "all_data"    # choose one of the two

p.depth <- ggplot(plot, aes(x = depth, y = cover)) + 
  geom_point(aes(col = species, shape = wind_edge), size = 8, alpha = 0.5) +
  stat_smooth(aes(col = species), method = "loess", size = 2, alpha = 1, se = F, show.legend = F, span = 1) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F, span = 1,
              aes(col = species, ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("wind-edge", "not wind-edge"),
                     values = c(8, 18)) +
  scale_x_continuous(limits = c(0, 55), breaks = seq(10, 50, 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = "top",
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 20, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "Soil depth (cm)",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 50,  y = 0.9, hjust = 0, vjust = 0, label = "D", size = 20) +
  theme(legend.position = "none")
p.depth
# ggsave(p.depth, file = paste("Cover_soildepth_", header, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm" )

# pH
obs.all <- obs.all %>% filter(ID_position > 30) # Friseurexperiment und Monolithenexperiment nicht nehmen
df      <- reshape2::melt(obs.all[,c("ID_position", "pH")], id.vars = 1) %>%   # HERE SWITCH BETWEEN MEAN AND MEDIAN (Median not good because e.g. mostly 1, 1.5 or 2 for Temp)
  dplyr::rename(pH = value, type = variable) %>%  
  left_join(obs.all %>%
              select(ID_position, Carex, Nardus) %>%
              dplyr::rename(Carex = Carex, Nardus = Nardus)) %>% 
  gather("Carex", "Nardus",key = "species", value = "cover") %>% 
  tibble %>% 
  left_join(obs %>% select(ID_position, wind_edge), by = "ID_position")
df$wind_edge         <- as.factor(df$wind_edge)
df$wind_edge         <- relevel(df$wind_edge,  "TRUE")
levels(df$wind_edge) <- c("wind-edge", "not wind-edge")

all_data     <- df
no_windedges <- df %>% filter(wind_edge == "not wind-edge")

# make ggplot with both data frames: "all_data" and "no_windedges"
plot    <-  all_data      # choose one of the two
header  <- "all_data"    # choose one of the two

p.pH <- ggplot(plot, aes(x = pH, y = cover)) + 
  geom_point(aes(col = species, shape = wind_edge), size = 8, alpha = 0.5) +
  stat_smooth(aes(col = species), method = "loess", size = 2, alpha = 1, se = F, show.legend = F, span = 1) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F, span = 1,
              aes(col = species, ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("wind-edge", "not wind-edge"),
                     values = c(8, 18)) +
  scale_x_continuous(limits = c(3, 4.5), breaks = seq(3, 4.5, 0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = "top",
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 20, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "Soil pH",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 4.4,  y = 0.9, hjust = 0, vjust = 0, label = "E", size = 20) +
  theme(legend.position = "none")
p.pH
# ggsave(p.pH, file = paste("Cover_pH_", header, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm" )

# C/N
obs.all <- obs.all %>% filter(ID_position > 30) # Friseurexperiment und Monolithenexperiment nicht nehmen
df      <- reshape2::melt(obs.all[,c("ID_position", "C_N_ratio")], id.vars = 1) %>%   # HERE SWITCH BETWEEN MEAN AND MEDIAN (Median not good because e.g. mostly 1, 1.5 or 2 for Temp)
  dplyr::rename(C_N = value, type = variable) %>%  
  left_join(obs.all %>%
              select(ID_position, Carex, Nardus) %>%
              dplyr::rename(Carex = Carex, Nardus = Nardus)) %>% 
  gather("Carex", "Nardus",key = "species", value = "cover") %>% 
  tibble %>% 
  left_join(obs %>% select(ID_position, wind_edge), by = "ID_position")
df$wind_edge         <- as.factor(df$wind_edge)
df$wind_edge         <- relevel(df$wind_edge,  "TRUE")
levels(df$wind_edge) <- c("wind-edge", "not wind-edge")

all_data     <- df
no_windedges <- df %>% filter(wind_edge == "not wind-edge")

# make ggplot with both data frames: "all_data" and "no_windedges"
plot    <-  all_data      # choose one of the two
header  <- "all_data"     # choose one of the two

p.CN <- ggplot(plot, aes(x = C_N, y = cover)) + 
  geom_point(aes(col = species, shape = wind_edge), size = 8, alpha = 0.5) +
  stat_smooth(aes(col = species), method = "loess", size = 2, alpha = 1, se = F, show.legend = F, span = 1) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F, span = 1,
              aes(col = species, ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("wind-edge", "not wind-edge"),
                     values = c(8, 18)) +
  scale_x_continuous(limits = c(10, 22), breaks = seq(12, 20, 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = "top",
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 20, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "C/N-ratio",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 21,  y = 0.9, hjust = 0, vjust = 0, label = "F", size = 20) +
  theme(legend.position = "none")
p.CN
# ggsave(p.CN, file = paste("Cover_CN_ratio_", header, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm" )

# Phosphorus
# make two plots: one with citric acid soluble, one with water soluble
extract      <- "water_soluble_P_mg_per_kg"   # choose citricacid_soluble_... or water_soluble_...
extract.name <- "Water soluble P"             # choose "Citric acid soluble P" or "Water soluble P"

obs.all <- obs.all %>% filter(ID_position > 30) # Friseurexperiment und Monolithenexperiment nicht nehmen
df      <- reshape2::melt(obs.all[,c("ID_position", extract)], id.vars = 1) %>%   # HERE SWITCH BETWEEN MEAN AND MEDIAN (Median not good because e.g. mostly 1, 1.5 or 2 for Temp)
  dplyr::rename(phosphorus = value, type = variable) %>%  
  left_join(obs.all %>%
              select(ID_position, Carex, Nardus) %>%
              dplyr::rename(Carex = Carex, Nardus = Nardus)) %>% 
  gather("Carex", "Nardus",key = "species", value = "cover") %>% 
  tibble %>% 
  left_join(obs %>% select(ID_position, wind_edge), by = "ID_position")
df$wind_edge         <- as.factor(df$wind_edge)
df$wind_edge         <- relevel(df$wind_edge,  "TRUE")
levels(df$wind_edge) <- c("wind-edge", "not wind-edge")

all_data     <- df
no_windedges <- df %>% filter(wind_edge == "not wind-edge")

# make ggplot with both data frames: "all_data" and "no_windedges"
plot    <-  all_data      # choose one of the two
header  <- "all_data"     # choose one of the two

p.P<- ggplot(plot, aes(x = phosphorus, y = cover)) +  
  geom_point(aes(col = species, shape = wind_edge), size = 8, alpha = 0.5) +
  stat_smooth(aes(col = species), method = "loess", size = 2, alpha = 1, se = F, show.legend = F, span = 1) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F, span = 1,
              aes(col = species, ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_shape_manual(breaks = c("wind-edge", "not wind-edge"),
                     values = c(8, 18)) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +               # for water: 0,45 / 0, 45, 10 /// for citric acid 0, 240 / 0, 240, 50
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = "top",
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 20, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = eval(bquote(expression(.(extract.name) ~ "(mg kg"^-1*")"))),
        y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(plot.margin = margin(t = 30, r = 10, b = 10, l = 10)) +
  annotate("text", x = 41,  y = 0.9, hjust = 0, vjust = 0, label = "H", size = 20) + # for water "H" and 42, for citric acid "G" and 220
  theme(legend.position = "none")
p.P
# ggsave(p.P, file = paste("Cover_P_", substr(extract.name,1,3), "_", header, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm" )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Supplementary material: Figure - hairdresser experiment (day and night in one plot, not boxplot - only single points) ----
hair <- logger[,1:15]                                              # choose only logger from hairdresser experiment
hair <- hair %>%                                                   # select time of the experiment
  filter(Time_GMT > as.POSIXct("2020-07-10 22:00:00")) %>%         # approx 2 weeks before and 2 weeks after clipping
  filter(Time_GMT < as.POSIXct("2020-08-07 22:00:00"))
for (i in 1:7){                                                    # create temperature differences for each experimental pair
  harvest <- hair.harvest[2*i-1,2] %>% matrix                      # e.g. Temp1-Temp2 (clipped - control) --> Temperature difference to the control
  hair[,14+2*i] <- hair[,2*i]-hair[,2*i+1]                         # Temp_Diff negative --> clipped colder than control
  hair[,15+2*i] <- ifelse(hair[,1] < harvest, "before_clipping", "after_clipping")
  colnames(hair)[14+2*i] <- paste("Diff_Pair", i, sep = "")        
  colnames(hair)[15+2*i] <- paste("Treat_Pair", i, sep = "")
}
hairdresser <- data.frame(pair = rep(1:7,2), clipping = rep(c("before_clipping", "after_clipping"), each = 7))
for (i in 1:7){
  data <- hair[-c(2:15)][c(1, 2*i, 2*i+1)]
  hairdresser[i,  "mean_temp"]      <- data %>% filter(.[[3]] == "before_clipping") %>% select(2) %>% colMeans()
  hairdresser[i+7,"mean_temp"]      <- data %>% filter(.[[3]] == "after_clipping")  %>% select(2) %>% colMeans()
  hairdresser[i,  "mean_daytemp"]   <- data %>% filter(.[[3]] == "before_clipping") %>%
    filter(lubridate::hour(Time_GMT) >= 8) %>% filter(lubridate::hour(Time_GMT) <= 19) %>%  # daytime: 9.00 MEZ - 20.00 MEZ
    select(2) %>% colMeans()
  hairdresser[i+7,"mean_daytemp"]   <- data %>% filter(.[[3]] == "after_clipping") %>%
    filter(lubridate::hour(Time_GMT) >= 8) %>% filter(lubridate::hour(Time_GMT) <= 19) %>%  
    select(2) %>% colMeans()
  hairdresser[i,  "mean_nighttemp"] <- data %>% filter(.[[3]] == "before_clipping") %>%
    filter(lubridate::hour(Time_GMT) < 8 | lubridate::hour(Time_GMT) > 19) %>%              # nighttime: 21.00 MEZ - 8.00 MEZ
    select(2) %>% colMeans()
  hairdresser[i+7,"mean_nighttemp"] <- data %>% filter(.[[3]] == "after_clipping") %>%
    filter(lubridate::hour(Time_GMT) < 8 | lubridate::hour(Time_GMT) > 19) %>%              # nighttime: 21.00 MEZ - 8.00 MEZ
    select(2) %>% colMeans()
  hairdresser[i,  "min_temp"]       <- data %>% filter(.[[3]] == "before_clipping") %>% select(2) %>% min()
  hairdresser[i+7,"min_temp"]       <- data %>% filter(.[[3]] == "after_clipping")  %>% select(2) %>% min()
  hairdresser[i,  "max_temp"]       <- data %>% filter(.[[3]] == "before_clipping") %>% select(2) %>% max()
  hairdresser[i+7,"max_temp"]       <- data %>% filter(.[[3]] == "after_clipping")  %>% select(2) %>% max()
}

hairdresser.long <- hairdresser %>%
  select(-c(min_temp, max_temp)) %>% 
  gather(key = "time", value = "mean_temp", c("mean_daytemp", "mean_nighttemp"))
hairdresser.long$time <- hairdresser.long$time %>% 
  recode_factor(mean_daytemp = "day", mean_nighttemp = "night")
hairdresser.long$clipping <- hairdresser.long$clipping %>% 
  recode_factor(before_clipping = "before", after_clipping = "after")

p <- ggplot(hairdresser.long, aes(x = clipping, y = mean_temp, col = time)) + 
  facet_grid(cols = vars(time)) +
  geom_point(size = 6, alpha = 0.5) +
  geom_line(aes(group = pair)) +
  scale_color_manual(breaks = c("day", "night"),
                     values = c("darkorange", "darkblue")) +
  scale_y_continuous(limits = c(-2.5,2.5),
                     breaks = seq(-2,2,1)) +
  labs(x = "Clipping",
       y = "Temperature difference \n to control plot (K)") +
  theme(axis.title.x   = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 25, margin = margin(t = 0, r = 18, b = 0, l = 0)), 
        axis.text.x    = element_text(size = 25, margin = margin(t = 15, r = 0, b = 0, l = 0), face = "italic"),
        axis.text.y    = element_text(size = 25, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt"),
        strip.text.x = element_text(size = 25, colour = "black", face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "none")
p
# ggsave(p, file = "Hairdresser.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm" )

# mean_daytemp (clipping effect = +0.63K, p = 0.022, n = 6) --> brown leaf tips cool down during the day
wide <- hairdresser %>% select(pair, clipping, mean_daytemp) %>% spread(key = clipping, value = mean_daytemp)
test <- with(wide, t.test(after_clipping, before_clipping, paired = TRUE))
test

# mean_nighttemp (clipping effect = -0.11K, p = 0.022, n = 6) --> brown leaf tips isolate during the night
wide <- hairdresser %>% select(pair, clipping, mean_nighttemp) %>% spread(key = clipping, value = mean_nighttemp)
test <- with(wide, t.test(after_clipping, before_clipping, paired = TRUE))
test

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Create masterfile "R_pos_microsites.csv" ----
obs    <-  read_csv("DATA/R_pos_characteristics.csv",
                    col_names = T,
                    col_types = cols(.default = col_double(),
                                     macroexposure = col_factor(), exposure = col_factor(), topography = col_factor(), wind_edge = col_logical(),
                                     species = col_factor(), control_spec_1 = col_factor(), control_spec_2 = col_factor(), control_spec_3 = col_factor(),
                                     position_description = col_character()),
                    na = "NA")
soil   <- read_csv("DATA/R_pos_soil.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), sample_GMT = col_datetime() ),
                   na = "NA")
therm  <- read_csv("DATA/R_pos_temperature.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), wintersnow19_GMT = col_datetime(), wintersnow20_GMT = col_datetime(), snowfree20_GMT = col_datetime(), snowmelt20_GMT = col_datetime()),
                   na = "NA")
EIV  <- read_csv("DATA/R_pos_EIV.csv",
                 col_names = T,
                 col_types = cols(.default = col_double()),
                 na = "NA")
veg    <- read_csv("DATA/raw_pos_vegetation.csv",
                   col_names = T,
                   col_types = cols(.default = col_double()),
                   na = "NA")

microsites <- obs %>%
  left_join(soil,  by = "ID_position") %>% 
  left_join(therm, by = "ID_position") %>% 
  left_join(EIV,   by = "ID_position") %>% 
  left_join(veg,   by = "ID_position") %>% 
  filter(ID_position > 40) %>%                   # choose only microsites from observational study
  dplyr::rename(microtopography = topography,
                soil_sample_GMT = sample_GMT,
                pH_CaCl2 = pH, 
                water_soluble_P = water_soluble_P_mg_per_kg,
                citricacid_soluble_P = citricacid_soluble_P_mg_per_kg,
                carbon = C_mg_per_g,
                nitrogen = N_mg_per_g,
                seasonstart20_GMT = snowmelt20_GMT,
                seasonstart20_day = snowmelt20_day,
                Indval_T = T.0.1.mean,
                Indval_N = N.0.1.mean,
                Indval_L = L.0.1.mean,
                Indval_F = F.0.1.mean,
                Indval_H = H.0.1.mean,
                Indval_R = R.0.1.mean) %>% 
  select(-c(corresp_control, control_spec_1, control_spec_2, control_spec_3,
            C_percent, N_percent,
            T.weighted.mean, N.weighted.mean, L.weighted.mean, F.weighted.mean, H.weighted.mean, R.weighted.mean, W.weighted.mean, MV.weighted.mean, WV.weighted.mean, TV.weighted.mean, FW.weighted.mean,
            W.0.1.mean, MV.0.1.mean, WV.0.1.mean, TV.0.1.mean, FW.0.1.mean,
            T.0.1.median, N.0.1.median, L.0.1.median, F.0.1.median, H.0.1.median, R.0.1.median, W.0.1.median, MV.0.1.median, WV.0.1.median, TV.0.1.median, FW.0.1.median)) %>% 
  print

# write.csv(microsites, file = "DATA/R_pos_microsites.csv", row.names = FALSE)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# END OF SCRIPT ----