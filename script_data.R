# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 16th April 2021
## Last modification: XXXX

## Simon and Carmen dos Santos
## email: simon.vonsachsencoburgundgotha@imbrsea.eu / cbsantos@ualg.pt

## CODE FOR
#1 HANDLING DATA
#2 DATA EXPLORATORY ANALYSIS DURING COMPILATION


# SETTINGS ----------------------------------------------------------------

# load libraries
packages <- c("tidyverse",      # for data science (general) - includes ggplot2
              "readxl")         # for reading xlsx files

for (i in seq_along(packages)) {
  if(!do.call(require, list(package = packages[i]))) {
    do.call(install.packages, list(pkgs = packages[i]))
    do.call(require, list(package = packages[i]))
  }
}

# clean working environment
rm(list=ls())

# set working directory
setwd("~/Documents/IMBRSea/Thesis S4")
getwd()

# DATA --------------------------------------------------------------------

# load SOURCES
data.sou  <- read_excel("./Database uptake rate_12.04.2021.xlsx",
                        sheet="sources",na="NA",skip=3)
str(data.sou)
names(data.sou)

# load ENVIRONMENTAL
data.env  <- read_excel("./Database uptake rate_12.04.2021.xlsx",
                        sheet="environmental",na="NA",skip=3)
str(data.env)
names(data.env)

# load EXPERIMENTAL
data.exp  <- read_excel("./Database uptake rate_12.04.2021.xlsx",
                        sheet="experimental",na="NA",skip=3)
str(data.exp)
names(data.env)

data.exp$Vmax   <- as.numeric(data.exp$Vmax)
data.exp$Km     <- as.numeric(data.exp$Km)
data.exp$alpha  <- as.numeric(data.exp$alpha)

# quality controls
length(unique(data.env$id_short))
length(unique(data.exp$id_short))

# MERGE data.exp and data.env
data.all <- merge(data.env,data.exp,by="id_short")





# EXPLORATORY -------------------------------------------------------------

###DATA.SOU

##Publication type
ggplot(data.sou, aes(x=publication_type)) +
        geom_bar(stat="count")

##Publication year
ggplot(data.sou, aes(x=year)) +
        geom_bar(stat="count")


###DATA.EXP

##Species type
ggplot(data.exp, aes(x=species_type)) +
        geom_bar(stat="count")

ggplot(data.exp, aes(x=species_type, colour=species_compartm, fill=species_compartm)) +
        geom_bar(stat="count", position=position_dodge())


##Vmax
#Distribution - Hist
ggplot(data.exp, aes(x=Vmax))+
        geom_histogram(binwidth=10) +
        facet_grid(.~species_type)

ggplot(data.exp, aes(x=Vmax, fill=species_type))+
        geom_histogram(binwidth=10)

ggplot(data.exp, aes(x=Vmax, fill=species_type))+
        geom_histogram(binwidth=10) +
        facet_grid(species_compartm~species_type)

#Boxplot + jitter
ggplot(data.exp, aes(x=species_type, y=Vmax)) +
        geom_boxplot()

ggplot(data.exp, aes(x=species_type, y=Vmax)) +
        geom_boxplot() +
        facet_grid(.~nutrient)

ggplot(data.exp, aes(x=species_type, y=Vmax)) +
        geom_boxplot() +
        geom_jitter() +
        facet_grid(.~nutrient)
        