# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 16th April 2021
## Last modification: 23/04/2021

## Simon Coburg and Carmen dos Santos
## email: simon.vonsachsencoburgundgotha@imbrsea.eu / cbsantos@ualg.pt

## CODE FOR
#1 HANDLING DATA
#2 DATA EXPLORATORY ANALYSIS DURING COMPILATION


# SETTINGS ----------------------------------------------------------------

# load libraries
packages <- c("tidyverse",      # for data science (general) - includes ggplot2
              "readxl",         # for reading xlsx files
              "devtools",
              "ggpubr")


for (i in seq_along(packages)) {
  if(!do.call(require, list(package = packages[i]))) {
    do.call(install.packages, list(pkgs = packages[i]))
    do.call(require, list(package = packages[i]))
  }
}


# clean working environment
rm(list=ls())

# set working directory
setwd("~/Documents/IMBRSea/Thesis S4/RStudio Uptake rates")
getwd()


# DATA --------------------------------------------------------------------

# load SOURCES
data.sou  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_23.04.2021.xlsx",
                        sheet="sources",na="NA",skip=3)
str(data.sou)
names(data.sou)

# load ENVIRONMENTAL
data.env  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_23.04.2021.xlsx",
                        sheet="environmental",na="NA",skip=3)
str(data.env)
names(data.env)

# load EXPERIMENTAL
data.exp  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_23.04.2021.xlsx",
                        sheet="experimental",na="NA",skip=3)
str(data.exp)
names(data.exp)

data.exp$Vmax   <- as.numeric(data.exp$Vmax)
data.exp$Km     <- as.numeric(data.exp$Km)
data.exp$alpha  <- as.numeric(data.exp$alpha)
data.exp$sampling_inter <- as.numeric(data.exp$sampling_inter)

# quality controls
length(unique(data.env$id_short))
length(unique(data.exp$id_short))

#x <- unique(data.env$id_short)
#y <- unique(data.exp$id_short)

#x <- as.list(x)
#y <- as.list(y)

#capture.output(x, file = "env.csv")
#capture.output(y, file = "exp.csv")

# MERGE data.exp and data.env
data.all <- merge(data.env,data.exp,by="id_short")



# EXPLORATORY -------------------------------------------------------------

###DATA.SOU

##Publication type
ggplot(data.sou, aes(x=publication_type)) +
        geom_bar(stat="count") +
        theme_bw()

##Publication year
ggplot(data.sou, aes(x=year)) +
        geom_bar(stat="count") +
        theme_bw()

###DATA.ENV

#Sampling country
ggplot(data.env, aes(x=sample_country)) +
        geom_bar(stat="count", na.rm=FALSE) + coord_flip() +
        theme_bw() 
#       + theme(axis.text.x = element_text(angle=0, vjust=0.5, hjust=1))
        

###DATA.EXP

#Incubation type / surge or int. contr. phase
ggplot(data.exp, aes(x=Vmax))+
  geom_histogram(binwidth=10) +
  facet_grid(type_incub~species_type) +
  theme_bw()


###Species type

ggplot(data.exp, aes(x=species_type)) +
        geom_bar(stat="count",  width = .5) +
        geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
        theme_bw()

  
ggplot(data.exp, aes(x=species_type, colour=species_compartm, fill=species_compartm)) +
        geom_bar(stat="count", position=position_dodge()) +
        #geom_text(aes(label = ..count..), stat = "count", vjust = .5, colour = "black") +
        theme_bw() +
        theme(legend.position="bottom")


ggplot(data.exp, aes(x=species_type, fill=species_phyla)) +
        scale_fill_manual(values = c("forestgreen", "salmon4", "red2", "springgreen4")) +
        geom_bar(stat="count", position=position_dodge(),  width = .6) +
        #geom_text(aes(label = ..count..), stat = "count", vjust = .5, colour = "black") +
        theme_bw() +
        theme(legend.position="bottom")

#library(ggpubr)
#ggarrange(spcomp, spphyl, ncol=2, nrow=1)


# Vmax Range -------------------------------------------------------------------

#Distribution - Hist
ggplot(data.exp, aes(x=Vmax))+
        geom_histogram(binwidth=10) +
        facet_grid(.~species_type) +
        theme_bw()


#Vmax for species compartment
ggplot(data.exp, aes(x=Vmax))+
  geom_histogram(binwidth=10) +
  facet_grid(species_compartm~species_type) +
  theme_bw()


#Vmax for int. contr. phase / surge
ggplot(data=subset(data.exp, !is.na(Vmax)), aes(x=Vmax), fill=type_uptake)+
  geom_histogram(binwidth=10) +
  facet_grid(type_uptake~species_type) +
  theme_bw()


#Scatterplot for temperature
ggplot(data.exp, aes(x=temperature_experiment, y=Vmax)) +
  geom_point() +
  facet_grid(.~species_type) +
  theme_bw()


#Boxplot + jitter for nutrients
ggplot(data.exp, aes(x=species_type, y=Vmax)) +
        geom_boxplot() +
        geom_jitter() +
        facet_grid(.~nutrient) +
        theme_bw()



# Vmax Single -------------------------------------------------------------

#Distribution - Hist
ggplot(data.exp, aes(x=uptake_rate_dw))+
        geom_histogram(binwidth=10) +
        facet_grid(.~species_type) +
        theme_bw()


#Vmax for species compartment
ggplot(data.exp, aes(x=uptake_rate_dw))+
        geom_histogram(binwidth=10) +
        facet_grid(species_compartm~species_type) +
        theme_bw()


#Vmax for int. contr. phase / surge
ggplot(data=subset(data.exp, !is.na(uptake_rate_dw)), aes(x=uptake_rate_dw))+
        geom_histogram(binwidth=10) +
        facet_grid(type_uptake~species_type) +
        theme_bw()


#Scatterplot for temperature
ggplot(data.exp, aes(x=temperature_experiment, y=uptake_rate_dw)) +
        geom_point() +
        facet_grid(.~species_type) +
        theme_bw()

ggplot(data.exp, aes(x=temperature_experiment, y=uptake_rate_dw)) +
        geom_point() +
        facet_grid(species_type~type_uptake) +
        theme_bw()


#Boxplot + jitter for nutrients
ggplot(data.exp, aes(x=species_type, y=uptake_rate_dw)) +
        geom_boxplot() +
        geom_jitter() +
        facet_grid(~nutrient) +
        theme_bw()

ggplot(data.exp, aes(x=species_type, y=uptake_rate_dw)) +
      geom_boxplot() +
      geom_jitter() +
      facet_grid(type_uptake~nutrient) +
      theme_bw()

    #Boxplot + jitter for nutrients coloured
    ggplot(data.exp, aes(x=species_type, y=uptake_rate_dw)) +
            geom_boxplot(aes(colour = factor(type_uptake))) +
            geom_jitter(aes(colour = factor(type_uptake))) +
            facet_grid(.~nutrient) +
            theme_bw()
        