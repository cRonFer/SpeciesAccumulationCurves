# SpeciesAccumulationCurves (SAC's)
This repository contains an R Script to perform and vizualize species acumulation curves

## Load packages
```{.r }
library(data.table)
library(tidyverse)
```
## Read dataset and process records
```{.r }
data <- fread('data.csv', sep=";", encoding="UTF-8", header=TRUE)
data <- data[,c('ScientificName', 'year')]
data <- data[!is.na(year),] # exclude NA's in Year data
# data <- data[year >= 1850,] # filter temporal range of data

data <- data[order(data$year),] # sort the year in ascending order 
data$duplicated <- duplicated(data$ScientificName) # assign a false to each new species 
data$specacum <- ifelse(data$duplicated == FALSE, 1, 0)# assign "1" to each new species and 0 to duplicated
year <- data[,c('ScientificName','year','specacum')][specacum == 1,]
year$Counts <- as.integer(1) # assign 1 to each occurrence record
```
## Prepare data for plotting Counts x year
```{.r}
occ_year <- aggregate(Counts ~ year, year, sum) #Sum species by year
occ_year <- aggregate(cbind(Counts, specacum)  ~ year, year, sum) # groups by year
occ_year$acumulado <- cumsum(occ_year$specacum) # sums the number of species described each year
```
***
## Plot: Species Acumulation curve 
```{.r }
ploty <- function(x, y, color){
  ggplot() + 
    geom_line(mapping = aes(x = x, y = y), size = 1, color = color) + 
    scale_x_continuous(name= "Year", breaks = seq(min(data$year), 2025, by = 25)) +
    ylab("Observed number of species") +
    theme(axis.line = element_line(colour = "gray20", size = 0.5, linetype = "solid"), 
          panel.background = element_rect(fill = "white"),
          axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")), 
          axis.text.y.right = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
          plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.title = element_text(size = 10), 
          axis.text = element_text(size = 10), 
          plot.title = element_text(size = 10))
}
```
```{.r }
 (sac <- ploty(x = occ_year$year, y = occ_year$acumulado, 'black'))
# ggsave('sac.png', sac, width = 20, height = 10, units = "cm")
```
## Plot: Occurrences by year 
```{.r }
ploty2 <- function(x, y, color){
  ggplot() + 
    geom_bar(mapping = aes(x = x, y = y), stat = "identity", fill = color) + 
    scale_x_continuous(name= "Year", breaks = seq(min(data$year), 2025, by = 25)) +
    ylab("Observed number of species") +
    theme(axis.line = element_line(colour = "gray20", size = 0.5, linetype = "solid"), 
          panel.background = element_rect(fill = "white"),
          axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")), 
          axis.text.y.right = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
          plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.title = element_text(size = 10), 
          axis.text = element_text(size = 10), 
          plot.title = element_text(size = 10))
}
(occ <- ploty2(x = occ_year$year, y = occ_year$Counts, 'black'))
# ggsave('occ.png', sac, width = 20, height = 10, units = "cm")
```
***
## Double Plot: Species Acumulation curve + number of records by year ####
```{.r }
prop <- round(max(occ_year$acumulado)/max(occ_year$Counts),0)
plotyDouble <- function(x, y, z, color1, color2, temprange){
  ggplot() + 
  geom_bar(mapping = aes(x = x, y = y*prop), stat = "identity", fill = color1) +
  geom_line(mapping = aes(x = x, y = z*1), size = 1, color = color2) + 
  scale_x_continuous(name= "Year", breaks = seq(min(x), 2025, by = 30))+
  scale_y_continuous(name = "Observed number of species", breaks = seq(0, max(z), by = 100),
                     sec.axis = sec_axis(~ ./prop, name = "Number of records", 
                                         breaks = seq(0, max(y), by = 10)))+
  theme(axis.line = element_line(colour = "gray20", size = 0.5, linetype = "solid"), 
        panel.background = element_rect(fill = "white")) + 
  theme(axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")), 
        axis.text.y.right = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"))) + 
  theme(plot.subtitle = element_text(vjust = 1), 
         plot.caption = element_text(vjust = 1), 
         axis.title = element_text(size = 18), 
         axis.text = element_text(size = 12), 
         plot.title = element_text(size = 18))+
  geom_vline(xintercept = temprange, linetype = "dashed")
}
```
```{.r }
(sacRecs <- plotyDouble(x = occ_year$year, 
                   y = occ_year$Counts, 
                   z = occ_year$acumulado,
                   "darkgrey", "black",
                   c(1950,1970) # draw lines in the temporal range established
                   ))
# ggsave('sacsRecs.png', sacRecs, width = 20, height = 10, units = "cm")
```
