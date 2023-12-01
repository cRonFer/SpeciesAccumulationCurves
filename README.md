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
```
## Prepare data for plotting Counts x year

```{.r }
data <- data[order(data$year),] # sort the year in ascending order 
data$duplicated <- duplicated(data$ScientificName) # assign a false to each new species 
data$specacum <- ifelse(data$duplicated == FALSE, 1, 0)# assign "1" to each new species and 0 to duplicated
data$recs <- as.integer(1) # assign 1 to each occurrence record
occ_year <- aggregate(recs ~ year, data, sum) # Sum species by year

acum_year <- data[,c('ScientificName','year','specacum')][specacum == 1,]
acum_year$Counts <- as.integer(1) # assign 1 to each occurrence record

acum_year2 <- aggregate(Counts ~ year, acum_year, sum) # Sum species by year
acum_year2 <- aggregate(cbind(Counts, specacum)  ~ year, acum_year, sum) # groups by year
acum_year2$acumulado <- cumsum(acum_year2$specacum) # sums the number of species described each year

occ_year <- merge(occ_year, acum_year2, by = 'year', all.x = TRUE)
```
***
## Plot: Species Acumulation curve 
```{.r }
ploty <- function(x, y, color){
  ggplot() + 
    geom_line(mapping = aes(x = x, y = y), linewidth = 1, color = color) + 
    scale_x_continuous(name= "Year", breaks = seq(min(x), 2025, by = 25)) +
    ylab("Observed number of species") +
    theme(axis.line = element_line(colour = "gray20", linewidth = 0.5, linetype = "solid"), 
          panel.background = element_rect(fill = "white"),
          axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")), 
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
          axis.text = element_text(size = 10)
          )
}
sacs <- occ_year[!is.na(occ_year$acumulado),]
(sacPlot <- ploty(x = sacs$year, y = sacs$acumulado, 'black'))
# ggsave('sac.png', sacPlot, width = 20, height = 10, units = "cm")
```
![image](https://github.com/cRonFer/SpeciesAccumulationCurves/assets/76005368/2ac7d552-75f4-45e1-a802-ba958e69efb6)

## Plot: Occurrences by year 
```{.r }
ploty2 <- function(x, y, color){
  ggplot() + 
    geom_bar(mapping = aes(x = x, y = y), stat = "identity", fill = color) + 
    scale_x_continuous(name = "Year", breaks = seq(min(x), 2025, by = 25)) +
    ylab("Number of records") +
    theme(axis.line = element_line(colour = "gray20", linewidth = 0.5, linetype = "solid"), 
          panel.background = element_rect(fill = "white"),
          axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")), 
          axis.text.y.right = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
          axis.text = element_text(size = 10)
          )
}
(occPlot <- ploty2(x = occ_year$year, y = occ_year$recs, 'black'))
# ggsave('occPlot.png', occPlot, width = 20, height = 10, units = "cm")
```
![image](https://github.com/cRonFer/SpeciesAccumulationCurves/assets/76005368/f65a1fe2-e493-497f-9c54-0f9a480db356)

***
## Double Plot: Species Acumulation curve + number of records by year ####
```{.r }
prop <- round(max(occ_year$recs)/max(sacs$acumulado), -1)

plotyDouble <- function(data, x, y, z, color1, color2, temprange){
  ggplot() + 
    geom_bar(mapping = aes(x = x, y = y*1), stat = "identity", fill = 'black') +
    geom_line(data, 
              mapping = aes(x = year, y = acumulado*prop), 
              linewidth = 1, color = 'darkgrey') +
    
    scale_x_continuous(name= "Year", breaks = seq(min(x), 2030, by = 25)) +
    scale_y_continuous(name = "Number of records",
                       sec.axis = sec_axis(~ ./prop, name = "Observed number of species", 
                                           breaks = seq(0, max(y), by = 5))) +
    
    theme(axis.line = element_line(colour = "gray20", linewidth = 0.5, linetype = "solid"), 
          panel.background = element_rect(fill = "white"),
          axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")), 
          axis.text.y.right = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
          axis.text = element_text(size = 12)) +
    geom_vline(xintercept = temprange, linetype = "dashed")
}
(sacRecs <- plotyDouble(sacs, 
                        x = occ_year$year, 
                        y = occ_year$recs, 
                        z = occ_year$acumulado,
                        "darkgrey", "black",
                        c(1950,1970) # draw lines in the temporal range established
                   ))
# ggsave('sacsRecs.png', sacRecs, width = 20, height = 10, units = "cm")
```
![image](https://github.com/cRonFer/SpeciesAccumulationCurves/assets/76005368/759c4821-c289-4676-a207-cc9649b1500e)
