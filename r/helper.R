# R helper file for SEGKC 

# k.palof  katie.palof@alaska.gov  
# updated: 10-23-19

# load -----------
library(tidyverse)
library(lubridate)
library(readxl)
#library(ggthemes)
library(gridExtra)
library(extrafont)
library(tidyr)
library(padr)
library(anytime)
library(RColorBrewer)
library(cowplot)
library(ggridges)
library(rgeos)
library(maptools)
library(rgdal)
library(ggmap)
library(ggrepel)
library(PBSmapping)
library(janitor)
library(here)
library(tidyquant)
library(DiagrammeR)
library(patchwork)
library(gganimate)
library(gifski)
library(anytime)
library(AICcmodavg)
library(FSA)
library(ggfortify)
library(GGally)
library(corrplot)
library(FishForecast)
library(olsrr)
library(caret)
library(tsibble)

##THEMES FOR GRAPHS ---------
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=14,base_family='serif')
          +theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 axis.text = element_text(size = 14)))

#COLOR BLIND PALETTE --------------
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



