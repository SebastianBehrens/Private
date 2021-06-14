# Setup ---------------------------------
library(tidyverse)
library(wordcloud)
library(rvest)
library(tidyverse)

theme_set(
    theme_classic() + 
        theme(
            axis.ticks.length = unit(-0.25, "cm"),
            axis.text.x = element_text(margin = unit(c(0.4,0,0,0), "cm")),
            axis.text.y = element_text(margin = unit(c(0,0.4,0,0), "cm")),
            axis.line = element_blank(),
            panel.grid.major.y = element_line(linetype = 2),
            plot.title = element_text(hjust = 0.5),
            text = element_text(family = "serif"),
            legend.justification = c("right", "top"),
            # legend.position = c(1, 1),
            legend.position = c(.98, .98),
            legend.background = element_rect(fill = NA, color = "black"),
            panel.border = element_rect(fill = NA, size = 1.25),
            strip.text = element_text(size = 12)
            # legend.margin = margin(6, 10, 6, 6)
            # legend.box.background = element_rect(colour = "black")
        )
    
)


# Import ---------------------------------
watsonarticles <- read_delim("/Users/sebastianbehrens/Downloads/WatsonArticles.txt", delim = ",", col_names = F)
zwanzigminutenarticles <- read_delim("/Users/sebastianbehrens/Downloads/zwanzigminuten.txt", delim = ",", col_names = F)
