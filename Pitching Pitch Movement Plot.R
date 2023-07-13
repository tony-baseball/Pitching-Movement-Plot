library(plyr)
library(tidyverse)
library(ggplot2)
library(plotly)

{
  # load csv
  csv <- read.csv("C:/Users/tdmed/OneDrive/_Github/Hitting-Depth-of-Contact/sample csv.csv")
  # see pitchers in data set
  unique(csv$Pitcher)
  # filter by specific pitcher
  pitcher <-csv %>% 
  filter(grepl("Schlotman" , Pitcher)) %>%
    # factor TaggedPitchType so that the order is always the same
  dplyr:: mutate(TaggedPitchType = factor(TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball",
                                                                       "Slider", "Changeup", "Splitter", 'Knuckleball')),
                 # recode TaggedPitchType so the pitch names are shorter
                 TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                          Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Knuckleball = 'KN' ),
                 ) 

  # create a text hover for the plotly
  pitch_info = paste('\nPitch #',pitcher$PitchNo, '\n',
                   round(pitcher$RelSpeed,1), "mph\n", 
                   round(pitcher$SpinRate), 'rpm\n',
                   round(pitcher$yt_Efficiency), '%')

ggplotly(
  ggplot(data = pitcher, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType, 
                             label = pitch_info)) +
    labs(color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)", title = "Horizontal/Vertical Break Chart",
         subtitle = pitcher$Pitcher[1]) +
    xlim(-22, 22) + ylim(-22, 22) +
    geom_segment(aes(x = 0, y = -22, xend = 0, yend = 22), size = 1, color = "grey55") +
    geom_segment(aes(x = -22, y = 0, xend = 22, yend = 0), size = 1, color = "grey55") +
    geom_point(size =4, alpha = .5) +
    scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = 'orange',  'SL'='cornflowerblue',
                                  'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
    theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14)) ) %>% 
  layout(autosize = T,
         showlegend = TRUE,
         legend = list(orientation = "v",   # show entries horizontally
                       xanchor = "left",  # use center of legend as anchor
                       x = 1,
                       y = .5),
         title = list(text = paste0('Hotizontal/Vertical Break Chart',
                                    '<br>',
                                    '<sup>',
                                    pitcher$Pitcher[1], ' - ', pitcher$Date[1] ,
                                    '</sup>')))
}

