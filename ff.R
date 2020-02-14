#installation

if (!require(remotes)) {
  install.packages("remotes") 
}

remotes::install_github("wiscostret/fplscrapR")

library(fplscrapR)
library(tidyverse)

#ids for each team
#https://fantasy.premierleague.com/leagues/297094/standings/c
#ids <- c(1471904,2480938,1330387,4993266,1727451,1239227,843852,4115020,2166963,3586900,2459155,2443647,2476502,4099040)
ids <- c(568651,1229681,933902,1908330,1734934,707074,580266,4083952,207891,1966144)

#scrape data
myleague <- lapply(ids, get_entry_season)

#add to one data frame
myleague_df <- bind_rows(myleague)
myleague_df <- arrange(myleague_df,event,overall_rank)

#work out WBW rank
myleague_df <- myleague_df %>%
  dplyr::group_by(event) %>%
  dplyr::mutate(week_rank = rank(overall_rank, ties.method = 'first'))

#title of league
league_title = 'Top 10 players'


#colour picker: http://tristen.ca/hcl-picker/#/hlc/15/0.81/25303A/F2ED7C

ggplot(myleague_df, aes(x = event, y = week_rank, group = name)) + 
  geom_line(aes(colour = name), size = 1.2) + 
  geom_point(aes(colour = name, size = points)) + scale_y_reverse(breaks = seq(1,14,1), labels = seq(1,14,1)) + theme_minimal() +
  geom_text(data = myleague_df[myleague_df$event == max(myleague_df$event),
                               ], aes(label = paste0(name,' (',total_points,' pts)')), vjust = -0.25, nudge_x = 2, size = 6) + 
  scale_x_continuous(limits = c(0,max(myleague_df$event)+3), breaks = seq(1,max(myleague_df$event),1), labels = seq(1,max(myleague_df$event),1)) + 
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 18)) +
  labs(title = paste0(league_title, ' league'), x = 'Gameweek', y = 'Rank', subtitle = paste0('Gameweek ',max(myleague_df$event))) + 
  scale_colour_manual(values = c('#25303A','#2A3D46','#2D4B51','#31585C','#356765','#3B756C','#448472','#509276','#60A078','#72AE79','#87BC7A','#9EC979','#B8D679','#D4E27A')) 

#save png
setwd('C:/Users/Rob/Documents/R')
ggsave(paste0('GW',max(myleague_df$event),'.png'), last_plot(), height = 9.5, width = 16.8)

