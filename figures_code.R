#import ggplot2
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(janitor)
library(cowplot)
library(tidyr)
library(scales)
library(likert)
library(RColorBrewer)
#install.packages("here")
library(here)

#find filepath of file
#file.choose()

data <- read.csv(here("data","data.csv"), header = TRUE) %>%
  clean_names() #remove dots from title

#or manually set path
#filename <- "~/Documents/GitHub/accountabilitycapture/data.csv"
#data <- read.csv(filename, header = TRUE) %>%
#  clean_names() #remove dots from title

names(data)

###fig1
# q8 reasons and drivers for record keeping 
q8 = "q8_the_main_reasons_and_drivers_behind_your_organisation_keeping_records_are_select_all_that_apply_selected_choice"
names(data)[names(data) == q8] <- 'q8'

colourCount = length(unique(unlist(strsplit(data$q8, ","))))
getPalette = colorRampPalette(brewer.pal(colourCount-1, "Blues"))

data.frame(quest = unlist(strsplit(data$q8, ","))) %>%
  filter(quest != "Other (please specify)") %>%
  ggplot(aes(x = fct_rev(fct_infreq(quest)), fill=quest)) +
  geom_bar(fill=getPalette(colourCount-1)) +
  theme_cowplot(font_size = 70,) +
  coord_flip() +
  ylab('Percentage')+
  xlab('')+
  scale_x_discrete(labels = label_wrap(30),
                   guide = guide_axis(n.dodge = 1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill='white',color=NA))+
  # ggtitle("Who designs these algorithmic systems?") +
  theme(plot.title = element_text(hjust = 1),)

ggsave(path = (here("figs")), filename = 'fig1.png',
       width = 30, height = 25, units = "in")

### fig2
# q65 and other likert style question responses 
agree_levels = c("Strongly disagree",
                 "Somewhat disagree",
                 "Neither agree nor disagree",
                 "Somewhat agree",
                 "Strongly agree")

data_factored <- data %>% dplyr::filter(q65_not_having_record_keeping_processes_has_previously_led_to_unforeseen_consequences != '')
data_factored[] <-  lapply(data_factored, factor, levels=agree_levels)

#fix name formatting for display in graph
names(data_factored) <- gsub("^q+[0-9]*", "\\1", colnames(data_factored)) #remove question numbers
names(data_factored) <- snakecase::to_sentence_case(colnames(data_factored), sep_out = " ", sep_in = "_")
names(data_factored) 

#select desired questions
select(data_factored, matches("Overall the records |The information that my organisation records"))

#create likert objects
x <- likert(select(data_factored, matches("Overall the records that my organisation generates about our algorithmic systems have proven useful for my |The information that my organisation records |My organisation is recording information |My organisation is not recording information |Having record keeping processes"))) 
x
summary(x)

#graph likert plot
p <- plot(x,legend.position = "right", 
          centered=FALSE, 
          plot.percents=FALSE,
          plot.percent.neutral =FALSE, 
          plot.percent.low=FALSE, 
          plot.percent.high=FALSE)+
  scale_fill_brewer(palette="RdYlBu",direction=1) +
  theme_cowplot(font_size = 24, ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill='white',color=NA),
        legend.position="bottom",
        axis.text.y = element_text(lineheight=.7))+
  guides(fill=guide_legend(nrow=1))+
  ylab('Percentage') +
  theme(#legend.box.margin = margin(),
    legend.justification = "right",
    legend.position.inside = c(0.0, 0.0),
    legend.text=element_text(size=14),)+
  labs(fill='')
p

ggsave(path = (here("figs")), filename = paste('fig2','.png',sep=""),
   width = 12, height = 6, dpi=600, units = "in")

### fig s1a
# q71 - map of participant countries 
#q71 <- "q71_list_of_countries"

as.data.frame(table(data[203]))[,2]

countries <- as.data.frame(table(data[203]))[,1]

metric <- as.data.frame(table(data[203]))[,2]

country_df <- data.frame(countries, metric)

country_df <- country_df %>%
  mutate(countries = recode(countries, 'United States of America' = 'USA',
                            'United Kingdom of Great Britain and Northern Ireland' = 'UK'))

world_map <- map_data(map = "world") %>% 
  filter(region != "Antarctica") 

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() +
  coord_cartesian(ylim = c(-50, 90)) 

dev.off()

g <- ggplot(country_df) +
  geom_map(aes(map_id = countries, fill = metric), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradient(low="lightblue", high="darkblue", name="Participant Count") +
  theme_void()+
  theme(plot.background = element_rect(fill='white',color=NA))+
  theme(legend.position="bottom") +
  coord_fixed()+
  theme( legend.text = element_text(size=100),
         legend.title = element_text( size=80),
         legend.key.width = unit(5, "in"))

g
ggsave(path = (here("figs")), filename = 'figs1a.png',
       width = 40, height = 30, units = "in")

### fig s1b
#

q75 = "q75_what_sectors_does_your_organisation_primarily_focus_on_check_all_that_apply_selected_choice"

names(data)[names(data) == q75] <- 'q75'

colourCount = length(unique(unlist(strsplit(data$q75, ","))))
#getPalette = colorRampPalette(brewer.pal(colourCount, "Blues"))

data.frame(quest = unlist(strsplit(data$q75, ","))) %>%
  
  ggplot(aes(x = fct_rev(fct_infreq(quest)), fill=quest)) +
  geom_bar(fill=getPalette(colourCount)) +
  theme_cowplot(font_size = 100,) +
  coord_flip() +
  ylab('Count')+
  xlab('')+
  scale_x_discrete(labels = label_wrap(40),
                   guide = guide_axis(n.dodge = 1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill='white',color=NA))+
  theme(plot.title = element_text(hjust = 1),)

ggsave(path = (here("figs")), filename = 'figs1b.png',
       width = 40, height = 30, units = "in")


### fig s1c
# q72 - how would you describe your role

q72 = "q72_how_would_you_describe_your_role_selected_choice"
names(data)[names(data) == q72] <- 'q72'

colourCount = length(unique(unlist(strsplit(data$q72, ","))))
#getPalette = colorRampPalette(brewer.pal(colourCount, "Blues"))

data.frame(quest = unlist(strsplit(data$q72, ","))) %>%
  
  ggplot(aes(x = fct_rev(fct_infreq(quest)), fill=quest)) +
  geom_bar(fill=getPalette(colourCount)) +
  theme_cowplot(font_size = 100,) +
  coord_flip() +
  ylab('Count')+
  xlab('')+
  scale_x_discrete(labels = label_wrap(40),
                   guide = guide_axis(n.dodge = 1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill='white',color=NA))+
  theme(plot.title = element_text(hjust = 1),)

ggsave(path = (here("figs")), filename = 'figs1c.png',
       width = 40, height = 30, units = "in")

### sig s1d
# q73 - years of professional experience 
q73 = "q73_how_many_years_of_professional_experience_do_you_have"
names(data)[names(data) == q73] <- 'q73'


colourCount = length(unique(unlist(strsplit(data$q73, ","))))
getPalette = colorRampPalette(brewer.pal(colourCount, "Blues"))

#fix formatting errors 
data$q73<- gsub(" â€“ ", "-", data$q73)
data$q73 <- factor(data$q73, levels = c('21+', '16-20', '11-15', '6-10', '0-5'))


ggplot(data, aes(x =q73)) +
  geom_bar(fill=getPalette(colourCount)) +
  scale_fill_brewer(palette = "Paired") +
  theme_cowplot(font_size = 100, 
                # fontface = "bold",
  ) +
  coord_flip() +
  ylab('Count')+
  xlab('')+
  scale_x_discrete(labels = label_wrap(40),
                   guide = guide_axis(n.dodge = 1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill='white',color=NA))+
  theme(plot.title = element_text(hjust = 1),)

ggsave(path = (here("figs")), filename = 'figs1d.png',
       width = 40, height = 30, units = "in")



