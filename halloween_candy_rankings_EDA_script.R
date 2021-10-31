#### Halloween Candy Rankings ####

### Load libraries ###
library(tidyverse)
library(corrplot)
library(fivethirtyeight)

# Import the candy_rankings dataset from 538 library
data(candy_rankings)

# Glimpse
glimpse(candy_rankings)


### Distributions of Different Feature Types of Each Candy ###
# Gather the categorical variables into a long-format
candy_rankings_long <- gather(candy_rankings,
                              c('chocolate', 'fruity', 'caramel', 'peanutyalmondy',
                                'nougat', 'crispedricewafer', 'hard', 'bar', 'pluribus'),
                              key = 'feature',
                              value = 'value')

# Make a bar plot showing the distribution of each variable
ggplot(candy_rankings_long, aes(x = value, fill = value)) +
  geom_bar(stat = 'count', show.legend = FALSE) +
  xlab(element_blank()) +
  ylab('Count') +
  scale_fill_manual(values = c('orange', 'black')) +
  facet_wrap(. ~ feature) +
  theme_classic()


### Pricepercent Comparison ###
# Make a lollipop chart of pricepercent
ggplot(candy_rankings, aes(x = reorder(competitorname, pricepercent), y = pricepercent)) +
  geom_segment(aes(xend = reorder(competitorname, pricepercent), yend = 0),
               linetype = 'dashed', size = 0.05) +
  geom_point(size = 1, color = 'orange', fill = alpha('orange', 1),
             shape = 21, stroke = 2) +
  xlab(element_blank()) +
  ylab('Price Percent') +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_text(vjust = 0.25, hjust = 1)) +
  coord_flip()


### Winpercent Distribution ###
# Make a histogram showing the distribution of winpercent
ggplot(candy_rankings, aes(x = winpercent)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = .95) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  xlab('Win Percent') +
  ylab('Frequency') +
  theme_classic()


### Winpercent comparison ###
# Make a lollipop chart of winpercent
ggplot(candy_rankings, aes(x = reorder(competitorname, winpercent), y = winpercent)) +
  geom_segment(aes(xend = reorder(competitorname, winpercent), yend = 0),
               linetype = 'dashed', size = 0.05) +
  geom_point(size = 1, color = 'orange', fill = alpha('orange', 1),
             shape = 21, stroke = 2) +
  xlab(element_blank()) +
  ylab('Win Percent') +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_text(vjust = 0.25, hjust = 1)) +
  coord_flip()