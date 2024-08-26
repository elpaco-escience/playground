# playground plots

# playground for new talkr plots

# packages and useful functions -------------------------------------------

# install talkr and ifadv from github
devtools::install_github("elpaco-escience/talkr")
devtools::install_github("elpaco-escience/ifadv")

list.of.packages <- c("tidyverse","ggthemes","talkr","ifadv","RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


# data --------------------------------------------------------------------

# initialize a dataset
data <- init(ifadv::ifadv)

# simplify participant names
conv <- data |>
  group_by(source) |>
  mutate(participant = as.character(factor(participant, labels=c("A","B"),ordered=T)))



# Bateson plot ------------------------------------------------------------

library(viridis)

# iteration 1

# not great: when using geom_text() at an angle we can't benefit from ggplot's
# nice plot margin calculations so we have to set ylim manually using
# coord_cartesion

conv |>
  filter(source == "/dutch2/DVA12S",
         end < 30000 ) |>
  ggplot(aes(x = end, y = participant)) +
  ggtitle("Experimental: Bateson plot") + 
  geom_turn(aes(
    begin = begin,
    end = end,
    fill = participant),
    height=0.8) +
  geom_text(aes(label=utterance_raw,
                x=begin,
                colour=participant),
            y = 3,
            angle = 45,
            hjust=0) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  xlab("Time (ms)") +
  ylab("") +
  coord_cartesian(ylim=c(1,10)) +
  theme_turnPlot() + theme(legend.position="none")

# Iteration 2: Fish bone plot

conv <- init(ifadv::ifadv) |>
  group_by(source) |>
  mutate(participant = as.character(factor(participant, labels=c("A","B"),ordered=T)))

# what's very hacky (and ugly) about this is that in addition to more manual
# settings for coord_cartesion, we hardcode the angle to be used by
# `geom_text()`, as well as the y coordinate for y placement of the text.

this_conv <- conv |>
  filter(source == "/dutch2/DVA12S",
         end < 30000 ) |>
  mutate(angle = case_when(participant == "A" ~ -45,
                           participant == "B" ~ 45,
                           .default = NULL),
         y_text = case_when(participant == "A" ~ 0,
                            participant == "B" ~ 3,
                            .default = NULL))

this_conv |>
  ggplot(aes(x = end, y = participant)) +
  ggtitle("Experimental: Fish bone plot") + 
  geom_turn(aes(
    begin = begin,
    end = end,
    fill = participant),
    height=0.8) +
  geom_text(aes(label=utterance_raw,
                x=begin,
                angle=angle,
                colour=participant,
                y = y_text),
            hjust=0,
            ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  xlab("Time (ms)") +
  ylab("") +
  coord_cartesian(ylim=c(-10,10)) +
  theme_turnPlot() + theme(legend.position="none")
ggsave("samples/playground-plots_fishbone.png",width=12,height=6,bg="white")

# Iteration 3: using ggrepel

library(ggrepel)

this_conv <- conv |>
  filter(source == "/dutch2/DVA12S",
         end < 30000 )

this_conv |>
  ggplot(aes(x = begin, y = participant, label=utterance_raw,colour=participant)) +
  ggtitle("Experimental: Using geom_text_repel()") + 
  geom_turn(aes(
    begin = begin,
    end = end,
    fill = participant),
    height=0.8) +
  geom_text_repel(data=this_conv |> dplyr::filter(participant=="A"),
                  hjust=0,nudge_y = -1,direction="y"
  ) +
  geom_text_repel(data=this_conv |> dplyr::filter(participant=="B"),
                  hjust=0,nudge_y = 1,direction="both"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  xlab("Time (ms)") +
  ylab("") +
  coord_cartesian(ylim=c(-2,4)) +
  theme_turnPlot() + theme(legend.position="none")
ggsave("samples/playground-plots_repel1.png",width=12,height=6,bg="white")

