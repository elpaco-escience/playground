# Toy example providing a conceptual replication of the analysis of the
# following paper: 

# Liesenfeld, Andreas, and Mark Dingemanse. 2022. “Bottom-up Discovery of
# Structure and Variation in Response Tokens (‘Backchannels’) across Diverse
# Languages.” In Proceeding of Interspeech 2022. 2022.
# https://doi.org/10.21437/Interspeech.2022-11288.


# packages and useful functions -------------------------------------------

list.of.packages <- c("tidyverse","ggthemes","ggridges","viridis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# install talkr and ifadv from github

# load IFADV data

# define streaks

# set streak counter

# identify continuers

# extract list of UIDs of interest for audio extraction