# Toy example providing a conceptual replication of the analysis of the
# following paper: 

# Liesenfeld, Andreas, and Mark Dingemanse. 2022. “Bottom-up Discovery of
# Structure and Variation in Response Tokens (‘Backchannels’) across Diverse
# Languages.” In Proceeding of Interspeech 2022. 2022.
# https://doi.org/10.21437/Interspeech.2022-11288.


# packages and useful functions -------------------------------------------

list.of.packages <- c("tidyverse","tidytext")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# install talkr and ifadv from github
devtools::install_github("elpaco-escience/talkr")
devtools::install_github("elpaco-escience/ifadv")

# load IFADV data
d <- ifadv::ifadv

# get top turns
topturns <- d %>%
  ungroup() %>%
  drop_na(n) %>% drop_na(FTO) %>% drop_na(utterance_stripped) %>% 
  filter(n > 1) %>%
  select(language,n,rank,rank_perc,freq,freq_perc,utterance,utterance_stripped,corpus) %>%
  group_by(language,freq,utterance_stripped) %>%
  slice(1) %>%
  group_by(language) %>%
  arrange(desc(freq)) %>%
  slice_max(n=20,freq,with_ties=F)

# tokenize IFADV data and get top 5 tokens

d.tokens <- d %>%
    ungroup() %>%
    drop_na(utterance_stripped) %>%
    tidytext::unnest_tokens(word, utterance_stripped) %>%
    count(word, sort=T, name="n") %>%
    mutate(rank = row_number(desc(n)),
           total = sum(n),
           freq = n/total,
           freq_log = log(freq),
           language = "dutch")

toptokens <- d.tokens %>%
  group_by(language) %>%
  select(language,word,n,total,rank,freq) %>%
  slice(1:5)


# Get streaks -------------------------------------------------------------

# Streaks: when one speaker produces a succession of similar turns

# we arrange by source and participant, then increment a counter for every
# successive similar turn

# the streak counter uses this cumsum trick by akrun: 
# https://stackoverflow.com/questions/58109098/cumulative-sum-based-on-a-condition-but-reset-after-condition-ends

d <- d %>% 
  group_by(language,source) %>%
  arrange(source,participant) %>% 
  mutate(flag = ifelse(utterance_stripped == lead(utterance_stripped) | 
                         utterance_stripped == lag(utterance_stripped),T,F)) %>%
  mutate(flag = ifelse(is.na(flag),F,flag)) %>%
  group_by(participant,group=cumsum(flag != lag(flag,default=first(flag)))) %>%
  mutate(streak = if(first(flag)) cumsum(flag) else NA,
         is_streak = ifelse(!is.na(streak),1,0)) %>%
  ungroup() %>%
  select(-c(flag,group)) %>%
  arrange(language,uid) 

# get streaks: a dataframe of all utterances that occur in a streak 
streak_cols <- c("uid","source","language","utterance","utterance_stripped","begin","end","participant","duration","FTO","overlap","nature","nchar","n","freq","rank","streak")

streaks <- d %>%
  select(all_of(streak_cols)) %>%
  filter(streak > 1,
         nature=="talk")

# get a simple list of continuer formats in this dataset
continuers <- streaks %>% 
  filter(streak > 2, n > 19,utterance==utterance_stripped) %>%
  group_by(language,utterance_stripped) %>% 
  summarise(utterance,streaks=n(),n,rank,nchar) %>% 
  slice(1) %>% 
  arrange(language,desc(n)) %>% 
  group_by(language) 

# extract list of UIDs of interest for audio extraction
write_csv(streaks,'data/continuers_in_streaks.csv')



# Plot some streaks as example --------------------------------------------

# get some uids for top turns
topturndata <- d[d$utterance %in% streaks$utterance,]


# select some streaks to plot

d |> filter(streak>2) |> 
  select(uid,utterance_stripped) |>
  group_by(utterance_stripped) |>
  slice(1:2)

these_uids <- c("dutch-15-276-328715","dutch-02-101-124408","dutch-15-062-72800")

# generate df with only these extracts
see <- talkr::convplot(data=d,uids=these_uids,datamode = T) 

# add column `in_streak` to use for highlighting
# (this is not ideal: we had this in d, convplot has stripped it)
see <- see |>
  mutate(in_streak = as.character(ifelse(uid %in% topturndata$uid,1,0)))

see |>
  ggplot(aes(y=participant_int,fill=in_streak)) +
  theme_tufte() + ylab("") + theme(legend.position = "none")  + xlab("relative time (ms)") +
  theme(axis.ticks.y = element_blank()) + theme(plot.title.position = "plot") + 
  ggtitle("Continuers in their natural sequential environment") +
  theme_tufte() + ylab("") + theme(legend.position = "none")  + xlab("relative time (ms)") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title.position = "plot",
        strip.text.y.left = element_text(angle=0,hjust=0)) + 
  scale_y_continuous(breaks=c(1:max(see$participant_int)),
                     labels=rev(LETTERS[1:max(see$participant_int)])) +
  geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.6,ymax=participant_int+0.6),
            linewidth=1) +
  geom_text(data=see %>% filter(utterance %in% topturndata$utterance),
            aes(label=utterance_stripped,y=participant_int,x=begin0+90),
            hjust="left",size=3,colour="black") +
  facet_wrap(~ scope, nrow=length(unique(see$scope)),strip.position = "left")


