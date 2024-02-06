# simple convplot demo


# packages and useful functions -------------------------------------------

# install talkr and ifadv from github
devtools::install_github("elpaco-escience/talkr")
devtools::install_github("elpaco-escience/ifadv")

list.of.packages <- c("tidyverse","ggthemes","talkr","ifadv")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# load helper functions
source("helper_functions.R")

# load ifadv data
d <- ifadv::ifadv


# sample some dyadic conversations ----------------------------------------

convplot(data=d,n_uid=4,content=T,printuids=T,window=10000)
# explanation
# use d (data=d)
# sample 4 uids (n_uid=4)
# show annotation content (content=T)
# print the sampled UIDs in the console (printuids=T)
# show a 10 second window (window=10000)


# Sample a specific conversation ------------------------------------------

# say we have a uid
this_uid <- "dutch-20-651-795872"

# we want to plot a few seconds around it, highlighting the uid in focus
convplot(data=d,this_uid,before=10000,after=10000,highlight=T)



# Use convplot to generate a dataframe to be plotted ----------------------

this_extract <- convplot(data=d,this_uid,before=10000,after=10000,datamode=T)


this_extract |> 
  ggplot(aes(y=participant_int)) +
  theme_tufte() + theme(legend.position = "none",
                        strip.text = element_blank(),
                        axis.ticks.y = element_blank()) +
  ylab("") + xlab("time (s)") +
  # scale_y_continuous(breaks = c(1:2),
  #                      labels = rev(LETTERS[1:2])) +
  scale_y_continuous(breaks = c(1:2)) +
  scale_x_continuous(breaks=seq(0,window_size,10000),
                     label=seq(0,window_size/1000,10)) +
  geom_rect(aes(xmin=begin0,xmax=end0,
                ymin=participant_int+0.4,
                ymax=participant_int-0.4))



# Plot an extract consisting of multiple lines ----------------------------

extract_length <- 120000 # 2 min
window_size <- 30000 # 30s
window_breaks <- as.integer(c(0:round(extract_length/window_size)) * window_size)

extract <- convplot(data=d,this_uid,datamode=T,before=0,after=extract_length)


# we make lines by categorizing begin values into intervals the width of window_size
# we drop turns that fall outside the larger interval

# to check: this should use add_lines but that function currently generates an error: 
# extract <- add_lines(data=extract,line_duration=30000)

extract <- extract |>
  mutate(end = end - min(begin), # reset timestamps to start from 0
         begin = begin - min(begin),
         line = cut(begin,window_breaks,right=F,labels=F)) |>
  drop_na(line) |>
  group_by(line) |>
  mutate(begin0 = begin - min(begin), # reset timestamps to 0 for each new line
         end0 = end - min(begin)) |>
  ungroup()

library(viridis)

extract |> 
  ggplot(aes(y=participant_int,fill=load)) +
  theme_tufte() + theme(legend.position = "none",
                        #strip.text = element_blank(),
                        axis.ticks.y = element_blank()) +
  ylab("") + xlab("time (s)") +
  scale_fill_viridis(option="plasma",direction=1,begin=0.2,end=0.8) +
  scale_y_reverse(breaks=seq(1,max(extract$line,1)),
                  labels=seq(1,max(extract$line,1))) +
  coord_cartesian(xlim=c(0,window_size)) +
  scale_x_continuous(breaks=seq(0,window_size,10000),
                     label=seq(0,window_size/1000,10)) +
  geom_rect(aes(xmin=begin0,xmax=end0,
                ymin=line-0.75+participant_int/2-0.2,
                ymax=line-0.75+participant_int/2+0.2),
            linewidth=0.3,colour="white") +
  scale_colour_viridis(option="plasma",direction=1,begin=0.2,end=0.8) +
  facet_wrap(~ scope,ncol=1) 



