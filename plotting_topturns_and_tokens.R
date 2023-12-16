# Toy example replicating the plot in this paper: 

# Dingemanse, Mark. 2024. ‘Interjections at the Heart of Language’. Annual
# Review of Linguistics.
# https://doi.org/10.1146/annurev-linguistics-031422-124743.


# packages and useful functions -------------------------------------------

# install talkr and ifadv from github
devtools::install_github("elpaco-escience/talkr")
devtools::install_github("elpaco-escience/ifadv")

list.of.packages <- c("tidyverse","tidytext","ggthemes","viridis","talkr","ifadv")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# load helper functions
source("helper_functions.R")

# prepare data


d <- ifadv::ifadv

# add topturns: highly frequent recurring turn formats

# add topturns: highly frequent recurring turn formats
# add streaks: successive occurrences of top turns by same participant
# we set a flag whenever a prior or next turn by same participant is also a top turn,
# then set streak=1 for consecutive flags and sum the flags to form streak_pos

d <- d |>
  ungroup() |>
  mutate(topturn = ifelse(rank < 21 & n > 20,1,0)) |>
  group_by(source,participant) |> 
  mutate(flag = ifelse(topturn==1 & ( lead(topturn)==1 | lag(topturn)==1),T,F)) |>
  mutate(flag = ifelse(is.na(flag),F,flag)) |> # set all NA flags to F
  arrange(source,participant) |> # arrange by source and participant
  group_by(group=cumsum(flag != lag(flag,default=first(flag)))) |>
  mutate(streak_pos = if(first(flag)) cumsum(flag) else NA,
         streak = ifelse(!is.na(streak_pos),1,0)) |>
  ungroup() |>
  select(-c(flag,group)) |>
  arrange(language,uid)



# create df with top 10 interjection forms (= single token turns)

interjections <- d |>
  drop_na(utterance_stripped) |>
  group_by(language) |>
  mutate(total_turns = n()) |>
  group_by(language,utterance_stripped) |>
  filter(topturn==1,nwords==1) |> 
  slice(1) |>
  group_by(language) |>
  arrange(desc(n)) |> slice(1:10) |> ungroup() |>
  select(language,utterance_stripped,n,rank,total_turns)


# descriptive stats -------------------------------------------------------

# get metadata at source level

totals_by_source <- d |> group_by(language,source) |>
  drop_na(duration) |>
  summarize(firstuid=uid[1],
            start=min.na(begin),
            finish=max.na(end),
            turns=n_distinct(uid),
            totaltime = finish - start,
            notiming = sum(is.na(duration)),
            participants = length(unique(participant)),
            useless = ifelse(notiming==turns,1,0))


totals_by_language <- totals_by_source |>
  group_by(language) |> 
  summarise(totalturns = sum.na(turns),
            totaltime = sum.na(totaltime),
            totalsources = n_distinct(source),
            hours= totaltime / 1000 / 3600,
            participants = sum.na(participants))



# count the proportion of turns that is an interjection

interjections_proportional <- interjections |>
  group_by(language) |>
  #  filter(total_turns > minimum_turns ) |>
  summarise(interjection_turns = sum(n),
            total_turns,
            prop_interjections = interjection_turns / total_turns) |>
  slice(1) |>
  arrange(desc(total_turns))

interjections_proportional

# all interjections
interjections_proportional |>
  ungroup() |>
  summarise(allinterjections = sum(interjection_turns))


# prepare plotting --------------------------------------------------------

extract_length <- 600000 # 10 min
window_size <- 60000 # 1 min
window_breaks <- as.integer(c(0:round(extract_length/window_size)) * window_size)

# select convos of >extract_length that are dyadic
long_dyadic_convos <- totals_by_source |> 
  filter(participants==2,
         totaltime > extract_length, 
         notiming == 0) # max n per language

these_convos <- long_dyadic_convos |> slice(1:6)

# this for loop takes these_convos
for(i in 1:nrow(these_convos)) {
  current_convo <- these_convos$firstuid[i]
  cat(paste0("Now doing: "))
  
  
  # i <- 1  
  # create dataframe
  extract <- convplot(data=d,current_convo,datamode=T,before=0,after=extract_length)
  
  # we make lines by categorizing begin values into intervals the width of window_size
  # we drop turns that fall outside the larger interval
  extract <- extract |>
    mutate(end = end - min(begin), # reset timestamps to start from 0
           begin = begin - min(begin),
           line = cut(begin,window_breaks,right=F,labels=F)) |>
    drop_na(line) |>
    group_by(line) |>
    mutate(begin0 = begin - min(begin), # reset timestamps to 0 for each new line
           end0 = end - min(begin))
  
  
  
  # get some basic stats
  
  number_of_turns <- length(unique(extract$uid))
  number_of_interjection_turns <- length(unique(extract |> ungroup() |> filter(nwords==1,topturn==1) |> select(uid) |> unlist() |> as.character()))
  prop_of_interjections <- round(100*number_of_interjection_turns / number_of_turns,1)
  
  # highlight recurrent one-word turn formats (=interjections) 
  # fill by rank
  
  extract |> 
    ggplot(aes(y=participant_int)) +
    theme_tufte() + theme(legend.position = "none",
                          strip.text = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_blank()) +
    ggtitle(paste0(extract$language[1]," (",extract$source[1],")\n",
                   number_of_interjection_turns," interjections in this segment of ",number_of_turns," turns (",prop_of_interjections,"%)")) +
    ylab("") + xlab("time (s)") +
    scale_fill_viridis(option="plasma",direction=1,begin=0.2,end=0.8) +
    scale_y_reverse(breaks=c(1:2),
                    labels=LETTERS[1:2]) +
    coord_cartesian(xlim=c(0,window_size)) +
    scale_x_continuous(breaks=seq(0,window_size,10000),
                       label=seq(0,window_size/1000,10)) +
    geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.6,ymax=participant_int+0.6),
              linewidth=0.3,colour="white",fill="lightgrey") +
    geom_point(data=extract |> filter(nwords == 1, topturn == 1),
               aes(x=begin0+200,fill=rank),colour="white",size=3,shape=21,stroke=1) +
    facet_wrap(~line,ncol=1)
  filename_pdf <- paste0("samples/rakeplot-10m-circles-",extract$uid[1],".pdf")
  filename_png <- paste0("samples/rakeplot-10m-circles-",extract$uid[1],".png")
  ggsave(filename_pdf,width=5,height=4)
  ggsave(filename_png,width=5,height=4,bg="white")
  
}
  


# We can also plot the actual interjections -------------------------------


# generate version with interjections forms as transcribed

extract_length <- 600000 # 10 min
window_size <- 60000 # 1 min
window_breaks <- as.integer(c(0:round(extract_length/window_size)) * window_size)

these_uids <- these_convos$firstuid[1:4]

# create dataframe
extract <- convplot(data=d,these_uids,datamode=T,before=0,after=extract_length)

# we make lines by categorizing begin values into intervals the width of window_size
# we drop turns that fall outside the larger interval

# to check: this should use add_lines but that function currently generates an error: 
# extract <- add_lines(data=extract,line_duration=60000)

extract <- extract |>
  mutate(end = end - min(begin), # reset timestamps to start from 0
         begin = begin - min(begin),
         line = cut(begin,window_breaks,right=F,labels=F)) |>
  drop_na(line) |>
  group_by(line) |>
  mutate(begin0 = begin - min(begin), # reset timestamps to 0 for each new line
         end0 = end - min(begin)) |>
  ungroup()

# highlight recurrent one-word turn formats (=interjections) 
# fill by rank

extract |> 
  ggplot(aes(y=participant_int)) +
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
  geom_rect(aes(xmin=begin0,xmax=end0,ymin=line-1+participant_int/2-0.2,ymax=line-1+participant_int/2+0.2),
            linewidth=0.3,colour="white",fill="lightgrey") +
  scale_colour_viridis(option="plasma",direction=1,begin=0.2,end=0.8) +
  geom_label(data=extract |> filter(nwords==1,topturn==1),
             aes(x=begin0,fill=rank,y=line-1+participant_int/2,label=utterance_stripped),
             colour="white",size=2.2,label.padding = unit(0.1, "lines"),hjust=0) +
  facet_wrap(~ scope,ncol=2) 
filename <- paste0("samples/panel-ifadv-interjections.png")
ggsave(filename,width=8,height=6,bg="white")


