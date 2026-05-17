library(tidyverse)
library(janitor)
library(skimr)

#Read the file#

netflix<-read_csv("data/netflix_titles.csv")

#Lowercase and snake case column names#

netflix<-clean_names(netflix)

#Inspect the structure#
head(netflix)

#check structure#
glimpse(netflix)

#check missing values#
colSums(is.na(netflix))

#Full data Profile#
skim(netflix)

############################################
#########STEP 3 -Data Cleaning##############
############################################


#convert date_added to date format
netflix<-netflix%>%
  mutate(date_added= as.Date(date_added,format="%B %d, %Y"))

summary(netflix$date_added)

#Extract numeric duration
netflix<-netflix%>%
  mutate(
    duration_num=parse_number(duration),
    duration_type=if_else(str_detect(duration,"Season"),"Season","Minute")
  )

table(netflix$duration_type)

#Split Genres into a usable format
netflix<-netflix%>%
  mutate(primary_genre=str_trim(str_extract(listed_in,"^[^,]+")))

head(netflix$primary_genre)

#Replace missing country with Unknown
netflix<-netflix%>%
  mutate(country=replace_na(country,"Unknown"))

#replace missing rating with not rated
netflix<-netflix%>%
  mutate(rating=replace_na(rating,"Not Rated"))

#convert categorical variables to factors
netflix<-netflix%>%
  mutate(
    type=factor(type),
    rating=factor(rating),
    primary_genre=factor(primary_genre)
  )

skim(netflix)

#############################################
######Exploratory Data Analysis##############
#############################################


#Movies vs TV shows
ggplot(netflix,aes(x=type,fill=type))+
  geom_bar()+
  scale_fill_manual(values=c("Movie"="#E50914","TV Show"="#221F1F"))+
  labs(
    title = "Distribution of Movies vs TV shows on netflix",
    x="Type",
    y="Count"
  )+
  theme_minimal()


#Top 10 Genres
netflix %>%
  count(primary_genre,sort=TRUE)%>%
  slice(1:10)%>%
  ggplot(aes(x=reorder(primary_genre,n),y=n))+
  geom_col(fill="#E50914")+
  coord_flip()+
  labs(
    title="Top 10 Genres on Netflix",
    x="Genre",
    y="Count"
  )+
  theme_minimal()
  
#content added overtime
netflix %>%
  mutate(year_added=lubridate::year(date_added))%>%
  count(year_added) %>%
  filter(!is.na(year_added))%>%
  ggplot(aes(x=year_added,y=n))+
  geom_line(color="#E50914",size=1.2)+
  geom_point(color="#221F1F",size=2)+
  labs(
    title="Number of Titles Added to Netflix over Time",
    x="Year Added",
    y="Count"
  )+
  theme_minimal()
  )


#Top countries producing netflix content
netflix %>%
  separate_rows(country,sep=",") %>%
  mutate(country=str_trim(country)) %>%
  count(country,sort=TRUE) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(country,n),y=n))+
  geom_col(fill="#E50914")+
  coord_flip()+
  labs(
    title="Top 10 Countries Producing Netflix Content",
    x="Country",
    y="Count"
  ) +
  theme_minimal()


#########################################
##Text Analysis on Netflix Descriptions##
#########################################


#Load text-analysis pakages
install.packages('wordcloud')
install.packages('tidytext')

library(tidytext)
library(wordcloud)
library(RColorBrewer)
  
#Tokenize the description column
words<-netflix %>%
  select(show_id,type,description) %>%
  unnest_tokens(word,description)

head(words)

#Remove stopwords

data("stop_words")

clean_words<-words %>%
  anti_join(stop_words,by="word") %>%
  filter(!str_detect(word,"^[0-9]+$"))


#count the most common words
top_words<-clean_words%>%
  count(word,sort=TRUE)%>%
  slice(1:20)

top_words


set.seed(123)


#Word cloud
wordcloud(
  words=top_words$word,
  freq=top_words$n,
  min.freq = 1,
  max.words=100,
  random.order=FALSE,
  colors=brewer.pal(8,"Reds")
)

#Compare movies vs tv shows
movie_words<-clean_words %>%
  filter(type=="Movie") %>%
  count(word,sort=TRUE) %>%
  slice(1:15)

tv_words<-clean_words %>%
  filter(type=="TV Show") %>%
  count(word,sort=TRUE) %>%
  slice(1:15)



movie_words
tv_words

##########################################
#########Deep Dive Analysis###############
##########################################

summary(netflix)
head(netflix)
netflix$country


#countries produce most netflix content
top_countries<-netflix %>%
  separate_rows(country,sep=",") %>%
  mutate(country=str_trim(country)) %>%
  count(country,sort=TRUE) %>%
  slice(1:10)

top_countries
  
#Directors produce most often
netflix$director

top_director<-netflix %>%
  separate_rows(director,sep=",") %>%
  mutate(director=str_trim(director)) %>%
  count(director,sort=TRUE) %>%
  slice(1:10)

top_director

#Actors appear most often
netflix$cast

top_cast<-netflix%>%
  separate_rows(cast,sep=",")%>%
  mutate(cast=str_trim(cast)) %>%
  count(cast,sort=TRUE) %>%
  slice(1:10)

top_cast

#how netflix content changed over time
content_over_time<-netflix%>%
  mutate(year_added=lubridate::year(date_added))%>%
  count(year_added)%>%
  filter(!is.na(year_added))

content_over_time 


ggplot(content_over_time,aes(x=year_added,y=n))+
  geom_line(color="#E50914",size=1.2)+
  geom_point(color="#221F1F",size=2)+
  labs(
    title="Netflix Content change over Time",
    x="Year",
    y="Number of Titles"
)+
  theme_minimal()
