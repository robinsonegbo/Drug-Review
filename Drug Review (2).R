library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

#reviews <- read_csv("https://raw.githubusercontent.com/hillt5/DATA607_Final_Project/master/drugsComTrain_raw.csv", col_types = "dffcdcd") #Set column datatypes
reviews <- read.csv("drugsComTest_raw.csv")
reviews$date <- as.Date(reviews$date, format = "%d-%b-%y") #set date format

head(reviews)
summary(reviews)

num_obs <- nrow(reviews) #Number of reviews
summary(reviews$drugName, 25) #Top 25 drugs by review frequency

round(100*(num_obs-126229)/num_obs, 1) #Percent of reviews that fall within the top 25 drugs
summary(reviews$condition, 25) #top 25 conditions being treated

round(100*(num_obs-61287)/num_obs, 2) #Percent of reviews that fall within the top 25 conditions

reviews_edit <- reviews #Create new file from raw input
reviews_edit$condition <- reviews_edit$condition %>%
  recode_factor(ibromyalgia = "Fibromyalgia", atigue = "Fatigue") %>% #Fix two spelling errors
  na_if("Not Listed / Othe") #Recode values as 'NA'

error_span <- str_detect(reviews_edit$condition, pattern = "</span>") #Identify erroneous entries
reviews_edit$condition <- replace(reviews_edit$condition, list = error_span, NA) #Replace with NA
summary(reviews_edit$condition, 25) #First 25 entries


review_yr <- reviews_edit %>%
  mutate(year = year(date)) %>% #Find year of review
  group_by(year) %>% #Group by drug, date of review
  count()
ggplot(review_yr) +
  geom_line(aes(x = year, y = n)) +
  geom_point(aes(x = year, y = n)) +
  labs(title = "Number of reviews over time", x = "Year", y = "Number of Reviews") + #Change in reviews over time
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016))


#Contraseptive
oc_reviews <- reviews_edit %>%
  filter(condition == 'Birth Control')
head(oc_reviews)


n_oc_reviews <-nrow(oc_reviews) #Number of reviews of birth control
oc_reviews %>%
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_oc_reviews),1)) %>% #Percent of all birth control reviews
  arrange(desc(n)) #Highest frequency first


ggplot(oc_reviews) +
  geom_bar(aes(x = rating), fill = "#042f66")+
  labs(x = "Rating (out of 10)", y = "Number of ratings", title = "Ratings for Oral Contraceptives") +
  scale_x_continuous(breaks = c(1:10))


oc_reviews_bad <- oc_reviews %>%
  filter(rating == 1) %>% #Looking closer at the anomaly with rating '1'
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_oc_reviews),1)) %>% #As percent of total reviews
  arrange(desc(n)) 

oc_reviews_bad


oc_reviews_popular <- oc_reviews %>%
  filter(drugName == c('Etonogestrel', 'Ethinyl estradiol / norethindrone', 'Nexplanon', 'Ethinyl estradiol / norgestimate', 'Ethinyl estradiol / levonorgestrel'))


ggplot(oc_reviews_popular) +
  geom_bar(aes(x = rating, fill = drugName))+
  labs(x = "Rating (out of 10)", y = "Number of ratings", title = "Ratings for Top 5 Contraceptives") +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = c(1:10))

####Anxiety & Depression
ad_reviews <- reviews_edit %>%
  filter(condition == c('Depression', 'Insomnia', 'Anxiety', 'Anxiety and Stress', 'Major Depressive Disorder', 'Panic Disorder'))


n_ad_reviews <- nrow(ad_reviews) #number of reviews for depression/ anxiety meds

ad_reviews %>%
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_ad_reviews),1)) %>% #Percent of all ax/dep reviews
  arrange(desc(n)) #Highest first


ggplot(ad_reviews) +
  geom_bar(aes(x = rating), fill= "#042f66") +
  labs(x = "Rating (out of 10)", y = "Number of ratings", title = "Ratings for Anxiety and Depression")

ad_reviews_popular <- ad_reviews %>%
  filter(drugName == c('Escitalopram', 'Sertraline', 'Citalopram', 'Bupropion', 'Fluoxetine'))

ggplot(ad_reviews_popular) +
  geom_bar(aes(x = rating, fill = drugName)) +
  labs(x = "Rating (out of 10)", y = "Number of ratings", title = "Ratings for Top 5 Treatments for Anxiety or Depression") +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = c(1:10))


ad_reviews_bad <- ad_reviews %>%
  filter(rating == 1) %>% #Worst ratings
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_ad_reviews),1)) %>% #Percent of total ratings
  arrange(desc(n)) #Most frequently given first

ad_reviews_bad


ad_reviews_good <- ad_reviews %>% 
  filter(rating == 10) %>% #Best ratings
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_ad_reviews),1)) %>% #Perecnt of total ratings
  arrange(desc(n)) #Most frequently first

ad_reviews_good


ad_reviews_wo_insomnia <- ad_reviews %>%
  filter(condition != 'Insomnia') #Omit treatment for insomnia

n_ad_reviews_wo_insomnia <-nrow(ad_reviews_wo_insomnia) #Number of reviews for anxiety and depression, not insomnia

ad_reviews_wo_insomnia %>%
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_ad_reviews_wo_insomnia),1)) %>% #Percent of reviews
  arrange(desc(n)) #Most frequent first


ad_reviews_wo_insomnia_bad <- ad_reviews_wo_insomnia %>%
  filter(rating == 1) %>% #Worst ratings
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_ad_reviews_wo_insomnia),1)) %>% #Percent of reviews 
  arrange(desc(n)) #Most frequent first

ad_reviews_wo_insomnia_bad


ad_reviews_wo_insomnia_good <- ad_reviews_wo_insomnia %>%
  filter(rating == 10) %>% #Best ratings
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_ad_reviews_wo_insomnia),1)) %>% #Percent of reviews
  arrange(desc(n)) #Most frequent first

ad_reviews_wo_insomnia_good


ad_reviews_edit <- ad_reviews #Create duplicate of anxiety/depression reviews dataframe

#Below, I've recoded the medication names, which are factors, as their generic equivalent. This is largely possible because there are only a handful reviewed
ad_reviews_edit$drugName <- ad_reviews$drugName %>% 
  recode_factor('Klonopin' = 'Clonazepam', 'Xanax' = 'Alprazolam', 'Xanax XR' = 
                  'Alprazolam', 'Ambien' = 'Zolpidem', 'Ambien CR' = 'Zolpidem',  'Lunesta' = 'Eszopiclone', 'Ativan' = 'Lorazepam', 'Restoril'= 'Temazepam')

#Below are the generic names for the sedatives I was talking about earlier: benzodiazepines --usualy ends in '-zolam' -- and two hyptnotics that affect the same receptors 
ad_reviews_bzd <- ad_reviews %>%
  filter(drugName == c('Clonazepam', 'Alprazolam', 'Zolpidem', 'Temazepam', 'Eszopiclone',  'Diazepam', 'Lorazepam', 'Oxazepam', 'Triazolam', 'Chlordiazepoxide'))


ggplot(ad_reviews_bzd) +
  geom_bar(aes(x = rating, fill = drugName)) +
  labs(x = "Rating (out of 10)", y = "Number of ratings", title = "Ratings of Sedatives", fill = "Drug") +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = c(1:10))


##Weight loss
wl_reviews <- reviews_edit %>%
  filter(condition == 'Weight Loss')
n_wl_reviews <-nrow(wl_reviews) #Number of reviews for weight loss drugs
head(wl_reviews)


wl_reviews %>%
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_wl_reviews),1)) %>%
  arrange(desc(n))


wl_reviews_edit <- wl_reviews #Create duplicate dataframe for edits

#Recode brand products to generic
wl_reviews_edit$drugName <- recode_factor(wl_reviews$drugName, 'Megace' = 'Megestrol', 'Megace ES' = 'Megestrol', 'Megestrol' = 'Megestrol')

#Remove medication not used for weight loss
wl_reviews_edit <- wl_reviews_edit%>%
  filter(drugName != 'Megestrol')

n_wl_reviews <- nrow(wl_reviews_edit) #Find true number of reviews

#Recode the rest, set default generic to phentermine as this is the most common entry
wl_reviews_edit$drugName <- wl_reviews_edit$drugName %>%
  recode_factor(Belviq = "Locaserin", "Belviq XR" = "Locaserin", Locaserin = "Locaserin", Contrave = "Bupropion / naltrexone", "Bupropion / naltrexone" = "Bupropion / naltrexone", Qsymia = "Phentermine/ topiramate", "Phentermine/ topiramate" = "Phentermine/ topiramate", Saxenda = "Liraglutide",Victoza = "Liraglutide", Liraglutide = "Liraglutide", .default = "Phentermine")

wl_reviews_edit %>%
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_wl_reviews),1)) %>% #Percent of total reviews
  arrange(desc(n)) #Highest frequency first


ggplot(wl_reviews_edit) +
  geom_bar(aes(x = rating, fill = drugName)) +
  labs(x = "Rating (out of 10)", y = "Number of ratings", title = "Ratings for Weight Loss") +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = c(1:10))


ggplot(wl_reviews_edit) +
  geom_bar(aes(x = rating)) +
  labs(x = "Rating (out of 10)", y = "Number of ratings", title = "Ratings for Weight Loss") + 
  facet_wrap(~drugName) + #For each separate drug 
  scale_x_continuous(breaks = c(1:10))


wl_yrly_reviews <- wl_reviews_edit %>%
  mutate(year = year(date)) %>% #Find year of review
  group_by(drugName, year) %>% #Group by drug, date of review
  count()


ggplot(wl_yrly_reviews) +
  geom_line(aes(x = year, y = n, color = drugName)) +
  geom_point(aes(x = year, y = n, color = drugName)) +
  labs(title = "Number of ratings for weight loss products over time", x = "Year", y = "Number of Reviews", color = "Drug") + #Change in reviews over time
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016))


##Smoking Cessasion
cs_reviews <- reviews_edit %>%
  filter(condition == 'Smoking Cessation')
n_cs_reviews <-nrow(cs_reviews) #Number of smoking cessation drugs reviewed
head(cs_reviews)


cs_reviews %>%
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_cs_reviews),1)) %>%
  arrange(desc(n))


cs_reviews_edit <- cs_reviews

#Recode products as generic drug names
cs_reviews_edit$drugName <- cs_reviews$drugName %>% 
  recode_factor(Chantix = "Varenicline", Varenicline = "Varenicline", Bupropion = "Bupropion",  Buproban = "Bupropion", Zyban = "Bupropion", Nortriptyline = "Nortriptyline", Pamelor = "Nortriptyline", Topiramate = "Topiramate", Topamax = "Topiramate", .default = "Nicotine")

cs_reviews_edit %>%
  group_by(drugName) %>%
  count() %>%
  mutate(pct_reviews = round((100*n/n_cs_reviews),1)) %>% #Percent of total reviews
  arrange(desc(n)) #Most frequent first


ggplot(cs_reviews_edit) +
  geom_bar(aes(x = rating, fill = drugName)) +
  labs(x = "Rating (out of 10)", y = "Number of ratings, log scale", title = "Ratings for Smoking Cessation") +
  scale_x_discrete(breaks = 10) +
  scale_fill_viridis_d()


cs_yrly_reviews <- cs_reviews_edit %>% 
  mutate(year = year(date)) %>% #Find the year of review
  group_by(drugName, year) %>% #
  count()

ggplot(cs_yrly_reviews) +
  geom_line(aes(x = year, y = n, color = drugName)) +
  geom_point(aes(x = year, y = n, color = drugName)) +
  labs(title = "Number of ratings for smoking cessation products over time", x = "Year", y = "Number of Reviews", color = "Drug") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016))


##Sentiment Analysis of Some Selected Medications
library(textdata)
library(tidytext)
library(lexicon)
library(wordcloud)


sentiword <- hash_sentiment_sentiword
names(sentiword)[names(sentiword) == "x"] <- "word"
names(sentiword)[names(sentiword) == "y"] <- "score"
get_sentiments("afinn")


get_sentiments("bing")
get_sentiments("nrc")


nrc_anger <- get_sentiments("nrc") %>%
  filter(sentiment == 'anger')

tidy_oc_reviews <- oc_reviews %>% 
  unnest_tokens(word, review)

library(viridisLite)
color_pal <- viridis(n = 9, direction = -1)

custom_stop_words <- bind_rows(tibble(word = c('bad', 'awful', 'horrible', 'terrible', 'feeling', 'lose'), 
                                      lexicon = c("custom")), 
                               stop_words)

tidy_oc_reviews %>%
  filter(rating == 1) %>% #The worst ratings
  anti_join(custom_stop_words) %>% 
  inner_join(nrc_anger) %>%
  count(word) %>%
  with(wordcloud(word, n, colors = color_pal, max.words = 75))


nrc_pos <- get_sentiments("nrc") %>%
  filter(sentiment == 'positive')

nrc_neg <- get_sentiments("nrc") %>%
  filter(sentiment == 'negative')

tidy_ad_reviews <- ad_reviews %>%
  unnest_tokens(word, review)


custom_stop_words_ad <- bind_rows(tibble(word = c('bad', 'awful', 'horrible', 'terrible', 'anxiety', 'depression', 'taking', 'pill', 'effect', 'feeling', 'lose', 'anxious', 'panic', 'disorder', 'medication', 'medicine', 'don'), 
                                         lexicon = c("custom")), 
                                  stop_words)

tidy_ad_reviews %>%
  filter(rating == 10, condition != 'Insomnia') %>% #omitting medications for insomnia
  anti_join(custom_stop_words_ad) %>% 
  inner_join(nrc_pos) %>%
  count(word) %>%
  with(wordcloud(word, n, colors = color_pal, max.words = 75))


tidy_ad_reviews %>%
  filter(rating == 1, condition != 'Insomnia') %>%
  anti_join(custom_stop_words_ad) %>% 
  inner_join(nrc_neg) %>%
  count(word) %>%
  with(wordcloud(word, n, colors = color_pal, max.words = 75))


##Weight loss and Smoking cessasion
tidy_cs_reviews <- cs_reviews_edit %>%
  unnest_tokens(word, review)


custom_stop_words_cs <- bind_rows(tibble(word = c('bad', 'awful', 'horrible', 'terrible', 'taking', 'pill', 'effect', 'feeling', 'haven', 'medication', 'nicotine', 'quit', 'don', 'recommend', 'medicine', 'cold', 'doctor'), 
                                         lexicon = c("custom")), 
                                  stop_words)


tidy_cs_reviews %>%
  filter(rating == 10, drugName == 'Varenicline') %>%
  anti_join(custom_stop_words_cs) %>% 
  inner_join(nrc_pos) %>%
  count(word) %>%
  with(wordcloud(word, n, colors = color_pal, max.words = 75))


tidy_cs_reviews %>%
  filter(rating == 1, drugName == 'Varenicline') %>%
  anti_join(custom_stop_words_cs) %>% 
  inner_join(nrc_neg) %>%
  count(word) %>%
  with(wordcloud(word, n, colors = color_pal, max.words = 75))


tidy_wl_reviews <- wl_reviews_edit %>%
  unnest_tokens(word, review)


custom_stop_words_wl <- bind_rows(tibble(word = c('bad', 'awful', 'horrible', 'terrible', 'taking', 'pill', 'effect', 'feeling', 'medication', 'day', 'weight', 'lose', 'lost', 'nicotine', 'quit', 'haven', 'don', 'recommend', 'medicine', 'doctor'), 
                                         lexicon = c("custom")), 
                                  stop_words)


tidy_wl_reviews %>%
  filter(rating == 10, drugName == 'Phentermine') %>%
  anti_join(custom_stop_words_wl) %>% 
  inner_join(sentiword) %>%
  count(word) %>%
  with(wordcloud(word, n, colors = color_pal, max.words = 75))


tidy_wl_reviews %>%
  filter(rating == 1, drugName == 'Phentermine') %>%
  anti_join(custom_stop_words_wl) %>% 
  inner_join(nrc_neg) %>%
  count(word) %>%
  with(wordcloud(word, n, colors = color_pal, max.words = 75))

attach(reviews)

model <- lm(rating ~ uniqueID + usefulCount, data = reviews)
summary(model)
plot(model)

#confidence interval
confint(model)
plot(model,which=1, col=c("blue"))

#residual plot
res <- residuals(model)
res <- as.data.frame(res)
ggplot(res,aes(res))+geom_histogram(fill='blue', alpha=0.5)

library(party)
# Create the tree.
output.tree <- ctree(rating ~ uniqueID + usefulCount, data = reviews)

# Plot the tree.
plot(output.tree)
