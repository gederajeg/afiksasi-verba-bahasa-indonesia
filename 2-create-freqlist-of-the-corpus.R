library(tidyverse)
library(tidytext) # this package is used to tokenise corpus text in a tidy data frame format

genres_file_names_save <- read_lines("genres_file_names_save.txt")

# read the corpus into list
corpsall <- map(genres_file_names_save, read_rds)

# name the element of the list (i.e., the genre of the corpus)
names(corpsall) <- str_replace_all(genres_file_names_save, "(^.\\_|\\.rds$)", "")

# tokenise the corpus
corpsall_tokenised <- corpsall %>% 
  map(~unnest_tokens(., word_form, 
                     bodytext, 
                     token = "regex", 
                     pattern = "([^A-Z0-9a-z-]|--)", 
                     to_lower = FALSE))
# corpsall_tokenised <- corpsall_tokenised %>% 
# map(~separate(., col = "file_id", into = c("year", "file_id"), sep = "_", remove = TRUE))

# create frequency list for all genre by file id (to keep track of the year)
freqlist_year_all <- corpsall_tokenised %>% 
  map(~mutate(., word_form = str_to_lower(word_form))) %>% 
  map(~count(., file_id, word_form, sort = TRUE))
freqlist_year_all1 <- freqlist_year_all %>% 
  map(~separate(., col = "file_id", into = c("year", "file_id"), sep = "_", remove = TRUE)) %>% 
  map(~group_by(., year, word_form)) %>% 
  map(~summarise(., n = sum(n), .groups = "drop"))
names(freqlist_year_all1) <- str_replace_all(genres_file_names_save, "(^.\\_|\\.rds$)", "")
freqlist_year_all1 <- freqlist_year_all1 %>% map(~ungroup(.))
write_rds(freqlist_year_all1, "freqlist_year_all.rds")

# create frequency list for all genre and save it
freqlist_all <- corpsall_tokenised %>% 
  map(~mutate(., word_form = str_to_lower(word_form))) %>% 
  map(~count(., word_form, sort = TRUE))
write_rds(freqlist_all, "freqlist_all.rds")
