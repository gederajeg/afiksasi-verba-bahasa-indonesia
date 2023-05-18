library(tidyverse)
library(tidytext) # this package is used to tokenise corpus text in a tidy data frame format

# retrieve the file path of the corpus
corpuspath <- dir(recursive = TRUE, pattern = ".txt") |>
  str_subset("^Korpus_TBIK") |> 
  str_subset(pattern = "[^~]") # exclude the temporary file cache

# retrieve the file path of the corpus metadata
metadata_path <- dir(recursive = TRUE, pattern = ".xlsx") |>
  str_subset(pattern = "[^~]") # exclude the temporary file cache

# create the list of genre as a data frame
genres_df <- data.frame(genres = unique(str_extract(corpuspath, "(?<=\\/)[A-Z]\\.\\s[^0-9]+(?=\\s\\d)")))
genres_df <- genres_df |> 
  separate(genres, c("id", "genre"), "\\.\\s", remove = FALSE)

# retrieve the genre names
genres <- genres_df[[1]]
genres_file_names <- str_replace_all(str_replace_all(str_replace(genres, "\\.\\s", "_"), "[,-]\\s?", "_"), "\\s", "_")

# prepare the file name of the corpus by genre
genres_file_names_save <- paste(genres_file_names, ".rds", sep = "")
# readr::write_lines(genres_file_names_save, file = "genres_file_names_save.txt")

# create year indices if needed later
years <- paste("20", 11:20, sep = "")

### === For myself: DO NOT RUN THE FOLLOWING CODES (which have been executed) === ###

## === genres that needs to be matched with the year are NOVEL, BIOGRAFI, dan POPULER == ##
### genre ID for Novel is 4; genre ID for Biografi is 8; genre ID for Populer is 9

# subset corpus path by genre and year
## subset by genre
g <- 6
corpuspath_genre <- str_subset(corpuspath, genres[g]) |> str_subset("J1A17001", negate = TRUE)

## subset then by year
# y <- 1
# corpuspath_genre_year <- str_subset(corpuspath_genre, paste("\\/", years[y], "\\/", sep = ""))

## get the file name
# filenames_temp <- str_replace(str_extract(corpuspath_genre_year, "(?<=\\/)[0-9]+\\/[^.]+?\\.txt"), "\\/", "_")
if (g %in% c(4, 8, 9)) {
	filenames_temp <- basename(corpuspath_genre)
	filenames_temp1 <- str_replace_all(filenames_temp, "\\.txt$", "") |> 
	  tibble(Kode = .)
	metadata_file <- metadata_path |> str_subset(genres[g]) |> readxl::read_xlsx()
	filenames_temp <- filenames_temp1 |> 
	  left_join(metadata_file) |> 
	  mutate(filenames_temp = paste(Tahun, "_", Kode, ".txt", sep = "")) |> 
	  pull(filenames_temp)

} else {
	filenames_temp <- str_replace(str_extract(corpuspath_genre, "(?<=\\/)[0-9]+\\/[^.]+\\.txt$"), "\\/", "_")
}

## load the corpus
corpus_temp <- map(corpuspath_genre, 
                   scan, what = "char", sep = "\n", quiet = TRUE, skipNul = TRUE) |>
	map(~tibble(bodytext = .)) |>
	map2(.y = filenames_temp, ~mutate(.x, file_id = .y)) |>
	map_df(bind_rows) |>
	mutate(genre = genres_file_names[g]) |>
	filter(str_detect(bodytext, "\xa0", negate = TRUE),
	       nzchar(bodytext)) |>
	mutate(bodytext = str_replace_all(bodytext, "\\\x93", '"'),
		   bodytext = str_replace_all(bodytext, "\\\u0015", " "))
	
# save the corpus
write_rds(corpus_temp, file = genres_file_names_save[g])
print(paste("finish with corpus ID no.", g, sep = " "))

## === DO NOT RUN THE ABOVE CODES (which have been excetuted) === ###

## === genres that needs to be matched with the year are NOVEL, BIOGRAFI, dan POPULER == ##

# load the corpus ========
## OLD CODES below were replaced with list-operation
# koran <- readr::read_rds(genres_file_names_save[1])
# majalah <- readr::read_rds(genres_file_names_save[2])
# cerpen <- readr::read_rds(genres_file_names_save[3])
# novel <- readr::read_rds(genres_file_names_save[4])
# buku <- readr::read_rds(genres_file_names_save[5])
# jurnal <- readr::read_rds(genres_file_names_save[6])
# tesis <- readr::read_rds(genres_file_names_save[7])
# biografi <- readr::read_rds(genres_file_names_save[8])
# populer <- readr::read_rds(genres_file_names_save[9])
# uud <- readr::read_rds(genres_file_names_save[10])
# laman <- readr::read_rds(genres_file_names_save[11])
# surat <- readr::read_rds(genres_file_names_save[12])

# tokenise the corpus =========
## OLD CODES below were replaced with list-operation
## using the unnest_tokens() function from the tidytext package (this is fast)
# koran_toks <- koran |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# majalah_toks <- majalah |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# cerpen_toks <- cerpen |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# novel_toks <- novel |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# buku_toks <- buku |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# jurnal_toks <- jurnal |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# tesis_toks <- tesis |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# biografi_toks <- biografi |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# populer_toks <- populer |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# uud_toks <- uud |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# laman_toks <- laman |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)
# surat_toks <- surat |> 
#   unnest_tokens(word_form, bodytext, token = "regex", pattern = "([^A-Z0-9a-z-]|--)", to_lower = FALSE)

# create frequency list for each genre and overall =======
## OLD CODES below were replaced with list-operation
# koran_toks_count <- koran_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# koran_toks_count_all <- koran_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# majalah_toks_count <- majalah_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# majalah_toks_count_all <- majalah_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# cerpen_toks_count <- cerpen_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# cerpen_toks_count_all <- cerpen_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# novel_toks_count <- novel_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# novel_toks_count_all <- novel_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# buku_toks_count <- buku_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# buku_toks_count_all <- buku_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# jurnal_toks_count <- jurnal_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# jurnal_toks_count_all <- jurnal_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# tesis_toks_count <- tesis_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# tesis_toks_count_all <- tesis_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# biografi_toks_count <- biografi_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# biografi_toks_count_all <- biografi_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# populer_toks_count <- populer_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# populer_toks_count_all <- populer_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# uud_toks_count <- uud_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# uud_toks_count_all <- uud_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# laman_toks_count <- laman_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# laman_toks_count_all <- laman_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))
# 
# surat_toks_count <- surat_toks |> 
#   mutate(word_form = str_to_lower(word_form)) |> 
#   count(file_id, word_form, sort = TRUE)
# surat_toks_count_all <- surat_toks_count |> 
#   group_by(word_form) |> 
#   summarise(n = sum(n), .groups = "drop") |> 
#   arrange(desc(n))

# filter the potentially verbal affixation =======

## arguments: 
  # - list of freqlist by genre
  # - regex for filtering the freqlist by genre


# workflow
## - filter the potential verbal affix from MALINDO MORPH and/or MORPHIND-tagged
# me_words_df <- filter(wlist_all_types_df_unique3, str_detect(morphind, "^meN\\+"))
## - pull only the word_form column
# me_words <- pull(me_words_df, word)
##   - create regex to search through the frequency list of a genre
# me_words_rgx <- "^(?i)me"
## - filter the frequency list of a genre with the affix regex
# me_words_in_corps <- biografi_toks_count_all |> 
#   filter(str_detect(word_form, me_words_rgx))
##    - then matched with the entry in the MALINDO MORPH and/or MORPHIN-tagged Leipzig frequency list
# me_words_in_corps <- me_words_in_corps |> 
#   filter(word_form %in% me_words)

## - left join the filtered MORPHIND to the filtered frequency list of a genre
# me_words_in_corps_tagged <- me_words_in_corps |> left_join(me_words_df |> select(-n) |> rename(word_form = word))

## filtering
### filter only the meN+N pattern
# me_words_in_corps_tagged |> 
#   filter(str_detect(morphind, "<[^v]>_V"), 
#          str_detect(morphind, "(\\+(kan|i)|\\+per\\+)", TRUE)) |> 
#   filter(str_detect(morphind, "<n>"))
### filter only the meN+A pattern
# me_words_in_corps_tagged |> 
#   filter(str_detect(morphind, "<[^v]>_V"), 
#          str_detect(morphind, "(\\+(kan|i)|\\+per\\+)", TRUE)) |> 
#   filter(str_detect(morphind, "<a>"))
### filter only the meN+per+A pattern
# me_words_in_corps_tagged |> 
#   filter(str_detect(morphind, "<[^v]>_V"), 
#          str_detect(morphind, "\\+(kan|i)", TRUE)) |> 
#   filter(str_detect(morphind, "<a>"),
#          str_detect(morphind, "^meN\\+per\\+"))
### filter only the meN+per+X+kan pattern
# me_words_in_corps_tagged |> 
#   filter(str_detect(morphind, "meN\\+per\\+[^+]+?\\+kan_"))
### filter only the meN+per+X+i pattern
# me_words_in_corps_tagged |> 
#   filter(str_detect(morphind, "meN\\+per\\+[^+]+?\\+i_"))


## - left join the filtered MALINDO MORPH to the filtered frequency list of a genre
##    - when left-joining, 
##      - exclude the id and source column with select(-id, -source)
##      - lower case the word_form column
##      - retrieve the distinct entries with distinct()
##      - left-join to the filtered frequency list


# get word_form for certain affix
## for example, retrieve the word_form with -KAN suffix (regardless of PREFIX) from mmorph_id_affix
# kan_only <- mmorph_id_affix |> filter(suff == "-kan") |> pull(word_form)
# kan_only_rgx <- paste("^(?i)(", paste(kan_only, collapse = "|"), ")$", sep = "")
# biografi_kan_df <- biografi_toks_count_all |> filter(str_detect(word_form, kan_only_rgx))
