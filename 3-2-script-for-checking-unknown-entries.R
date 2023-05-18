# meN- =============
wlist_all_types_df_unique3 |> 
  filter(str_detect(word, "^besan$")) |> 
  select(1:3) |> 
  as.data.frame()
me_freqlist_df3 |> 
  filter(str_detect(word_form, 'memasung')) |> 
  select(1, 3, morphind, root_morphind, root_pos_morphind, pref_morphind, suff_morphind, verb_tagged) |> 
  distinct() |> 
  as.data.frame()

# check df in Karlina new freqlist
karlina_freqlist <- read_rds("karlina_freqlist_all.rds")
karlina_freqlist |> 
  filter(str_detect(Word, 'embel-embel'))

# check the corpus usage
library(corplingr) # this package is needed
freqlist_all <- readr::read_rds("freqlist_all.rds")
word_use <- words_use_in_corpus(rgx = "bercemong", context_char = 70); word_use

# check if words exist
check_if_words_exist(dbase = "verbs", 
                     wordform = c(per_a_kan, per_n_kan, 
                                  per_v_kan_obj_as_patient, 
                                  per_v_kan_obj_is_caused_to_do_sth, 
                                  per_v_kan_subj_someone_obj_to_do_sth))
verbs |> 
  filter(str_detect(root_morphind, "rasap$")) |> 
  select(-year, -genre_size, -year_size, -is_particle, -genres, -n) |> 
  distinct()
verbs |> 
  filter(str_detect(word_form, "^meny"))

# find example
find_example_verb(dbase = c('me', 'di', 'ter', 'ber'), 
                  pref = c("ber-"), 
                  suff = "an", 
                  root_pos = c("v"), 
                  is_sampled = FALSE, 
                  n_sample = 10)

# check unparsed words (X--) where the root is available in Malindo Morph
(unparsed <- me_freqlist_df3 |> 
    filter(str_detect(morphind, "X--"), !is.na(root)) |> 
    select(1, 3, root, root_morphind, root_pos_morphind, 
           pref_morphind, pref, suff_morphind, suff, root_pos_morphind) |> 
    distinct())

# check unequal root in Malindo Morph and Morphind
me_freqlist_df3 |> 
  filter(root != root_morphind, str_detect(morphind, "(S--|W--|_N|_A)", TRUE)) |> 
  select(1, 3, root, root_morphind, 
         pref_morphind, pref, suff_morphind, suff) |> 
  distinct() |> 
  as.data.frame() |> 
  tail(200)

# check the affix
me_freqlist_df3 |> 
  filter(affix_morphind=="0_an") |> 
  select(word_form, morphind, 
         root, root_morphind, 
         root_pos_morphind, affix_morphind) |> 
  distinct()


# ber- =============
wlist_all_types_df_unique3 |> 
  filter(str_detect(word, 'dikencing')) |> 
  select(1:3) |> 
  as.data.frame()

ber_freqlist3 |> 
  filter(str_detect(word_form, 'pusing')) |> 
  select(word_form, morphind) |> 
  distinct()

# check the corpus usage
words_use_in_corpus(rgx = "diempati", context_char = 70)
