library(tidyverse)
library(morphindtagr)
library(corplingr) # required for running `words_use_in_corpus()` function

# read all freqlist list-data for all genre
freqlist_all <- read_rds("freqlist_all.rds")

# genre size (in word tokens defined as "[a-zA-Z-]{2,}")
genresnames <- names(freqlist_all)
genressizes <- freqlist_all |> 
  map(~filter(., str_detect(word_form, "[a-zA-Z-]{2,}"))) |>
  map(~tally(., n)) |> 
  map2_df(.y = genresnames, ~mutate(., genres = .y)) |> 
  select(genres, sizes = n)
(corpussizes_all <- sum(genressizes$sizes))

# check occurrence in the corpus
words_use_in_corpus <- function(freqlist_df = freqlist_all, 
                                rgx = NULL, 
                                case_insensitive = TRUE, 
                                context_char = 50, 
                                to_lower_conc = FALSE) {
  genres_file_names_save <- read_lines("genres_file_names_save.txt")
  rgx <- paste("\\b", rgx, "\\b", sep = "")
  corpus_genres <- freqlist_df |> 
    map(~filter(., str_detect(word_form, regex(rgx, ignore_case = case_insensitive)))) |> 
    map_dbl(~dim(.)[1])
  if (all(corpus_genres == 0)) {
    message("Sorry, the pattern is not attested in the whole corpus!")
  } else {
    corpus_genres_id <- which(corpus_genres >= 1)
    # read the corpus into list
    corpsall <- map_df(genres_file_names_save[corpus_genres_id], read_rds)
    usages <- corpsall |> 
      filter(str_detect(bodytext, regex(rgx, ignore_case = case_insensitive))) # |> 
    # mutate(usages = paste(genre, "___", bodytext, sep = "")) |> 
    # pull(usages)
    genres <- usages$genre |> 
      unique() |> 
      str_replace("^._", "")
    usages_conc <- usages |> 
      split(usages$genre) |> 
      map(~concord_others(.$bodytext, 
                          pattern = rgx, 
                          case_insensitive = case_insensitive, 
                          context_char = context_char, 
                          to_lower_corpus = to_lower_conc)) |> 
      map2_df(.y = genres, ~mutate(., GENRE = .y))
    return(usages_conc[sample(1:nrow(usages_conc)), ])
  }
}

# read the manually identified some roots from the PER- -KAN project
# adj_root_updated <- read_tsv("/Volumes/GoogleDrive/Other computers/My MacBook Pro/Documents/research/2020-12-18-KOLITA-per-vs-kan/ADJ_root_kbbi_check - updated-main.tsv")
# write_rds(adj_root_updated, "adj_root_updated.rds")
adj_root_updated <- read_rds("adj_root_updated.rds") |> 
  mutate(kbbi_pos_no_1 = replace(kbbi_pos_no_1, kbbi_pos_no_1 == "precat", "pre-cat"),
         combine_kbbi_pos_InputK = ifelse(combine_kbbi_pos_InputK == "#N/A", 
                                          kbbi_pos_no_1, 
                                          combine_kbbi_pos_InputK),
         combine_kbbi_pos_InputK = str_replace_all(combine_kbbi_pos_InputK, "\\,.+", "")) |> 
  rename(root_morphind = root, 
         root_pos_add = combine_kbbi_pos_InputK) |> 
  mutate(root_pos_add = ifelse(!is.na(InputKarlina) & 
                                 is.na(root_pos_add) & 
                                 nchar(InputKarlina) == 1, 
                               str_extract(InputKarlina, "^[a-z]$"), 
                               root_pos_add),
         root_pos_add = ifelse(!is.na(InputKarlina) & 
                                 is.na(root_pos_add) & 
                                 nchar(InputKarlina) > 1, 
                               str_extract(InputKarlina, "(?<=[(])[a-z](?=[)])"), 
                               root_pos_add))
verbsroot <- c("kalah", "menang", "timbul", "hilang", "usai", "roboh", "tenggelam", "tewas", "ambruk", "hancur", "mati", "selesai", "musnah", "kandas", "acuh", "putus")

# read MALINDO MORPH dictionary from GitHub ==========
# mmorph <- read_tsv("https://raw.githubusercontent.com/matbahasa/MALINDO_Morph/master/malindo_dic_20211116.tsv", col_names = c("id", "root", "word_form", "pref", "suff", "confix", "reduplication", "source", "stem", "lemma"))
## save the MALINDO MORPH dictionary
# write_rds(mmorph, file = "mmorph.rds")
mmorph <- read_rds("mmorph.rds")

# remove the Malay source and only select the "checked" entries ===========
mmorph_id <- mmorph |> 
  filter(str_detect(source, "^Melayu", negate = TRUE), 
         str_detect(id, "^ex", negate = TRUE))

# run the source code to load the morphind-parsed leipzig frequency list
## this source code includes codes fixing the parsing in the frequency list
source("3-1-morphind-freqlist-editing.R")

# fixing some entries error in MALINDO MORPH =======
mmorph_id_edits <- mmorph_id |> 
  mutate(pref = replace(pref, 
                        pref == "ber-+meN-" & 
                          word_form == "membelajari", 
                        "meN-+ber-"),
         reduplication = replace(reduplication, 
                                 pref == "ber-+meN-" & 
                                   word_form == "Belajar-mengajar", 
                                 "R-penuh"),
         stem = replace(stem, pref == "ber-+meN-" & 
                          word_form == "Belajar-mengajar", 
                        "belajar-mengajar"),
         lemma = replace(lemma, 
                         pref == "ber-+meN-" & 
                           word_form == "Belajar-mengajar", 
                         "belajar-mengajar"),
         # stem = replace(stem, stem == "eduduki" & lemma == "mengeduduki", "duduki"),
         # stem = replace(stem, stem == "etuai" & lemma == "mengetuai", "ketuai"),
         # stem = replace(stem, stem == "etuakan" & lemma == "mengetuakan", "ketuakan"),
         # pref = replace(pref, stem %in% c("ketuai", "ketuakan"), "meN-"),
         # stem = replace(stem, stem == "elupai" & lemma == "mengelupai", "lupai"),
         # stem = replace(stem, stem == "emahui" & lemma == "mengemahui", "mahui"),
         # stem = replace(stem, stem == "epunyai" & lemma == "mengepunyai", "punyai"),
         # stem = replace(stem, stem == "esopani" & lemma == "mengesopani", "sopani"),
         # stem = replace(stem, stem == "etahui" & lemma == "mengetahui", "tahui"),
         # stem = replace(stem, stem == "etakuti" & lemma == "mengetakuti", "takuti"),
         # stem = replace(stem, stem == "etawai" & lemma == "mengetawai", "tawai"),
         # stem = replace(stem, stem == "etawakan" & lemma == "mengetawai", "tawakan"),
         # pref = replace(pref, stem %in% c("ketawai", "ketawakan"), "meN-"),
         # stem = replace(stem, stem == "etemukan" & lemma == "mengetemukan", "temukan"),
         # pref = replace(pref, stem %in% c("ketemukan"), "meN-"),
         # pref = replace(pref, stem %in% c("kehendaki"), "meN-"),
         # stem = replace(stem, stem == "enepikan" & lemma == "mengenepikan", "tepikan"),
         stem = replace(stem, stem == "eratakan", "meratakan"),
         lemma = replace(lemma, stem == "meratakan", "memeratakan"),
         stem = str_replace(stem, "^t(tertawa(i|kan))", "\\2"),
         lemma = replace(lemma, lemma == "menerlantarkan", "menelantarkan"),
         stem = replace(stem, 
                        stem == "skola" & 
                          word_form == "Memikirkan", "pikirkan"),
         lemma = replace(lemma, 
                         lemma == "skola" & 
                           word_form == "Memikirkan", "memikirkan"))

# filter entries that have at least one affixation =======
mmorph_id_affix <- mmorph_id_edits |> 
  filter(if_any(matches("pref|suff|confix"), ~ . != "0"))

# count the types of the affix in MALINDO MORPH ======
# confix_red_count <- mmorph_id_affix |> 
#   filter(confix != "0") |> 
#   count(confix, reduplication, sort = TRUE)
# confix_only_count <- mmorph_id_affix |> 
#   filter(confix != "0") |> 
#   count(confix, sort = TRUE)
# 
# suff_red_count <- mmorph_id_affix |> 
#   filter(suff != "0") |> 
#   count(suff, reduplication, sort = TRUE)
# suff_only_count <- mmorph_id_affix |> 
#   filter(suff != "0") |> 
#   count(suff, sort = TRUE)
# 
# pref_red_count <- mmorph_id_affix |> 
#   filter(pref != "0") |> 
#   count(pref, reduplication, sort = TRUE)
# pref_only_count <- mmorph_id_affix |> 
#   filter(pref != "0") |> 
#   count(pref, sort = TRUE)
# 
# pref_suff_red_count <- mmorph_id_affix |> 
#   count(pref, suff, reduplication, sort = TRUE) |> 
#   filter(if_any(where(is.character), ~ . != "0"))
# pref_suff_only_count <- mmorph_id_affix |> 
#   count(pref, suff, sort = TRUE) |> 
#   filter(if_any(where(is.character), ~ . != "0"))

# filter the corpus freqlist for the regex of verbal affixes =======
## meN- =======
me_irrelevant <- readr::read_lines("me_irrelevant.txt")
me_regex <- "^(?i)me[a-z-]{3,}$" # old, including hypen.
# me_regex <- "^(?i)me[a-z]{3,}$"
me_freqlist <- freqlist_all |> 
  map(~filter(., str_detect(word_form, me_regex))) |> 
  # remove some irrelevant me- words
  map(~filter(., !word_form %in% me_irrelevant))

### left join the morphind tag ======
me_freqlist <- me_freqlist |> 
  map(~left_join(., wlist_all_types_df_unique3 |> 
                   rename(word_form = word) |> 
                   select(-n), by = "word_form"))

### join all freqlist across all genres into a single data frame ======
me_freqlist_df <- me_freqlist |> 
  map2(names(me_freqlist), ~mutate(., genres = .y)) |> 
  map_df(bind_rows) |> 
  mutate(morphind = replace(morphind, word_form %in% c("menguyupi"), "meN+kuyup<a>+i_VSA"),
         morphind = replace(morphind, word_form %in% c("menguyupkan"), "meN+kuyup<a>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("membayangin"), "meN+bayang<n>+in_VSA"),
         morphind = replace(morphind, word_form %in% c("men-cuekin", "mencuekin"), "meN+cuek<a>+in_VSA"),
         morphind = replace(morphind, word_form %in% c("men-cueki"), "meN+cuek<a>+i_VSA"),
         morphind = replace(morphind, word_form %in% c("melatarbelakangin"), "meN+latarbelakang<n>+in_VSA"),
         morphind = replace(morphind, word_form %in% c("mempermiskin"), "meN+per+miskin<a>_VSA"),
         morphind = replace(morphind, word_form %in% c("memilin-milin"), "meN+pilin<n>_VPA"),
         morphind = replace(morphind, word_form %in% c("membikin-bikin"), "meN+bikin<v>_VPA"),
         morphind = replace(morphind, word_form %in% c("memangguk"), "meN+angguk<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("menggentayangiku"), "meN+gentayang<pre-cat>+i_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% c("mengasa"), "meN+asah<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("mengamban"), "meN+emban<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("menajak"), "meN+tanjak<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("menyelinap-nyelinap"), "meN+selinap<v>_VPA"),
         morphind = replace(morphind, word_form %in% c("menggerak-gerakkan"), "meN+gerak<v>+kan_VPA"),
         morphind = replace(morphind, word_form %in% c("menghabis-habiskan"), "meN+habis<v>+kan_VPA"),
         morphind = replace(morphind, word_form %in% c("menghabisiku"), "meN+habis<v>+i_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% c("menghadiahimu"), "meN+hadiah<n>+i_VSA+kamu<p>_PS2"),
         morphind = replace(morphind, word_form %in% c("meng-hadiahkannya"), "meN+hadiah<n>+kan_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("menghirup-hirup"), "meN+hirup<v>_VPA"),
         morphind = replace(morphind, word_form %in% c("terpenuhi"), "ter+penuh<a>+i_VSP"),
         morphind = replace(morphind, word_form %in% c("terpenuhinya"), "ter+penuh<a>+i_VSP+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("menghubung-hubungkan"), "meN+hubung<v>+kan_VPA"),
         morphind = replace(morphind, word_form %in% c("menghubung-hubungkannya"), "meN+hubung<v>+kan_VPA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("menghitung-hitung", "menghitunghitung"), "meN+hitung<v>_VPA"),
         morphind = replace(morphind, word_form %in% c("menghitung-hitungnya", "menghitunghitungnya"), "meN+hitung<v>_VPA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("menperhitungkan", "meperhitungkan"), "meN+per+hitung<v>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("memberhitungkan"), "meN+ber+hitung<v>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("mencecarkan"), "meN+cecar<v>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("mengejap"), "meN+cecar<v>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("meng-indonesia"), "meN+kejap<n>_VSA"),
         morphind = replace(morphind, word_form %in% c("meng-klik"), "meN+klik<n>_VSA"),
         morphind = replace(morphind, word_form %in% c("meng-kontekstualisasikan", "mengkontekstualisasikan"), "meN+kontekstualisasi<n>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("menginternasionalisasikan"), "meN+internasionalisasi<n>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("meng-internasional", "menginternasional"), "meN+internasional<a>_VSA"),
         morphind = replace(morphind, word_form %in% c("mengonfigurasinya"), "meN+konfigurasi<n>_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("meng-onfigurasi"), "meN+konfigurasi<n>_VSA"),
         morphind = replace(morphind, word_form %in% c("meng-ongkosi"), "meN+ongkos<n>+i_VSA"),
         morphind = replace(morphind, word_form %in% c("meng-oper"), "meN+oper<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("meng-udara"), "meN+udara<n>_VSA"),
         morphind = replace(morphind, word_form %in% c("mengapkir"), "meN+afkir<a>_VSA"),
         morphind = replace(morphind, word_form %in% c("mengembangkempiskan"), "meN+kembang kempis<v>_VPA"),
         morphind = replace(morphind, word_form %in% c("membantingtulang"), "meN+banting tulang<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("meng-utarakan"), "meN+utara<n>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("mengabai"), "meN+abai<v>_VSA"),
         morphind = replace(morphind, word_form %in% "mengabsorp", "meN+absorb<f>_VSA"),
         morphind = replace(morphind, word_form %in% "menyampuli", "meN+sampul<n>+i_VSA"),
         morphind = replace(morphind, word_form %in% "menyambarmu", "meN+sambar<v>_VSA+kamu<p>_PS2"),
         morphind = replace(morphind, word_form %in% "mengabsorpsinya", "meN+absorbsi<n>_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% "mengabstraksikannya", "meN+abstraksi<n>+kan_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% "menyandingi", "meN+sanding<n>+i_VSA"),
         morphind = replace(morphind, word_form %in% "menyenteri", "meN+senter<n>+i_VSA"),
         morphind = replace(morphind, word_form %in% "melayang-layang", "meN+layang<v>_VPA"),
         morphind = replace(morphind, word_form %in% "menyetrumku", "meN+setrum<n>_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% "menyandingku", "meN+sanding<n>_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% "menyantapmu", "meN+santap<v>_VSA+kamu<p>_PS2"),
         morphind = replace(morphind, word_form %in% "menyengatmu", "meN+sengat<n>_VSA+kamu<p>_PS2"),
         morphind = replace(morphind, word_form %in% "me-nyantap", "meN+santap<v>_VSA"),
         morphind = replace(morphind, word_form %in% "menyatu-kan", "meN+satu<c>+kan_VSA"),
         morphind = replace(morphind, word_form %in% "mentawarkan", "meN+tawar<a>+kan_VSA"),
         morphind = replace(morphind, word_form %in% "menyatulah", "meN+satu<c>_VSA+lah<t>_T--"),
         morphind = replace(morphind, word_form %in% "menyeruaklah", "meN+seruak<pre-cat>_VSA+lah<t>_T--"),
         morphind = replace(morphind, word_form %in% "menyasarnya", "meN+sasar<v>_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% "menyelam-nyelam", "meN+selam<v>_VPA"),
         morphind = replace(morphind, word_form %in% "menyeret-nyeret", "meN+seret<v>_VPA"),
         morphind = replace(morphind, word_form %in% "menyesap-nyesap", "meN+sesap<v>_VPA"),
         morphind = replace(morphind, word_form %in% "menyesapnya", "meN+sesap<v>_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% "menyeret-nyeretnya", "meN+seret<v>_VPA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% "menyeretmu", "meN+seret<v>_VSA+kamu<p>_PS2"),
         morphind = replace(morphind, word_form %in% "menyeretkan", "meN+seret<v>+kan_VSA"),
         morphind = replace(morphind, word_form %in% "menyelonjor", "meN+selonjor<v>_VSA"),
         morphind = replace(morphind, word_form %in% "menyembulnyembul", "meN+sembul<v>_VPA"),
         morphind = replace(morphind, word_form %in% "menyepah", "meN+sepah<v>_VSA"),
         morphind = replace(morphind, word_form %in% "menjertai", "meN+serta<r>+i_VSA"),
         morphind = replace(morphind, word_form %in% "melahapi", "meN+lahap<a>+i_VSA"),
         morphind = replace(morphind, word_form %in% "menebas-nebas", "meN+tebas<v>_VPA"),
         morphind = replace(morphind, word_form %in% "menebas-nebaskan", "meN+tebas<v>+kan_VPA"),
         morphind = replace(morphind, word_form %in% "meruakkan", "meN+ruak<v>+kan_VSA"),
         morphind = replace(morphind, word_form %in% "menyerapahi", "meN+serapah<n>+i_VSA"),
         morphind = replace(morphind, word_form %in% "menyerebak", "meN+se+rebak<n>_VSA"),
         morphind = replace(morphind, word_form %in% "mengguyuriku", "meN+guyur<v>+i_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% "mengguyurku", "meN+guyur<v>_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% "menyetopku", "meN+setop<v>_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% "menyerapahkan", "meN+serapah<n>+kan_VSA"),
         morphind = replace(morphind, word_form %in% "merengkuhkan", "meN+rengkuh<v>+kan_VSA"),
         morphind = replace(morphind, word_form == "menggulai" & is.na(morphind), "meN+gula<n>+i_VSA"),
         morphind = replace(morphind, word_form %in% "menyepahkannya", "meN+sepah<v>+kan_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("membentengiku"), "meN+benteng<n>+i_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% "menyeberangkanku", "meN+seberang<v>+kan_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% "mengerem-ngerem", "meN+rem<n>_VPA"),
         morphind = replace(morphind, word_form %in% "memelotot", "meN+pelotot<pre-cat>_VSA"),
         morphind = replace(morphind, word_form %in% c("meyakinkan", "meyakin-kan"), "meN+yakin<a>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("meyakini"), "meN+yakin<a>+i_VSA"),
         morphind = replace(morphind, word_form %in% c("menggondoli"), "meN+gondol<v>+i_VSA"),
         morphind = replace(morphind, word_form %in% c("meyakinkanmu"), "meN+yakin<a>+kan_VSA+kamu<p>_PS2"),
         morphind = replace(morphind, word_form %in% c("mengimani-mu"), "meN+iman<n>+i_VSA+kamu<p>_PS2"),
         morphind = replace(morphind, word_form %in% c("meyakinkannya"), "meN+yakin<a>+kan_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("meyakin-yakinkan"), "meN+yakin<a>+kan_VPA"),
         morphind = replace(morphind, word_form %in% c("menyeringai-nyeringai"), "meN+seringai<n>_VPA"),
         morphind = replace(morphind, word_form %in% c("meyakini-nya", "meyakininya"), "meN+yakin<a>+i_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("mematahhatikan"), "meN+patah hati<a>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("memaksahatiku"), "meN+paksa hati<v>_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% c("menyewaku"), "meN+sewa<v>_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% c("mendesir-desir"), "meN+desir<n>_VPA"),
         morphind = replace(morphind, word_form %in% c("menikam-nikam"), "meN+tikam<n>_VPA"),
         morphind = replace(morphind, word_form %in% c("menyibak-sibakkan"), "meN+sibak<v>+kan_VPA"),
         morphind = replace(morphind, word_form %in% c("menikam-nikamnya"), "meN+tikam<n>_VPA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("menyaduri"), "meN+sadur<n>+i_VSA"),
         morphind = replace(morphind, word_form %in% c("menengarainya"), "meN+tengara<n>+i_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("memper-murah"), 'meN+per+murah<a>_VSA'),
         morphind = replace(morphind, word_form %in% c("mermak"), 'N+permak<v>_VSA'),
         morphind = replace(morphind, word_form %in% c("memermak"), 'meN+permak<v>_VSA'),
         morphind = replace(morphind, word_form %in% c("mencontoh"), 'meN+contoh<n>_VSA'),
         morphind = replace(morphind, word_form %in% c("mencontohnya"), 'meN+contoh<n>_VSA+dia<p>_PS3'),
         morphind = replace(morphind, word_form %in% c("mencontohi"), 'meN+contoh<n>+i_VSA'),
         morphind = replace(morphind, word_form %in% c("mengicip"), 'meN+icip<v>_VSA'),
         morphind = replace(morphind, word_form %in% c("menyihirku"), "meN+sihir<n>_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% c("menyimaki"), "meN+simak<v>+i_VSA"),
         morphind = replace(morphind, word_form %in% c("menyimakmu"), "meN+simak<v>_VSA+kamu<p>_PS2"),
         morphind = replace(morphind, word_form %in% c("menyimpanku"), "meN+simpan<v>_VSA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% c("menyinggung-nyjnggung"), "meN+singgung<v>_VPA"),
         morphind = replace(morphind, word_form %in% c("mengombangambing"), "meN+ombang ambing<pre-cat>_VPA"),
         morphind = replace(morphind, word_form %in% c("mengombangngambingkannya"), "meN+ombang ambing<pre-cat>+kan_VPA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("mengombang-ambingkanku"), "meN+ombang ambing<pre-cat>+kan_VPA+aku<p>_PS1"),
         morphind = replace(morphind, word_form %in% c("meninggung"), "meN+tinggung<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("membasah-kuyupkan"), "meN+basah kuyup<a>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("mencomot-comot"), "meN+comot<v>_VPA"),
         morphind = replace(morphind, word_form %in% c("mentongsong"), "meN+songsong<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("menggenggami"), "meN+genggam<v>+i_VSA"),
         morphind = replace(morphind, word_form %in% c("beringsut"), "ber+ingsut<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("mengingsut"), "meN+ingsut<v>_VSA"),
         morphind = replace(morphind, word_form %in% c("menjeluak"), "meN+jeluak<n>_VSA"),
         morphind = replace(morphind, word_form %in% c("beringsut-ingsut"), "ber+ingsut<v>_VPA"),
         morphind = replace(morphind, word_form %in% c("menggenggamkannya"), "meN+genggam<v>+kan_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("menyumpal-kannya"), "meN+sumpal<n>+kan_VSA+dia<p>_PS3"),
         morphind = replace(morphind, word_form %in% c("memsosialisasikan", "mesosialisasikan"), "meN+sosialisasi<n>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("menyimpangsiurkan"), "meN+simpang siur<v>+kan_VSA"),
         morphind = replace(morphind, word_form %in% c("meruyup"), "meN+ruyup<a>_VSA"),
         morphind = replace(morphind, word_form %in% c("menjarang"), "meN+jarang<a>_VSA"),
         verb_tagged = if_else(str_detect(morphind, "_V"), TRUE, FALSE))

### extract information from MORPHIND parsing =========
me_freqlist_df0 <- me_freqlist_df |> 
  mutate(root_morphind = morphind,
         root_morphind = str_replace_all(root_morphind, "^[^+<]+?\\+([^+<]+?\\+)?\\b", ""),
         root_morphind = if_else(str_detect(morphind, "\\-\\-\\+") & 
                                   verb_tagged, 
                                 str_replace_all(root_morphind, 
                                                 "\\b([^<]+?)\\<.+?\\+([^<]+)\\<.+", 
                                                 "\\1 \\2"), 
                                 root_morphind),
         root_morphind = if_else(verb_tagged & 
                                   str_detect(root_morphind, "\\<"), 
                                 str_extract(root_morphind, "([^<]+?)(?=<)"), 
                                 root_morphind),
         root_pos_morphind = str_extract(morphind, "(?<=<)([a-z-]+)((?=>_V)|(?=>\\+(k?an|in?)_V)|(?=>_[A-Z]..\\+))"),
         root_pos_morphind = replace(root_pos_morphind, 
                                     root_morphind %in% verbsroot, 
                                     "v"),
         pref_morphind = str_extract(morphind, "^[^+<]+?\\+([^+<]+?\\+)?"),
         pref_morphind = str_replace(pref_morphind, "\\+(?=[a-zA-Z])", "-+"),
         pref_morphind = str_replace(pref_morphind, "\\+$", "-"),
         pref_morphind = replace(pref_morphind, 
                                 morphind %in% c("menang<a>+i_VSA", "merek<n>+i_VSA", "meter<n>+i_VSA"), 
                                 "0"),
         suffixed_morphind = if_else(str_detect(morphind, ">\\+(k?an|in?)_V") & 
                                       verb_tagged, TRUE, FALSE),
         suff_morphind = if_else(suffixed_morphind, 
                                 str_extract(morphind, "(?<=\\+)[a-z]+(?=_V)"), "0"),
         suff_morphind = replace(suff_morphind, 
                                 word_form %in% c("mengemukakan", "mengedepankan", "mengetengahkan"), "kan"),
         pers_pron_morphind = if_else(str_detect(morphind, "_PS(1|2|3)"), TRUE, FALSE),
         pers_pron_num_morphind = if_else(pers_pron_morphind & 
                                            str_detect(morphind, "_PS1"), 
                                          "first", 
                                          "NA"),
         pers_pron_num_morphind = if_else(pers_pron_morphind & 
                                            str_detect(morphind, "_PS2"), 
                                          "second", 
                                          pers_pron_num_morphind),
         pers_pron_num_morphind = if_else(pers_pron_morphind & 
                                            str_detect(morphind, "_PS3"), 
                                          "third", 
                                          pers_pron_num_morphind)) |> 
  left_join(adj_root_updated) |> 
  mutate(root_pos_morphind = if_else(root_morphind %in% adj_root_updated$root_morphind & 
                                       root_pos_morphind != root_pos_add, 
                                     root_pos_add, 
                                     root_pos_morphind))

### check the multiple roots for a given word form in MALINDO MORPH =========
mmorph_id_affix_to_join <- mmorph_id_affix |> 
  select(-id, -source, -stem, -lemma) |> 
  mutate(word_form = str_to_lower(word_form), root = str_to_lower(root)) |> 
  distinct() |> 
  group_by(word_form, pref, suff, confix, reduplication) |> 
  mutate(n_root = n_distinct(root), 
         root_multiple = paste(root, collapse = "_")) |> 
  select(-root) |> 
  rename(root = root_multiple) |> 
  select(-n_root) |> 
  distinct() |> ungroup()

### join the MALINDO MORPH to the frequency list database =========
me_freqlist_df1 <- me_freqlist_df0 |> 
  left_join(mmorph_id_affix_to_join, 
            by = "word_form") |> 
  group_by(word_form, genres) |> 
  mutate(n_root_malindo = n_distinct(root),
         root = replace(root, root == "tindak lanjut", "tindaklanjut")) |> 
  arrange(desc(n_root_malindo)) |> 
  ungroup()

### removing irrelevant (multiple) roots for a word form in the MALINDO MORPH database ======
me_freqlist_df2 <- me_freqlist_df1 |> 
  mutate(remove_roots = if_else(root=='merah' & word_form=='meramu', TRUE, FALSE),
         remove_roots = if_else(root=="meleset" & word_form=='meleset', TRUE, remove_roots),
         remove_roots = if_else(root=="landa" & word_form=='melandai', TRUE, remove_roots),
         remove_roots = if_else(root=="relai_lerai" & word_form=='merelai', TRUE, remove_roots),
         remove_roots = if_else(root=="mere" & word_form=='merekah', TRUE, remove_roots),
         remove_roots = if_else(root=="meleset" & word_form=='melesetnya', TRUE, remove_roots),
         remove_roots = if_else(root=="lintir" & word_form=='melintir', TRUE, remove_roots),
         remove_roots = if_else(root=="mengga" & word_form=='menggalah', TRUE, remove_roots),
         remove_roots = if_else(root=="gulai" & word_form=='menggulai', TRUE, remove_roots),
         remove_roots = if_else(root=="awan" & word_form=='merawan', TRUE, remove_roots),
         remove_roots = if_else(root=="menisbat" & word_form=='menisbatkan', TRUE, remove_roots),
         remove_roots = if_else(root=="iring" & word_form=='mengiri', TRUE, remove_roots),
         remove_roots = if_else(root=="mengi" & word_form=='mengilah', TRUE, remove_roots))

### read the edited unparsed word form =======
me_freqlist_unknown_morphind_root_pos <- read_tsv("me_freqlist_unknown_morphind_root_pos.txt")
me_freqlist_unknown_all_edit <- read_tsv("me_freqlist_unknown_all.txt")
me_freqlist_na_all <- read_tsv("me_freqlist_na_all.txt")
me_freqlist_na_all_foreign <- me_freqlist_na_all |> 
  filter(foreign_root == "y") |> 
  mutate(morphind2 = paste(str_replace(pref2, "-", ""), "+", root2, "<", root_pos2, ">+", suff2, sep = "")) |> 
  select(-2, -3) |> 
  mutate(morphind2 = ifelse(suff2 == "0", str_replace(morphind2, "\\+0", "_VSA"), paste(morphind2, "_VSA", sep = "")),
         morphind2 = ifelse(!is.na(enclitic2) & enclitic2 == "nya", paste(morphind2, "+dia<p>_PS3", sep = ""), morphind2))

me_freqlist_na_all_nonforeign <- me_freqlist_na_all |> 
  filter(to_be_included=='y', is.na(foreign_root), !is.na(root2)) |> 
  mutate(morphind3 = "", 
         morphind3 = if_else(suff2 == "0" & !is.na(enclitic2), paste(str_replace(pref2, "\\-", ""), "+", root2, "<", root_pos2, ">", redup_voice_tag, "+", enclitic_tag, sep = ""), morphind3),
         morphind3 = if_else(suff2 == "0" & is.na(enclitic2), paste(str_replace(pref2, "\\-", ""), "+", root2, "<", root_pos2, ">", redup_voice_tag, sep = ""), morphind3),
         morphind3 = if_else(suff2 != "0" & is.na(enclitic2), paste(str_replace(pref2, "\\-", ""), "+", root2, "<", root_pos2, ">+", suff2, redup_voice_tag, sep = ""), morphind3),
        morphind3 = if_else(suff2 != "0" & !is.na(enclitic2), paste(str_replace(pref2, "\\-", ""), "+", root2, "<", root_pos2, ">+", suff2, redup_voice_tag, "+", enclitic_tag, sep = ""), morphind3)) |> 
  rename(word_form = word_form_orig2, root3 = root2, pref3 = pref2, suff3 = suff2, root_pos3 = root_pos2, reduplication3 = reduplication2) |> 
  select(-enclitic2, -enclitic_tag, -to_be_included, -foreign_root, -morphind2, -confix2)

me_freqlist_unknown_vector <- me_freqlist_unknown_all_edit |> 
  filter(to_be_included == "?" | is.na(to_be_included)) |> 
  pull(word_form_orig)
me_freqlist_df2 <- me_freqlist_df2 |> 
  filter(!word_form %in% me_freqlist_unknown_vector,
         !word_form %in% pull(filter(me_freqlist_unknown_morphind_root_pos, is.na(root_pos)), word_form_orig),
         !remove_roots) |> 
  left_join(me_freqlist_na_all_foreign |> rename(word_form = word_form_orig)) |> 
  mutate(root = ifelse(word_form %in% me_freqlist_na_all_foreign$word_form_orig, root2, root),
         root_morphind = ifelse(word_form %in% me_freqlist_na_all_foreign$word_form_orig, root2, root_morphind),
         morphind = ifelse(word_form %in% me_freqlist_na_all_foreign$word_form_orig, morphind2, morphind),
         pref = ifelse(word_form %in% me_freqlist_na_all_foreign$word_form_orig, pref2, pref),
         suff = ifelse(word_form %in% me_freqlist_na_all_foreign$word_form_orig, suff2, suff),
         pref_morphind = ifelse(word_form %in% me_freqlist_na_all_foreign$word_form_orig, pref2, pref_morphind),
         suff_morphind = ifelse(word_form %in% me_freqlist_na_all_foreign$word_form_orig, suff2, suff_morphind),
         root_pos_morphind = ifelse(word_form %in% me_freqlist_na_all_foreign$word_form_orig, root_pos2, root_pos_morphind),
         verb_tagged = if_else(str_detect(morphind, "_V"), TRUE, FALSE)) |> 
  select(-morphind2, -root2, -pref2, -suff2, -confix2, -reduplication2, -redup_voice_tag, -enclitic2, -enclitic_tag, -root_pos2, -word_form_orig2)

me_freqlist_df2 <- me_freqlist_df2 |> 
  left_join(me_freqlist_na_all_nonforeign) |> 
  mutate(root = ifelse(word_form %in% me_freqlist_na_all_nonforeign$word_form_orig, root3, root),
         root_morphind = ifelse(word_form %in% me_freqlist_na_all_nonforeign$word_form_orig, root3, root_morphind),
         morphind = ifelse(word_form %in% me_freqlist_na_all_nonforeign$word_form_orig, morphind3, morphind),
         pref = ifelse(word_form %in% me_freqlist_na_all_nonforeign$word_form_orig, pref3, pref),
         suff = ifelse(word_form %in% me_freqlist_na_all_nonforeign$word_form_orig, suff3, suff),
         pref_morphind = ifelse(word_form %in% me_freqlist_na_all_nonforeign$word_form_orig, pref3, pref_morphind),
         suff_morphind = ifelse(word_form %in% me_freqlist_na_all_nonforeign$word_form_orig, suff3, suff_morphind),
         root_pos_morphind = ifelse(word_form %in% me_freqlist_na_all_nonforeign$word_form_orig, root_pos3, root_pos_morphind),
         reduplication = ifelse(word_form %in% me_freqlist_na_all_nonforeign$word_form_orig & !is.na(reduplication3), reduplication3, reduplication),
         verb_tagged = if_else(str_detect(morphind, "_V"), TRUE, FALSE)) |> 
  select(-root3, -morphind3, -root_pos3, -pref3, -suff3, -word_form_orig, -InputKarlina, -kbbi_pos_no_1, -note, -root_pos_add, -reduplication3, -redup_voice_tag, -remove_roots)

### create new morphind tag for the edited word form ========
me_freqlist_unknown_all_edit_included <- me_freqlist_unknown_all_edit |> 
  filter(to_be_included == "y") |> 
  mutate(morphind_old = morphind,
         pref = str_replace_all(pref, "^me\\-", "meN-"),
         pref = str_replace_all(pref, "^N\\-", "N-"),
         suff = ifelse(suff != "0", paste("-", suff, sep = ""), suff),
         enclitic_tag = "dia<p>_PS3",
         enclitic_tag = ifelse(enclitic == "mu", "kamu<p>_PS2", enclitic_tag),
         enclitic_tag = ifelse(enclitic == "ku", "aku<p>_PS1", enclitic_tag),
         morphind = ifelse(suff!="0" & is.na(enclitic), paste(pref, "+", root_edit, "<", root_pos, ">+", suff, "_VSA", sep = ""), morphind),
         morphind = ifelse(suff=="0" & is.na(enclitic), paste(pref, "+", root_edit, "<", root_pos, ">_VSA", sep = ""), morphind),
         morphind = ifelse(suff=="0" & !is.na(enclitic), paste(pref, "+", root_edit, "<", root_pos, ">_VSA+", enclitic_tag, "", sep = ""), morphind),
         morphind = ifelse(suff!="0" & !is.na(enclitic), paste(pref, "+", root_edit, "<", root_pos, ">+", suff, "_VSA+", enclitic_tag, sep = ""), morphind),
         morphind = str_replace_all(morphind, "((?<=\\+)\\-|\\-(?=\\+))", ""),
         morphind = ifelse(!is.na(redup), str_replace_all(morphind, "(?<=_V).", "P"), morphind)) |> 
  rename(morphind_new = morphind, word_form = word_form_orig, pref_new = pref, suff_new = suff)

me_freqlist_unknown_morphind_root_pos_included <- me_freqlist_unknown_morphind_root_pos |> 
  select(-stem, -lemma) |> 
  distinct() |> 
  mutate(suff = if_else(suff != "0", paste("-", suff, sep = ""), suff),
         morphind_new = if_else(suff != "0" & is.na(enclitic), paste(pref, "+", root, "<", root_pos, ">+", suff, "_VSA", sep = ""), ""),
         morphind_new = if_else(suff == "0" & is.na(enclitic), paste(pref, "+", root, "<", root_pos, ">_VSA", sep = ""), morphind_new),
         morphind_new = if_else(suff == "0" & !is.na(enclitic), paste(pref, "+", root, "<", root_pos, ">_VSA", sep = ""), morphind_new),
         morphind_new = if_else(suff != "0" & !is.na(enclitic), paste(pref, "+", root, "<", root_pos, ">+", suff, "_VSA", sep = ""), morphind_new),
         morphind_new = str_replace_all(morphind_new, "((?<=\\+)\\-|\\-(?=\\+))", "")) |> 
  rename(word_form = word_form_orig, 
         pref_new1 = pref, 
         suff_new1 = suff, 
         morphind_new1 = morphind_new, 
         root_pos1 = root_pos, 
         enclitic1 = enclitic)

### combine with the original database ========
me_freqlist_df3 <- me_freqlist_df2 |> 
  left_join(me_freqlist_unknown_all_edit_included |> 
              select(word_form, morphind_new, root_edit, pref_new, suff_new, enclitic, root_pos, redup)) |> 
  mutate(morphind = ifelse(!is.na(morphind_new), morphind_new, morphind),
         root_morphind = ifelse(!is.na(root_edit), root_edit, root_morphind),
         root_pos_morphind = ifelse(!is.na(root_pos), root_pos, root_pos_morphind),
         pref_morphind = ifelse(!is.na(pref_new), pref_new, pref_morphind),
         suff_morphind = ifelse(!is.na(suff_new), suff_new, suff_morphind),
         reduplication = ifelse(!is.na(redup), redup, reduplication)) |> 
  left_join(me_freqlist_unknown_morphind_root_pos_included) |> 
  mutate(root_morphind = ifelse(word_form %in% me_freqlist_unknown_morphind_root_pos_included$word_form, root, root_morphind), 
         morphind = ifelse(word_form %in% me_freqlist_unknown_morphind_root_pos_included$word_form, morphind_new1, morphind),
         root_pos_morphind = ifelse(word_form %in% me_freqlist_unknown_morphind_root_pos_included$word_form, root_pos1, root_pos_morphind),
         pref_morphind = ifelse(word_form %in% me_freqlist_unknown_morphind_root_pos_included$word_form, pref_new1, pref_morphind),
         suff_morphind = ifelse(word_form %in% me_freqlist_unknown_morphind_root_pos_included$word_form, suff_new1, suff_morphind),
         suff_morphind = str_replace(suff_morphind, "^-", ""),
         pref_morphind = replace(pref_morphind, word_form == "menggulai", "meN-"),
         suff_morphind = replace(suff_morphind, word_form == "menggulai", "i")) |> 
  mutate(affix_morphind_wclass = paste(pref_morphind, root_pos_morphind, suff_morphind, sep = "_"),
         affix_morphind = paste(pref_morphind, suff_morphind, sep = "_"),
         affix_morphind = replace(affix_morphind, affix_morphind == "NA_kan", "0_kan"),
         affix_morphind_wclass = str_replace(affix_morphind_wclass, "^NA_", "0_"),
         verb_tagged = if_else(str_detect(morphind, "_V"), TRUE, FALSE)) |> 
  select(-morphind_new, -root_edit, -pref_new, -suff_new, -root_pos, -enclitic, -redup, -root_pos1, -pref_new1, -suff_new1, -enclitic1, -morphind_new1)

### get the roots of word_forms where the root_morphind is NAs ========
me_freqlist_df3 <- me_freqlist_df3 |> 
  mutate(root_morphind = ifelse(is.na(root_morphind) & !is.na(root) & !is.na(pref) & !is.na(suff) & !is.na(confix), 
                                root, 
                                root_morphind),
         root = ifelse(is.na(root) & str_detect(morphind, "_V") & !is.na(root_morphind) & !is.na(pref_morphind) & !is.na(suff_morphind), root_morphind, root),
         pref_morphind = ifelse(is.na(morphind) & !is.na(root_morphind) & !is.na(root) & !is.na(pref) & !is.na(suff) & !is.na(confix),
                                pref,
                                pref_morphind),
         suff_morphind = ifelse(is.na(morphind) & !is.na(root_morphind) & !is.na(root) & !is.na(pref) & !is.na(suff) & !is.na(confix),
                                suff,
                                suff_morphind),
         suff_morphind = str_replace(suff_morphind, "^-", "")) |> 
  mutate(affix_morphind_wclass = paste(pref_morphind, root_pos_morphind, suff_morphind, sep = "_"),
         affix_morphind = paste(pref_morphind, suff_morphind, sep = "_"),
         affix_morphind = replace(affix_morphind, affix_morphind == "NA_kan", "0_kan"),
         affix_morphind_wclass = str_replace(affix_morphind_wclass, "^NA_", "0_"))

### get the word class of the roots whose root_morphind is NAs ========
unparsed_id <- which(str_detect(me_freqlist_df3$morphind, "X--") & !is.na(me_freqlist_df3$root) | is.na(me_freqlist_df3$morphind) & !is.na(me_freqlist_df3$root) & str_detect(me_freqlist_df3$root_morphind, "X--"))
pos <- me_freqlist_df3[match(me_freqlist_df3[unparsed_id, ][["root"]], me_freqlist_df3$root_morphind),][['root_pos_morphind']]

unparsed_id_2 <- which(is.na(me_freqlist_df3$morphind) & !is.na(me_freqlist_df3$root) & is.na(me_freqlist_df3$root_pos_morphind) & !is.na(me_freqlist_df3$pref) & !is.na(me_freqlist_df3$suff) & !is.na(me_freqlist_df3$confix) & !is.na(me_freqlist_df3$reduplication))
pos2 <- me_freqlist_df3[match(me_freqlist_df3[unparsed_id_2, ][["root"]], me_freqlist_df3$root_morphind),][['root_pos_morphind']]

# check if the unparsed id is equal to the pos
length(unparsed_id) == length(pos)
length(unparsed_id_2) == length(pos2)

# x <- me_freqlist_df3
# x$root_pos_morphind[unparsed_id] <- pos
# x$root_pos_morphind[unparsed_id_2] <- pos2
me_freqlist_df3$root_pos_morphind[unparsed_id] <- pos
me_freqlist_df3$root_pos_morphind[unparsed_id_2] <- pos2

me_freqlist_df3 <- me_freqlist_df3 |> 
  mutate(pref_morphind = ifelse(str_detect(morphind, "X--") & 
                                  !is.na(root_pos_morphind) & 
                                  !is.na(pref) & 
                                  !is.na(suff) & 
                                  !is.na(root), 
                                pref, 
                                pref_morphind),
         suff_morphind = ifelse(str_detect(morphind, "X--") & 
                                  !is.na(root_pos_morphind) & 
                                  !is.na(pref) & 
                                  !is.na(suff) & 
                                  !is.na(root), 
                                suff, 
                                suff_morphind),
         pref_morphind = ifelse(!is.na(root_pos_morphind) & 
                                  !is.na(root) & 
                                  !is.na(root_morphind) & 
                                  !is.na(pref) & 
                                  !is.na(suff) & 
                                  is.na(pref_morphind), 
                                pref, 
                                pref_morphind),
         suff_morphind = ifelse(!is.na(root_pos_morphind) & 
                                  !is.na(root) & 
                                  !is.na(root_morphind) & 
                                  !is.na(pref) & 
                                  !is.na(suff) & 
                                  is.na(suff_morphind), 
                                suff, 
                                suff_morphind),
         suff_morphind = str_replace_all(suff_morphind, "^\\-", ""),
         suff_morphind = str_replace_all(suff_morphind, "((?<=_)-|^\\-)", ""),
         suff_morphind = str_replace_all(suff_morphind, "\\+\\-(ku|mu)$", ""),
         verb_tagged = ifelse(str_detect(morphind, "X--") & 
                                !is.na(root_pos_morphind) & 
                                !is.na(root_morphind) & 
                                !is.na(pref_morphind) & 
                                !is.na(suff_morphind), 
                              TRUE, 
                              verb_tagged),
         verb_tagged = ifelse(is.na(morphind) & 
                                !is.na(root_pos_morphind) & 
                                !is.na(root) & 
                                !is.na(root_morphind) & 
                                !is.na(pref) & 
                                !is.na(suff), 
                              TRUE, 
                              verb_tagged)) |> 
  mutate(affix_morphind_wclass = paste(pref_morphind, root_pos_morphind, suff_morphind, sep = "_"),
         affix_morphind = paste(pref_morphind, suff_morphind, sep = "_"),
         affix_morphind = replace(affix_morphind, affix_morphind == "NA_kan", "0_kan"),
         affix_morphind_wclass = str_replace(affix_morphind_wclass, "^NA_", "0_"))

### get the roots and word_forms whose root_pos_morphind is STILL NAs ======
root_with_pos_na <- me_freqlist_df3 |> 
  filter(is.na(morphind), 
         !is.na(root), 
         is.na(root_pos_morphind)) |> 
  select(word_form, root_morphind, root_pos_morphind, root, pref, suff) |> 
  distinct() |> 
  pull(root)

root_with_pos_df <- me_freqlist_df3 |> 
  filter(root %in% root_with_pos_na & 
           root_morphind %in% root_with_pos_na & 
           !is.na(root_pos_morphind)) |> 
  select(root, root_morphind, root_pos_morphind2 = root_pos_morphind) |> 
  distinct()

root_with_pos_na_df <- read_tsv("root_with_pos_na_df.txt") |> 
  mutate(SUFF2 = ifelse(is.na(SUFF2), "0", SUFF2)) |> 
  mutate(MORPHIND2 = ifelse(SUFF2 == "0", 
                            paste("meN+", ROOT2, "<", POS2, ">", REDUP_VOICE_TAG, sep = ""), 
                            paste("meN+", ROOT2, "<", POS2, ">+", SUFF2, REDUP_VOICE_TAG, sep = "")))

me_freqlist_df3 <- me_freqlist_df3 |> 
  left_join(root_with_pos_df) |> 
  mutate(root_pos_morphind = ifelse(root %in% root_with_pos_na & 
                                      root_morphind %in% root_with_pos_na & 
                                      is.na(root_pos_morphind), 
                                    root_pos_morphind2, 
                                    root_pos_morphind)) |> 
  left_join(root_with_pos_na_df) |> 
  mutate(root_pos_morphind = ifelse(root %in% root_with_pos_na_df$ROOT2 & 
                                      is.na(morphind) &
                                      !is.na(POS2), 
                                    POS2, 
                                    root_pos_morphind),
         morphind = ifelse(word_form %in% root_with_pos_na_df$word_form & 
                                      is.na(morphind) &
                                      !is.na(MORPHIND2), 
                                    MORPHIND2, 
                                    morphind),
         verb_tagged = ifelse(is.na(morphind) & 
                                !is.na(root_pos_morphind) & 
                                !is.na(root) & 
                                !is.na(root_morphind) & 
                                !is.na(pref) & 
                                !is.na(suff), 
                              TRUE, 
                              verb_tagged),
         verb_tagged = ifelse(is.na(morphind) & 
                                !is.na(root_pos_morphind) & 
                                !is.na(root) & 
                                !is.na(root_morphind) & 
                                !is.na(pref_morphind) & 
                                !is.na(suff_morphind), 
                              TRUE, 
                              verb_tagged),
         verb_tagged = ifelse(word_form %in% root_with_pos_na_df$word_form, TRUE, verb_tagged),
         affix_morphind_wclass = paste(pref_morphind, root_pos_morphind, suff_morphind, sep = "_"),
         affix_morphind = paste(pref_morphind, suff_morphind, sep = "_"),
         affix_morphind = replace(affix_morphind, affix_morphind == "NA_kan", "0_kan"),
         affix_morphind_wclass = str_replace(affix_morphind_wclass, "^NA_", "0_")) |> 
  select(-root_pos_morphind2, -ROOT2, -POS2, -SUFF2, -REDUP_VOICE_TAG, -MORPHIND2)

write_rds(me_freqlist_df3, "me_freqlist_df.rds")

# root_with_pos_na_df <- me_freqlist_df3 |> 
#   filter(root %in% root_with_pos_na & is.na(root_pos_morphind)) |> 
#   select(root, word_form) |> 
#   distinct()
# root_with_pos_na_df |> write_tsv("root_with_pos_na_df.txt")


# me_freqlist_df3 <- me_freqlist_df3 |>
#   # get the root, pref, suff from the Malindo Morph where the morphind parsing is NA or X--
#   mutate(root_morphind = ifelse(str_detect(root_morphind, "X--") & str_detect(morphind, "X--") & !is.na(root), root, root_morphind),
#          pref_morphind = ifelse(!is.na(root) & !is.na(pref) & is.na(pref_morphind), pref, pref_morphind),
#          suff_morphind = ifelse(!is.na(root) & !is.na(suff) & is.na(suff_morphind), suff, suff_morphind),
#          affix_morphind = paste(pref_morphind, suff_morphind, sep = "_"),
#          affix_morphind = replace(affix_morphind, affix_morphind == "NA_kan", "0_kan"),
#          verb_tagged = if_else(str_detect(morphind, "_V") | !is.na(pref_morphind), TRUE, FALSE))
# root_in_malindo_morph_df <- me_freqlist_df3 |> 
#   filter(str_detect(morphind, "X--"), !is.na(root)) |> 
#   select(1:3, root, root_morphind, root_pos_morphind, pref_morphind, pref, suff_morphind, suff, root_pos_add)

## di- ======
di_irrelevant <- read_lines('di_irrelevant.txt')
di_regex <- "^(?i)di[a-z-]{3,}$"
di_freqlist <- freqlist_all |> 
  map(~filter(., str_detect(word_form, di_regex))) |> 
  # remove some irrelevant di- words
  map(~filter(., !word_form %in% di_irrelevant))

### left join the morphind tag =====
di_freqlist <- di_freqlist |> 
  map(~left_join(., wlist_all_types_df_unique3 |> 
                   rename(word_form = word) |> 
                   select(-n), by = "word_form"))

### join all freqlist across all genres into a single data frame =====
di_freqlist_df <- di_freqlist |> 
  map2(names(di_freqlist), ~mutate(., genres = .y)) |> 
  map_df(bind_rows) |> 
  filter(str_detect(word_form, "\\-$", negate = TRUE)) |> 
  mutate(morphind = replace(morphind, word_form == "digotongnya", "di+gotong<v>_VSP+dia<p>_PS3"),
         morphind = replace(morphind, word_form == "digotonglah", "di+gotong<v>_VSP+lah<t>_T--")) |> 
  mutate(parsed = if_else(is.na(morphind), "no", "yes"),
         parsed = if_else(str_detect(morphind, "X--"), "no", parsed),
         verb_tagged = if_else(str_detect(morphind, "_V"), TRUE, FALSE))

### run morphind on the NAs DI- word=======
# di_freqlist_df_morphind_na <- di_freqlist_df |>
#   filter(is.na(morphind)) |>
#   select(word_form) |>
#   distinct()
# di_freqlist_df_morphind_na
# di_freqlist_df_morphind_na_parsed <- morph_parse(di_freqlist_df_morphind_na$word_form, TRUE, morphind_wd = "/Users/Primahadi/Documents/morphind")
# di_freqlist_df_morphind_na_parsed |>
#   rename(word_form = input_strings, morphind2 = morphind) |>
#   write_tsv("di_freqlist_df_morphind_na_parsed.txt")
# di_freqlist_df_morphind_na_parsed <- read_tsv("di_freqlist_df_morphind_na_parsed.txt")

# di_freqlist_df_morphind_na_parsed |> 
#   filter(str_detect(morphind2, "V"), str_detect(morphind2, "X", TRUE)) |> 
#   write_tsv("di_freqlist_df_morphind_na_parsed-OK.txt")

# di_freqlist_df_morphind_na_parsed |> 
#   filter(str_detect(morphind2, "X") & str_detect(morphind2, "V") | str_detect(morphind2, "X")) |> 
#   filter(!word_form %in% di_irrelevant) |> 
#   write_tsv("di_freqlist_df_morphind_na_parsed-NOT-OK.txt")

di_freqlist_df_morphind_na_parsed <- read_tsv("di_freqlist_df_morphind_na_parsed-OK.txt") |> distinct() # read the parsed data
di_freqlist_df_morphind_na_parsed_not_ok <- read_tsv("di_freqlist_df_morphind_na_parsed-NOT-OK.txt") # read the unparsed data
ok_temp <- di_freqlist_df_morphind_na_parsed_not_ok |> # select the parsed tokens from the unparsed data
  filter(str_detect(morphind2, "V"), str_detect(morphind2, "X", TRUE))
di_freqlist_df_morphind_na_parsed <- bind_rows(di_freqlist_df_morphind_na_parsed, ok_temp) # combined the old parsed data with the newly parsed data
di_freqlist_df_morphind_na_parsed |> 
  distinct() |> 
  write_tsv("di_freqlist_df_morphind_na_parsed-OK.txt") # and save it again the parsed data

di_freqlist_df <- di_freqlist_df |> 
  left_join(di_freqlist_df_morphind_na_parsed) |> 
  mutate(morphind = if_else(is.na(morphind) & !is.na(morphind2), morphind2, morphind)) |> 
  select(-morphind2) |> 
  mutate(parsed = "yes",
         parsed = replace(parsed, is.na(morphind), "no"),
         parsed = replace(parsed, str_detect(morphind, "X--"), "no"),
         verb_tagged = FALSE,
         verb_tagged = replace(verb_tagged, str_detect(morphind, "_V") & str_detect(morphind, "X--", TRUE), TRUE))

di_freqlist_df_morphind_na_parsed_not_ok |> # save again the remaining unparsed data
  filter(!word_form %in% di_irrelevant, !word_form %in% ok_temp$word_form, !word_form %in% pull(filter(di_freqlist_df, parsed == "yes"), word_form)) |>
  write_tsv("di_freqlist_df_morphind_na_parsed-NOT-OK.txt")

# di_freqlist_df_morphind_x <- di_freqlist_df |>
#   filter(str_detect(morphind, "X--")) |>
#   select(word_form, morphind) |>
#   distinct()
# di_freqlist_df_morphind_x
# di_freqlist_df_morphind_x_parsed <- morph_parse(di_freqlist_df_morphind_x$word_form, TRUE, morphind_wd = "/Users/Primahadi/Documents/morphind")
# di_freqlist_df_morphind_x_parsed |>
#   rename(word_form = input_strings, morphind2 = morphind) |>
#   write_tsv("di_freqlist_df_morphind_x_parsed.txt")
di_freqlist_df_morphind_x_parsed <- 
  read_tsv("di_freqlist_df_morphind_x_parsed.txt") |> 
  filter(str_detect(morphind2, "X", TRUE))

di_freqlist_df1 <- di_freqlist_df |> 
  left_join(di_freqlist_df_morphind_x_parsed) |> 
  mutate(morphind = if_else(word_form %in% di_freqlist_df_morphind_x_parsed$word_form & !is.na(morphind2), morphind2, morphind)) |> 
  select(-morphind2) |> 
  mutate(parsed = "yes",
         parsed = replace(parsed, is.na(morphind), "no"),
         parsed = replace(parsed, str_detect(morphind, "X--"), "no"),
         verb_tagged = FALSE,
         verb_tagged = replace(verb_tagged, str_detect(morphind, "_V"), TRUE))


### extract information from MORPHIND parsing =====
di_freqlist_df2 <- di_freqlist_df1 |> 
  mutate(morphind = replace(morphind, word_form =='di-anton-kan', 'di+anton<p>+kan_VSP'),
         morphind = replace(morphind, word_form =='di-dope', 'di+dope<f>_VSP'),
         morphind = replace(morphind, word_form =='dipajakkan', 'di+pajak<n>+kan_VSP'),
         parsed = "yes",
         parsed = replace(parsed, is.na(morphind), "no"),
         parsed = replace(parsed, str_detect(morphind, "X--"), "no"),
         verb_tagged = FALSE,
         verb_tagged = replace(verb_tagged, str_detect(morphind, "_V"), TRUE)) |> 
  mutate(root_morphind = morphind,
         root_morphind = str_replace_all(root_morphind, "^[^+<]+?\\+([^+<]+?\\+)?\\b", ""),
         root_morphind = if_else(str_detect(morphind, "\\-\\-\\+") & verb_tagged, 
                                 str_replace_all(root_morphind, "\\b([^<]+?)\\<.+?\\+([^<]+)\\<.+", "\\1 \\2"), root_morphind),
         root_morphind = if_else(verb_tagged & str_detect(root_morphind, "\\<"), 
                                 str_extract(root_morphind, "([^<]+?)(?=<)"), root_morphind),
         root_pos_morphind = str_extract(morphind, "(?<=<)([a-z-]+)((?=>_V)|(?=>\\+(k?an|in?)_V)|(?=>_[A-Z]..\\+))"),
         root_pos_morphind = replace(root_pos_morphind, root_morphind %in% verbsroot, "v"),
         pref_morphind = str_extract(morphind, "^[^+<]+?\\+([^+<]+?\\+)?"),
         pref_morphind = str_replace(pref_morphind, "\\+(?=[a-zA-Z])", "-+"),
         pref_morphind = str_replace(pref_morphind, "\\+$", "-"),
         suffixed_morphind = if_else(str_detect(morphind, ">\\+(kan|an|i|in)_V..") & verb_tagged, TRUE, FALSE),
         suff_morphind = if_else(suffixed_morphind, str_extract(morphind, "(?<=\\+)[a-z]+(?=_V)"), "0"),
         pers_pron_morphind = if_else(str_detect(morphind, "_PS(1|2|3)"), TRUE, FALSE),
         pers_pron_num_morphind = if_else(pers_pron_morphind & str_detect(morphind, "_PS1"), "first", "NA"),
         pers_pron_num_morphind = if_else(pers_pron_morphind & str_detect(morphind, "_PS2"), "second", pers_pron_num_morphind),
         pers_pron_num_morphind = if_else(pers_pron_morphind & str_detect(morphind, "_PS3"), "third", pers_pron_num_morphind)) |> 
  left_join(adj_root_updated) |> 
  mutate(root_pos_morphind = if_else(root_morphind %in% adj_root_updated$root_morphind & root_pos_morphind != root_pos_add, 
                                     root_pos_add, root_pos_morphind)) |> 
  mutate(parsed = "yes",
         parsed = replace(parsed, is.na(morphind), "no"),
         parsed = replace(parsed, str_detect(morphind, "X--"), "no"),
         verb_tagged = FALSE,
         verb_tagged = replace(verb_tagged, str_detect(morphind, "_V"), TRUE),
         affix_morphind_wclass = paste(pref_morphind, root_pos_morphind, suff_morphind, sep = "_"),
         affix_morphind = paste(pref_morphind, suff_morphind, sep = "_"),
         affix_morphind = replace(affix_morphind, affix_morphind == "NA_kan", "0_kan"),
         affix_morphind_wclass = str_replace(affix_morphind_wclass, "^NA_", "0_"),
         affix_morphind = str_replace(affix_morphind, "^NA_", "0_"))


### check proportion of parsability =====
#### type freq of parsed token =======
di_freqlist_df2 |> 
  select(word_form, morphind, parsed, verb_tagged) |> 
  distinct() |> 
  count(parsed) |> 
  mutate(perc = n/sum(n) * 100) |> 
  rename(n_type = n)

#### token freq of parsed token =======
di_freqlist_df2 |> 
  select(word_form, n, morphind, parsed, verb_tagged) |> 
  group_by(parsed) |> 
  summarise(n_token = sum(n), .groups = "drop") |> 
  mutate(perc = n_token/sum(n_token) * 100)

#### type freq of verb tagged token =======
di_freqlist_df2 |> 
  select(word_form, morphind, parsed, verb_tagged) |> 
  distinct() |> 
  count(verb_tagged) |> 
  mutate(perc = n/sum(n) * 100) |> 
  rename(n_type = n)

#### token freq of verb tagged token =======
di_freqlist_df2 |> 
  select(word_form, n, morphind, parsed, verb_tagged) |> 
  group_by(verb_tagged) |> 
  summarise(n_token = sum(n), .groups = "drop") |> 
  mutate(perc = n_token/sum(n_token) * 100)

write_rds(di_freqlist_df2, "di_freqlist_df.rds")




## ter- =======
ter_regex <- "^(?i)ter[a-z-]{3,}$"
ter_freqlist <- freqlist_all |> 
  map(~filter(., str_detect(word_form, ter_regex))) # |> 
  # remove some irrelevant di- words
  # map(~filter(., !word_form %in% di_irrelevant))

### left join the morphind tag =====
ter_freqlist <- ter_freqlist |> 
  map(~left_join(., wlist_all_types_df_unique3 |> 
                   rename(word_form = word) |> 
                   select(-n), by = "word_form"))

### join all freqlist across all genres into a single data frame =====
ter_freqlist_df <- ter_freqlist |> 
  map2(names(ter_freqlist), ~mutate(., genres = .y)) |> 
  map_df(bind_rows) |> 
  filter(str_detect(word_form, "\\-$", negate = TRUE)) |> 
  mutate(parsed = "yes",
         parsed = replace(parsed, is.na(morphind), "no"),
         parsed = replace(parsed, str_detect(morphind, "X--"), "no"),
         verb_tagged = FALSE,
         verb_tagged = replace(verb_tagged, str_detect(morphind, "_V"), TRUE),
         split_ter_verb = FALSE,
         split_ter_verb = if_else(verb_tagged & str_detect(morphind, "^ter\\+"), TRUE, split_ter_verb),
         split_ter_verb = if_else(verb_tagged & str_detect(morphind, "^ter\\+", TRUE), FALSE, split_ter_verb))

ter_na <- ter_freqlist_df |> filter(is.na(morphind)) |> select(word_form) |> distinct() |>
  mutate(word_form2 = str_replace_all(word_form, "^(ter)\\-", "\\1"))

# use this in MBA M1
# ter_na_parsed <- morph_parse(ter_na$word_form2, TRUE, morphind_wd = "/Users/Primahadi/Documents/morphind")
# write_tsv(ter_na_parsed, "ter_na_parsed.txt")
ter_na_parsed <- read_tsv("ter_na_parsed.txt") |> 
  rename(word_form2 = input_strings, morphind2 = morphind)
ter_na_parsed <- ter_na |> 
  left_join(ter_na_parsed) |> 
  select(-word_form2)
ter_freqlist_df <- ter_freqlist_df |> 
  left_join(ter_na_parsed) |> 
  mutate(morphind = ifelse(!is.na(morphind2), morphind2, morphind)) |> 
  select(-morphind2) |> 
  mutate(parsed = "yes",
         parsed = replace(parsed, is.na(morphind), "no"),
         parsed = replace(parsed, str_detect(morphind, "X--"), "no"),
         verb_tagged = FALSE,
         verb_tagged = replace(verb_tagged, str_detect(morphind, "_V"), TRUE),
         split_ter_verb = FALSE,
         split_ter_verb = if_else(verb_tagged & str_detect(morphind, "^ter\\+"), TRUE, split_ter_verb),
         split_ter_verb = if_else(verb_tagged & str_detect(morphind, "^ter\\+", TRUE), FALSE, split_ter_verb))

## ===== any fixing code for TER- tokens should go in the file: "ter_na_parsed.txt" ===== ##

ter_freqlist_df1 <- ter_freqlist_df |> 
  mutate(root_morphind = morphind,
         root_morphind = str_replace_all(root_morphind, "^[^+<]+?\\+([^+<]+?\\+)?\\b", ""),
         root_morphind = if_else(str_detect(morphind, "\\-\\-\\+") & verb_tagged, 
                                 str_replace_all(root_morphind, "\\b([^<]+?)\\<.+?\\+([^<]+)\\<.+", "\\1 \\2"), root_morphind),
         root_morphind = if_else(verb_tagged & str_detect(root_morphind, "\\<"), 
                                 str_extract(root_morphind, "([^<]+?)(?=<)"), root_morphind),
         root_pos_morphind = str_extract(morphind, "(?<=<)([a-z-]+)((?=>_V)|(?=>\\+(k?an|in?)_V)|(?=>_[A-Z]..\\+))"),
         root_pos_morphind = replace(root_pos_morphind, root_morphind %in% verbsroot, "v"),
         pref_morphind = str_extract(morphind, "^[^+<]+?\\+([^+<]+?\\+)?"),
         pref_morphind = str_replace(pref_morphind, "\\+(?=[a-zA-Z])", "-+"),
         pref_morphind = str_replace(pref_morphind, "\\+$", "-"),
         suffixed_morphind = if_else(str_detect(morphind, ">\\+(kan|an|i|in)_V..") & verb_tagged, TRUE, FALSE),
         suff_morphind = if_else(suffixed_morphind, str_extract(morphind, "(?<=\\+)[a-z]+(?=_V)"), "0"),
         pers_pron_morphind = if_else(str_detect(morphind, "_PS(1|2|3)"), TRUE, FALSE),
         pers_pron_num_morphind = if_else(pers_pron_morphind & str_detect(morphind, "_PS1"), "first", "NA"),
         pers_pron_num_morphind = if_else(pers_pron_morphind & str_detect(morphind, "_PS2"), "second", pers_pron_num_morphind),
         pers_pron_num_morphind = if_else(pers_pron_morphind & str_detect(morphind, "_PS3"), "third", pers_pron_num_morphind)) |> 
  left_join(adj_root_updated) |> 
  mutate(root_pos_morphind = if_else(root_morphind %in% adj_root_updated$root_morphind & root_pos_morphind != root_pos_add, 
                                     root_pos_add, root_pos_morphind)) |> 
  mutate(parsed = "yes",
         parsed = replace(parsed, is.na(morphind), "no"),
         parsed = replace(parsed, str_detect(morphind, "X--"), "no"),
         verb_tagged = FALSE,
         verb_tagged = replace(verb_tagged, str_detect(morphind, "_V"), TRUE),
         affix_morphind_wclass = paste(pref_morphind, root_pos_morphind, suff_morphind, sep = "_"),
         affix_morphind = paste(pref_morphind, suff_morphind, sep = "_"))

# save the database
write_rds(ter_freqlist_df1, "ter_freqlist_df.rds")



## ber- ALL =======
### filter the ber- related word forms using regex =======
ber_irrelevant <- read_lines("ber_irrelevant.txt")
ber_regex <- "^(?i)ber[a-z-]{3,}$"
ber_freqlist <- freqlist_all |> 
  map(~filter(., str_detect(word_form, ber_regex))) |> 
  map(~filter(., !word_form %in% c("berpedomanpadalembarobservasikeaktifansiswadanketerlaksanaanpembelajaran", "berpengaruhpositifterhadapprestasibelajar", "berperilku", "berperikemanu", "berpen-", "ber-an", "berpengaruhpositifterhadappartisipasianggota", "berpengaruhterhadapkinerjapenyuluh", "berpeng", "berpendu", "berpet", "berpengaruf", ber_irrelevant))) |> 
  map2_df(.y = genresnames, ~mutate(., genres = .y)) |> 
  filter(word_form != "berikan") |>  # ambiguous between beri+kan or ber+ikan
  filter(word_form != "berilah") # ambiguous between beri+lah or ber+ilah

### join the freqlist with the morphind tag =======
ber_freqlist0 <- ber_freqlist |> 
  left_join(wlist_all_types_df_unique3 |> 
              rename(word_form = word) |> 
              select(-n), by = "word_form") |> 
  mutate(parsing_status = if_else(str_detect(morphind, "_X"), "x", "others"),
         parsing_status = if_else(str_detect(morphind, "_V"), "verb", parsing_status),
         parsing_status = if_else(str_detect(morphind, "_A.."), "adj", parsing_status),
         parsing_status = if_else(is.na(morphind), "na", parsing_status))

ber_parsing_status <- ber_freqlist0 |> 
  select(word_form, n, morphind, parsing_status) |> 
  group_by(word_form, morphind, parsing_status) |> 
  summarise(n = sum(n), .groups = "drop")

ber_parsing_status |> 
  count(parsing_status) |> 
  mutate(perc = n/sum(n) * 100) |> 
  arrange(desc(n))

### join the MALINDO Morph database =====
# ber_freqlist0 <- ber_freqlist0 |>
#   left_join(mmorph_id_affix_to_join,
#             by = "word_form") |>
#   group_by(word_form, genres) |>
#   mutate(n_root_malindo = n_distinct(root),
#          root = replace(root, root == "tindak lanjut", "tindaklanjut")) |>
#   arrange(desc(n_root_malindo)) |>
#   mutate(pref = replace(pref, suff == "ber-", "ber-"),
#          suff = replace(suff, suff == "ber-", "0")) |> 
#   ungroup()


#### ber-peN-(/-an) ========
ber_pe_unknown <- ber_freqlist0 |>
  filter(str_detect(word_form, '^berpe[a-z-]+$')) |>
  filter(is.na(morphind)) |> 
  select(-n, -genres) |> 
  distinct()
ber_pe_unknown
ber_pe_unknown <- ber_pe_unknown |>
  mutate(word2 = ifelse(str_detect(word_form, "^berpe(nam|lu|ndu|nd[ia]|rs|ri(ke)?|n|ng[ea]|ngha(sil)?|ngucap|ndapa|ngala|rawak|rikemanusia)?\\-"),
                        str_replace_all(word_form, "^(berpe(nam|lu|ndu|nd[ia]|rs|ri(ke)?|ng?|ng[ea]|ngha(sil)?|ngucap|ndapa|ngala|rawak|rikemanusia)?)\\-", "\\1"), word_form),
         word2 = ifelse(str_detect(word2, "(?<=kkhu)\\-(an$)"), str_replace_all(word2, "(?<=kkhu)\\-(an$)", "\\1"), word2),
         word2 = str_replace_all(word2, "\\-(serta|teguh|roket|sipil|berjilbab|hidrolis|aktif|budaya|utama|kendali|terbaik|liquid|besar|art|cordite|presisi|lelah|tertinggi|terbesar|padat)$", " \\1"))

# use this in MBA M1
ber_pe_unknown_parsed <- morph_parse(ber_pe_unknown$word2, TRUE, morphind_wd = "/Users/Primahadi/Documents/morphind")

# use this in MBA Intel 2015
# ber_pe_unknown_parsed <- morph_parse(ber_pe_unknown$word2, TRUE)
ber_pe_unknown_parsed <- rename(ber_pe_unknown_parsed, word2 = input_strings, morphind2 = morphind)
ber_pe_unknown <- ber_pe_unknown |>
  left_join(ber_pe_unknown_parsed) |>
  distinct() |>
  select(word_form, morphind2)

ber_freqlist1 <- ber_freqlist0 |> 
  left_join(ber_pe_unknown) |> 
  mutate(morphind = ifelse(!is.na(morphind2), morphind2, morphind)) |> 
  select(-morphind2) |> 
  mutate(parsing_status = if_else(str_detect(morphind, "_X"), "x", parsing_status),
         parsing_status = if_else(str_detect(morphind, "_V"), "verb", parsing_status),
         parsing_status = if_else(str_detect(morphind, "_A.."), "adj", parsing_status),
         parsing_status = if_else(is.na(morphind), "na", parsing_status))

ber_parsing_status1 <- ber_freqlist1 |> 
  select(word_form, n, morphind, parsing_status) |> 
  group_by(word_form, morphind, parsing_status) |> 
  summarise(n = sum(n), .groups = "drop")

ber_parsing_status1 |> 
  count(parsing_status) |> 
  mutate(perc = n/sum(n) * 100) |> 
  arrange(desc(n))

# ber_morphind_na <- ber_freqlist1 |>
#   filter(parsing_status == "na") |>
#   select(word_form) |>
#   distinct()
# ber_morphind_na_parsed <- morph_parse(ber_morphind_na$word_form, TRUE, morphind_wd = "/Users/Primahadi/Documents/morphind")
# ber_morphind_na_parsed |>
#   write_tsv("ber_morphind_na_parsed.txt")

ber_morphind_na_parsed <- read_tsv("ber_morphind_na_parsed.txt")
ber_freqlist2 <- ber_freqlist1 |> 
  left_join(ber_morphind_na_parsed |> rename(word_form = input_strings, morphind2 = morphind)) |> 
  mutate(morphind = ifelse(!is.na(morphind2), morphind2, morphind)) |> 
  select(-morphind2) |> 
  mutate(parsing_status = if_else(str_detect(morphind, "_X"), "x", parsing_status),
         parsing_status = if_else(str_detect(morphind, "_V"), "verb", parsing_status),
         parsing_status = if_else(str_detect(morphind, "_A.."), "adj", parsing_status),
         parsing_status = if_else(str_detect(morphind, "_(V|A|X)", TRUE) & !is.na(morphind), "others", parsing_status),
         parsing_status = if_else(is.na(morphind), "na", parsing_status))

ber_parsing_status2 <- ber_freqlist2 |> 
  select(word_form, n, morphind, parsing_status) |> 
  group_by(word_form, morphind, parsing_status) |> 
  summarise(n = sum(n), .groups = "drop")

ber_parsing_status2 |> 
  count(parsing_status) |> 
  mutate(perc = n/sum(n) * 100) |> 
  arrange(desc(n))

# ber_freqlist2 |> filter(parsing_status == "x") |> select(word_form, morphind) |> distinct() |> 
#   write_tsv("ber_morphind_x_parsed.txt")

ber_morphind_x_parsed_ok <- read.table(file = "ber_morphind_x_parsed.txt", 
                                       sep = "\t", header = TRUE, quote = "", comment.char = "") |> 
  filter(included == "y" | !is.na(root2)) |> 
  filter(!word_form %in% pull(filter(ber_morphind_na_parsed, str_detect(morphind, "X--", TRUE)), input_strings)) |> 
  mutate(morphind2 = "",
         morphind2 = ifelse(is.na(suff2) & is.na(clitic_tag), paste(pref2, root2, "<", root_pos2, ">", word_tag, sep = ""), morphind2),
         morphind2 = ifelse(is.na(suff2) & !is.na(clitic_tag), paste(pref2, root2, "<", root_pos2, ">", word_tag, "+", clitic_tag, sep = ""), morphind2),
         morphind2 = ifelse(!is.na(suff2) & is.na(clitic_tag), paste(pref2, root2, "<", root_pos2, ">+", suff2, word_tag, sep = ""), morphind2),
         morphind2 = ifelse(!is.na(suff2) & !is.na(clitic_tag), paste(pref2, root2, "<", root_pos2, ">+", suff2, word_tag, "+", clitic_tag, sep = ""), morphind2)) |> 
  select(-included, -morphind)

read.table(file = "ber_morphind_x_parsed.txt", 
           sep = "\t", header = TRUE, quote = "", comment.char = "") |> 
  filter(is.na(included) | is.na(root2) & included != "n") |> 
  filter(!word_form %in% ber_morphind_x_parsed_ok$word_form) |> 
  write_tsv("ber_morphind_x_parsed.txt")

# ber_morphind_x_parsed_ok |> write_tsv("ber_morphind_x_parsed-OK.txt")
read_tsv("ber_morphind_x_parsed-OK.txt") |>
  bind_rows(ber_morphind_x_parsed_ok) |> 
  write_tsv("ber_morphind_x_parsed-OK.txt")

ber_morphind_x_parsed_ok2 <- read_tsv("ber_morphind_x_parsed-OK.txt")
ber_freqlist3 <- ber_freqlist2 |> 
  left_join(ber_morphind_x_parsed_ok2 |> select(word_form, morphind2)) |> 
  mutate(morphind = ifelse(!is.na(morphind2), morphind2, morphind)) |> 
  select(-morphind2) |> 
  mutate(parsing_status = if_else(str_detect(morphind, "_X"), "x", parsing_status),
         parsing_status = if_else(str_detect(morphind, "_V"), "verb", parsing_status),
         parsing_status = if_else(str_detect(morphind, "_A.."), "adj", parsing_status),
         parsing_status = if_else(str_detect(morphind, "_(V|A|X)", TRUE) & !is.na(morphind), "others", parsing_status),
         parsing_status = if_else(is.na(morphind), "na", parsing_status))
  

### extract information from MORPHIND parsing =====
ber_freqlist4 <- ber_freqlist3 |> 
  mutate(verb_tagged = if_else(str_detect(morphind, "_V"), TRUE, FALSE)) |> 
  mutate(root_morphind = morphind,
         root_morphind = str_replace_all(root_morphind, "^[^+<]+?\\+([^+<]+?\\+)?\\b", ""),
         root_morphind = if_else(str_detect(morphind, "\\-\\-\\+") & verb_tagged, 
                                 str_replace_all(root_morphind, "\\b([^<]+?)\\<.+?\\+([^<]+)\\<.+", "\\1 \\2"), root_morphind),
         root_morphind = if_else(verb_tagged & str_detect(root_morphind, "\\<"), 
                                 str_extract(root_morphind, "([^<]+?)(?=<)"), root_morphind),
         root_pos_morphind = str_extract(morphind, "(?<=<)([a-z-]+)((?=>_V)|(?=>\\+(k?an|in?)_V)|(?=>_[A-Z]..\\+))"),
         root_pos_morphind = replace(root_pos_morphind, root_morphind %in% verbsroot, "v"),
         pref_morphind = str_extract(morphind, "^[^+<]+?\\+([^+<]+?\\+)?"),
         pref_morphind = str_replace(pref_morphind, "\\+(?=[a-zA-Z])", "-+"),
         pref_morphind = str_replace(pref_morphind, "\\+$", "-"),
         suffixed_morphind = if_else(str_detect(morphind, ">\\+(kan|an|i|in)_V..") & verb_tagged, TRUE, FALSE),
         suff_morphind = if_else(suffixed_morphind, str_extract(morphind, "(?<=\\+)[a-z]+(?=_V)"), "0"),
         pers_pron_morphind = if_else(str_detect(morphind, "_PS(1|2|3)"), TRUE, FALSE),
         pers_pron_num_morphind = if_else(pers_pron_morphind & str_detect(morphind, "_PS1"), "first", "NA"),
         pers_pron_num_morphind = if_else(pers_pron_morphind & str_detect(morphind, "_PS2"), "second", pers_pron_num_morphind),
         pers_pron_num_morphind = if_else(pers_pron_morphind & str_detect(morphind, "_PS3"), "third", pers_pron_num_morphind)) |> 
  left_join(adj_root_updated) |> 
  mutate(root_pos_morphind = if_else(root_morphind %in% adj_root_updated$root_morphind & root_pos_morphind != root_pos_add, 
                                     root_pos_add, root_pos_morphind)) |> 
  select(-kbbi_pos_no_1, -note, -InputKarlina, -root_pos_add) |> 
  mutate(affix = NA,
         affix = ifelse(verb_tagged, paste(pref_morphind, suff_morphind, sep = "_"), affix))


ber_parsing_status2 <- ber_freqlist4 |> 
  select(word_form, n, morphind, parsing_status) |> 
  group_by(word_form, morphind, parsing_status) |> 
  summarise(n = sum(n), .groups = "drop")

ber_parsing_status2 |> 
  count(parsing_status) |> 
  mutate(perc = n/sum(n) * 100) |> 
  arrange(desc(n))

write_rds(ber_freqlist4, "ber_freqlist_df.rds")


#### ber-ke-(/-an) ========
# ber_ke_all <- wlist_all_types_df_unique3 |> 
#   filter(str_detect(word, '^berke[a-z-]+$')) |> 
#   arrange(desc(n))
# ber_ke_unknown <- ber_ke_all |> 
#   filter(is.na(morphind))
# ber_ke_unknown_parsed <- morph_parse(ber_ke_unknown$word, TRUE, morphind_wd = "/Users/Primahadi/Documents/morphind")
# ber_ke_unknown <- ber_ke_unknown |> 
#   left_join(ber_ke_unknown_parsed |> 
#               rename(word = input_strings, morphind2 = morphind)) |> 
#   distinct() |> 
#   select(word, morphind2)
# ber_ke_all <- ber_ke_all |> 
#   left_join(ber_ke_unknown, by = "word") |> 
#   mutate(morphind = if_else(is.na(morphind), morphind2, morphind)) |> 
#   select(-morphind2)
