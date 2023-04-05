# Kode pemrograman untuk analisis kuantitatif pada makalah
# "Afiksasi Verba dalam Bahasa Indonesia"
# penulis: Gede Primahadi Wijaya Rajeg & Karlina Denistia (2022) 
# Tautan repositori: https://doi.org/10.17605/OSF.IO/NUXD4

library(tidyverse)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)

# verbs <- readr::read_tsv("verbs_main.txt")
verbs <- readr::read_rds("verbs_main.rds")
genre_levels <- c("Surat_Resmi", "Perundang_undangan", "Jurnal", 
                  "Disertasi_Tesis_Skripsi", "Buku_Teks", "Laman_Resmi", 
                  "Populer", "Biografi", "Majalah", 
                  "Koran", "Cerpen", "Novel")
verbs <- verbs %>% 
  mutate(genres = factor(genres, levels = genre_levels))
genressizes <- readr::read_tsv("genressizes.txt") %>% 
  mutate(genres = factor(genres, levels = genre_levels))
yearsizes <- readr::read_tsv("yearsizes.txt") %>% 
  mutate(year = as.character(year))
corpussizes_all <- as.numeric(readr::read_lines("corpussizes_all.txt"))

# Deriving the sizes of the verb database ======
## tokens
verbs_tokens_dbase_total <- sum(verbs$n)
verbs_tokens_dbase_total
## types
verbs_types_dbase_total <- verbs %>% 
  group_by(pref_morphind, root_morphind, suff_morphind) %>% 
  distinct() %>% 
  nrow()
verbs_types_dbase_total
## hapax legomena
verbs_hapax_dbase_total <- verbs %>% 
  group_by(pref_morphind, root_morphind, suff_morphind) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  filter(n == 1) %>% 
  nrow()
verbs_hapax_dbase_total

# Create the content of "Tabel 1." ========
# samples_of_database <- verbs %>%
#   group_by(dbase) %>% 
#   slice_sample(n = 1) %>% 
#   ungroup() %>% 
#   select(tahun=year, `bentukan kata`=word_form, token = n, 
#          genre = genres, morphind, `akar kata`=root_morphind, 
#          `kelas kata akar` = root_pos_morphind)
# samples_of_database %>% 
#   write_rds('samples_of_database.rds')
samples_of_database <- read_rds('samples_of_database.rds')


# Analysis ============
### 1. number of tokens, types, and hapaxes for me, di, ter, and ber verbs (prefix only) ======
prefix_overall <- verbs %>% 
  group_by(dbase) %>% 
  summarise(tokens = sum(n),
            types  = n_distinct(word_form)) %>%  # by word form %>% 
  # join the type freq analysis by unique combination of root and affixes
  left_join(verbs %>% 
              group_by(dbase, affix_morphind, root_morphind) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              group_by(dbase) %>% 
              summarise(types_root = n(), .groups = "drop")) %>% 
  # join the hapax analysis by word_form
  left_join(verbs %>% 
              group_by(dbase, word_form) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(dbase) %>% 
              summarise(hapaxes = sum(is_hapax)) %>% 
              arrange(desc(hapaxes))) %>% 
  # join the hapax analysis by root by unique combination of root and affixes
  left_join(verbs %>% 
              group_by(dbase, affix_morphind, root_morphind) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(dbase) %>% 
              summarise(hapaxes_root = sum(is_hapax)) %>% 
              arrange(desc(hapaxes_root))) %>% 
  arrange(desc(tokens)) %>% 
  mutate(dbase = factor(dbase, levels = .$dbase)) %>% 
  pivot_longer(-dbase, names_to = "analyses", values_to = "values") %>% 
  mutate(corpussize = corpussizes_all,
         pmw = (values/corpussize) * 1000000)

#### 1.1 visualisation ======
##### 1.1.1 tokens plot =======
toks_fig <- prefix_overall %>% 
  filter(analyses == "tokens") %>% 
  mutate(dbase = paste(toupper(dbase), "-", sep = ""),
         dbase = factor(dbase, levels = c("ME-", "DI-", "BER-", "TER-"))) %>% 
  ggplot(aes(x = dbase, y = pmw, fill = dbase)) + # `pmw` = RELATIVE PER MILLION count
  # ggplot(aes(x = dbase, y = values, fill = dbase)) + # `values` = RAW count
  geom_col(position = 'dodge') +
  geom_text(aes(label = prettyNum(round(pmw, 2), big.mark = ".", decimal.mark = ",")), 
            position = position_dodge(.9), vjust = -.5, size = 2.5) +
  labs(x = "", 
       title = "Kekerapan/kemunculan relatif\n(frekuensi-token relatif per 1 juta kemunculan kata)", 
       y = "") +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size = 8.75)) +
  scale_fill_brewer(type = "qual", palette = "Set2")
##### 1.1.2 types plot ========
types_fig <- prefix_overall %>% 
  filter(analyses == "types_root") %>% 
  mutate(dbase = paste(toupper(dbase), "-", sep = ""),
         dbase = factor(dbase, levels = c("ME-", "DI-", "BER-", "TER-"))) %>% 
  ggplot(aes(x = dbase, y = pmw, fill = dbase)) + # `pmw` = RELATIVE PER MILLION count
  # ggplot(aes(x = dbase, y = values, fill = dbase)) + # `values` = RAW count
  geom_col(position = 'dodge') +
  geom_text(aes(label = prettyNum(round(pmw, 2), big.mark = ".", decimal.mark = ",")), 
            position = position_dodge(.9), vjust = -.5, size = 2.5) +
  labs(x = "", 
       title = "Jumlah bentukan kata relatif\n(frekuensi-tipe relatif per 1 juta kemunculan kata)", 
       y = "") +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size = 8.75)) +
  scale_fill_brewer(type = "qual", palette = "Set2")
##### 1.1.3 hapax plot ==========
hapax_fig <- prefix_overall %>% 
  filter(analyses == "hapaxes_root") %>% 
  mutate(dbase = paste(toupper(dbase), "-", sep = ""),
         dbase = factor(dbase, levels = c("ME-", "DI-", "BER-", "TER-"))) %>% 
  ggplot(aes(x = dbase, y = pmw, fill = dbase)) + # `pmw` = RELATIVE PER MILLION count
  # ggplot(aes(x = dbase, y = values, fill = dbase)) + # `values` = RAW count
  geom_col(position = 'dodge') +
  geom_text(aes(label = prettyNum(round(pmw, 2), big.mark = ".", decimal.mark = ",")), 
            position = position_dodge(.9), vjust = -.5, size = 2.5) +
  labs(x = "", 
       title = "Jumlah bentukan kata berkekerapan 1\n(hapax per 1 juta kemunculan kata)", 
       y = "") +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size = 8.75)) +
  scale_fill_brewer(type = "qual", palette = "Set2")
##### 1.1.4 combined plot ==========
prefix_overall_plot <- grid.arrange(toks_fig, types_fig, hapax_fig, nrow = 1, ncol = 3)
##### 1.1.5 save the plot ==========
ggsave(plot = prefix_overall_plot, 
       filename = "figures/1-prefix-overall-freq-profiles.png", 
       width = 11, height = 4, 
       dpi = 300, units = "in")

### 3. types, tokens, and hapax for each affix SUB-PATTERN in the WHOLE CORPUS ======
overall_prods <- verbs %>% 
  group_by(dbase, affix_morphind) %>% 
  summarise(types_root = n_distinct(root_morphind),
            types_wordform = n_distinct(word_form),
            tokens = sum(n),
            .groups = "drop") %>% 
  mutate(perc_tokens = round(tokens/sum(tokens) * 100, 2),
         perc_type_root = round(types_root/sum(types_root) * 100, 2),
         perc_type_wordform = round(types_wordform/sum(types_wordform) * 100, 2)) %>% 
  arrange(dbase, desc(types_root)) %>% 
  # join the hapax analysis by root
  left_join(verbs %>% 
              group_by(dbase, affix_morphind, root_morphind) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(affix_morphind) %>% 
              summarise(hapaxes_root = sum(is_hapax)) %>% 
              arrange(desc(hapaxes_root))) %>% 
  # join the hapax analysis by word_form
  left_join(verbs %>% 
              group_by(dbase, affix_morphind, word_form) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(affix_morphind) %>% 
              summarise(hapaxes_wordform = sum(is_hapax)) %>% 
              arrange(desc(hapaxes_wordform))) %>% 
  arrange(dbase, desc(types_root), desc(hapaxes_root)) %>% 
  mutate(htr_root = round(hapaxes_root/tokens, 6),
         htr_wordform = round(hapaxes_wordform/tokens, 6)) %>% 
  group_by(dbase) %>% 
  mutate(focused = if_else(types_root > 5, 
                           TRUE, # mark the focused affixes with type freq. > 5
                           FALSE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = -c(dbase, affix_morphind, focused), 
               names_to = "freq_profile", 
               values_to = "values") %>% 
  mutate(corpussize = corpussizes_all,
         pmw = 0,
         pmw = if_else(str_detect(freq_profile, "^(perc|htr)", negate = TRUE), 
                       ((values/corpussize) * 1000000), 
                       pmw))

#### 3.0 Get the values of the focused affixes =======
focused_affixes <- overall_prods %>% 
  filter(focused) %>% 
  pull(affix_morphind) %>% 
  unique()
focused_affixes

#### 3.1 visualisation for each affix sub-pattern BY FREQUENCY PROFILE =======
# unitising the colouring for ME, DI, BER, and TER
colour_values <- data.frame(dbase = c("me", "di", "ber", "ter"),
                            colvals = c("#66C2A5", "#FC8D62", 
                                        "#8DA0CB", "#E78AC3"))
overall_prods <- overall_prods %>% 
  left_join(colour_values)

##### 3.1.1 token frequency ==========
token_plots <- overall_prods %>% 
  filter(focused, freq_profile == "tokens") %>% 
  mutate(dbase = factor(dbase, levels = c("me", "di", "ber", "ter"))) %>% 
  mutate(affix_morphind = str_replace_all(affix_morphind, "_0", ""),
         affix_morphind = str_replace_all(affix_morphind, "_([a-z])", "/-\\1"),
         affix_morphind = str_replace_all(affix_morphind, "-(\\+)", "\\1")) %>% 
  split(.$dbase) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values, fill = colvals)) + # `values` = RAW count
    ~ggplot(., aes(x = reorder(affix_morphind, pmw), y = pmw, fill = colvals)) + 
      geom_col() + 
      coord_flip() + 
      theme_light() +
      geom_text(data = filter(., pmw < 10000 & affix_morphind != "ter-"), 
                aes(label = prettyNum(round(pmw, 1), big.mark = ".", decimal.mark = ",")), 
                hjust = -.2, size = 2.2) +
      geom_text(data = filter(., pmw > 10000 | affix_morphind == "ter-"), 
                aes(label = prettyNum(round(pmw, 1), big.mark = ".", decimal.mark = ",")), 
                hjust = 1.5, size = 2.2) +
      scale_fill_manual(values = .$colvals) +
      labs(x = "", y = "Kekerapan relatif per 1 juta kata", fill = "") +
      theme(axis.title.x = element_text(size = 7),
            axis.text.x = element_text(size = 6.1),
            legend.position = "none"))
token_plots1 <- grid.arrange(grobs = token_plots, ncol = 2, nrow = 2)
ggsave(plot = token_plots1, 
       filename = "figures/2-sub-pattern-tokens.png", 
       width = 7.85, height = 5.5, 
       dpi = 300, units = "in")

##### 3.1.2 type frequency ==========
type_plots <- overall_prods %>% 
  filter(focused, freq_profile == "types_root") %>% 
  mutate(dbase = factor(dbase, levels = c("me", "di", "ber", "ter"))) %>% 
  mutate(affix_morphind = str_replace_all(affix_morphind, "_0", ""),
         affix_morphind = str_replace_all(affix_morphind, "_([a-z])", "/-\\1"),
         affix_morphind = str_replace_all(affix_morphind, "-(\\+)", "\\1")) %>% 
  split(.$dbase) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values, fill = colvals)) + # `values` = RAW count
    ~ggplot(., aes(x = reorder(affix_morphind, pmw), y = pmw, fill = colvals)) + 
      geom_col() + 
      coord_flip() + 
      theme_light() +
      geom_text(data = filter(., pmw < 50), 
                aes(label = prettyNum(round(pmw, 1), big.mark = ".", decimal.mark = ",")), 
                hjust = -.2, size = 2.2) +
      geom_text(data = filter(., pmw > 50), 
                aes(label = prettyNum(round(pmw, 1), big.mark = ".", decimal.mark = ",")), 
                hjust = 1.5, size = 2.2) +
      scale_fill_manual(values = .$colvals) +
      labs(x = "", 
           y = "Jumlah bentukan kata (frekuensi tipe)\n(relatif per 1 juta kata)", 
           fill = "") +
      theme(axis.title.x = element_text(size = 7),
            axis.text.x = element_text(size = 6.1),
            legend.position = "none"))
type_plots1 <- grid.arrange(grobs = type_plots, ncol = 2, nrow = 2)
ggsave(plot = type_plots1, 
       filename = "figures/3-sub-pattern-types.png", 
       width = 7.85, height = 5.5, 
       dpi = 300, units = "in")

##### 3.1.3 hapax ==========
hapax_plots <- overall_prods %>% 
  filter(focused, freq_profile == "hapaxes_root") %>% 
  mutate(dbase = factor(dbase, levels = c("me", "di", "ber", "ter"))) %>% 
  mutate(affix_morphind = str_replace_all(affix_morphind, "_0", ""),
         affix_morphind = str_replace_all(affix_morphind, "_([a-z])", "/-\\1"),
         affix_morphind = str_replace_all(affix_morphind, "-(\\+)", "\\1")) %>% 
  split(.$dbase) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values, fill = colvals)) + # `values` = RAW count
    ~ggplot(., aes(x = reorder(affix_morphind, pmw), y = pmw, fill = colvals)) +
      geom_col() + 
      coord_flip() + 
      theme_light() +
      geom_text(data = filter(., pmw < 10), 
                aes(label = prettyNum(round(pmw, 1), big.mark = ".", decimal.mark = ",")), 
                hjust = -.2, size = 2.2) +
      geom_text(data = filter(., pmw > 10), 
                aes(label = prettyNum(round(pmw, 1), big.mark = ".", decimal.mark = ",")), 
                hjust = 1.5, size = 2.2) +
      scale_fill_manual(values = .$colvals) +
      labs(x = "", 
           y = "Jumlah bentukan kata berkekerapan 1 (hapax)\n(relatif per 1 juta kata)", 
           fill = "") +
      theme(axis.title.x = element_text(size = 7),
            axis.text.x = element_text(size = 6.1),
            legend.position = "none"))
hapax_plots1 <- grid.arrange(grobs = hapax_plots, ncol = 2, nrow = 2)
ggsave(plot = hapax_plots1, 
       filename = "figures/4-sub-pattern-hapax.png", 
       width = 7.85, height = 5.5, 
       dpi = 300, units = "in")

#### 3.3 Correlation analysis of sub-patterns productivity measures =============
correlation_analysis_df <- overall_prods %>% 
  filter(str_detect(freq_profile, '(perc|htr_|wordform)', TRUE)) %>% 
  select(dbase, affix_morphind, freq_profile, values) %>% 
  pivot_wider(names_from = "freq_profile", values_from = "values")

##### 3.3.1 cor.test ME- and DI- subtypes========
me_df <- correlation_analysis_df %>% 
  filter(dbase %in% c("me", "di")) %>% 
  mutate(affix_morphind = str_replace(affix_morphind, "^(meN-|di-)", ""),
         affix_morphind = str_replace(affix_morphind, "^[+]", ""))
intersecting_affixes <- intersect(pull(filter(me_df, dbase=='me'), affix_morphind), 
                                  pull(filter(me_df, dbase=='di'), affix_morphind))
me_df <- me_df %>% 
  filter(affix_morphind %in% intersecting_affixes) %>% 
  mutate(affix_morphind = factor(affix_morphind, levels = intersecting_affixes)) %>% 
  arrange(dbase, affix_morphind)

###### correlation tests (Kendall)
####### 3.3.1.1 tokens
cortest_me_di_tokens <- cor.test(pull(filter(me_df, dbase == "me"), tokens),
                                 pull(filter(me_df, dbase == "di"), tokens),
                                 method = "kendall",
                                 exact = FALSE) # Warning message: Cannot compute exact p-value with ties' hence exact = FALSE
cortest_me_di_tokens
####### 3.3.1.2 types
cortest_me_di_types <- cor.test(pull(filter(me_df, dbase == "me"), types_root),
                                pull(filter(me_df, dbase == "di"), types_root),
                                method = "kendall",
                                exact = FALSE) # Warning message: Cannot compute exact p-value with ties' hence exact = FALSE
cortest_me_di_types
####### 3.3.1.3 hapaxes
cortest_me_di_hapaxes <- cor.test(pull(filter(me_df, dbase == "me"), hapaxes_root),
                                  pull(filter(me_df, dbase == "di"), hapaxes_root),
                                  method = "kendall",
                                  exact = FALSE) # Warning message: Cannot compute exact p-value with ties' hence exact = FALSE
cortest_me_di_hapaxes
##### 3.3.2 pair-wise correlation between values for each of the four affix sub-patterns========
cor(filter(correlation_analysis_df, dbase=='me')[, 3:5], method = "kendall") # ME-
cor(filter(correlation_analysis_df, dbase=='di')[, 3:5], method = "kendall") # DI-
cor(filter(correlation_analysis_df, dbase=='ber')[, 3:5], method = "kendall") # BER-
cor(filter(correlation_analysis_df, dbase=='ter')[, 3:5], method = "kendall") # TER-


### 4. types, tokens, and hapax for each focused affix sub-patterns by YEAR ======
year_prods <- verbs %>% 
  group_by(dbase, affix_morphind, year) %>% 
  summarise(types_root = n_distinct(root_morphind),
            types_wordform = n_distinct(word_form),
            tokens = sum(n),
            .groups = "drop") %>% 
  mutate(perc_tokens = round(tokens/sum(tokens) * 100, 2),
         perc_type_root = round(types_root/sum(types_root) * 100, 2),
         perc_type_wordform = round(types_wordform/sum(types_wordform) * 100, 2)) %>% 
  arrange(dbase, desc(types_root)) %>% 
  # join the hapax analysis by root
  left_join(verbs %>% 
              group_by(dbase, affix_morphind, root_morphind, year) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(affix_morphind, year) %>% 
              summarise(hapaxes_root = sum(is_hapax)) %>% 
              arrange(desc(hapaxes_root))) %>% 
  # join the hapax analysis by word_form
  left_join(verbs %>% 
              group_by(dbase, affix_morphind, word_form, year) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(affix_morphind, year) %>% 
              summarise(hapaxes_wordform = sum(is_hapax)) %>% 
              arrange(desc(hapaxes_wordform))) %>% 
  arrange(dbase, desc(hapaxes_root), desc(types_root)) %>% 
  mutate(htr_root = round(hapaxes_root/tokens, 6),
         htr_wordform = round(hapaxes_wordform/tokens, 6)) %>% 
  group_by(dbase) %>% 
  mutate(focused = if_else(affix_morphind %in% focused_affixes, TRUE, FALSE)) %>% 
  ungroup() %>% 
  arrange(dbase, year) %>% 
  pivot_longer(cols = -c(dbase, affix_morphind, focused, year), 
               names_to = "freq_profile", values_to = "values") %>% 
  left_join(yearsizes, by = 'year') %>% 
  mutate(pmw = 0,
         pmw = if_else(str_detect(freq_profile, "^(perc|htr)", negate = TRUE), 
                       ((values/year_size) * 1000000), 
                       pmw))

#### 4.x visualisation for each affix patterns ========
# focused_affixes %>% factor(levels = focused_affixes) %>% tibble(affix_morphind_colour = ., affix_morphind = .) -> focused_affix_colour
focused_affix_colour <- focused_affixes %>%
  factor(levels = focused_affixes) %>%
  tibble(affix_morphind_colour = ., affix_morphind = .) %>%
  mutate(affix_morphind_colour = str_replace_all(affix_morphind_colour, "_0", ""),
         affix_morphind_colour = str_replace_all(affix_morphind_colour, "_([a-z])", "/-\\1"),
         affix_morphind_colour = str_replace_all(affix_morphind_colour, "-(\\+)", "\\1"),
         affix_morphind_colour = factor(affix_morphind_colour, 
                                        levels = c("ber-", "ber-/-an", "ber-/-kan", "ber+ke-/-an", 
                                                   "ber+peN-", "ber+peN-/-an", 
                                                   "ber+si-", "ber+per-/-an", "ber-/-i", "di-", 
                                                   "di-/-kan", "di-/-i", "di+per-", 
                                                   "di+per-/-kan", "di+per-/-i", "di-/-in", 
                                                   "meN-", "meN-/-kan", "meN-/-i", 
                                                   "meN+per-", "meN+per-/-kan", "meN+per-/-i", 
                                                   "meN-/-in", "meN+ber-/-kan", "ter-", 
                                                   "ter-/-kan", "ter-/-i", "ter-/-an")))

##### 4.1 token frequency ==========
token_plots <- year_prods %>% 
  filter(focused, freq_profile == "tokens") %>%
  mutate(year = as.numeric(year), 
         dbase = factor(dbase, levels = c("me", "di", "ber", "ter"))) %>% 
  left_join(focused_affix_colour) %>% 
  split(.$dbase) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values)) + # `values` = RAW count
      ~ggplot(., aes(x = year, y = pmw,
                     fill = affix_morphind_colour,
                     colour = affix_morphind_colour,
                     group = affix_morphind_colour)) + 
      geom_line() + 
      geom_point(size = 2, shape = 22, alpha = .7) +
      theme_light() +
      labs(x = NULL, colour = NULL, y = NULL, fill = paste(toupper(.$dbase), "-", sep = "")) + 
      geom_text_repel(data = filter(., year == '2020'), 
                      aes(label = affix_morphind_colour), 
                      direction = "y", 
                      hjust = 0, 
                      segment.size = .7, 
                      segment.alpha = .5, 
                      segment.linetype = "dotted", 
                      box.padding = .4, 
                      xlim = c(2020.8, NA), 
                      segment.curvature = -0.1, 
                      segment.ncp = 3, 
                      segment.angle = 20, 
                      size = 2, fontface = 'bold') + 
      guides(colour = "none") + 
      scale_x_continuous(expand = c(0, 0),
                         limits = c(2010, 2022),
                         breaks = seq(2011, 2020, by = 1)) +
      theme(axis.title.y = element_text(size = 8),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.text = element_text(size = 8)))
token_plots1 <- grid.arrange(grobs = token_plots, ncol = 2, nrow = 2)
ggsave(plot = token_plots1, 
       filename = "figures/12-sub-pattern-tokens-by-year.png", 
       width = 11.5, height = 6, 
       dpi = 300, units = "in")

###### 4.1.1 correlation test for token frequency of some afix subtypes ==========
men <- filter(year_prods, affix_morphind == "meN-_0", freq_profile == "tokens")
cor.test(as.numeric(men$year), men$pmw, method = "kendall")
di <- filter(year_prods, affix_morphind == "di-_0", freq_profile == "tokens")
cor.test(as.numeric(di$year), di$pmw, method = "kendall")
men_kan <- filter(year_prods, affix_morphind == "meN-_kan", freq_profile == "tokens")
cor.test(as.numeric(men_kan$year), men_kan$pmw, method = "kendall")
di_kan <- filter(year_prods, affix_morphind == "di-_kan", freq_profile == "tokens")
cor.test(as.numeric(di_kan$year), di_kan$pmw, method = "kendall")
men_i <- filter(year_prods, affix_morphind == "meN-_i", freq_profile == "tokens")
cor.test(as.numeric(men_i$year), men_i$pmw, method = "kendall")
di_i <- filter(year_prods, affix_morphind == "di-_i", freq_profile == "tokens")
cor.test(as.numeric(di_i$year), di_i$pmw, method = "kendall")
ber <- filter(year_prods, affix_morphind == "ber-_0", freq_profile == "tokens")
cor.test(as.numeric(ber$year), ber$pmw, method = "kendall")
ter <- filter(year_prods, affix_morphind == "ter-_0", freq_profile == "tokens")
cor.test(as.numeric(ter$year), ter$pmw, method = "kendall")


##### 4.2 type frequency ==========
type_plots <- year_prods %>% 
  filter(focused, freq_profile == "types_root") %>%
  mutate(year = as.numeric(year), 
         dbase = factor(dbase, levels = c("me", "di", "ber", "ter"))) %>% 
  left_join(focused_affix_colour) %>% 
  split(.$dbase) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values)) + # `values` = RAW count
      ~ggplot(., aes(x = year, y = pmw,
                     fill = affix_morphind_colour,
                     colour = affix_morphind_colour,
                     group = affix_morphind_colour)) + 
      geom_line() + 
      geom_point(size = 2, shape = 22, alpha = .7) +
      theme_light() +
      labs(x = NULL, colour = NULL, y = NULL, fill = paste(toupper(.$dbase), "-", sep = "")) + 
      geom_text_repel(data = filter(., year == '2020'), 
                      aes(label = affix_morphind_colour), 
                      direction = "y", 
                      hjust = 0, 
                      segment.size = .7, 
                      segment.alpha = .5, 
                      segment.linetype = "dotted", 
                      box.padding = .4, 
                      xlim = c(2020.8, NA), 
                      segment.curvature = -0.1, 
                      segment.ncp = 3, 
                      segment.angle = 20, 
                      size = 2, 
                      fontface = 'bold') + 
      guides(colour = "none") + 
      scale_x_continuous(expand = c(0, 0),
                         limits = c(2010, 2022),
                         breaks = seq(2011, 2020, by = 1)) +
      theme(axis.title.y = element_text(size = 8),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.text = element_text(size = 8)))
type_plots1 <- grid.arrange(grobs = type_plots, ncol = 2, nrow = 2)
ggsave(plot = type_plots1, 
       filename = "figures/13-sub-pattern-types-by-year.png", 
       width = 11.5, height = 6, 
       dpi = 300, units = "in")

###### 4.2.1 correlation test for relative type frequency of some subtypes ==========
men <- filter(year_prods, affix_morphind == "meN-_0", freq_profile == "types_root")
cor.test(as.numeric(men$year), men$pmw, method = "kendall")
cor.test(as.numeric(men$year), men$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(men$year), men$pmw, method = "kendall")$estimate < -0.5
di <- filter(year_prods, affix_morphind == "di-_0", freq_profile == "types_root")
cor.test(as.numeric(di$year), di$pmw, method = "kendall")
cor.test(as.numeric(di$year), di$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(di$year), di$pmw, method = "kendall")$estimate < -0.5
men_kan <- filter(year_prods, affix_morphind == "meN-_kan", freq_profile == "types_root")
cor.test(as.numeric(men_kan$year), men_kan$pmw, method = "kendall")
cor.test(as.numeric(men_kan$year), men_kan$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(men_kan$year), men_kan$pmw, method = "kendall")$estimate < -0.5
di_kan <- filter(year_prods, affix_morphind == "di-_kan", freq_profile == "types_root")
cor.test(as.numeric(di_kan$year), di_kan$pmw, method = "kendall")
cor.test(as.numeric(di_kan$year), di_kan$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(di_kan$year), di_kan$pmw, method = "kendall")$estimate < -0.5
men_i <- filter(year_prods, affix_morphind == "meN-_i", freq_profile == "types_root")
cor.test(as.numeric(men_i$year), men_i$pmw, method = "kendall")
cor.test(as.numeric(men_i$year), men_i$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(men_i$year), men_i$pmw, method = "kendall")$estimate < -0.5
di_i <- filter(year_prods, affix_morphind == "di-_i", freq_profile == "types_root")
cor.test(as.numeric(di_i$year), di_i$pmw, method = "kendall")
cor.test(as.numeric(di_i$year), di_i$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(di_i$year), di_i$pmw, method = "kendall")$estimate < -0.5
ber <- filter(year_prods, affix_morphind == "ber-_0", freq_profile == "types_root")
cor.test(as.numeric(ber$year), ber$pmw, method = "kendall")
cor.test(as.numeric(ber$year), ber$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(ber$year), ber$pmw, method = "kendall")$estimate < -0.5
ter <- filter(year_prods, affix_morphind == "ter-_0", freq_profile == "types_root")
cor.test(as.numeric(ter$year), ter$pmw, method = "kendall")
cor.test(as.numeric(ter$year), ter$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(ter$year), ter$pmw, method = "kendall")$estimate < -0.5

##### 4.3 hapax frequency ==========
hapax_plots <- year_prods %>% 
  filter(focused, freq_profile == "hapaxes_root") %>%
  mutate(year = as.numeric(year), 
         dbase = factor(dbase, levels = c("me", "di", "ber", "ter"))) %>% 
  left_join(focused_affix_colour) %>% 
  split(.$dbase) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values)) + # `values` = RAW count
      ~ggplot(., aes(x = year, y = pmw, 
                     fill = affix_morphind_colour, 
                     colour = affix_morphind_colour, 
                     group = affix_morphind_colour)) + 
      geom_line() + 
      geom_point(size = 2, shape = 22, alpha = .7) +
      theme_light() +
      labs(x = NULL, colour = NULL, y = NULL, fill = paste(toupper(.$dbase), "-", sep = "")) + 
      geom_text_repel(data = filter(., year == '2020'), 
                      aes(label = affix_morphind_colour), 
                      direction = "y", 
                      hjust = 0, 
                      segment.size = .7, 
                      segment.alpha = .5, 
                      segment.linetype = "dotted", 
                      box.padding = .4, 
                      xlim = c(2020.8, NA), 
                      segment.curvature = -0.1, 
                      segment.ncp = 3, 
                      segment.angle = 20, 
                      size = 2, 
                      fontface = 'bold') + 
        guides(colour = "none") + 
        scale_x_continuous(expand = c(0, 0),
                           limits = c(2010, 2022),
                           breaks = seq(2011, 2020, by = 1)) +
        theme(axis.title.y = element_text(size = 8),
              axis.text.y = element_text(size = 7),
              axis.text.x = element_text(size = 7),
              legend.text = element_text(size = 8)))
hapax_plots1 <- grid.arrange(grobs = hapax_plots, ncol = 2, nrow = 2)
ggsave(plot = hapax_plots1, 
       filename = "figures/14-sub-pattern-hapax-by-year.png", 
       width = 11.5, height = 6, 
       dpi = 300, units = "in")

###### 4.3.1 correlation test for relative hapax of some subtypes ==========
men <- filter(year_prods, affix_morphind == "meN-_0", freq_profile == "hapaxes_root")
cor.test(as.numeric(men$year), men$pmw, method = "kendall")
cor.test(as.numeric(men$year), men$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(men$year), men$pmw, method = "kendall")$estimate < -0.5
di <- filter(year_prods, affix_morphind == "di-_0", freq_profile == "hapaxes_root")
cor.test(as.numeric(di$year), di$pmw, method = "kendall")
cor.test(as.numeric(di$year), di$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(di$year), di$pmw, method = "kendall")$estimate < -0.5
men_kan <- filter(year_prods, affix_morphind == "meN-_kan", freq_profile == "hapaxes_root")
cor.test(as.numeric(men_kan$year), men_kan$pmw, method = "kendall")
cor.test(as.numeric(men_kan$year), men_kan$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(men_kan$year), men_kan$pmw, method = "kendall")$estimate < -0.5
di_kan <- filter(year_prods, affix_morphind == "di-_kan", freq_profile == "hapaxes_root")
cor.test(as.numeric(di_kan$year), di_kan$pmw, method = "kendall")
cor.test(as.numeric(di_kan$year), di_kan$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(di_kan$year), di_kan$pmw, method = "kendall")$estimate < -0.5
men_i <- filter(year_prods, affix_morphind == "meN-_i", freq_profile == "hapaxes_root")
cor.test(as.numeric(men_i$year), men_i$pmw, method = "kendall")
cor.test(as.numeric(men_i$year), men_i$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(men_i$year), men_i$pmw, method = "kendall")$estimate < -0.5
di_i <- filter(year_prods, affix_morphind == "di-_i", freq_profile == "hapaxes_root")
cor.test(as.numeric(di_i$year), di_i$pmw, method = "kendall")
cor.test(as.numeric(di_i$year), di_i$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(di_i$year), di_i$pmw, method = "kendall")$estimate < -0.5
ber <- filter(year_prods, affix_morphind == "ber-_0", freq_profile == "hapaxes_root")
cor.test(as.numeric(ber$year), ber$pmw, method = "kendall")
cor.test(as.numeric(ber$year), ber$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(ber$year), ber$pmw, method = "kendall")$estimate < -0.5
ter <- filter(year_prods, affix_morphind == "ter-_0", freq_profile == "hapaxes_root")
cor.test(as.numeric(ter$year), ter$pmw, method = "kendall")
cor.test(as.numeric(ter$year), ter$pmw, method = "kendall")$p.value < 0.05
cor.test(as.numeric(ter$year), ter$pmw, method = "kendall")$estimate < -0.5

### 5. types, tokens, and hapax for each focused affix sub-patterns by GENRE ======
genre_prods <- verbs %>% 
  group_by(dbase, affix_morphind, genres) %>% 
  summarise(types_root = n_distinct(root_morphind),
            types_wordform = n_distinct(word_form),
            tokens = sum(n),
            .groups = "drop") %>% 
  mutate(perc_tokens = round(tokens/sum(tokens) * 100, 2),
         perc_type_root = round(types_root/sum(types_root) * 100, 2),
         perc_type_wordform = round(types_wordform/sum(types_wordform) * 100, 2)) %>% 
  arrange(dbase, desc(types_root)) %>% 
  # join the hapax analysis by root
  left_join(verbs %>%
              group_by(dbase, affix_morphind, root_morphind, genres) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(affix_morphind, genres) %>% 
              summarise(hapaxes_root = sum(is_hapax)) %>% 
              arrange(desc(hapaxes_root))) %>% 
  # join the hapax analysis by word_form
  left_join(verbs %>% 
              group_by(dbase, affix_morphind, word_form, genres) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(affix_morphind, genres) %>% 
              summarise(hapaxes_wordform = sum(is_hapax)) %>% 
              arrange(desc(hapaxes_wordform))) %>% 
  arrange(dbase, desc(hapaxes_root), desc(types_root)) %>% 
  mutate(htr_root = round(hapaxes_root/tokens, 6),
         htr_wordform = round(hapaxes_wordform/tokens, 6)) %>% 
  group_by(dbase) %>% 
  mutate(focused = if_else(affix_morphind %in% focused_affixes, TRUE, FALSE)) %>% 
  ungroup() %>% 
  arrange(dbase, genres) %>% 
  pivot_longer(cols = -c(dbase, affix_morphind, focused, genres), 
               names_to = "freq_profile", values_to = "values") %>% 
  left_join(genressizes, by = 'genres') %>% 
  mutate(pmw = 0,
         pmw = if_else(str_detect(freq_profile, "^(perc|htr)", negate = TRUE), 
                       ((values/genre_size) * 1000000), 
                       pmw))

#### 5.x visualisation for each affix patterns
focused_affix_colour <- focused_affixes %>% 
  factor(levels = focused_affixes) %>% 
  tibble(affix_morphind_colour = ., affix_morphind = .) %>% 
  mutate(affix_morphind_colour = str_replace_all(affix_morphind_colour, "_0", ""),
         affix_morphind_colour = str_replace_all(affix_morphind_colour, "_([a-z])", "/-\\1"),
         affix_morphind_colour = str_replace_all(affix_morphind_colour, "-(\\+)", "\\1"),
         affix_morphind_colour = factor(affix_morphind_colour, 
                                        levels = c("ber-", "ber-/-an", "ber-/-kan", 
                                                   "ber+ke-/-an", "ber+peN-", 
                                                   "ber+peN-/-an", "ber+si-", "ber+per-/-an", "ber-/-i", 
                                                   "di-", "di-/-kan", "di-/-i", 
                                                   "di+per-", "di+per-/-kan", "di+per-/-i", 
                                                   "di-/-in", "meN-", "meN-/-kan", 
                                                   "meN-/-i", "meN+per-", "meN+per-/-kan", 
                                                   "meN+per-/-i", "meN-/-in", "meN+ber-/-kan", 
                                                   "ter-", "ter-/-kan", "ter-/-i", "ter-/-an")))

##### 5.1 token frequency ==========
token_plots <- genre_prods %>% 
  filter(focused, freq_profile == "tokens") %>% 
  mutate(dbase = factor(dbase, levels = c("me", "di", "ber", "ter"))) %>% 
  left_join(focused_affix_colour) %>% 
  split(.$dbase) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values)) + 
    ~ggplot(., aes(x = genres, y = pmw, fill = affix_morphind_colour)) + 
      geom_col() + 
      theme_light() +
      labs(x = NULL, 
           fill = paste(toupper(.$dbase), "-", sep = ""), 
           y = "Kekerapan relatif\n(per 1 juta kemunculan kata)") +
      theme(axis.title.x = element_text(size = 8),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.text = element_text(size = 8)) +
      coord_flip() + 
      scale_fill_brewer(palette = "Spectral", direction = -1))
token_plots1 <- grid.arrange(grobs = token_plots, ncol = 2, nrow = 2)
ggsave(plot = token_plots1, 
       filename = "figures/9-sub-pattern-tokens-by-genre.png", 
       width = 11, height = 6, 
       dpi = 300, units = "in")

##### 5.2 type frequency ==========
type_plots <- genre_prods %>% 
  filter(focused, freq_profile == "types_root") %>% 
  mutate(dbase = factor(dbase, levels = c("me", "di", "ber", "ter"))) %>% 
  left_join(focused_affix_colour) %>% 
  split(.$dbase) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values)) + 
    ~ggplot(., aes(x = genres, y = pmw, fill = affix_morphind_colour)) + 
      geom_col() + 
      theme_light() +
      labs(x = NULL, 
           fill = paste(toupper(.$dbase), "-", sep = ""), 
           y = "Jumlah bentukan kata\n(relatif per 1 juta kata)") +
      theme(axis.title.x = element_text(size = 8),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.text = element_text(size = 8)) +
      coord_flip() +
      scale_fill_brewer(palette = "Spectral", direction = -1))
type_plots1 <- grid.arrange(grobs = type_plots, ncol = 2, nrow = 2)
ggsave(plot = type_plots1, 
       filename = "figures/10-sub-pattern-types-by-genre.png", 
       width = 11, height = 6, 
       dpi = 300, units = "in")

##### 5.3 hapax frequency ==========
hapax_plots <- genre_prods %>% 
  filter(focused, freq_profile == "hapaxes_root") %>% 
  mutate(dbase = factor(dbase, levels = c("me", "di", "ber", "ter"))) %>% 
  left_join(focused_affix_colour) %>% 
  split(.$dbase) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values)) + 
    ~ggplot(., aes(x = genres, y = pmw, fill = affix_morphind_colour)) + 
      geom_col() + 
      theme_light() +
      labs(x = NULL, 
           fill = paste(toupper(.$dbase), "-", sep = ""), 
           y = "Jumlah bentukan kata berkekerapan 1 (hapax)\n(relatif per 1 juta kata)") +
      theme(axis.title.x = element_text(size = 8),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.text = element_text(size = 8)) +
      coord_flip() +
      scale_fill_brewer(palette = "Spectral", direction = -1))
hapax_plots1 <- grid.arrange(grobs = hapax_plots, ncol = 2, nrow = 2)
ggsave(plot = hapax_plots1, 
       filename = "figures/11-sub-pattern-hapax-by-genre.png", 
       width = 11, height = 6, 
       dpi = 300, units = "in")


### 6. number of tokens, types, and hapaxes for me, di, ter, and ber verbs (prefix only OVERALL) BY GENRES ======
prefix_overall_genres <- verbs %>% 
  group_by(dbase, genres) %>% 
  summarise(types_root = n_distinct(root_morphind),
            types_wordform = n_distinct(word_form),
            tokens = sum(n),
            .groups = "drop") %>% 
  mutate(perc_tokens = round(tokens/sum(tokens) * 100, 2),
         perc_type_root = round(types_root/sum(types_root) * 100, 2),
         perc_type_wordform = round(types_wordform/sum(types_wordform) * 100, 2)) %>% 
  arrange(dbase, desc(types_root)) %>% 
  # join the hapax analysis by root
  left_join(verbs %>% 
              group_by(dbase, affix_morphind, root_morphind, genres) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(dbase, genres) %>% 
              summarise(hapaxes_root = sum(is_hapax)) %>% 
              arrange(desc(hapaxes_root))) %>% 
  # join the hapax analysis by word_form
  left_join(verbs %>% 
              group_by(dbase, affix_morphind, word_form, genres) %>% 
              summarise(n = sum(n), .groups = 'drop') %>% 
              mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) %>% 
              group_by(dbase, genres) %>% 
              summarise(hapaxes_wordform = sum(is_hapax)) %>% 
              arrange(desc(hapaxes_wordform))) %>% 
  arrange(dbase, desc(hapaxes_root), desc(types_root)) %>% 
  mutate(htr_root = round(hapaxes_root/tokens, 6),
         htr_wordform = round(hapaxes_wordform/tokens, 6)) %>% 
  group_by(dbase) %>% 
  ungroup() %>% 
  arrange(dbase, genres) %>% 
  pivot_longer(cols = -c(dbase, genres), 
               names_to = "freq_profile", 
               values_to = "values") %>% 
  left_join(genressizes, by = 'genres') %>% 
  mutate(pmw = 0,
         pmw = if_else(str_detect(freq_profile, "^(perc|htr)", negate = TRUE), 
                       ((values/genre_size) * 1000000), 
                       pmw))

#### 6.1 visualisation========
toks_fig <- prefix_overall_genres %>% 
  filter(freq_profile == "tokens") %>% 
  mutate(dbase = fct_reorder(paste(toupper(dbase), "-", sep =""), pmw)) %>%
  ggplot(aes(x = genres, y = pmw, fill = dbase)) +
  # ggplot(aes(x = dbase, y = values, fill = dbase)) +
  geom_col(position = 'dodge') +
  labs(x = "", 
       title = "Kekerapan/kemunculan relatif\n(frekuensi-token relatif per 1 juta kata)", 
       y = "", 
       fill = "awalan") +
  theme_light() +
  coord_flip() +
  theme(legend.position = "none",
        plot.title = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.35),
        axis.text.x = element_text(size = 6)) +
  scale_fill_brewer(type = "qual", palette = "Set2", limits = c("ME-", "DI-", "BER-", "TER-"))

types_fig <- prefix_overall_genres %>% 
  filter(freq_profile == "types_root") %>% 
  mutate(dbase = fct_reorder(paste(toupper(dbase), "-", sep =""), pmw)) %>%
  ggplot(aes(x = genres, y = pmw, fill = dbase)) +
  # ggplot(aes(x = dbase, y = values, fill = dbase)) +
  geom_col(position = 'dodge') +
  labs(x = "", 
       title = "Jumlah bentukan kata relatif\n(frekuensi-tipe relatif per 1 juta kata)", 
       y = "", 
       fill = "awalan") +
  theme_light() +
  coord_flip() +
  theme(legend.position = "none",
        plot.title = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.35),
        axis.text.x = element_text(size = 6)) +
  scale_fill_brewer(type = "qual", palette = "Set2", limits = c("ME-", "DI-", "BER-", "TER-"))

hapax_fig <- prefix_overall_genres %>% 
  filter(freq_profile == "hapaxes_root") %>% 
  mutate(dbase = fct_reorder(paste(toupper(dbase), "-", sep =""), pmw)) %>%
  ggplot(aes(x = genres, y = pmw, fill = dbase)) +
  # ggplot(aes(x = dbase, y = values, fill = dbase)) +
  geom_col(position = 'dodge') +
  labs(x = "", 
       title = "Jumlah bentukan kata berkekerapan 1\n(hapax per 1 juta kata)", 
       y = "", 
       fill = "") +
  theme_light() +
  coord_flip() +
  theme(legend.position = "right",
        plot.title = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.35),
        axis.text.x = element_text(size = 6)) +
  scale_fill_brewer(type = "qual", palette = "Set2", limits = c("ME-", "DI-", "BER-", "TER-"))
prefix_overall_plot_genres <- grid.arrange(toks_fig, types_fig, hapax_fig, nrow = 1, ncol = 3)
ggsave(plot = prefix_overall_plot_genres, 
       filename = "figures/8-prefix-overall-freq-profiles-genres.png", 
       width = 11.5, height = 4.5, 
       dpi = 300, units = "in")

### 7. Word class preference OVERALL CORPUS =======
#### 7.1 TYPES for each affix sub-patterns =====
affix_wclass_types <- verbs %>% 
  group_by(dbase, affix_morphind, affix_morphind_wclass) %>% 
  summarise(types_root = n_distinct(root_morphind), .groups = "drop") %>% 
  arrange(dbase, affix_morphind, desc(types_root)) %>% 
  group_by(dbase, affix_morphind) %>% 
  mutate(total_types_by_affix = sum(types_root), perc_types = round((types_root/total_types_by_affix) * 100, 2))
# affix_wclass_types %>% write_excel_csv2(file = "2_affix_wclass_types_stats.txt")

#### 7.2 TOKENS for each affix sub-patterns =====
affix_wclass_tokens <- verbs %>% 
  group_by(dbase, affix_morphind, affix_morphind_wclass) %>% 
  summarise(tokens = sum(n), .groups = "drop") %>% 
  arrange(dbase, affix_morphind, desc(tokens)) %>% 
  group_by(dbase, affix_morphind) %>% 
  mutate(total_tokens_by_affix = sum(tokens), perc_tokens = round((tokens/total_tokens_by_affix) * 100, 2))
# affix_wclass_tokens %>% write_excel_csv2(file = "1_affix_wclass_tokens_stats.txt")

#### 7.3 HAPAX for each affix sub-patterns =====
affix_wclass_hapax <- verbs %>% 
  group_by(dbase, affix_morphind, affix_morphind_wclass, root_morphind) %>% 
  summarise(n=sum(n), .groups = "drop") %>% 
  mutate(hapax = if_else(n == 1, TRUE, FALSE)) %>% 
  group_by(dbase, affix_morphind, affix_morphind_wclass) %>% 
  summarise(n_hapax = sum(hapax), .groups = "drop") %>% 
  arrange(dbase, affix_morphind, desc(n_hapax)) %>% 
  group_by(dbase, affix_morphind) %>% 
  mutate(total_hapax_by_affix = sum(n_hapax), perc_hapax = round((n_hapax/total_hapax_by_affix) * 100, 2))
# affix_wclass_hapax %>% write_excel_csv2(file = "3_affix_wclass_hapax_stats.txt")

#### 7.4 TYPES, TOKENS, and HAPAX for each affix sub-patterns =====
affix_wclass_prods <- affix_wclass_tokens %>% 
  select(-perc_tokens) %>% 
  left_join(affix_wclass_types %>% select(-perc_types)) %>% 
  left_join(affix_wclass_hapax %>% select(-perc_hapax)) %>% 
  mutate(htr = n_hapax/tokens) %>% 
  mutate(corpussize = corpussizes_all,
         tokens_pmw = (tokens/corpussize) * 1000000,
         types_root_pmw = (types_root/corpussize) * 1000000,
         hapax_pmw = (n_hapax/corpussize) * 1000000) %>% 
  arrange(dbase, affix_morphind, desc(types_root_pmw)) %>% 
  select(-corpussize)

#### 7.5 For each affix sub-pattern + wordclass group, select the one(s) with types per mill. over 5
affix_wclass_prods %>% 
  group_by(dbase, affix_morphind) %>% 
  filter(types_root_pmw > 5) %>% 
  select(-htr) %>% 
  arrange(dbase, affix_morphind, desc(n_hapax))

# > brewer.pal(n=11, "BrBG")
# [1] "#543005" "#8C510A" "#BF812D" "#DFC27D" "#F6E8C3" "#F5F5F5" "#C7EAE5" "#80CDC1"
# [9] "#35978F" "#01665E" "#003C30"

wclass_colvals <- data.frame(affix_morphind_wclass = factor(c("n", "v", "a", "f", "pre-cat"), 
                                                            levels = c("n", "v", "a", "f", "pre-cat")), 
                             colvals = c("#543005", "#BF812D", "#DFC27D", "#80CDC1", "#35978F"))

##### 7.5.1 visualisation: wordclass attached to meN-X (type per mill. > 5) ======
dfs <- affix_wclass_prods %>% 
  filter(affix_morphind %in% c("meN-_0", "di-_0", "ber-_0", "ter-_0")) %>% 
  ungroup() %>% 
  select(matches("pmw|affix_")) %>% 
  group_by(affix_morphind) %>% 
  arrange(desc(types_root_pmw)) %>% 
  mutate(affix_morphind_wclass = str_replace_all(affix_morphind_wclass, "(_0$|^(meN|di|ber|ter)\\-_)", ""),
         affix_morphind = str_replace(affix_morphind, "_0", ""),
         affix_morphind = factor(affix_morphind, levels = c("meN-", "di-", "ber-", "ter-"))) %>% 
  filter(types_root_pmw > 1) %>% 
  mutate(affix_morphind_wclass = factor(affix_morphind_wclass, 
                                        levels = c("n", "v", "a", "f", "pre-cat"))) %>% 
  arrange(affix_morphind, affix_morphind_wclass) %>% 
  left_join(wclass_colvals)

dfs <- dfs %>%
  pivot_longer(cols = -c(affix_morphind_wclass, affix_morphind, colvals), 
               names_to = "freq_profile", 
               values_to = "vals")

p <- dfs %>% 
  mutate(freq_profile = factor(freq_profile, 
                               levels = c("tokens_pmw", "types_root_pmw", "hapax_pmw")),
         freq_labels = "Frekuensi token relatif (per 1 juta kata)",
         freq_labels = if_else(freq_profile == "types_root_pmw", 
                               "Frekuensi tipe relatif (per 1 juta kata)", 
                               freq_labels),
         freq_labels = if_else(freq_profile == "hapax_pmw", 
                               "Hapax relatif (per 1 juta kata)", 
                               freq_labels)) %>% 
  split(.$freq_profile) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values, fill = colvals)) + 
    ~ggplot(., aes(x = affix_morphind, y = vals, fill = affix_morphind_wclass)) + 
      geom_col(position = "dodge") + 
      theme_light() +
      scale_fill_manual(values = .$colvals) +
      labs(x=str_replace(.$freq_profile, "(_root)?_pmw", ""), 
           y = .$freq_labels, 
           caption = "n=nomina; v=verba; a=ajektiva\nf=asing; pre-cat=tanpa kategori", 
           fill = "") +
      theme(axis.title.y = element_text(size = 7.5),
            axis.title.x = element_text(size = 7.5),
            axis.text.y = element_text(size = 6),
            axis.text.x = element_text(size = 6.5),
            plot.caption = element_text(size = 6),
            legend.text = element_text(size = 6.25),
            legend.key.size = unit(.4, 'cm')))
p <- grid.arrange(grobs = p, ncol = 3, nrow = 1)
ggsave(plot = p, 
       filename = "figures/5-wclass-dist-me-di-ber-ter.png", 
       width = 8.5, height = 2.85, 
       dpi = 300, units = "in")

###### statistical comparison in the body text
base_dfs <- affix_wclass_prods %>% 
  filter(affix_morphind %in% c("meN-_0", "di-_0", "ber-_0", "ter-_0")) %>% 
  ungroup() %>% 
  filter(types_root_pmw > 1) %>% 
  select(matches("(^affix|tokens$|types_root$|n_hapax$)")) %>% 
  group_by(affix_morphind) %>% 
  arrange(desc(types_root)) %>% 
  mutate(affix_morphind_wclass = str_replace_all(affix_morphind_wclass, "(_0$|^(meN|di|ber|ter)\\-_)", ""),
         affix_morphind = str_replace(affix_morphind, "_0", ""),
         affix_morphind = factor(affix_morphind, levels = c("meN-", "di-", "ber-", "ter-"))) %>% 
  mutate(affix_morphind_wclass = factor(affix_morphind_wclass, 
                                        levels = c("n", "v", "a", "f", "pre-cat"))) %>% 
  arrange(affix_morphind, affix_morphind_wclass)
####### comparing token frequency of wordclasses for base
base_tokens <- base_dfs %>% 
  select(-types_root, -n_hapax) %>% 
  pivot_wider(names_from = affix_morphind, values_from = tokens, values_fill = 0)
names(base_tokens[[2]]) <- base_tokens[[1]]
names(base_tokens[[3]]) <- base_tokens[[1]]
names(base_tokens[[4]]) <- base_tokens[[1]]
names(base_tokens[[5]]) <- base_tokens[[1]]
chisq.test(base_tokens$`meN-`)
chisq.test(base_tokens$`meN-`)$stdres
chisq.test(base_tokens$`di-`)
chisq.test(base_tokens$`di-`)$stdres
chisq.test(base_tokens$`ber-`)
chisq.test(base_tokens$`ber-`)$stdres
chisq.test(base_tokens$`ter-`)
chisq.test(base_tokens$`ter-`)$stdres

####### comparing type frequency of wordclasses for base
base_types <- base_dfs %>% 
  select(-tokens, -n_hapax) %>% 
  pivot_wider(names_from = affix_morphind, values_from = types_root, values_fill = 0)
names(base_types[[2]]) <- base_types[[1]]
names(base_types[[3]]) <- base_types[[1]]
names(base_types[[4]]) <- base_types[[1]]
names(base_types[[5]]) <- base_types[[1]]
chisq.test(base_types$`meN-`)
chisq.test(base_types$`meN-`)$stdres
chisq.test(base_types$`di-`)
chisq.test(base_types$`di-`)$stdres
chisq.test(base_types$`ber-`)
chisq.test(base_types$`ber-`)$stdres
chisq.test(base_types$`ter-`)
chisq.test(base_types$`ter-`)$stdres

####### comparing type frequency of wordclasses for base
base_hapax <- base_dfs %>% 
  select(-tokens, -types_root) %>% 
  pivot_wider(names_from = affix_morphind, values_from = n_hapax, values_fill = 0)
names(base_hapax[[2]]) <- base_hapax[[1]]
names(base_hapax[[3]]) <- base_hapax[[1]]
names(base_hapax[[4]]) <- base_hapax[[1]]
names(base_hapax[[5]]) <- base_hapax[[1]]
chisq.test(base_hapax$`meN-`)
chisq.test(base_hapax$`meN-`)$stdres
chisq.test(base_hapax$`di-`)
chisq.test(base_hapax$`di-`)$stdres
chisq.test(base_hapax$`ber-`)
chisq.test(base_hapax$`ber-`)$stdres
chisq.test(base_hapax$`ter-`)
chisq.test(base_hapax$`ter-`)$stdres

##### 7.5.2 visualisation: wordclass attached to me/di/ter-X-kan (type per mill. > 5) ======
wclass_with_kan <- verbs %>% 
  filter(pref_morphind %in% c("ter-", "di-", "meN-", "ber-"), 
         suff_morphind == "kan") %>% 
  group_by(affix_morphind, root_morphind, root_pos_morphind, suff_morphind) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  mutate(is_hapax = if_else(n == 1, TRUE, FALSE),
         affix_morphind = str_replace(affix_morphind, "\\-_", "-/-"))

wclass_with_kan1 <- wclass_with_kan %>% 
  group_by(affix_morphind, root_pos_morphind) %>% 
  summarise(tokens = sum(n),
            types = n_distinct(root_morphind),
            hapaxes = sum(is_hapax),
            .groups = "drop") %>% 
  mutate(corpussize = corpussizes_all,
         tokens_pmw = (tokens/corpussize) * 1000000,
         types_pmw = (types/corpussize) * 1000000,
         hapax_pmw = (hapaxes/corpussize) * 1000000,
         affix_morphind = factor(affix_morphind, 
                                 levels = c("meN-/-kan", "di-/-kan", "ber-/-kan", "ter-/-kan"))) %>% 
  arrange(affix_morphind, desc(types_pmw)) %>% 
  select(-corpussize)

wclass_with_kan1 <- wclass_with_kan1 %>% 
  mutate(root_pos_morphind = factor(root_pos_morphind),
         root_pos_morphind = fct_reorder(root_pos_morphind, -types_pmw)) %>% 
  filter(types_pmw > 1) %>% # only plot affix with types pmw > 1
  left_join(wclass_colvals %>% 
              rename(root_pos_morphind = affix_morphind_wclass)) %>% 
  pivot_longer(cols = -c(root_pos_morphind, affix_morphind, colvals), 
               names_to = "freq_profile", values_to = "vals") %>% 
  mutate(freq_profile = factor(freq_profile, 
                               levels = c("tokens_pmw", "types_pmw", "hapax_pmw", 
                                          "tokens", "types", "hapaxes")))

p <- wclass_with_kan1 %>% 
  filter(!freq_profile %in% c("tokens", "types", "hapaxes")) %>% 
  droplevels() %>% 
  mutate(freq_labels = "Frekuensi token relatif (per 1 juta kata)",
         freq_labels = if_else(freq_profile == "types_pmw", 
                               "Frekuensi tipe relatif (per 1 juta kata)", 
                               freq_labels),
         freq_labels = if_else(freq_profile == "hapax_pmw", 
                               "Hapax relatif (per 1 juta kata)", 
                               freq_labels)) %>% 
  split(.$freq_profile) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values, fill = colvals)) + 
     ~ggplot(., aes(x = affix_morphind, y = vals, fill = root_pos_morphind)) + 
      geom_col(position = "dodge") + 
      theme_light() +
      scale_fill_manual(values = .$colvals) +
      labs(x=str_replace(.$freq_profile, "_pmw", ""), 
           y = .$freq_labels, 
           caption = "n=nomina; v=verba; a=ajektiva\nf=asing; pre-cat=tanpa kategori", 
           fill = "") +
      theme(axis.title.y = element_text(size = 7.5),
            axis.title.x = element_text(size = 7.5),
            axis.text.y = element_text(size = 6),
            axis.text.x = element_text(size = 5.5),
            plot.caption = element_text(size = 6),
            legend.text = element_text(size = 6.25),
            legend.key.size = unit(.4, 'cm')))
p <- grid.arrange(grobs = p, ncol = 3)
ggsave(plot = p, 
       filename = "figures/6-wclass-dist-kan.png", 
       width = 8.5, height = 2.85, 
       dpi = 300, units = "in")

###### statistical comparison in the body text
####### comparing type frequency of wordclasses for -kan
kan_tokens <- pivot_wider(select(filter(wclass_with_kan1, freq_profile=='tokens', str_detect(affix_morphind, "^[mdbt]")), -colvals, -freq_profile), names_from = affix_morphind, values_from = vals, values_fill = 0)
names(kan_tokens$`meN-/-kan`) <- kan_tokens$root_pos_morphind
names(kan_tokens$`di-/-kan`) <- kan_tokens$root_pos_morphind
names(kan_tokens$`ber-/-kan`) <- kan_tokens$root_pos_morphind
names(kan_tokens$`ter-/-kan`) <- kan_tokens$root_pos_morphind
chisq.test(kan_tokens$`meN-/-kan`)
chisq.test(kan_tokens$`meN-/-kan`)$stdres
chisq.test(kan_tokens$`di-/-kan`)
chisq.test(kan_tokens$`di-/-kan`)$stdres
chisq.test(kan_tokens$`ber-/-kan`)
chisq.test(kan_tokens$`ber-/-kan`)$stdres
chisq.test(kan_tokens$`ter-/-kan`)
chisq.test(kan_tokens$`ter-/-kan`)$stdres

####### comparing type frequency of wordclasses for -kan
kan_types <- pivot_wider(select(filter(wclass_with_kan1, freq_profile=='types', str_detect(affix_morphind, "^[mdbt]")), -colvals, -freq_profile), names_from = affix_morphind, values_from = vals, values_fill = 0)
names(kan_types$`meN-/-kan`) <- kan_types$root_pos_morphind
names(kan_types$`di-/-kan`) <- kan_types$root_pos_morphind
names(kan_types$`ber-/-kan`) <- kan_types$root_pos_morphind
names(kan_types$`ter-/-kan`) <- kan_types$root_pos_morphind
chisq.test(kan_types$`meN-/-kan`)
chisq.test(kan_types$`meN-/-kan`)$stdres
chisq.test(kan_types$`di-/-kan`)
chisq.test(kan_types$`di-/-kan`)$stdres
chisq.test(kan_types$`ber-/-kan`)
chisq.test(kan_types$`ber-/-kan`)$stdres
chisq.test(kan_types$`ter-/-kan`)
chisq.test(kan_types$`ter-/-kan`)$stdres

####### comparing hapax frequency of wordclasses for -kan
kan_hapax <- pivot_wider(select(filter(wclass_with_kan1, freq_profile=='hapaxes', str_detect(affix_morphind, "^[mdbt]")), -colvals, -freq_profile), names_from = affix_morphind, values_from = vals, values_fill = 0)
names(kan_hapax$`meN-/-kan`) <- kan_hapax$root_pos_morphind
names(kan_hapax$`di-/-kan`) <- kan_hapax$root_pos_morphind
names(kan_hapax$`ber-/-kan`) <- kan_hapax$root_pos_morphind
names(kan_hapax$`ter-/-kan`) <- kan_hapax$root_pos_morphind
chisq.test(kan_hapax$`meN-/-kan`)
chisq.test(kan_hapax$`meN-/-kan`)$stdres
chisq.test(kan_hapax$`di-/-kan`)
chisq.test(kan_hapax$`di-/-kan`)$stdres
chisq.test(kan_hapax$`ber-/-kan`)
chisq.test(kan_hapax$`ber-/-kan`)$stdres
chisq.test(kan_hapax$`ter-/-kan`)
chisq.test(kan_hapax$`ter-/-kan`)$stdres

##### 7.5.3 visualisation: wordclass attached to me/di/ter-X-i (type per mill. > 5) ======
wclass_with_i <- verbs %>% 
  filter(pref_morphind %in% c("ter-", "di-", "meN-", "ber-"), 
         suff_morphind == "i") %>% 
  group_by(affix_morphind, root_morphind, root_pos_morphind, suff_morphind) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  mutate(is_hapax = if_else(n == 1, TRUE, FALSE),
         affix_morphind = str_replace(affix_morphind, "\\-_", "-/-"))

wclass_with_i1 <- wclass_with_i %>% 
  group_by(affix_morphind, root_pos_morphind) %>% 
  summarise(tokens = sum(n),
            types = n_distinct(root_morphind),
            hapaxes = sum(is_hapax),
            .groups = "drop") %>% 
  mutate(corpussize = corpussizes_all,
         tokens_pmw = (tokens/corpussize) * 1000000,
         types_pmw = (types/corpussize) * 1000000,
         hapax_pmw = (hapaxes/corpussize) * 1000000,
         affix_morphind = factor(affix_morphind, 
                                 levels = c("meN-/-i", "di-/-i", "ber-/-i", "ter-/-i"))) %>% 
  arrange(affix_morphind, desc(types_pmw)) %>% 
  select(-corpussize)

wclass_with_i1 <- wclass_with_i1 %>% 
  mutate(root_pos_morphind = factor(root_pos_morphind),
         root_pos_morphind = fct_reorder(root_pos_morphind, -types_pmw)) %>% 
  filter(types_pmw > 1) %>% # only plot affix with types pmw > 1
  left_join(wclass_colvals %>% 
              rename(root_pos_morphind = affix_morphind_wclass)) %>% 
  pivot_longer(cols = -c(root_pos_morphind, affix_morphind, colvals), 
               names_to = "freq_profile", values_to = "vals") %>% 
  mutate(freq_profile = factor(freq_profile, 
                               levels = c("tokens_pmw", "types_pmw", "hapax_pmw", 
                                          "tokens", "types", "hapaxes")))

p <- wclass_with_i1 %>% 
  filter(!freq_profile %in% c("tokens", "types", "hapaxes")) %>% 
  droplevels() %>% 
  mutate(freq_labels = "Frekuensi token relatif (per 1 juta kata)",
         freq_labels = if_else(freq_profile == "types_pmw", 
                               "Frekuensi tipe relatif (per 1 juta kata)", 
                               freq_labels),
         freq_labels = if_else(freq_profile == "hapax_pmw", 
                               "Hapax relatif (per 1 juta kata)", 
                               freq_labels)) %>% 
  mutate(root_pos_morphind = fct_relevel(root_pos_morphind, "n", "v")) %>% 
  split(.$freq_profile) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values, fill = colvals)) + 
    ~ggplot(., aes(x = affix_morphind, y = vals, fill = root_pos_morphind)) + 
      geom_col(position = "dodge") + 
      theme_light() +
      scale_fill_manual(values = .$colvals) +
      labs(x=str_replace(.$freq_profile, "_pmw", ""), 
           y = .$freq_labels, 
           caption = "n=nomina; v=verba; a=ajektiva", 
           fill = "") +
      theme(axis.title.y = element_text(size = 7.5),
            axis.title.x = element_text(size = 7.5),
            axis.text.y = element_text(size = 6),
            axis.text.x = element_text(size = 6.35),
            plot.caption = element_text(size = 6),
            legend.text = element_text(size = 6),
            legend.key.size = unit(.4, 'cm')))
p <- grid.arrange(grobs = p, ncol = 3)
ggsave(plot = p, 
       filename = "figures/7-wclass-dist-i.png", 
       width = 8.5, height = 2.85, 
       dpi = 300, units = "in")

###### statistical comparison in the body text
####### comparing type frequency of wordclasses for -i
i_tokens <- pivot_wider(select(filter(wclass_with_i1, freq_profile=='tokens', str_detect(affix_morphind, "^[mdbt]")), -colvals, -freq_profile), names_from = affix_morphind, values_from = vals, values_fill = 0)
names(i_tokens$`meN-/-i`) <- i_tokens$root_pos_morphind
names(i_tokens$`di-/-i`) <- i_tokens$root_pos_morphind
names(i_tokens$`ter-/-i`) <- i_tokens$root_pos_morphind
chisq.test(i_tokens$`meN-/-i`)
chisq.test(i_tokens$`meN-/-i`)$stdres
chisq.test(i_tokens$`di-/-i`)
chisq.test(i_tokens$`di-/-i`)$stdres
chisq.test(i_tokens$`ter-/-i`)
chisq.test(i_tokens$`ter-/-i`)$stdres

####### comparing type frequency of wordclasses for -i
i_types <- pivot_wider(select(filter(wclass_with_i1, freq_profile=='types', str_detect(affix_morphind, "^[mdbt]")), -colvals, -freq_profile), names_from = affix_morphind, values_from = vals, values_fill = 0)
names(i_types$`meN-/-i`) <- i_types$root_pos_morphind
names(i_types$`di-/-i`) <- i_types$root_pos_morphind
names(i_types$`ter-/-i`) <- i_types$root_pos_morphind
chisq.test(i_types$`meN-/-i`)
chisq.test(i_types$`meN-/-i`)$stdres
chisq.test(i_types$`di-/-i`)
chisq.test(i_types$`di-/-i`)$stdres
chisq.test(i_types$`ter-/-i`)
chisq.test(i_types$`ter-/-i`)$stdres

####### comparing hapax frequency of wordclasses for -i
i_hapax <- pivot_wider(select(filter(wclass_with_i1, freq_profile=='hapaxes', str_detect(affix_morphind, "^[mdbt]")), -colvals, -freq_profile), names_from = affix_morphind, values_from = vals, values_fill = 0)
names(i_hapax$`meN-/-i`) <- i_hapax$root_pos_morphind
names(i_hapax$`di-/-i`) <- i_hapax$root_pos_morphind
names(i_hapax$`ber-/-i`) <- i_hapax$root_pos_morphind
names(i_hapax$`ter-/-i`) <- i_hapax$root_pos_morphind
chisq.test(i_hapax$`meN-/-i`)
chisq.test(i_hapax$`meN-/-i`)$stdres
chisq.test(i_hapax$`di-/-i`)
chisq.test(i_hapax$`di-/-i`)$stdres
chisq.test(i_hapax$`ber-/-i`)
chisq.test(i_hapax$`ber-/-i`)$stdres
chisq.test(i_hapax$`ter-/-i`)
chisq.test(i_hapax$`ter-/-i`)$stdres

##### 7.5.4 visualisation: wordclass attached to PER-X (regardless of voice, and using raw values) ======
wclass_colvals <- data.frame(affix_morphind_wclass = factor(c("n", "v", "a", "f", "r"), 
                                                            levels = c("n", "v", "a", "f", "r")), 
                             colvals = c("#543005", "#BF812D", "#DFC27D", "#80CDC1", "#35978F"))
per_tokens_by_root_wclass <- verbs %>% 
  filter(str_detect(affix_morphind, 'per-_0$')) %>% 
  group_by(root_pos_morphind, root_morphind) %>% 
  summarise(tokens = sum(n)) %>% 
  mutate(root_pos_morphind = factor(root_pos_morphind, 
                                    levels = c("n", "v", "a", "f", "r")))
per_tokens_by_class <- per_tokens_by_root_wclass %>% 
  tally(tokens, name = "tokens")
per_types_by_class <- per_tokens_by_root_wclass %>% 
  group_by(root_pos_morphind) %>% 
  summarise(types = n_distinct(root_morphind)) %>% 
  mutate(root_pos_morphind = fct_reorder(root_pos_morphind, -types))
per_hapax_by_class <- per_tokens_by_root_wclass %>% 
  filter(tokens==1) %>% 
  group_by(root_pos_morphind) %>% 
  summarise(hapax = n_distinct(root_morphind))
per_prods <- per_tokens_by_class %>% 
  left_join(per_types_by_class) %>% 
  left_join(per_hapax_by_class) %>% 
  left_join(wclass_colvals %>% 
              rename(root_pos_morphind = affix_morphind_wclass)) %>% 
  mutate(across(where(is.integer), ~replace_na(., 0L))) %>% 
  mutate(corpussize = corpussizes_all,
         tokens_pmw = (tokens/corpussize) * 1000000,
         types_pmw = (types/corpussize) * 1000000,
         hapax_pmw = (hapax/corpussize) * 1000000) %>% 
  select(-corpussize) %>% 
  pivot_longer(cols = -c(root_pos_morphind, colvals), 
               names_to = "freq_profile", 
               values_to = "vals") %>% 
  mutate(freq_profile = factor(freq_profile, 
                               levels = c("tokens", "types", "hapax", 
                                          "tokens_pmw", "types_pmw", "hapax_pmw")))

###### in-text stats for per- =====
per_types_by_class %>% tally(types) # jumlah keseluruhan bentuk PER- verb
per_types_by_class %>% 
  mutate(perc = types/sum(types) * 100) # persentase adjectival root for per- transitive

p <- per_prods %>% 
  filter(str_detect(freq_profile, 'pmw', TRUE), vals > 0) %>% 
  droplevels() %>% 
  split(.$freq_profile) %>% 
  map(# ~ggplot(., aes(x = reorder(affix_morphind, values), y = values, fill = colvals)) + 
    ~ggplot(., aes(x = root_pos_morphind, y = vals, fill = root_pos_morphind)) + 
      geom_col() + 
      theme_light() +
      scale_fill_manual(values = .$colvals) +
      labs(x=.$freq_profile, 
           y = "Nilai riil",
           caption = "n=nomina; v=verba; a=ajektiva\nf=asing; r=preposisi", 
           fill = "") +
      theme(axis.title.y = element_text(size = 7.5),
            axis.title.x = element_text(size = 7.5),
            axis.text.y = element_text(size = 6),
            axis.text.x = element_text(size = 5),
            plot.caption = element_text(size = 6),
            legend.text = element_text(size = 6),
            legend.key.size = unit(.4, 'cm')))
p <- grid.arrange(grobs = p, ncol = 3)
# ggsave(plot = p, filename = "figures/15-wclass-dist-per-all-voice.png", width = 8.5, height = 3, dpi = 300, units = "in")

##### 7.5.5 visualisation: wordclass attached to PER-X-KAN (regardless of voice, and using raw values) ======
per_kan_voice_collapsed <- verbs %>% 
  filter(str_detect(affix_morphind, '^.+?\\+per-_kan$')) %>% 
  mutate(affix_morphind2 = str_replace(affix_morphind, "^[^+]+?\\+", "")) %>% 
  group_by(affix_morphind2, root_morphind, root_pos_morphind) %>% 
  summarise(tokens = sum(n), .groups = "drop")
per_kan_tokens <- sum(per_kan_voice_collapsed$tokens)
per_kan_types <- per_kan_voice_collapsed %>% select(root_morphind) %>% distinct() %>% nrow()
per_kan_hapax <- per_kan_voice_collapsed %>% filter(tokens==1) %>% nrow()
per_kan_tokens_wclass <- per_kan_voice_collapsed %>% 
  group_by(root_pos_morphind) %>% 
  summarise(tokens_wclass = sum(tokens)) %>% 
  arrange(desc(tokens_wclass)) %>% 
  mutate(perc_tokens = tokens_wclass/sum(tokens_wclass) * 100)
per_kan_types_wclass <- per_kan_voice_collapsed %>% 
  group_by(root_pos_morphind) %>% 
  summarise(types_wclass = n_distinct(root_morphind)) %>% 
  arrange(desc(types_wclass)) %>% 
  mutate(perc_types = types_wclass/sum(types_wclass) * 100)
per_kan_hapax_wclass <- per_kan_voice_collapsed %>% 
  group_by(root_pos_morphind) %>% 
  filter(tokens == 1) %>% 
  summarise(hapax_wclass = sum(tokens)) %>% 
  arrange(desc(hapax_wclass))

##### 7.5.6 Wordclass attached to BER-X-AN (regardless of voice, and using raw values) ======
affix_wclass_prods %>% 
  filter(str_detect(affix_morphind, '^ber\\-_an$')) %>% 
  select(affix_morphind_wclass, types_root, total_types_by_affix, n_hapax, tokens) %>% 
  mutate(types_perc = types_root/total_types_by_affix * 100)
####### get percentage and values of OTHERS root
affix_wclass_prods %>% 
  filter(str_detect(affix_morphind, '^ber\\-_an$')) %>% 
  select(affix_morphind_wclass, types_root, total_types_by_affix, n_hapax, tokens) %>% 
  mutate(types_perc = types_root/total_types_by_affix * 100) %>% 
  filter(types_perc < 10) %>% 
  summarise(types_perc_others = sum(types_perc),
            types_perc_n = sum(types_root))

##### 7.5.7 Wordclass attached to PER-X-KAN (regardless of voice, and using raw values) ======
per_kan_token_types_wclass <- verbs %>% 
  filter(str_detect(affix_morphind, 'per\\-_kan$')) %>% 
  group_by(root_pos_morphind) %>% 
  summarise(types = n_distinct(root_morphind), tokens = sum(n), .groups = "drop")
per_kan_hapax_wclass <- verbs %>% 
  filter(str_detect(affix_morphind, 'per\\-_kan$')) %>% 
  group_by(root_morphind, root_pos_morphind) %>% 
  summarise(tokens = sum(n), .groups = 'drop') %>% 
  mutate(is_hapax = if_else(tokens == 1, TRUE, FALSE)) %>% 
  group_by(root_pos_morphind) %>% 
  summarise(hapax = sum(is_hapax), .groups = 'drop')
per_kan_prods_wclass <- left_join(per_kan_token_types_wclass, per_kan_hapax_wclass) %>% 
  arrange(desc(types)) %>% 
  mutate(types_total = sum(types),
         types_perc = types/types_total * 100)
per_kan_prods_wclass
####### get percentage and values of OTHERS root
per_kan_prods_wclass %>% 
  filter(types_perc < 10) %>% 
  summarise(types_perc_others = sum(types_perc),
            types_perc_n = sum(types))