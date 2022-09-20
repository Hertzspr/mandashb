library(readxl)
library(tidyverse)
library(tidymodels)
library(encryptr)
library(janitor)
library(skimr)
library(plotly)
library(glue)
library(GGally)
library(tidytext)
library(factoextra)
library(FactoMineR)
library(ggiraphExtra)
library(shiny)
library(shinydashboard)
library(DT)


# prep

# import
harian <- read_excel("edit_bi_kelas_noni.xlsx", 
                     sheet = "harian")
semesteran <- read_excel("edit_bi_kelas_noni.xlsx", 
                         sheet = "phbo pat")

harian <- clean_names(harian)
semesteran <- clean_names(semesteran)

# encrypt
names_encryption <- 
  harian %>% 
  select(nama_siswa) %>%
  unique() %>% 
  mutate(
    name_code = nama_siswa
  ) %>% 
  encrypt(name_code)

harian_encrypted <- 
  harian %>% 
  left_join(names_encryption, 
            by = c("nama_siswa" = "nama_siswa"),
            keep = F) %>% 
  select(-nama_siswa) %>% 
  select(name_code, everything())

semesteran_encrypted <- 
  semesteran %>% 
  left_join(names_encryption, 
            by = c("nama_siswa" = "nama_siswa"),
            keep = F) %>% 
  select(-nama_siswa) %>% 
  select(name_code, everything())

## shuffle

harian_encrypted <- harian_encrypted %>% slice_sample(prop = 1)
semesteran_encrypted <- semesteran_encrypted %>% slice_sample(prop = 1)

# join table 

data <-
  harian_encrypted %>% left_join(semesteran_encrypted[,c(1, 3, 4)], 
                                 by = "name_code",
                                 suffix = c("_h", "_s"))

# data type conversion

data <- data %>% 
  mutate(across(.cols = c(where(is.character), -name_code),.fns = as.factor))

# pivot longer

data <- data %>% 
  pivot_longer(cols = 6:8,
               names_to = "ppp",
               values_to = "nilai_ppp") 

## summarize for viz

data_mean_ph <-
  data %>% 
  group_by(kelas, penilaian_h, materi, ppp) %>% 
  summarise(rata2_ph = round(mean(nilai_ph),digits = 2)) %>% 
  mutate(avg = glue("Nilai Rata-rata {rata2_ph}"))

acols <- c("Harian 1" = "#C6E0FF", 
           "Harian 2" = "#579A9E",
           "Harian 3" = "#3292C3",
           "Harian 4" = "#BCAB79")

fcols <- c("XI IPS 1" = "#BEEF9E", 
           "XI IPS 2" = "#3292C3",
           "XI MIPA" = "#BCAB79")

data <- data %>% 
  mutate( # repair ppp content
    ppp = str_replace(ppp, "p", "P" ) 
  ) %>% 
  mutate( # limit digits
    nilai_ph = round(nilai_ph, digits = 2)
  ) %>% 
  mutate( # creating tooltip
    exp = glue(
      "{kelas} 
      {penilaian_h}: {nilai_ph}
      {ppp}: {nilai_ppp}"
    )
  )

# correlation table

corp3ph.kelas.materi <- data %>% 
  group_by(kelas, materi) %>% 
  summarize(correlation = cor(nilai_ppp, nilai_ph))

corp3ph.kelas.p3 <- data %>% 
  group_by(kelas, ppp) %>% 
  summarize(correlation = cor(nilai_ppp, nilai_ph))

## s.p3.h.s

s.p3.h.s <-
  data %>% 
  group_by(ppp, penilaian_h, penilaian_s) %>% 
  summarise(kor_ppp_ph = cor(nilai_ppp, nilai_ph),
            kor_ph_s = cor(nilai_ph, nilai))

imputed.s.p3.h.s <-
  recipe(~ ., data = s.p3.h.s) %>%
  step_impute_bag(kor_ppp_ph) %>% 
  prep(s.p3.h.s)

df.imputed.s.p3.h.s <-
  bake(imputed.s.p3.h.s, new_data = s.p3.h.s) 
df.imputed.s.p3.h.s

# create factor reorder for plot reason
df.imputed.s.p3.h.s2 <- df.imputed.s.p3.h.s %>% 
  mutate(
    penilaian_s_f = factor(unique(df.imputed.s.p3.h.s$penilaian_s),
                           levels=c("PHBO", "PAT"))
)

# data edit col

data_edit_col <- 
  data %>% 
  mutate("Assignment Scores" = nilai_ppp,
         "Minor Test Results" = nilai_ph,
         "Major Test Results" = nilai,
         "Class" = kelas)

# cor test

uk.ppp.s <-
  data %>% 
  select(kelas, nilai_ppp, nilai) %>% 
  nest(data = c(nilai_ppp, nilai)) %>% 
  mutate(
    test = map(data, ~ cor.test(.$nilai_ppp,
                                .$nilai)),
    tidied = map(test, tidy)
  ) %>% 
  unnest(cols = tidied) %>% 
  select(-data, -test)

uk.ph.s <-
  data %>% 
  select(kelas, nilai_ph, nilai) %>% 
  nest(data = c(nilai_ph, nilai)) %>% 
  mutate(
    test = map(data, ~ cor.test(.$nilai_ph,
                                .$nilai)),
    tidied = map(test, tidy)
  ) %>% 
  unnest(cols = tidied) %>% 
  select(-data, -test)

# PCA

data_wide <-
  data %>%
  group_by(name_code) %>% 
  pivot_wider(
    names_from = ppp,
    values_from = nilai_ppp,
    values_fill = 0,
  ) %>% 
  pivot_wider(
    names_from = penilaian_h,
    values_from = nilai_ph,
    values_fill = 0
  ) %>% 
  pivot_wider(
    names_from = penilaian_s,
    values_from = nilai,
  ) %>% 
  summarise_all(mean) %>% 
  select(-c(kelas, materi, exp))

process_rec <- # define recipe
  recipe(~., data = data_wide) %>% 
  update_role(name_code, new_role = "id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors()) 

process_prep <- prep(process_rec)

pca_rec <-
  process_rec %>% 
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

sdev <- 
  pca_prep$steps[[3]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)

var_df <- data.frame(PC=paste0("PC",1:length(sdev)),
                     var_explained=percent_variation,
                     stringsAsFactors = FALSE)

# tidy pca

tidied_pca <- tidy(pca_prep, 
                   number = 3) # the number of step in recipe, normalize is 2, pca is 3

# facto

pca_rec2 <- # define recipe
  recipe(~., data = data_wide) %>% 
  update_role(name_code, new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  prep()

d4facto <- juice(pca_rec2)

# kmeans

d4kmeans <- d4facto %>% 
  select(-1)

set.seed(1)
kmsim <- 
  tibble(k = 1:9)%>% 
  mutate(
    # cluster the data 9 times
    kclust = map(k, ~ kmeans(d4kmeans, .))
  )%>% 
  mutate(
    glanced = map(kclust, glance)
  )

clustering <-
  kmsim %>% 
  unnest(cols = glanced)

thek <- kmsim %>% 
  filter(k == 3) %>% 
  pull(kclust)

d4kmeans$cluster <- thek[[1]][[1]]




