#!/usr/bin/env Rscript 

#args = commandArgs(trailingOnly=TRUE)

# test number of arguments
#if (length(args)!=3) {
#  stop("Three arguments (outfile, window, min_purch) must be supplied (input file).n", call.=FALSE)
#} 

setwd("/home/ivm/res_ht")

library(tidyverse)
library(data.table)
library(slider)

data_path <- "data"
file_in <- "R8_detailed_C0.txt"
nr_meds_in <- "ATC_codes_r8.csv"
file_out <- "nrmed_max_r8_pre55.tsv"
long_out <- "nrmed_long_r8_pre55.tsv.gz"
window <- 0.25
min_cl <- 4

#file_out <- args[1]
#window <- as.numeric(args[2])
#min_purch <- as.numeric(args[3])

#file_in <- "long_det_fake_filt_test.tsv"
#nr_meds_in <- "nr_meds_test.tsv"

#Table on how many drugs one drug code represents. 
#Excluded drugs have value "0".
nr_meds_exact <- fread(str_glue("{data_path}/{nr_meds_in}")) %>%
  rename(CODE1 = ATC) %>%
  select(CODE1, nr_med, Class1, Class2, Class3)


#Sliding window condition: drugcode and nr of drugs:code_nr, index: EVENT_AGE, window width: 3 moths
df <- fread(str_glue("{data_path}/{file_in}")) %>%
  #slice(1:10000) %>%
  filter(EVENT_AGE < 55) %>%
  left_join(nr_meds_exact, .by=CODE1) %>%
  mutate_at(c("Class1","Class2", "Class3"), ~if_else(.=="", NA_character_, .)) %>%
  group_by(FINNGENID) %>%
  mutate(sum_nr_med = slide_index2(.x = CODE1, .y = nr_med, .i = EVENT_AGE, .f = ~sum(unique(tibble(.x, .y))[,2]), .before = window),
         sum_nr_med = unlist(sum_nr_med)) %>%
  pivot_longer(cols=c("Class1","Class2", "Class3"), names_to = "tmp", values_to = "class", values_drop_na=T) %>%
  mutate(sum_nr_class = slide_index(.x = class, .i = EVENT_AGE,  .f = ~length(unique(.)), .before = 0.25),
         sum_nr_class = unlist(sum_nr_class)) %>%
  mutate(sum_classes = slide_index(.x = class, .i = EVENT_AGE,  .f = ~c(unique(.)), .before = 0.25))
  

#endpoint-file like format created

nrmed_max <- df %>%
  group_by(FINNGENID) %>%
  mutate(NRMED_MAX = max(sum_nr_med))%>%
  filter(sum_nr_med==NRMED_MAX) %>%
  mutate(NRMED_MAX_AGE = min(EVENT_AGE),
         NRMED_MAX_YEAR = min(year(APPROX_EVENT_DAY))) %>%
  select(FINNGENID, NRMED_MAX, NRMED_MAX_AGE, NRMED_MAX_YEAR) %>%
  unique() 


nrclass_max <- df %>%
  group_by(FINNGENID) %>%
  mutate(NRCLASS_MAX = max(sum_nr_class))%>%
  filter(sum_nr_class==NRCLASS_MAX) %>%
  mutate(NRCLASS_MAX_AGE = min(EVENT_AGE),
         NRCLASS_MAX_YEAR = min(year(APPROX_EVENT_DAY))) %>%
  select(FINNGENID, NRCLASS_MAX, NRCLASS_MAX_AGE, NRCLASS_MAX_YEAR) %>%
  unique() 


ht_res_cl <- df %>% 
  group_by(FINNGENID) %>%
  filter(sum_nr_class >= min_cl) %>%  #Lines below treshold removed
  mutate(HT_RES_CL = 1,
         HT_RES_CL_AGE = min(EVENT_AGE),
         HT_RES_CL_YEAR= min(year(APPROX_EVENT_DAY))) %>%   
  select(FINNGENID, HT_RES_CL, HT_RES_CL_AGE, HT_RES_CL_YEAR) %>%
  unique()



out <- nrmed_max %>%
  left_join(nrclass_max, .by="FINNGENID")  %>%
  filter(NRMED_MAX >0)
  

fwrite(out, str_glue("{data_path}/{file_out}"), sep="\t")
fwrite(df, str_glue("{data_path}/{long_out}"), sep="\t")
