library(igraph)
library(tidyverse)
library(tidytext)
library(pdftools)
library(ggraph)

# String Regex definitions ------------------------------------------------

ptn_respondent <- "^#[:digit:]+" #Starts with # followed by one or more digits
ptn_question <- "^Q[:digit:]+" #Starts with Q followed by one or more digits
ptn_titleCase_twoword <- "[:upper:][:lower:]+[:space:][:upper:][:lower:]+"
ptn_titleCase_oneword <- "[:upper:][:lower:]+"
# ptb_nameCase_oneword <- "[:upper:]+[:lower:]+[:upper:]+[:lower:]+"
ptn_separate_word <- "[:alpha:]+"
ptn_separate_word_or_end <- "[:alpha:]+[:space:]|[:alpha:]$"

# Import and wrangle ------------------------------------------------------

phone_names <- read_csv("data/mdc_phone_list.csv",col_names = TRUE) %>% 
  mutate(word_count = str_count(staffName, ptn_separate_word),
         type = if_else(str_detect(staffName, "Room|Library|Centre|Office"), "Room", "Staff")) 

hr_names <- read_csv("data/HRListOfStaff.csv", col_names = TRUE) %>% 
  rename(staffID_HR = "Staff Member", last = Surname, first = "Pref Name", location = "Pay Location") %>% 
  filter(str_detect(location, "1994|2005|Beehive") == TRUE)

# clean_names_staff <- phone_names %>% 
#   filter(type != "Room") %>% 
#   mutate(staffID = row_number(staffName),
#          first = str_extract(staffName, str_c(ptn_separate_word_or_end, "[:space:]")) %>% str_trim(),
#          last = str_extract(staffName, str_c(ptn_separate_word, "$"))) %>% 
#   select(staffID, staffName, first, last) %>% 
#   arrange(staffID)

## or

clean_names_staff <- hr_names %>% 
  mutate(staffID = row_number(last),
         staffName= paste(first, last, sep = " ")) %>% 
  select(staffID, staffName, first, last) %>% 
  arrange(staffID)

# mdc_names <- phone_names$X1[seq(1, nrow(phone_names), 2)]
# mdc_phone <- phone_names$X1[seq(2, nrow(phone_names), 2)] %>% as.numeric()
# phone_names_tidy <-  tibble(phone = mdc_phone, name = mdc_names) %>% 
#   mutate(wordcount = str_count(name, ptn_titleCase_oneword) + 1) 


textA <- pdf_text("data/staffresponses1.pdf") %>% {tibble(doc = "A", page = .)} 
textB <- pdf_text("data/staffresponses2.pdf") %>% {tibble(doc = "B", page = .)} 

text <- bind_rows(textA, textB) %>% 
  mutate(page_new = str_replace(page, pattern = "Floor layout survey - Staff", replacement = ""),
         text = str_split(page_new, "\n"),
         ) %>% 
  select(doc, text) %>% 
  unnest %>% 
  slice(str_which(text, pattern = "[:graph:]")) %>% 
  mutate(text = str_replace(text, "\r$",""))

label_TMgroup <- function(text, pattern, start_group = ""){
  idx <- str_which(text, pattern)
  idx_lead = lead(idx, default = length(text) +1)-1
  group_labels <- text[idx] %>% str_match(pattern) %>% as.character()
  if(idx[1] != 1){ #provide scope for catching info before questions begin
    idx_lead <- c(idx[1]-1, idx_lead)
    idx <- c(1, idx)
    group_labels <- c(start_group, group_labels)
  }
  group_vec <- tibble(group_labels, group_idx = map2(idx, idx_lead, ~c(.x:.y))) %>% unnest %>% pluck(1)
}

text_grouped <- text %>% 
  mutate(respondentID = label_TMgroup(text, pattern = ptn_respondent)) %>% 
  nest(text) %>% 
  mutate(question = data %>% map("text") %>% map(~label_TMgroup(.x, ptn_question, start_group = "I"))) %>% 
  unnest()
           
respondent_names <- text_grouped %>% 
  group_by(doc, respondentID, question) %>% 
  filter(question == "Q1") %>%
  slice(seq(2, length(text)-2, by = 4)) %>% 
  mutate(text = str_replace(text, "^Name ", "")) %>% 
  ungroup() %>% 
  select(doc, respondentID, text) %>% 
  rename(respondent_name = text)

text_grouped_named <- text_grouped %>% 
  inner_join(respondent_names)

ptn_clean_first_staffName <- str_c(clean_names_staff$first %>% str_replace_na("NA"), collapse = "|")
ptn_clean_last_staffName <- str_c(clean_names_staff$last, collapse = "|")
  
text_grouped_named_colleagues <- text_grouped_named %>% 
  filter(question == "Q4") %>% 
  slice(c(-str_which(text, "^Page 4"), -str_which(text, "^Q4"))) %>% 
  mutate(text = str_replace(text, "Name \\+ reason[:blank:]+",""),
         colleague = str_extract_all(text, ptn_titleCase_twoword),
         resp_name_count = str_count(respondent_name, ptn_separate_word_or_end)) %>% 
  unnest %>% 
  filter(resp_name_count > 1) %>% 
  mutate(resp_first = str_extract(respondent_name, str_c(ptn_separate_word_or_end, "[:space:]")) %>% str_trim,
         resp_last = str_extract(respondent_name, str_c(ptn_separate_word, "$")),
         resp_count_first = str_count(resp_first, ptn_clean_first_staffName),
         resp_count_last = str_count(resp_last, ptn_clean_last_staffName),
         coll_first = str_extract(colleague, str_c(ptn_separate_word_or_end, "[:space:]")) %>% str_trim,
         coll_last = str_extract(colleague, str_c(ptn_separate_word, "$")),
         coll_count_first = str_count(coll_first, ptn_clean_first_staffName),
         coll_count_last = str_count(coll_last, ptn_clean_last_staffName)) %>% 
  # select(-respondent_name, -resp_name_count, - colleague) %>% 
  # filter(resp_count_last > 0 & coll_count_last > 0 & resp_count_first > 0 & coll_count_first > 0) %>% 
  inner_join(clean_names_staff, by = c("resp_last" = "last", "resp_first" = "first")) %>% 
  select(doc:text, resp_name = staffName, resp_first, resp_last, resp_ID = staffID, coll_first, coll_last) %>% 
  inner_join(clean_names_staff, by = c("coll_first" = "first", "coll_last" = "last")) %>% 
  select(doc:resp_ID, coll_name = staffName, coll_first, coll_last, coll_ID = staffID)

staffID_in_survey <- text_grouped_named_colleagues %>% 
  select(resp_ID, coll_ID) %>% 
  flatten_int() %>% 
  unique

network_vertices <- clean_names_staff %>% 
  select(staffID, staffName) %>% 
  filter(staffID %in% staffID_in_survey)

network_edges <- text_grouped_named_colleagues %>% 
  select(resp_ID, coll_ID) 
  
network <- network_edges %>% 
  graph_from_data_frame(vertices = network_vertices, directed = FALSE)

theme_blank <- theme(axis.title = element_blank(), axis.text = element_blank(), 
                     axis.ticks = element_blank(), panel.background = element_blank())

ggraph(network, "nicely") + # "in_circle"
  geom_edge_link(alpha = 0.2, arrow = arrow(angle = 20, length = unit(0.1, "inches"), type = "closed", ends = "last")) +
  geom_node_point(size = 2) + 
  geom_node_text(aes(label = staffName), repel = TRUE) + 
  theme_blank


# mutate(respondendID_lgl = str_detect(respondent_name, ptn_clean_staffName)) 
# 
# %>% 
#   filter(respondendID_lgl == FALSE)
# 
# 
# 
# 
# colleague_staffNames <- text_grouped_named_colleagues %>% select(colleague) %>%  unique()
# 
# clean_staffNames <- colleague_staffNames %>% 
#   mutate(count = str_count(colleague, pattern = catch_words_colleagues))
#   
