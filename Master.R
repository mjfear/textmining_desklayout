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

# phone_names <- read_csv("data/mdc_phone_list.csv",col_names = TRUE) %>% 
#   mutate(word_count = str_count(staffName, ptn_separate_word),
#          type = if_else(str_detect(staffName, "Room|Library|Centre|Office"), "Room", "Staff")) 

hr_names <- read_csv("data/HRListOfStaff.csv", col_names = TRUE) 
hr_allStaff <- read_csv("data/HR_MATT.csv", col_names = TRUE, skip = 17) 
hr_newStaff <- read_csv("data/HR_NEW.csv", col_names = TRUE, skip = 15) %>% 
  select(-X9) %>% 
  slice(which(is.na(`Staff Member`) != TRUE)) %>% 
  mutate(`Staff Member` = as.integer(`Staff Member`)) %>% 
  rename(Title = `Position Number`)

# hr_formerStaff <- read_csv("data/HR_TERMIN.csv", col_names = TRUE, skip = 15) %>% 
#   select(-matches("X")) %>% 
#   mutate(last = str_split_fixed(str_to_upper(Employee), "[:space:]", n=2)[,1],
#          payNo = row_number()+10000)

clean_names_staff_full <- hr_allStaff %>% 
  full_join(hr_names) %>% 
  full_join(hr_newStaff, by = c("Staff Member", "Pay Location")) %>% 
  arrange(`Staff Member`) %>% 
  mutate(predicate = Surname.y %>% is.na,
         last = if_else(predicate, Surname.x, Surname.y),
         joined = if_else(predicate, Joined.x, Joined.y),
         jobTitle = if_else(predicate, Title.x, Title.y),
         first = if_else(predicate, `Pref Name.x`, `Pref Name.y`)) %>% 
  select(payNo = `Staff Member`, name = `Staff Member_1`, last, first, jobCode = `Position Number`, jobTitle,
         council = Council, department = Department, managers = Managers, section = Section,
         location = `Pay Location`, jobStart = `Position Start`, country = `Country Born`,
         everything(), -matches("\\.x|\\.y"), - predicate, -Status, -`Employment Status`) 

clean_names_staff <- clean_names_staff_full %>% 
  filter(str_detect(location, "1994|2005|Beehive") == TRUE) %>% 
  mutate(first = str_to_upper(first),
         last = str_to_upper(last)) %>% 
  arrange(department, managers, section) 

clean_names_staff_summary <- clean_names_staff %>% map_df(n_distinct) 

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

## Break text into question groups
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
  mutate(respondentID = label_TMgroup(text, pattern = ptn_respondent) %>% str_replace("#","") %>% as.integer()) %>% 
  nest(text) %>% 
  mutate(question = data %>% map("text") %>% map(~label_TMgroup(.x, ptn_question, start_group = "I"))) %>% 
  unnest()

respondent_names_full <- text_grouped %>% 
  group_by(doc, respondentID, question) %>% 
  filter(question == "Q1") %>%
  slice(seq(2, length(text)-2, by = 4)) %>% 
  mutate(resp_name = str_replace(text, "^Name ", "") %>% str_trim,
         resp_first = str_split_fixed(resp_name, "[:space:]", n = 2)[,1] %>% str_to_upper,
         resp_last = str_split_fixed(resp_name, "[:space:]", n = 2)[,2] %>% str_to_upper) %>% 
  mutate(n_match_first = str_which(resp_first, str_c("^", clean_names_staff$first, "$")) %>% {length(.)},
         n_match_last = str_which(resp_last, str_c("^", clean_names_staff$last, "$")) %>% {length(.)},
         confidence = (1/(n_match_first) + 1/(n_match_last))*0.5) %>% 
  arrange(doc, respondentID) %>% 
  filter(n_match_first + n_match_last > 0) #bin responses for which cannot find any matches in first or last name

respondent_names_last <- respondent_names_full %>% 
  inner_join(clean_names_staff, by = c("resp_last" = "last")) %>% 
  group_by(doc, respondentID) %>% 
  mutate(matches = n()) %>% 
  select(matches, everything()) %>% arrange(doc, respondentID) %>% 
  filter(n_match_last == 1 | first == resp_first)

respondent_names_first <- respondent_names_full %>% 
  anti_join(respondent_names_last) %>% 
  inner_join(clean_names_staff, by = c("resp_first" = "first")) %>% 
  group_by(doc, respondentID) %>% 
  mutate(matches = n()) %>% 
  select(matches, everything()) %>% arrange(doc, respondentID) %>% 
  filter(n_match_first == 1 | resp_last != "" & resp_last == last) 
  
respondent_names_unknown <- respondent_names_full %>% 
  anti_join(respondent_names_last) %>% 
  anti_join(respondent_names_first) 

respondent_names_known <- respondent_names_last %>% 
  bind_rows(respondent_names_first)
#   left_join(clean_names_staff, by = c("resp_last" = "last")) %>% 
#   nest(-doc, -respondentID) %>% 
#   mutate(n_group = map(data, "first") %>% map_int(length)) %>% 
#   unnest %>% 
#   filter(n_group == 1 | n_group > 1 & resp_first != first) %>% 
#   filter(resp_last == "" | is.na(staffName) == TRUE)
# 
# %>% 
#   unnest
# 
# %>% 
#   count()
# 
# %>% 
#   filter(is.na(staffID) == TRUE & resp_last == "" & countMatches == 1) %>% 
#   left_join(clean_names_staff, by = c("resp_first" = "first"))
#   # select(doc, respondentID, text) %>% 
#   # rename(respondent_name = text)

# test <- respondent_names %>% 
#   count(doc, respondentID)
# count(respondent_names$resp_first, clean_names_staff$first)


text_grouped_named <- respondent_names_known %>% 
  select(doc:respondentID, resp_name:resp_last, resp_payNo = payNo) %>% 
  inner_join(text_grouped) 
  
  # text_grouped %>% 
  # inner_join(respondent_names_known, by = c("doc", "respondentID"))
  # 

ptn_clean_first_staffName <- str_c(clean_names_staff$first %>% str_replace_na("NA"), collapse = "|")
ptn_clean_last_staffName <- str_c(clean_names_staff$last, collapse = "|")
ptn_clean_both_staffName <- str_c(ptn_clean_last_staffName, ptn_clean_first_staffName, sep = "|"))
  
text_grouped_named_colleagues <- text_grouped_named %>%
  filter(question == "Q4") %>%
  slice(c(-str_which(text, "^Page 4"), -str_which(text, "^Q4"))) %>% 
  mutate(text_upper = toupper(text),
         match_clean_word = str_detect(str_to_upper(text), )
  

%>%
  mutate(text = str_replace(text, "Name \\+ reason[:blank:]+",""),
         coll_name = str_extract_all(text, ptn_titleCase_twoword) %>% str_trim()
         # resp_name_count = str_count(resp_name, ptn_separate_word_or_end)
         ) %>%
  unnest 

%>% 
  mutate(coll_first = str_split_fixed(coll_name, "[:space:]", n = 2)[,1] %>% str_to_upper(),
         coll_last = str_split_fixed(coll_name, "[:space:]", n = 2)[,2] %>% str_to_upper())

text_matched_named_colleagues <- text_grouped_named_colleagues %>% 
  # filter(resp_name_count > 1) %>% 
  mutate(#resp_first = str_extract(respondent_name, str_c(ptn_separate_word_or_end, "[:space:]")) %>% str_trim,
         #resp_last = str_extract(respondent_name, str_c(ptn_separate_word, "$")),
         #resp_count_first = str_count(resp_first, ptn_clean_first_staffName),
         #resp_count_last = str_count(resp_last, ptn_clean_last_staffName),
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

staffID_in_survey <- text_matched_named_colleagues %>% 
  select(resp_ID, coll_ID) %>% 
  flatten_int() %>% 
  unique

network_vertices <- clean_names_staff %>% 
  select(staffID, staffName) %>% 
  filter(staffID %in% staffID_in_survey)

network_edges <- text_matched_named_colleagues %>% 
  select(resp_ID, coll_ID) 
  
network <- network_edges %>% 
  graph_from_data_frame(vertices = network_vertices, directed = FALSE)

theme_blank <- theme(axis.title = element_blank(), axis.text = element_blank(), 
                     axis.ticks = element_blank(), panel.background = element_blank())

ggraph(network, "nicely") + # "in_circle"
  geom_edge_link(alpha = 0.2) + #arrow = arrow(angle = 20, length = unit(0.1, "inches")), type = "closed", ends = "last" 
  geom_node_point(size = 2) + 
  geom_node_text(aes(label = staffName), repel = TRUE) + 
  theme_blank
