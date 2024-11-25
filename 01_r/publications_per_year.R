
 # publications per author
 # created 30-09-2024
 # to extract information using rentrez to quantify the number of papers produced per year 

library(rentrez)
library(tidyverse)
library(xml2)
library(ggplot2)


# search for candidate articles
df <- entrez_search(db = "pubmed", term = "prediction model[TITL] AND 2023[PDAT]", retmode = 'xml', use_history = TRUE)

# extract the articles as xml data type in string format
xml_string <- entrez_fetch(db = "pubmed", rettype = "xml", web_history = df$web_history)

# format to xml from string
xml_data <- read_xml(xml_string)

# find the individual articles in the xml data
articles <- xml_find_all(xml_data, "//PubmedArticle")

# list the authors by first and last name and gather the unique ORCID id
authors_list <- articles %>%
  xml_find_all(".//Author") %>%
  map_df(~ {
    last_name <- xml_text(xml_find_first(.x, ".//LastName"))
    initials <- xml_text(xml_find_first(.x, ".//Initials"))
    id <- xml_text(xml_find_first(.x, ".//Identifier"))
    tibble(LastName = last_name, Initials = initials, Identifier = id)
  })

# print the author list to check
print(authors_list)

# omit the authors without an attached ORCID id
# note only ~ 10% of authors had their ORCID id available
orcids <- authors_list %>% 
  na.omit()

# mutate the search string needed to find how many papers were published in a given year
# change "AND XXX[PDAT]" to the year time frame needed
# also delete any incomplete ORCID without a -
auth_pub_search <- orcids %>% 
  rowwise() %>%
  mutate(search_term = search_term <- paste0(Identifier, " AND 2023[PDAT]")) %>% 
  filter(str_detect(search_term, "-"))

# for each of the ORCID id search rentrez and return the count of how many publications they produced
auth_pub_total <- auth_pub_search %>% 
  rowwise() %>% 
  mutate(search_result = entrez_search(db = "pubmed", term = search_term, retmax=0)$count)

      
# Plot the distribution of publications per author
auth_pub_total %>% 
  ggplot(aes(x = search_result))+
  geom_histogram()+
  theme_classic()+
  stat_bin(binwidth = 1)+
  labs(title = "Distribution of Papers Published in 2023",
       x = "Number of Publications",
       y = "Count")

ggsave(filename = "C:/Users/alexd/OneDrive - Queensland University of Technology/PhD/04_prediction_lim/cpm_limits/03_figures/pubs_per_year.jpg", width = 6, height = 4)
