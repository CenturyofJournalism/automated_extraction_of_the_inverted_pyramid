# Automated Extraction of W-Questions (Who, Where, When, What) from Text Leads

library(readtext)
library(stringr)
library(quanteda)
library(udpipe)
library(spacyr)
library(dplyr)

data <- readtext("data/*txt", encoding="UTF-8")
data$doc_id <- str_sub(data$doc_id, start = 1, end = -5)

korpus_long <- corpus(data, docid_field = "doc_id") 
docvars(korpus_long, "Textnummer") <- sprintf("%02d", 1:ndoc(korpus_long))

korpus_beginning <- str_sub(korpus_long, start = 1, end = 300)
korpus_beginning <- corpus(korpus_beginning)

docnames(korpus_beginning) <- data$doc_id

rm(korpus_long)

sprachmodell.udpipe <- udpipe_load_model(file = "verschiedenes/german-gsd-ud-2.5-191206.udpipe")

data.pos <- udpipe_annotate(sprachmodell.udpipe, korpus_beginning, doc_id = docnames(korpus_beginning), tagger = "default", parser = "none")
data.pos.df <- as.data.frame(data.pos)

if (!require("spacyr")) {
  install.packages("spacyr")
  library("spacyr")
  spacy_install()
  spacy_download_langmodel("de")
}

spacy_initialize(model = "de_core_news_md")

data.pos <- spacy_parse(korpus_beginning, lemma = F, entity = T, dependency = T)
spacy_finalize()

#When?

when.dictionary <- dictionary(list(when = c("januar", "februar", "märz", "april", "mai", "juni", "juli", "august", "september", "oktober", "november", "dezember", "montag*", "dienstag*", "mittwoch*", "donnerstag*", "freitag*", "samstag*", "sonntag*", "morgens", "morgen", "vormittags", "vormittag", "mittags", "mittag", "nachmittags", "nachmittag", "abends", "abend", "nachts", "nacht", "heute", "gestern", "übermorgen", "vorgestern", "monat", "zeit", "tage", "tages", "stunden", "neue woche", "mittlerweile", "anfang des jahres", "ende des jahres","jetzt", "vergang* jahr", "spieltag", "fortsetzung", "demnächst", "zukünftig", "künftig", "früh", "wochenende", "wenig* stunden", "diese woche", "dieser woche", "wochen", "wochentag", "wochentage", "diesem jahr", "dieses jahr", "nächstes jahr", "nächste jahr", "nächstes quartal", "nächste monat", "nächsten monat", "nächst* monat",  "kommender", "kommende", "kommendes", "nächsten zeit", "nächste zeit", "nächst* zeit",  "nächste woche", "nächsten woche", "nächste tage", "nächster tag", "sofort", "wochenlang","180*", "181*", "182*", "183*", "184*", "185*", "186*", "187*", "188*", "189*", "1900","1901", "1902",  "1903", "1904", "1905", "1906", "1907", "1908", "1909",  "1910", "1911", "1912",  "1913", "1914", "1915", "1916", "1917", "1918", "1919",  "1920", "1921",  "1922", "1923", "1924", "1925",  "1926",  "1927", "1928", "1929",  "1930",  "1931", "1932", "1933",  "1934",  "1935",  "1936", "1937",  "1938", "1939", "1940", "1941", "1942",  "1943", "1944", "1945",  "1946", "1947", "1948",  "1949", "1950", "1951", "1952", "1953", "1954", "1955", "1956", "1957", "1958", "1959", "1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990","1991","1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")))

tokens_data <- tokens(korpus_beginning)
dfm_data <- dfm(tokens_data)
dfm_data_lookup <- dfm_lookup(dfm_data, dictionary = when.dictionary)
dfm_matrix <- convert(dfm_data_lookup, to = "matrix")
doc_ids_with_hits <- rownames(dfm_matrix)[rowSums(dfm_matrix) > 0]

when_data <- data.frame(doc_id = doc_ids_with_hits)

#Where?
LOC <- data.pos %>% subset(entity == "LOC_B") %>% subset(pos == "PROPN" | pos == "NOUN"  | dep_rel == "ROOT" )
LOC <- LOC[!duplicated(LOC$doc_id),]

#Who? What?
PER_NOUN.pos <- data.pos %>% subset(entity == "PER_B" | entity == "PER_I" | dep_rel == "sb") %>% subset(pos == "PROPN" | pos == "NOUN")
PER_NOUN.pos <- PER_NOUN.pos[!duplicated(PER_NOUN.pos$doc_id),]

verb.pos <- data.pos.df %>% subset(xpos == "VVFIN" | xpos == "VVPP" | xpos == "VAFIN")
verb.pos <- verb.pos[!duplicated(verb.pos$doc_id),]

all_doc_ids <- docnames(korpus_beginning)

ids_what <- unique(verb.pos$doc_id)
ids_who  <- unique(PER_NOUN.pos$doc_id)
ids_where <- unique(LOC$doc_id)
ids_when <- unique(when_data$doc_id)

common_ids <- Reduce(intersect, list(ids_what, ids_who, ids_where, ids_when))

cat("Number of documents containing all 4 W-questions:", length(common_ids), "\n")

result_df <- data.frame(doc_id = all_doc_ids, stringsAsFactors = FALSE)

result_df$has_all_four <- (result_df$doc_id %in% common_ids) * 1

head(result_df)



