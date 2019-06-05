library(tabulizer)

path <- "input/judges_list_external.pdf"

## # abbreviations:
## # * circuit admin judge
## # ** county admin judge
## # # district admin judge
## # CJ chief judge

endtbls <- extract_text(path, pages = 32:40, area = list(c(70, 50, 740, 540)))
md_counties <- tidycensus::fips_codes %>%
  filter(state == "MD") %>%
  pull(county) %>%
  str_remove("(?<!Baltimore) County") %>%
  str_replace("city", "City") %>%
  str_flatten(collapse = "|") %>%
  sprintf("(%s)", .)

out <- read_tsv(endtbls) %>%
  rename(text = 1) %>%
  mutate(court = str_extract(text, "(Appeals|District|Circuit)")) %>%
  mutate(group = str_extract(text, "^(Court .+|[A-Z\\s\\d]+)$") %>% str_to_title()) %>%
  fill(court, group, .direction = "down") %>%
  mutate(circuit_admin = str_detect(text, "(?<!\\*)\\*(?!\\*)"),
         county_admin = str_detect(text, "\\*{2}"),
         district_admin = str_detect(text, "#"),
         chief_judge = str_detect(text, "CJ")) %>%
  mutate(text = str_remove_all(text, "(CJ|\\*|#)") %>% str_trim()) %>%
  filter(str_detect(text, "^Hon")) %>%
  mutate(county = str_extract(text, md_counties)) %>%
  mutate(text = str_remove(text, md_counties)) %>%
  mutate_if(is.logical, as.numeric)

write_csv(out, "md_judges_list.csv")