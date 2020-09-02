# rlang test script
library(tidyverse)
head(iris)

rm(tdf)
tdf <- rlangtest(df = iris, grouper = 'Species')
tdf
# arrange(tdf, desc(Petal.Length))
rm(tdf)
tdf <- rlangtest2(df = iris, grouper = 'Species')
tdf

# argh this is stupdi
gm %>% mutate(across(starts_with("Species"), ~str_c(., 'test')))
gm %>% mutate(across(starts_with("Species"), ~str_c(., subgroup, sep = '_')))
gm %>% mutate(across(all_of(grouper), ~str_c(., subgroup, sep = '_')))

