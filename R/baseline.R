# function to allow baselining of long dataframes
baseline <- function(longdata, comp_col, baselev, groupers, val_col, FUN, join_geo = FALSE) {
  baseframe <- longdata %>% 
    select({{comp_col}}, {{groupers}}, {{val_col}}) %>% 
    filter({{comp_col}} == baselev) %>% 
    rename(ref_val = {{val_col}}) %>% 
    select(-{{comp_col}})
  
  if (join_geo) {
    longdata <- st_join(longdata, baseframe)
  }
  if (!join_geo) {
    longdata <- left_join(longdata, st_drop_geometry(baseframe))
  }
  
  longdata <- longdata %>% 
    mutate("{{val_col}}_{{FUN}}_{baselev}" := FUN({{val_col}}, ref_val))
  
}

# a couple convenience functions
difference <- function(x, y) {x-y}
relative <- function(x,y) {x/y}
# test <- baseline(lippiadf_cat, comp_col = strict_level, baselev = 'germ_Lippia', groupers = c(date, ValleyName), val_col = area_passing, FUN = difference)