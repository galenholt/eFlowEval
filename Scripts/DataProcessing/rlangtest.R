# rlang test function because rlang is an incomprehensible pile of ....

rlangtest <- function(df, grouper) {

  # Turn grouper into a sym
  # gs <- sym(grouper)
    gm <- df %>%
    group_by(across(all_of(grouper))) %>%
    summarize(across(starts_with("P"), mean)) %>%
    ungroup()
    
    # Shuffle so can test
    gm <- arrange(gm, desc(Petal.Length))
    
    # arrange on grouping variable
    gm <- gm %>% arrange(across(all_of(grouper)))
    
    # Can I index? Should be able to
    print(df[1, grouper])
    
    # select to remove the grouper
    gm <- gm %>%
      select(-grouper)
    
    return(gm)
}

rlangtest2 <- function(df, grouper) {
  
  stupidRlang <- 1
   
  # Turn grouper into a sym
  # gs <- sym(grouper) 
  gm <- df %>%
    group_by(across(all_of(grouper))) %>%
    mutate(subgroup = row_number()) %>%
    ungroup() %>%
    mutate(across(all_of(grouper), ~str_c(., subgroup, sep = '_'))) %>%
    select(-subgroup)
  
  # gm2 <- gm %>%
  #   mutate(grouper = str_c(grouper, subgroup, sep = '_'))
  # Shuffle so can test
  gm <- arrange(gm, desc(Petal.Length))
  
  # arrange on grouping variable
  gm <- gm %>% arrange(across(all_of(grouper)))
  
  # Can I index? Should be able to
  # print(df[1, grouper])
  
  # select to remove the grouper
    # WHY am I doing this?
  # gm <- gm %>%
  #   select(-grouper)
  
  return(gm)
}
