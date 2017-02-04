# https://www.reddit.com/r/dailyprogrammer/comments/5q9cll/20170126_challenge_300_easyintermediate_lets_make/

library(magrittr)
library(dplyr)
library(ggplot2)
library(rlist)

find_next_array <- function(current_array, this_rule_map) {
  padded_array <- c(tail(current_array, 1), current_array, head(current_array, 1))
  array_combs <- sapply(1:length(current_array), function(i) paste0(padded_array[i:(i + 2)], collapse = ''))
  unname(this_rule_map[array_combs])
}

el_cel_auto <- function(array_size, row_size, rule_number) {
  rule_base_data <- 0:7 %>% sapply(. %>% intToBits %>% as.integer %>% extract(1:3) %>% rev %>% paste0(collapse = ''))
  
  this_rule_data <- as.integer(intToBits(rule_number))[1:8]
  this_rule_map <- this_rule_data
  names(this_rule_map) <- rule_base_data
  
  current_array <- rep(0, array_size)
  current_array[floor(array_size/2) + 1] <- 1
  
  ans <- Reduce(f = find_next_array, x = rep(list(this_rule_map), times = row_size - 2), init = current_array, accumulate = TRUE) 
  
  ans
}

# plotting the results
plot_el_cel_out <- function(result) {
  toplot <- result %>% 
    list.map(data.frame(val = ., x_pos = seq_along(.), y_pos = .i)) %>% 
    bind_rows
  
  ggplot(toplot) +
    geom_raster(aes(x = x_pos, y = y_pos, fill = val)) +
    scale_y_reverse() +
    coord_cartesian(expand = FALSE) +
    scale_fill_continuous(guide = FALSE)
}
  
el_cel_auto(43, 40, 2) %>% plot_el_cel_out()
el_cel_auto(43, 17, 90) %>% plot_el_cel_out()
