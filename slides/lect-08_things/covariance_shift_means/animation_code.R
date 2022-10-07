library(tidyverse)
library(gganimate)

hline_price_seq =
  seq(0, mean(diamonds$price), length = 10)

vline_carat_seq =
  seq(0, mean(diamonds$carat), length = 10)


price_moves <- ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price)) +
  geom_hline(data = data.frame(hline_price_seq),
             yintercept = hline_price_seq, color = "red") +
  geom_vline(data = data.frame(vline_carat_seq),
             xintercept = 0, color = "yellow") +
  transition_time(hline_price_seq)
anim <- animate(price_moves)
anim_save(here("slides/lect-08_things/covariance_shift_means/price_moves.gif"), anim)

carat_moves <- ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price)) +
  geom_hline(yintercept = mean(diamonds$price),
             color = "red") +
  geom_vline(data = data.frame(vline_carat_seq),
             xintercept = vline_carat_seq, color = "yellow") +
  transition_time(vline_carat_seq)
anim <- animate(carat_moves)
anim_save(here("slides/lect-08_things/covariance_shift_means/carat_moves.gif"), anim)

ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price)) +
  geom_hline(yintercept = mean(diamonds$price),
             color = "red") +
  geom_vline(xintercept = mean(diamonds$carat),
             color = "yellow")




