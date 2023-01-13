# create sticker for package
library(hexSticker)
library(here)
library(ggdag)
library(tidyverse)

img_path <- here("sticker", "smdi_sublogo2.png")

sticker(
  img_path,
  package = "smdi",
  h_fill = "white",
  h_color = "deepskyblue1",
  #h_size = 5,
  p_size = 8,
  p_color = "black",
  # p_x = ,
  p_y = 1.5,
  s_x = 1.05,
  s_y= .85,
  s_width=.5,
  s_height =.4,
  dpi = 1200,
  filename="sticker/smdi_hexagon.png"
)
