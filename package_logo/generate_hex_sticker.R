# create hex sticker for package
library(hexSticker)
library(here)
hex_sticker_path <- here("package_logo", "hex_logo.png")
imgurl <- here("package_logo", "icon.png")
sticker(imgurl,
  package = "ViroReportR",
  p_y = .55, p_size = 20, p_color = "#01b78d",
  s_x = 1, s_y = 1, s_width = .5, s_height = 0.5,
  h_fill = "#FFFFFF", h_color = "#01b78d",
  filename = hex_sticker_path
)
usethis::use_logo(hex_sticker_path)
