library(hexSticker)
library(magick)
library(showtext)

# Loading Google fonts (http://www.google.com/fonts)
google_font_name <- "Cairo" # "Rubik"
font_add_google(google_font_name)

# Automatically use showtext to render text for future devices
showtext_auto()

# Flaticon take from:
# https://www.flaticon.com/free-icon/caduceus-symbol_3209049?term=medicine&related_id=3209049#
#
# Needs attribution in README acknowledgments section.
image <- image_read("ospsuite-hextools/caduceus-symbol.png")

sticker(
  image,
  package = "ospsuite",
  p_color	= "#545452",
  p_family = google_font_name,
  p_size = 35,
  p_x = 1,
  p_y = 1.50,
  s_x = 1,
  s_y = 0.8,
  s_width = 1.3,
  s_height = 1,
  h_color = "grey",
  filename = "ospsuite-hextools/ospsuite.png",
  h_fill = "white",
  url = "https://www.open-systems-pharmacology.org/",
  u_size = 6.5,
  u_color = "grey",
  dpi = 600
)
