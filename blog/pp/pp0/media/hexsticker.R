library(hexSticker)
library(magick)


pic <- image_read("/home/manuel/Bilder/pp1.png")

final_res <- sticker(
    pic,
    package = "",
    p_size = 50,
    p_y = 1.5,
    s_x = 1,
    s_y = 0.8,
    s_width = 1.1,
    s_height = 14,
    filename = "pp1.png",
    h_fill = "#062047",
    h_color = "#062047"
)

plot(final_res)
