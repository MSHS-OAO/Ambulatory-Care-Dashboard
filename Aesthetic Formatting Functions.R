### Color Functions for Graphs ============================================================
library(ggplot2)

theme_set(theme_minimal())

# Mount Sinai corporate colors "USE THIS TO ADD COLORS"
MountSinai_colors <- c(
  `light pink`   = "#fcc9e9",
  `med pink`     = "#fa93d4",
  `dark pink`    = "#d80b8c",
  `light purple` = "#c7c6ef",
  `med purple`   = "#8f8ce0",
  `light blue`   = "#5cd3ff",
  `med blue`     = "#06ABEB",
  `dark blue`    = "#212070",
  `light grey`   = "#b2b3b2",
  `yellow`       = "#E69F00"
  )

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Color Function that can be used to call all colors is "MountSinai_cols()"
# Examples
MountSinai_cols()       # will provide all colors and their hex codes in a table 
MountSinai_cols("pink") # will provide color name and the hex code for the pink color

# Create palettes 
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("med blue","dark pink","dark blue","light grey", "light blue",
                            "light pink", "light purple","med pink","med purple","yellow" ),
  
  `main`  = MountSinai_cols("med blue","dark pink","dark blue","light grey","light pink"),
  
  `pink`  = MountSinai_cols("light pink", "dark pink"),
  
  `blue`  = MountSinai_cols("light blue", "dark blue"),
  
  `grey`  = MountSinai_cols("light grey", "med blue")
  
)
MountSinai_palettes
# Return function to interpolate a Mount Sinai color palette
# default value is the main palette, reverse = True will change the order

MountSinai_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

# Also more shades of colors can be created by specifying the parameter of how many shades 
# you want between colors and can be used for ggplot scales, also an alpha level can be specified
# Examples
MountSinai_pal("Pink")(10) #the palette dark will use the blue and grey colors and create 10 shades


# Examples in ggplot 
ggplot(H1960, aes(Country.Code, Fertility.Rate, color = Region)) +
  geom_point(size = 4) + scale_color_manual(values=MountSinai_pal("main")(9))


# Scale Function for ggplot can be used instead of scale_color_manual
scale_color_MountSinai <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Examples in ggplot 
ggplot(H1960, aes(Country.Code, Fertility.Rate, color = Region)) +
  geom_point(size = 4) +
  scale_color_MountSinai("main")


### ggplot Theme ==========================================================================================================================

library(dplyr)
library(extrafont)
font_import()
loadfonts(device = "win") 
windowsFonts()

# ggplot theme functions ------------------------------------------------------------------
theme_new_line <-function(base_size = 12,
                          base_family = "Calibri",
                          base_line_size = base_size / 170,
                          base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        size = rel(1.5),
        face = "bold",
        hjust = 0),
      plot.subtitle = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255),
        size = rel(1.25), 
        face = "italic",
        hjust = 0,
        margin = margin(0,0,10,0)),
      axis.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255),
        size = rel(0.75)),
      axis.text.x = element_text(
        angle = 45,
        hjust = 0.5),
      axis.line = element_line(
        color = rgb(25, 43, 65, maxColorValue = 255)),
      panel.grid.minor = element_blank(),  
      panel.border = element_blank(),
      panel.background = element_blank(),
      
      complete = TRUE
    )
}


theme_new_line <-function(base_size = 12,
                          base_family = "Calibri",
                          base_line_size = base_size / 170,
                          base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20, margin=margin(0,0,30,0)),
          legend.position = "top",
          legend.text = element_text(size="12"),
          legend.direction = "horizontal",
          legend.key.size = unit(1.0,"cm"),
          legend.title = element_blank(),
          axis.title = element_text(size="14"),
          axis.text = element_text(size="14"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(r=5)),
          axis.text.x = element_text(angle = 90,hjust = 0.5, margin = margin(t=10)),
          axis.text.y = element_text(margin = margin(l=5, r=5)),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 0.3, colour = "black"),
          plot.margin = margin(30,30,30,30))
}