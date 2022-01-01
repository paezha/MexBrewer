
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MexBrewer [<img src="man/figures/MexBrewer.png" align="right" width=300 />](https://paezha.github.io/MexBrewer/)

<!-- badges: start -->
<!-- badges: end -->

MexBrewer is a package with color palettes inspired by the works of
Mexican muralists. The inspiration for this package was
[{MetBrewer}](https://github.com/BlakeRMills/MetBrewer) by [Blake
Mills](brm2143@columbia.edu). The structure of the package and coding,
like {MetBrewer}, is based on
[{PNWColors}](https://github.com/jakelawlor/PNWColors) and
[{wesanderson}](https://github.com/karthik/wesanderson).

## Installation

Currently, there is only a development version of {MexBrewer}, which can
be installed like so:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("paezha/MexBrewer") 
```

## Artists

### Aurora Reyes

#### El atentado a las maestras rurales

This palette is called `Atentado`.

<img src="man/figures/aurora_reyes_atentado.jpeg" title="Aurora, Concha, y Frida" alt="Aurora, Concha, y Frida" width="400px" />

<img src="man/figures/atentado.png" title="Aurora" alt="Aurora" width="185px" />

#### Aurora, Concha, y Frida

This work of Aurora Rivera inspired three palettes, called `Aurora`,
`Concha`, and `Frida`.

<img src="man/figures/aurora_concha_frida.jpg" title="Aurora, Concha, y Frida" alt="Aurora, Concha, y Frida" width="400px" />

<img src="man/figures/aurora.png" title="Aurora" alt="Aurora" width="185px" />

<img src="man/figures/concha.png" title="Concha" alt="Concha" width="185px" />

<img src="man/figures/frida.png" title="Frida" alt="Frida" width="185px" />

------------------------------------------------------------------------

### María Izquierdo

#### Alacena

This palette is called `Alacena`.

<img src="man/figures/maria_izquierdo_alacena.jpg" title="Alacena" alt="Alacena" width="400px" />

<img src="man/figures/alacena.png" title="Alacena" alt="Alacena" width="185px" />

#### La Tierra

This palette is called `Tierra`.

<img src="man/figures/maria_izquierdo_tierra.jpg" title="Tiera" alt="Tiera" width="400px" />

<img src="man/figures/tierra.png" title="Tierra" alt="Tierra" width="185px" />

------------------------------------------------------------------------

## Examples

``` r
library(aRtsy) # Ijeamaka Anyene's package for generative art 
library(flametree) # Danielle Navarro's package for generative art
library(MexBrewer)
library(mxmaps)
library(sf)
library(tidyverse)
```

Invoke data sets used in the examples:

``` r
data("mx_estados") # Simple features object with the boundaries of states in Mexico
data("df_mxstate_2020") # Data from {mxmaps }with population statistics at the state level
```

Join population statistics to state boundaries:

``` r
mx_estados <- mx_estados %>%
  left_join(df_mxstate_2015%>%
              dplyr::transmute(pop2015 = pop, # Population by state in 2015
                               am2015 = afromexican, # Afromexican population by state in 2015
                               state_name),
            by = c("nombre" = "state_name")) %>%
  left_join(df_mxstate_2020 %>%
              #Percentage of population that speak an indigenous language
              mutate(pct_ind_lang = indigenous_language/pop * 100) %>% 
              dplyr::transmute(pop2020 = pop,
                               am2020 = afromexican,
                               state_name, 
                               pct_ind_lang),
            by = c("nombre" = "state_name")) %>%
  # Calculate the change in popualtion and afromexican population between 2015 and 2020
  mutate(delta_pop = (pop2020 - pop2015)/pop2015,
         delta_am = (am2020 - am2015)/am2015)
```

Distribution of population by geographic region in Mexico:

``` r
ggplot(data = mx_estados,
       aes(x = region, y = pop2015, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = mex.brewer("Concha", n = 5)) +
  theme_minimal()
```

<img src="man/figures/README-population-distribution-1.png" width="100%" />

Percentage of population who speak an indigenous language in 2020 by
state:

``` r
ggplot() +
  geom_sf(data = mx_estados,
          aes(fill = pct_ind_lang),
          color = "white",
          size = 0.08) +
  scale_fill_gradientn(colors = mex.brewer("Tierra")) +
  theme_minimal()
```

<img src="man/figures/README-indigenous-languages-1.png" width="100%" />

Percentage change of population by state between 2015 and 2020:

``` r
ggplot() +
  geom_sf(data = mx_estados,
          aes(fill = delta_pop),
          color = "white",
          size = 0.08) +
  scale_fill_gradientn(colors = mex.brewer("Atentado")) +
  theme_minimal()
```

<img src="man/figures/README-population-change-1.png" width="100%" />

Percentage change of afromexican population by state between 2015 and
2020:

``` r
ggplot() +
  geom_sf(data = mx_estados,
          aes(fill = delta_am),
          color = "white",
          size = 0.08) +
  scale_fill_gradientn(colors = mex.brewer("Alacena")) +
  theme_minimal()
```

<img src="man/figures/README-afromexican-population-change-1.png" width="100%" />

# Some Rtistry

### Danielle Navarro’s [{flametree}](https://github.com/djnavarro/flametree)

The following three images were created using the {flametree} package.

``` r
# pick some colours
shades <- MexBrewer::mex.brewer("Tierra") %>%
  as.vector()

# data structure defining the trees
dat <- flametree_grow(seed = 3563,
                      time = 11, 
                      trees = 10)

# draw the plot
dat %>% 
  flametree_plot(
    background = shades[1],
    palette = shades[2:length(shades)], 
    style = "nativeflora"
  )
```

<img src="man/figures/README-flametree-1-1.png" width="100%" />

``` r
# pick some colours
shades <- MexBrewer::mex.brewer("Concha") %>%
  as.vector()

# data structure defining the trees
dat <- flametree_grow(seed = 3536,
                      time = 8, 
                      trees = 6)

# draw the plot
dat %>% 
  flametree_plot(
    background = shades[1],
    palette = rev(shades[2:length(shades)]), 
    style = "wisp"
  )
```

<img src="man/figures/README-flametree-2-1.png" width="100%" />

``` r
# pick some colours
shades <- MexBrewer::mex.brewer("Frida") %>%
  as.vector()

# data structure defining the trees
dat <- flametree_grow(seed = 3653,
                      time = 8, 
                      trees = 6)

# draw the plot
dat %>% 
  flametree_plot(
    background = shades[1],
    palette = shades[2:length(shades)], 
    style = "minimal"
  )
```

<img src="man/figures/README-flametree-3-1.png" width="100%" />

### Koen Derks’s [aRtsy](https://github.com/koenderks/aRtsy)

The following three images were created using the {aRtsy} package.

Functions:

``` r
my_formula <- list(
  x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2)),
  y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
)

canvas_function(colors = mex.brewer("Atentado"), 
                polar = FALSE,
                by = 0.005,
                formula = my_formula)
```

<img src="man/figures/README-aRtsy-1-1.png" width="100%" />

Mosaic:

``` r
canvas_squares(colors = mex.brewer("Alacena"), 
               cuts = 20,
               ratio = 1.5,
               resolution = 200,
               noise = TRUE)
```

<img src="man/figures/README-aRtsy-2-1.png" width="100%" />

Mandelbrot’s set:

``` r
canvas_mandelbrot(colors = mex.brewer("Aurora"), 
                  zoom = 8,
                  iterations = 200,
                  resolution = 500)
```

<img src="man/figures/README-aRtsy-3-1.png" width="100%" />

### Meghan S. Harris’s [waves](https://www.thetidytrekker.com/post/making-waves)

These plots are adaptations of Meghan Harris’s artsy waves. Create data
frames with wave functions:

``` r
##Set up the "range" on the x axis for horizontal waves=====
wave_theta <- seq(from = -pi,
                  to = -0, 
                  by = 0.01) 

# Create waves using functions
wave_1 <- data.frame(x = wave_theta) %>% 
  mutate(y = (sin(x) * cos(2 * wave_theta) + exp(x * 2)))

wave_2 <- data.frame(x = wave_theta) %>%
  mutate(y = (0.5 * sin(x) * cos(2.0 * wave_theta) + exp(x)) - 0.5)
```

<!--


```r
ggplot() +
  geom_ribbon(data = wave_1,
              aes(x, min = y - 0.025, 
                  ymax = y + 0.025),
              color = "black",
              fill = "orange") +
  geom_ribbon(data = wave_2,
              aes(x, min = y - 0.025, 
                  ymax = y + 0.025),
              color = "black",
              fill = "blue")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

-->

Define a function to convert a single wave into a set of `n` waves. The
function takes a data frame with a wave function and returns a data
frame with `n` waves:

``` r
# Creating a function for iterations====

wave_maker <- function(wave_df, n, shift){
  
  #Create an empty list to store our multiple dataframes(waves)#
  wave_list<- list()
  
  #Create a for loop to iteratively make "n" waves shifted a distance `shift` from each other #
  for(i in seq_along(1:n)){
    
    wave_list[[i]] <- wave_df %>%
      mutate(y = y - (shift * i),
             group = i)  
  }
  
  #return the completed data frame to the environment#
  return(bind_rows(wave_list))
}
```

Create layered waves using the data frames with the wave functions
above:

``` r
wave_layers <- rbind(wave_1 %>%
                       wave_maker(n = 5, 
                                  shift = 0.075),
                     wave_2 %>%
                       wave_maker(n = 5, 
                                  shift = 0.075) %>%
                       mutate(group = group + 5)) # adjust the group counter to identify waves uniquely
```

Plot layered waves using cartesian coordinates and palette `Alacena`:

``` r
ggplot(wave_layers) +
  geom_rect(aes(xmin = -pi, 
                xmax = -0.0, 
                ymin = min(y) - 0.50,
                ymax = max(y) + 0.30 ),
            size = 2.5,
            color = mex.brewer("Alacena")[6],
            fill = mex.brewer("Alacena")[4]) +
  geom_rect(aes(xmin = -pi, 
                xmax = -0.0, 
                ymin = min(y) - 0.50,
                ymax = max(y) + 0.30 ),
            size = 1,
            color = "black",
            fill = NA) +
  geom_ribbon(aes(x, 
                  ymin = y - 0.025 * 4 * x,
                  ymax = y + 0.015 * 10 * x, 
                  group = group, 
                  fill = group),
              color = "black",
              size = 0.5) + 
  scale_fill_gradientn(colors = mex.brewer("Alacena"))+
  theme_void() +
  theme(legend.position = "none")
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

Plot layered waves using polar coordinates and palette `Atentado`:

``` r
ggplot(wave_layers) +
  geom_rect(aes(xmin = -pi, 
                xmax = -0.0, 
                ymin = min(y) - 0.45,
                ymax = max(y) + 0.30 ),
            size = 2.5,
            color = mex.brewer("Atentado")[6],
            fill = mex.brewer("Atentado")[3]) +
  geom_rect(aes(xmin = -pi, 
                xmax = -0.0, 
                ymin = min(y) - 0.45,
                ymax = max(y) + 0.30 ),
            size = 1,
            color = "black",
            fill = NA) +
  geom_ribbon(aes(x, 
                  ymin = y - 0.025 * 4 * x,
                  ymax = y + 0.015 * 10 * x, 
                  group = group, 
                  fill = group),
              color = "black",
              size = 0.5) + 
  scale_fill_gradientn(colors = mex.brewer("Atentado")) +
  coord_polar(theta = "x",  
              start = 0, 
              direction = 1, 
              clip = "on") +
  theme_void() +
  theme(legend.position = "none")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
##Creating the "top" of our wave polygon====
curve_top <- tibble(x = wave_theta,
                    y = (sin(x)*cos(wave_theta))+exp(x*2)) %>%
  arrange(x)

##Grab the max X value in the wave from the top wave====
max_x_point <- curve_top[which(curve_top$x == max(curve_top$x)),]


##Create a subset of curve_top to create a "side" of the wave====
curve_side_right <- max_x_point %>%
  add_row(max_x_point - c(0,0.5))%>%
  arrange(desc(y))

##Create a copy of the curve_top dataset with the y values decreased by .5====
curve_bottom <- curve_top %>%
  mutate(y = y - 0.5) %>%
  arrange(desc(x))

##Grab the min X value in the wave from the top wave====
min_x_point <- curve_top[which(curve_top$x == min(curve_top$x)),]

##Create a side that will connect curve_top and curve_bottom on the left side====
curve_side_left <- min_x_point %>%
  add_row(min_x_point - c(0,0.5)) %>%
  arrange(y)

##Slap all of them together into one dataframe IN ORDER (top,right,bottom,left)
wave <- bind_rows(curve_top,curve_side_right,curve_bottom,curve_side_left)

##View the entire thing====
wave %>%
  ggplot(aes(x=x, y=y))+
  geom_polygon()
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />
