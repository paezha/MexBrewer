
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MexBrewer <img src="man/figures/MexBrewer.png" align="right" width=230 />

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
library(MexBrewer)
library(mxmaps)
library(sf)
#> Linking to GEOS 3.9.1, GDAL 3.2.1, PROJ 7.2.1; sf_use_s2() is TRUE
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.7
#> v tidyr   1.1.4     v stringr 1.4.0
#> v readr   2.1.1     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
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
