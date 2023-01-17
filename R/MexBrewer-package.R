#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' Mexican states.
#'
#' A simple features object with the boundaries of states in Mexico (unprojected; CRS is WGS 84).
#'
#' @format A simple features data frame with 32 rows and 4 variables:
#' \describe{
#'   \item{ID}{Unique identifier of polygon}
#'   \item{nombre}{Name of the state}
#'   \item{region}{Geographical region of the state; there are five regions in the country}
#'   \item{geometry}{Geometry information of the polygons}
#' }
#'
#' @docType data
#' @keywords datasets
#' @return A simple features data frame
#' @name mx_estados
#' @usage data(mx_estados)
#' @examples
#'  data(mx_estados)
#'  summary(mx_estados)
"mx_estados"

#' Mexican 2020 states dataset
#'
#' A data.frame containing population estimates for all the Mexican states in 2020
#'
#' \describe{
#'   \item{region}{INEGI code of the state}
#'   \item{state_name}{short state name (e.g. Coahuila)}
#'   \item{state_name_official}{Official state name (e.g. Coahuila de Zaragoza)}
#'   \item{state_abbr}{state abbreviation}
#'   \item{state_abbr_official}{official state abbreviation (it can be awkward to use Chis for Chiapas)
#'   according to the INEGI.}
#'   \item{year}{2015, the year of the Conteo from which the data is sourced}
#'   \item{pop}{total state population according to the Censo 2020}
#'   \item{pop_male}{male population according to the Censo 2020}
#'   \item{pop_female}{female population according to the Censo 2020}
#'   \item{afromexican}{afromexican population according to the Censo 2020}
#'   \item{indigenous_language}{Number of persons who speak an indigenous language according to the Censo 2020}
#' }
#' @name df_mxstate_2020
#' @docType data
#' @references Population estimates taken from the \href{https://www.inegi.org.mx/programas/ccpv/2020/default.html#Tabulados}{Censo 2020.}
#' @keywords data
#' @return A data.frame
#' @examples
#' data("df_mxstate_2020")
#' head(df_mxstate_2020)
"df_mxstate_2020"

#' Complete list of palettes
#'
#' Use names(MexPalettes) to return all possible palette names. Current choices are:
#' \code{Alacena}, \code{Atentado}, \code{Aurora}, \code{Casita1}, \code{Casita2}, \code{Casita3}, \code{Concha}, \code{Frida}, \code{Huida},  \code{Maiz}, \code{Naturaleza}, \code{Ofrenda},
#' \code{Revolucion}, \code{Ronda}, \code{Taurus1}, \code{Taurus2},\code{Tierra}, \code{Vendedora}.
#' Use \code{\link{mex.brewer}} to construct palettes.
#'
#' @export
MexPalettes <- list(
  Alacena = list(c("#693829", "#894b33", "#a56a3e", "#cfb267", "#d9c5b6", "#9ca9ba", "#5480b5", "#3d619d", "#405a95", "#345084"),
                  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                  sequential = FALSE),
  Atentado = list(c("#171e15", "#32373a", "#5e5752", "#9f9994", "#ccc2b5", "#e8cfab", "#f5bc5c", "#e2853a", "#cc4c24", "#3f1606"),
                  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                  sequential = FALSE),
  Aurora = list(c("#e7c7a8", "#c29786", "#874d4a", "#5f2f2d", "#3b141a", "#331718", "#27170b"),
                c(1, 2, 3, 4, 5, 6, 7),
                sequential = TRUE),
  Casita1 = list(c("#d2d5db", "#5c80fa", "#cfbe31", "#c14345", "#1b7a4f"),
                c(1, 2, 3, 4, 5),
                sequential = FALSE),
  Casita2 = list(c("#70b1e2", "#1c45da", "#cacd33", "#bb1544", "#06412f"),
                 c(1, 2, 3, 4, 5),
                 sequential = FALSE),
  Casita3 = list(c("#0424ac", "#0f1342", "#efb540", "#411913", "#202610"),
                 c(1, 2, 3, 4, 5),
                 sequential = FALSE),
  Concha = list(c("#d6d8d0", "#d4d5bf", "#708469", "#506446", "#3d4d34", "#37402b", "#2a3326"),
                c(1, 2, 3, 4, 5, 6, 7),
                sequential = TRUE),
  Frida = list(c("#d6d8d0", "#a4abb0", "#4c6c94", "#435e7f", "#2f415f", "#232c43", "#0b1829"),
               c(1, 2, 3, 4, 5, 6, 7),
               sequential = TRUE),
  Huida = list(c("#752305", "#ac420a", "#ce8a37", "#f8ca7c", "#ffde90", "#76a1cd", "#4e79a5", "#1b3a6b", "#0d1d3c", "#060b1f"),
              c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
              sequential = FALSE),
  Maiz = list(c("#1a2021", "#2f3536", "#555c4e", "#a5a79e", "#d2d7d1", "#9bc3c9", "#7cb0c1", "#3d72a2", "#144979", "#003464"),
              c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
              sequential = FALSE),
  Naturaleza = list(c("#faf7d2", "#e1ca89", "#be852c", "#99300c", "#80100c", "#660607", "#470607"),
               c(1, 2, 3, 4, 5, 6, 7),
               sequential = TRUE),
  Ofrenda = list(c("#593722", "#834d24", "#ab5d26", "#c3722a", "#e1c473", "#e6dab9", "#a4b591", "#55804d", "#416c39", "#2c5724"),
                 c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 sequential = FALSE),
  Revolucion = list(c("#871c0f", "#af2213", "#d9792e", "#f4c659", "#f9f2d8", "#f0f6eb", "#6fc0ba", "#368990", "#244e57", "#22394a"),
               c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
               sequential = FALSE),
  Ronda = list(c("#49295f", "#894d9f", "#bd6dc2", "#da91e0", "#e2a9dc", "#c19a8d", "#b2877d", "#a26867", "#964754", "#7e3146"),
               c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
               sequential = FALSE),
  Taurus1 = list(c("#fccc64", "#d4a43c", "#ef6f43", "#c7471b", "#bc3112", "#930816", "#7e0c01"),
                c(1, 2, 3, 4, 5, 6, 7),
                sequential = TRUE),
  Taurus2 = list(c("#a19e97", "#8f887a", "#7b7466", "#635c4a", "#4e4735", "#37352e", "#23211a"),
                 c(1, 2, 3, 4, 5, 6, 7),
                 sequential = TRUE),
  Tierra = list(c("#ccc5c3", "#8d7e4f", "#7a6431", "#69542b", "#573c22", "#4f330a", "#3b221a"),
               c(1, 2, 3, 4, 5, 6, 7),
               sequential = TRUE),
  Vendedora = list(c("#26322c", "#384d3f", "#777768", "#a6b2a5", "#e3efe2", "#ffd480", "#e09743", "#d25a3e", "#d42538", "#a6162b"),
               c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
               sequential = FALSE)
)


# Function for generating palettes

#' Mex Palette Generator
#'
#' These are a handful of color palettes from Mexican muralists.
#' Complete list of palette colors and the works that inspired them can be found \href{https://paezha.github.io/MexBrewer/}{here}.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Alacena}, \code{Atentado}, \code{Aurora}, \code{Casita1}, \code{Casita2}, \code{Casita3}, \code{Concha}, \code{Frida}, \code{Huida},  \code{Maiz},  \code{Naturaleza},\code{Ofrenda},
#' \code{Revolucion}, \code{Ronda}, \code{Taurus1}, \code{Taurus2},\code{Tierra}, \code{Vendedora}.
#' @param n Number of desired colors. If number of requested colors is beyond the scope of the palette,
#' colors are automatically interpolated. If n is not provided, the length of the palette is used.
#' @param type Either "continuous" or "discrete". Use continuous if you want to automatically
#' interpolate between colors.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @return A vector of colors for use in visualization tasks
#' @examples
#' mex.brewer("Atentado")
#'
#' mex.brewer("Concha", 6)
#'
#' mex.brewer("Frida", 10, "continuous")
#' @keywords colors
#' @export
mex.brewer <- function(palette_name, n, type = c("discrete", "continuous"), direction = c(1, -1), override.order=FALSE) {

  `%notin%` <- Negate(`%in%`)

  palette <- MexPalettes[[palette_name]]

  if (is.null(palette)|is.numeric(palette_name)){
    stop("Palette does not exist.")
  }

  if (missing(n)) {
    n <- length(palette[[1]])
  }

  if (missing(direction)) {
    direction <- 1
  }

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  if (missing(type)) {
    if(n > length(palette[[1]])){type <- "continuous"}
    else{type <- "discrete"}
  }

  type <- match.arg(type)


  if (type == "discrete" && n > length(palette[[1]])) {
    stop("Number of requested colors greater than what discrete palette can offer, \n  use as continuous instead.")
  }

  continuous <-  if(direction==1){grDevices::colorRampPalette(palette[[1]])(n)
  }else{
    grDevices::colorRampPalette(rev(palette[[1]]))(n)}

  discrete <- if(direction==1 & override.order==FALSE){
    palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)]
  }else if(direction==-1 & override.order==FALSE){
    rev(palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)])
  } else if(direction==1 & override.order==TRUE){
    palette[[1]][1:n]
  } else{
    rev(palette[[1]])[1:n]
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(palette[[1]])(n),
                discrete = palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)],
  )
  structure(out, class = "palette", name = palette_name)

}

# Function for printing palette

#' @export
#' @importFrom grDevices rgb
#' @importFrom graphics rect par image text

print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.92, n + 1, 1.08, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 2.5, family = "serif")
}


#' Names of sequential palettes
#'
#' Use \code{\link{mex.brewer}} to construct palettes.
#'
#' @export
sequential_palettes <- c("Aurora", "Concha", "Frida", "Naturaleza", "Taurus1", "Taurus2", "Tierra")

# Names whether a palette is sequential

#' Sequential Palette Check
#'
#' Checks whether a palette is Sequential.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Alacena},\code{Atentado}, \code{Aurora}, \code{Casita1}, \code{Casita2}, \code{Casita3}, \code{Concha}, \code{Frida}, \code{Huida},  \code{Maiz},  \code{Naturaleza},\code{Ofrenda},
#' \code{Revolucion}, \code{Ronda}, \code{Taurus1}, \code{Taurus2},\code{Tierra}, \code{Vendedora}.
#' @return TRUE/FALSE if palette is sequential
#' @examples
#' sequential.palette("Aurora")
#' @export
sequential.palette <- function(palette_name){

  `%notin%` <- Negate(`%in%`)

  if (palette_name %notin% names(MexPalettes)) {
    stop("Palette does not exist.")
  }

  sequential <- palette_name %in% sequential_palettes

  return(sequential)
}

# MexBrewer palettes for plotting with ggplot2


#' Continuous MexBrewer scales for use with ggplot2
#'
#' Functions \code{\link{scale_color_mex_c}} and \code{\link{scale_fill_mex_c}} for continuous scales enable the use of \code{MexBrewer} colors with \code{ggplot2} continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Alacena}, \code{Atentado}, \code{Aurora}, \code{Casita1}, \code{Casita2}, \code{Casita3}, \code{Concha}, \code{Frida}, \code{Huida},  \code{Maiz},  \code{Naturaleza},\code{Ofrenda},
#' \code{Revolucion}, \code{Ronda}, \code{Taurus1}, \code{Taurus2},\code{Tierra}, \code{Vendedora}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @name ggplot2-scales-continuous
#' @rdname ggplot2-scales-continuous
#' @return A `ScaleContinuous` object that can be added to a `ggplot` object
#' @family color scales
#' @import ggplot2
#' @export
scale_color_mex_c <- function(palette_name, direction=1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  scale_color_gradientn(colors = mex.brewer(palette_name = palette_name, direction = direction, override.order = F),
                        ...)
}

#' @rdname ggplot2-scales-continuous
#' @export
#'
scale_colour_mex_c <- scale_color_mex_c

#' @rdname ggplot2-scales-continuous
#' @export
#'
scale_fill_mex_c <- function(palette_name, direction=1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  scale_fill_gradientn(colors = mex.brewer(palette_name = palette_name, direction = direction, override.order = F),
                       ...)
}

#' Discrete MexBrewer scales for use with ggplot2
#'
#' Functions \code{\link{scale_color_mex_d}} and \code{\link{scale_fill_mex_d}} enable the use of \code{MexBrewer} colors with \code{ggplot2} discrete scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Alacena}, \code{Atentado}, \code{Aurora}, \code{Casita1}, \code{Casita2}, \code{Casita3}, \code{Concha}, \code{Frida}, \code{Huida},  \code{Maiz},  \code{Naturaleza},\code{Ofrenda},
#' \code{Revolucion}, \code{Ronda}, \code{Taurus1}, \code{Taurus2},\code{Tierra}, \code{Vendedora}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @family color scales
#' @name ggplot2-scales-discrete
#' @rdname ggplot2-scales-discrete
#' @return A `ScaleDiscrete` object that can be added to a `ggplot` object
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Species, y=Sepal.Length, fill=Species)) +
#' geom_violin() +
#' scale_fill_mex_d("Aurora")
#' @export
scale_color_mex_d <- function(palette_name, direction=1, override.order=FALSE, ...){
  mex.brewer.disc <- function(palette_name, direction = c(1, -1), override.order=FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- MexPalettes[[palette_name]]
    if (is.null(palette)|is.numeric(palette_name)){
      stop("Palette does not exist.")
    }

    if (direction %notin% c(1, -1)){
      stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
    }

    function(n) if(direction==1 & override.order==FALSE){
      palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)]
    }else if(direction==-1 & override.order==FALSE){
      rev(palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)])
    } else if(direction==1 & override.order==TRUE){
      palette[[1]][1:n]
    } else{
      rev(palette[[1]])[1:n]
    }

  }

  discrete_scale(aesthetics = "colour", scale_name="mex_d",
                 palette = mex.brewer.disc(palette_name=palette_name, direction=direction, override.order=override.order),
                 ...)
}

#' @rdname ggplot2-scales-discrete
#' @export
scale_colour_mex_d <- scale_color_mex_d

#' @rdname ggplot2-scales-discrete
#' @export
scale_fill_mex_d <- function(palette_name, direction = 1, override.order=FALSE, ...){
  mex.brewer.disc <- function(palette_name, direction = c(1, -1), override.order=FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- MexPalettes[[palette_name]]
    if (is.null(palette)|is.numeric(palette_name)){
      stop("Palette does not exist.")
    }

    if (direction %notin% c(1, -1)){
      stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
    }

    function(n) if(direction == 1 & override.order == FALSE){
      palette[[1]][which(palette[[2]] %in% c(1:n) == TRUE)]
    }else if(direction == -1 & override.order == FALSE){
      rev(palette[[1]][which(palette[[2]] %in% c(1:n) == TRUE)])
    } else if(direction == 1 & override.order == TRUE){
      palette[[1]][1:n]
    } else{
      rev(palette[[1]])[1:n]
    }
  }

  discrete_scale(aesthetics = "fill", scale_name="mex_d",
                 palette = mex.brewer.disc(palette_name=palette_name, direction=direction, override.order=override.order),
                 ...)
}

# TO-DO FUNCTION TO DISPLAY ALL PALETTES
