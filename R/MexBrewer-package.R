#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' Complete list of palettes
#'
#' Use \code{\link{met.brewer}} to construct palettes.
#'
#' @export
MexPalettes <- list(
  Atentado = list(c("#171e15", "#445452", "#b4bcb4", "#b9afa8", "#e8ccfab", "#f5bc5c", "#cc4c24", "#481904"),
                  c(1, 2, 3, 4, 5, 6, 7, 8),
                  sequential = FALSE),
  Aurora = list(c("#e7c7a8", "#c29786", "#874d4a", "#5f2f2d", "#3b141a", "331718", "#27170b"),
                c(1, 2, 3, 4, 5, 6, 7),
                sequential = TRUE),
  Concha = list(c("#d6d8d0", "#d4d5bf", "#708469", "#506446", "#3d4d34", "#37402b", "#2a3326"),
                c(1, 2, 3, 4, 5, 6, 7),
                sequential = TRUE),
  Frida = list(c("#d6d8d0", "#a4abb0", "#4c6c94", "#435e7f", "#2f415f", "#232c43", "#0b1829"),
               c(1, 2, 3, 4, 5, 6, 7),
               sequential = TRUE)
)


# Function for generating palettes

#' Mex Palette Generator
#'
#' These are a handful of color palettes from Met.
#'
#' @param name Name of Palette. Choices are:
#' \code{Atentado}, \code{Aurora}, \code{Concha}, \code{Frida}
#' @param n Number of desired colors. If number of requested colors is beyond the scope of the palette,
#' colors are automatically interpolated. If n is not provided, the length of the palette is used.
#' @param type Either "continuous" or "discrete". Use continuous if you want to automatically
#' interpolate between colors.
#' @return A vector of colors.
#' @examples
#' mex.brewer("Atentado")
#' met.brewer("Concha", 6)
#' met.brewer("Frida", 10, "continuous")
#' @keywords colors
#' @export
met.brewer <- function(name, n, type = c("discrete", "continuous")) {

  palette <- MexPalettes[[name]]

  if (is.null(palette)|is.numeric(name)){
    stop("Palette does not exist.")
  }

  if (missing(n)) {
    n <- length(palette[[1]])
  }

  if (missing(type)) {
    if(n > length(palette[[1]])){type <- "continuous"}
    else{type <- "discrete"}
  }

  type <- match.arg(type)


  if (type == "discrete" && n > length(palette[[1]])) {
    stop("Number of requested colors greater than what discrete palette can offer, \n  use as continuous instead.")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(palette[[1]])(n),
                discrete = palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)],
  )
  structure(out, class = "palette", name = name)

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
#' Use \code{\link{met.brewer}} to construct palettes.
#'
#' @export
sequential_palettes <- c("Aurora", "Concha", "Frida")


# Names whether a palette is sequential

#' Sequential Palette Check
#'
#' Checks whether a palette is Sequential.
#'
#' @param name Name of Palette. Choices are:
#' \code{Atentado}, \code{Aurora}, \code{Concha}, \code{Frida}
#' @examples
#' sequential.palette("Aurora")
#' @return TRUE/FALSE if palette is sequential
#' @export
sequential.palette <- function(palette.name){

  `%notin%` <- Negate(`%in%`)

  if (palette.name %notin% names(MexPalettes)) {
    stop("Palette does not exist.")
  }

  friendly <- palette.name %in% sequential_palettes

  return(friendly)
}
