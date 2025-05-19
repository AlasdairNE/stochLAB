#' Calculate the total proportion of bird flights at collision risk based
#' on a flight height distribution

#' @description Calculate the expected proportion of bird flights at collision risk
#'   height (i.e. at rotor height, between bottom and top of the rotor) based on the
#'   bird's flight height distribution  (\eqn{Q'_{2R}}).
#'
#' @param d_y Numeric vector with the proportion of birds at height bands across the rotor
#'   disc
#'
#' @param yinc A numeric value, the increment for height bands between the
#'   lowest and highest points of the rotor, expressed as a proportion of rotor
#'   radius (defaults to 0.05).
#'
#' @return The total proportion of birds at collision risk height derived from a flight
#'   height distribution
#'
#' @examples
#'  gen_fhd_dat <- Johnston_Flight_heights_SOSS %>%
#'       dplyr::filter(variable=="Gannet.est") %>%
#'       dplyr::select(height,prop)
#'
#'  gen_fhd <- gen_fhd_dat$prop
#'
#'  d_y <-
#'     get_fhd_rotor(
#'       hub_height = 150,
#'       fhd = gen_fhd,
#'       rotor_radius = 120,
#'       tidal_offset = 2.5,
#'       yinc = 0.05)
#'
#'  prop_chr_fhd <- get_prop_crh_fhd(d_y)
#'
#' @export
get_prop_crh_fhd <- function(d_y, yinc = 0.05) {

  # number of height bands
  n_bands <- length(d_y)

  # integration over the rotor's height bands
  yinc * (d_y[1]/2 + d_y[n_bands]/2 + sum(d_y[c(2:(n_bands-1))]))
}


