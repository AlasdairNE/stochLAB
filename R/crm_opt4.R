#' Number of collisions under model Option 4
#'
#' @description
#' Wrapper function to run the CRM calculations under option 4, i.e.:
#'  \itemize{
#'     \item Using the extended model, which takes into account the distribution
#'     of bird flight heights at risk height (above the minimum and below the
#'     maximum height of the rotor blade)
#'   \item Using site-specific flight height distribution of the species
#'   obtained from site survey data
#'  }
#'
#' @param d_y_surv a vector with the proportion of birds at height bands within
#'   the rotor disc, from a site-specific flight height distribution
#'
#' @inheritParams get_collisions_extended
#'
#' @return A numeric vector, the expected number of collisions per month based
#'    on model option 4
#'
#' @examples
#'
#'  rotor_grids <- generate_rotor_grids(yinc = 0.05, xinc = 0.05, chord_prof_5MW)
#'
#'  site_fhd_dat <- Johnston_Flight_heights_SOSS %>%
#'       dplyr::filter(variable=="Gannet.est") %>%
#'       dplyr::select(height,prop)
#'
#'  site_fhd <- site_fhd_dat$prop
#'
#'  surv_fhd_at_rotor <-
#'     get_fhd_rotor(
#'       hub_height = 150,
#'       fhd = site_fhd,
#'       rotor_radius = 120,
#'       tidal_offset = 2.5,
#'       yinc = 0.05)
#'
#'
#'  flux_fct <- get_flux_factor(
#'       n_turbines = 100,
#'       rotor_radius = 120,
#'       flight_speed = 13.1,
#'       bird_dens = c(1.19,0.85,1.05,1.45,1.41,1.45,1.12,1.45,0.93,0.902,1.06,1.23),
#'       daynight_hrs = Day_Length(52),
#'       noct_activity = 0.5,
#'       macro_avoidance_rate = 0.7,
#'       meso_avoidance_rate = 0.9
#'       )
#'
#' turb_oper <- data.frame(
#'    month = month.abb,
#'    prop_oper = runif(12,0.5,0.8)
#'    )
#' turb_oper_month <- turb_oper$prop_oper
#'
#' crm_opt4(
#'   rotor_grids = rotor_grids,
#'   d_y_surv = surv_fhd_at_rotor,
#'   rotor_radius = 120,
#'   blade_width = 5,
#'   rotor_speed = 15,
#'   blade_pitch = 15,
#'   flight_type = "flapping",
#'   wing_span = 1.01,
#'   flight_speed = 13.1,
#'   body_lt = 0.85,
#'   n_blades = 3,
#'   prop_upwind = 0.5,
#'   avoidance_rate = 0.981,
#'   flux_factor = flux_fct,
#'   mth_prop_oper = turb_oper_month,
#'   lac_factor = 0.9998299
#'   )
#'
#' @export
crm_opt4 <- function(rotor_grids,
                     d_y_surv,
                     rotor_radius,
                     blade_width,
                     rotor_speed,
                     blade_pitch,
                     flight_type,
                     wing_span,
                     flight_speed,
                     body_lt,
                     n_blades,
                     prop_upwind,
                     avoidance_rate,
                     flux_factor,
                     mth_prop_oper,
                     lac_factor){

  # number of collisions for the extended model...
  get_collisions_extended(
    rotor_grids,
    d_y = d_y_surv, # ... with site-specific FHD = Option 4!
    rotor_radius,
    blade_width,
    rotor_speed,
    blade_pitch,
    flight_type,
    wing_span,
    flight_speed,
    body_lt, n_blades,
    prop_upwind,
    avoidance_rate,
    flux_factor,
    mth_prop_oper,
    lac_factor
  )
}
