#' Single transit collision risk along the chord of the rotor at height band
#' \eqn{y}
#'
#' Calculates the single transit collision risk/probability along the horizontal
#' chord of the rotor at height \eqn{y} via numerical integration.
#'
#' @param x_at_y a vector, sequence of horizontal distances from rotor's
#'   vertical axis to points \eqn{x} along half of the rotor circle, expressed
#'   as the proportion of rotor radius, at height band \eqn{y}
#' @param pcoll_doty a vector, the single transit collision risk at horizontal
#'   distances \eqn{x}, at height band \eqn{y}
#'
#' @return a numeric value, the single transit collision risk along the whole
#'   horizontal chord of the rotor circle at height band \eqn{y}
#'
#' @seealso [get_x_grid()], [get_pcoll_grid()]
#'
#' @examples
#'   rotor_grids <- generate_rotor_grids(yinc = 0.05, xinc = 0.05, chord_prof_5MW)
#'
#'   y_lt <- dim(rotor_grids$r_grid)[1]
#'
#'   pcollxy_grid_up <- get_pcoll_grid(
#'     rotor_grids = rotor_grids,
#'     direction = 1,
#'     rotor_radius = 120,
#'     blade_width = 5,
#'     rotor_speed = 15,
#'     blade_pitch = 15,
#'     flight_type = "flapping",
#'     n_blades = 3,
#'     flight_speed = 13.1,
#'     wing_span = 1.01,
#'     body_lt = 0.85)
#'
#'     risk_up <- rep(NA, y_lt)
#'     for(i in 1:y_lt){
#'       risk_up[i] <- get_risk_y(rotor_grids$x_grid[i, ], pcollxy_grid_up[i, ])
#'     }
#' @export

get_risk_y <- function(x_at_y, pcoll_doty){

  if(length(pcoll_doty) != length(x_at_y)){
    stop("Invalid object size: length of argument vectors 'pcoll_doty' and
         'x_at_y' must be equal")
  }

  # drop NAs
  pcoll_doty <- pcoll_doty[!is.na(pcoll_doty)]
  x_at_y <- x_at_y[!is.na(x_at_y)]

  x_lt <- length(x_at_y)
  x_max <- x_at_y[x_lt]
  i_max <- x_lt - 1

  # numerical integration over the x-axis
  if(x_max == 0){
    return(0)
  }else{
    x_inc <- x_at_y[2] - x_at_y[1]
    risk <- (pcoll_doty[i_max]/2 + pcoll_doty[x_lt]/2) * (x_at_y[x_lt] - x_at_y[i_max])
    risk <- risk + (pcoll_doty[1]/2 + pcoll_doty[i_max]/2) * x_inc

    if(i_max > 2){
      risk <- sum(risk, pcoll_doty[2:(i_max-1)] * x_inc)
    }
  }

  # multiply by two for risk across chord of circle at height y
  risk * 2
}
