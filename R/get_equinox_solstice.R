# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

# This function is based on the calculations presented by
# Meeus (1991, p. 165-167). See the reference below for more information.
#
# Meeus, J. (1991). Astronomical algorithms. Willmann-Bell.

# Check with:
# <https://data.giss.nasa.gov/cgi-bin/ar5/srevents.cgi> or
# <https://www.timeanddate.com/calendar/seasons.html?year=2000>

# # To do
#
# - Adjust time for better precision.

# See Meeus (1991, p. 165-170) for more information.
get_equinox_solstice <- function(year, month = "march", tz = "UTC") {
  month_choices <- c("march", "june", "september", "december")

  checkmate::assert_int(year)
  checkmate::assert_choice(month, month_choices)
  checkmate::assert_choice(tz, OlsonNames())

  # Only for the years +1000 to +3000 (Table 26.B).
  # Check Meeus (1991, p. 165) for more implementations.
  # Meeus, J. (1991). Astronomical algorithms. Willmann-Bell.

  if (year < 1000 || year > 3000) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_red('year')}} must be between ",
      "{.strong 1000} and {.strong 3000}."
    ))
  }

  y <- (year - 2000) / 1000

  if (month == "march") { # Equinox
    jde_o <- (2451623.80984) + (365242.37404 * y) + (0.05169 * (y^2)) -
      (0.00411 * (y^3)) - (0.00057 * (y^4))
  } else if (month == "june") { # Solstice
    jde_o <- (2451716.56767) + (365241.62603 * y) + (0.00325 * (y^2)) +
      (0.00888 * (y^3)) - (0.00030 * (y^4))
  } else if (month == "september") { # Equinox
    jde_o <- (2451810.21715) + (365242.01767 * y) - (0.11575 * (y^2)) +
      (0.00337 * (y^3)) + (0.00078 * (y^4))
  } else if (month == "december") { # Solstice
    jde_o <- (2451900.05952) + (365242.74049 * y) - (0.06223 * (y^2)) -
      (0.00823 * (y^3)) + (0.00032 * (y^4))
  }

  t <- (jde_o - 2451545.0) / 36525
  w <- 35999.373 * t - 2.47
  dlambda <- 1 + 0.0334 * cos(w) + 0.0007 * cos(2 * w)

  s = sum(
    485 * cos(324.96 + 1934.136 * t),
    203 * cos(337.23 + 32964.467 * t),
    199 * cos(342.08 + 20.186 * t),
    182 * cos(27.85 + 445267.112 * t),
    156 * cos(73.14 + 45036.886 * t),
    136 * cos(171.52 + 22518.443 * t),
    77 * cos(222.54 + 65928.934 * t),
    74 * cos(296.72 + 3034.906 * t),
    70 * cos(243.58 + 9037.513 * t),
    58 * cos(119.81 + 33718.147 * t),
    52 * cos(297.17 + 150.678 * t),
    50 * cos(21.02 + 2281.226 * t),
    45 * cos(247.54 + 29929.562 * t),
    44 * cos(325.15 + 31555.956 * t),
    29 * cos(60.93 + 4443.417 * t),
    18 * cos(155.12 + 67555.328 * t),
    17 * cos(288.79 + 4562.452 * t),
    16 * cos(198.04 + 62894.029 * t),
    14 * cos(199.76 + 31436.921 * t),
    12 * cos(95.39 + 14577.848 * t),
    12 * cos(287.11 + 31931.756 * t),
    12 * cos(320.81 + 34777.259 * t),
    9 * cos(227.73 + 1222.114 * t),
    8 * cos(15.45 + 16859.074 * t)
  )

  jde <- jde_o + ((0.00001 * s) / dlambda)

  jde |>
    jde_to_posixct(as_list = FALSE) |>
    lubridate::with_tz(tzone = tz)
}

# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

# See Meeus (1991, p. 63 & p. 171-175) for more information.
jde_to_posixct <- function(jde, as_list = FALSE) {
  checkmate::assert_number(jde, lower = 0)
  checkmate::assert_flag(as_list)

  jde_ <- jde + 0.5
  z <- trunc(jde_)
  f <- jde_ - z

  if (z < 2299161) {
    a <- z
  } else {
    alpha <- trunc((z - 1867216.25) / 36524.25)
    a <- z + 1 + alpha - trunc(alpha / 4)
  }

  b <- a + 1524
  c <- trunc((b - 122.1) / 365.25)
  d <- trunc(365.25 * c)
  e <- trunc((b - d) / 30.6001)

  day <- b - d - trunc(30.6001 * e) + f

  if (e < 14) {
    month <- e - 1
  } else if (e == 14 || e == 15) {
    month <- e - 13
  }

  if (month > 2) {
    year <- c - 4716
  } else if (month == 1 || month == 2) {
    year <- c - 4715
  }

  # tau <- (jde - 2451545.0) / 365250 # (24.1)
  #
  # l_o <- (280.4664567) + (36000.76982779 * tau) + (0.03032028 * tau^2) +
  #   (tau^3 / 49931) - (tau^4 / 15299) - (tau^5 / 1988000)
  #
  # epsilon_o <- as.numeric(
  #   measurements::conv_unit(
  #     "23 26 11.448",
  #     from = "deg_min_sec",
  #     to = "dec_deg"
  #   )
  # ) -
  #   as.numeric(
  #     measurements::conv_unit(
  #       "0 0 46.8150",
  #       from = "deg_min_sec",
  #       to = "dec_deg"
  #     )
  #   ) * tau -
  #   as.numeric(
  #     measurements::conv_unit(
  #       "0 0 0.00059",
  #       from = "deg_min_sec",
  #       to = "dec_deg"
  #     )
  #   ) * tau^2 +
  #   as.numeric(
  #     measurements::conv_unit(
  #       "0 0 0.001813",
  #       from = "deg_min_sec",
  #       to = "dec_deg"
  #     )
  #   ) * tau^3

  frac_day_secs <- lubridate::ddays(day - trunc(day)) |> as.numeric()

  hour <- frac_day_secs / 3600
  min <- (hour - trunc(hour)) * 60
  sec <- (min - trunc(min)) * 60

  if (isTRUE(as_list)) {
    list(
      year = year,
      month = month,
      day = day,
      hour = trunc(hour),
      min = trunc(min),
      sec = sec
    )
  } else {
    lubridate::make_datetime(
      year = year,
      month = month,
      day = trunc(day),
      hour = trunc(hour),
      min = trunc(min),
      sec = sec,
      tz = "UTC"
    )
  }
}
