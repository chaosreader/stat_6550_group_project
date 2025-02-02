#' Bee Count Data
#' @description The bee dataset was produced by an AI using digital particle
#'  image velocimetry or DPIV to count the level of bee activity outside a hive
#'  marked R_4_5. These csv files are part of the output of Dr. Sarbajit
#'  Mukherjee during his dissertation at Utah State University. We have received
#'  permission of Dr. Kulyukin to use these files for time series analysis.
#'  The counts correspond to a 28 second period recorded approximately every
#'  15 minutes throughout the day. The monitors shutdown at night and went off
#'  line several times throughout the season leaving significant coverage gaps.
#'  Weather was also collected from Utah State University's Environmental
#'  Observatory to correspond with the same days and times as the bee data.
#'
#' A dataset containing the counts of bees alongside weather.
#'
#' @format A data.table with 9896 rows and 15 variables:
#' \describe{
#'   \item{DATE}{The date and time using the MDT timezone.}
#'   \item{MONTH}{Numeric month.}
#'   \item{DAY}{Day of the month.}
#'   \item{HOUR}{Hour of the day. 0 corresponds to midnight.}
#'   \item{MINUTE}{Minutes within the hour.}
#'   \item{TOTAL_COUNT}{How many bees are present.}
#'   \item{UPWARD}{How many bees are facing upward at the time of the count.}
#'   \item{DOWNWARD}{How many bees are downward at the time of the count.}
#'   \item{LATERAL}{How many bees are lateral at the time of the count.}
#'   \item{WINDOW}{Window (in minutes) of time during the count.}
#'   \item{OVELAP}{Any overlap (in minutes) of consecutive windows.}
#'   \item{TEMP}{Air temperature in degrees F.}
#'   \item{WIND}{Average windspeed in the hour (m/s).}
#'   \item{WET}{Percentage of surface wetness (due to precipitation)}
#'   \item{PRECIP}{Total precipitation (in the hour) in mm.}
#' }
#' @source \url{https://climate.usu.edu/mchd/}
"bee.data"
