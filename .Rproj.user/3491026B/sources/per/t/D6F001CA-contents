################################################################################
#' Read CSV file
#'
#' This function reads a csv file and returns a tibble. The function
#' returns an Error if the file cannot be found.
#'
#' @param filename Path to the csv file
#'
#' @return A tibble generated from the csv file
#'
#' @examples
#' \dontrun{
#' accident_2015 <- fars_read("data/accident_2013.csv.bz2)
#' }
#'
#' @importFrom readr read_csv
#' @importFrom deplyr tbl_df
#'
#' @export


fars_read <- function(filename) {
      if(!file.exists(filename))
            stop("file '", filename, "' does not exist")
      data <- suppressMessages({
            readr::read_csv(filename, progress = FALSE)
      })
      dplyr::tbl_df(data)
}

################################################################################
#' Creates a filename
#'
#' This function takes a year as numeric input and returns a filname
#' in the format 'accident_year.csv.bz2'.
#'
#' @param year The year the data was obtained
#'
#' @return A string in the format 'accident_year.csv.bz2'
#' that can be used as filename
#'
#' @examples
#' \dontrun{
#' makefilename(2014)
#' }
#'
#' @export

make_filename <- function(year) {
      if(is.na(as.integer(year))){
            stop("invalid year")
      }
      year <- as.integer(year)
      sprintf("accident_%d.csv.bz2", year)
}

################################################################################
#' Read Fars data for given years
#'
#' This function takes one or more years as an input
#' and returns a list of tibbles which for each year lists
#' the MONTH and year.
#' Returns an error if data for a given year is not found.
#'
#' @param years A list or vector of years.
#'
#' @return Returns a List of tibbles with MONTH and year of each
#' given year
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#'
#' @importFrom deplyr mutate
#' @importFrom deplyr select
#'
#' @export

fars_read_years <- function(years) {
      lapply(years, function(year) {
            file <- make_filename(year)
            tryCatch({
                  dat <- fars_read(file)
                  dplyr::mutate(dat, year = year) %>%
                        dplyr::select(MONTH, year)
            }, error = function(e) {
                  warning("invalid year: ", year)
                  return(NULL)
            })
      })
}

################################################################################
#' Summarizes Fars Data for each year
#'
#' This function takes one or more years as an input
#' and returns a tibble which summarizes the cases for each
#' given year and month.
#'
#'
#' @param years A list or vector of years.
#'
#' @return Returns a tibble with summarizes the Fars data for each
#' given year and month.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' }
#'
#' @importFrom deplyr group_by
#' @importFrom deplyr summarize
#' @importFrom tidyr spread
#'
#' @export

fars_summarize_years <- function(years) {
      dat_list <- fars_read_years(years)
      dplyr::bind_rows(dat_list) %>%
            dplyr::group_by(year, MONTH) %>%
            dplyr::summarize(n = n()) %>%
            tidyr::spread(year, n)
}

################################################################################
#' Plots the Fars data on a US stat map
#'
#' This function takes one or more year and a US
#' state.num as input and plots the Fars data for the
#' given years on the states map. If a year or the state number
#' does not exist in the data the function will return an error.
#'
#' @param year The year the data was obtained.
#' @param state.num A number representing a US state.
#'
#' @return Returns a map with the Fars data of the given year
#' plotted to a given US states map.
#'
#' @examples
#' \dontrun{
#' fars_map_state(45,2013)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export

fars_map_state <- function(state.num, year) {
      filename <- make_filename(year)
      data <- fars_read(filename)
      state.num <- as.integer(state.num)

      if(!(state.num %in% unique(data$STATE)))
            stop("invalid STATE number: ", state.num)
      data.sub <- dplyr::filter(data, STATE == state.num)
      if(nrow(data.sub) == 0L) {
            message("no accidents to plot")
            return(invisible(NULL))
      }
      is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
      is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
      with(data.sub, {
            maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                      xlim = range(LONGITUD, na.rm = TRUE))
            graphics::points(LONGITUD, LATITUDE, pch = 46)
      })
}
