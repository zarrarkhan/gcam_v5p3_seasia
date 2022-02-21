# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_Xbatch_liquids_limits_xml_Bangkok_Thailand
#'
#' Construct XML data structure for \code{socioeconomics_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_USA.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_USA.xml} (gcamusa XML).
module_Xbatch_liquids_limits_xml_Bangkok_Thailand <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.CreditMkt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "liquids_limits_Bangkok_Thailand.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.CreditMkt <- get_data(all_data, "L270.CreditMkt")

    PortfolioStd_Bangkok_Thailand <- L270.CreditMkt %>%
      write_to_breakout_regions(data = .,
                                composite_region = "Thailand",
                                disag_regions = c("Bangkok","Rest of Thailand"))

    # ===================================================

    # Produce outputs
    create_xml("liquids_limits_Bangkok_Thailand.xml") %>%
      add_xml_data(PortfolioStd_Bangkok_Thailand, "PortfolioStd") %>%
      add_precursors("L270.CreditMkt") ->
      liquids_limits_Bangkok_Thailand.xml

    return_data(liquids_limits_Bangkok_Thailand.xml)
  } else {
    stop("Unknown command")
  }
}
