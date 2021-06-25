# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_breakout_Xbatch_socioeconomics_xml_KualaLumpur_Malaysia
#'
#' Construct XML data structure for \code{socioeconomics_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_USA.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_USA.xml} (gcamusa XML).
module_breakout_Xbatch_socioeconomics_xml_KualaLumpur_Malaysia <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X201.Pop_KualaLumpur_Malaysia",
             "X201.BaseGDP_KualaLumpur_Malaysia",
             "X201.LaborForceFillout_KualaLumpur_Malaysia",
             "X201.LaborProductivity_KualaLumpur_Malaysia"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_KualaLumpur_Malaysia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X201.Pop_KualaLumpur_Malaysia <- get_data(all_data, "X201.Pop_KualaLumpur_Malaysia")
    X201.BaseGDP_KualaLumpur_Malaysia <- get_data(all_data, "X201.BaseGDP_KualaLumpur_Malaysia")
    X201.LaborForceFillout_KualaLumpur_Malaysia <- get_data(all_data, "X201.LaborForceFillout_KualaLumpur_Malaysia")
    X201.LaborProductivity_KualaLumpur_Malaysia <- get_data(all_data, "X201.LaborProductivity_KualaLumpur_Malaysia")

    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_KualaLumpur_Malaysia.xml") %>%
      add_xml_data(X201.Pop_KualaLumpur_Malaysia, "Pop") %>%
      add_xml_data(X201.BaseGDP_KualaLumpur_Malaysia, "BaseGDP") %>%
      add_xml_data(X201.LaborForceFillout_KualaLumpur_Malaysia, "LaborForceFillout") %>%
      add_xml_data(X201.LaborProductivity_KualaLumpur_Malaysia, "LaborProductivity") %>%
      add_precursors("X201.Pop_KualaLumpur_Malaysia",
                     "X201.BaseGDP_KualaLumpur_Malaysia",
                     "X201.LaborForceFillout_KualaLumpur_Malaysia",
                     "X201.LaborProductivity_KualaLumpur_Malaysia") ->
      socioeconomics_KualaLumpur_Malaysia.xml

    return_data(socioeconomics_KualaLumpur_Malaysia.xml)
  } else {
    stop("Unknown command")
  }
}
