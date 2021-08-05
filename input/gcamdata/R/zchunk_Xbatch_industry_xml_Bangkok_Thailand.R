# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_industry_xml_Bangkok_Thailand
#'
#' Construct XML data structure for \code{industry_Bangkok_Thailand}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_Bangkok_Thailand}. The corresponding file in the
#' original data system was \code{batch_industry_Bangkok_Thailand} (energy XML).
module_energy_Xbatch_industry_xml_Bangkok_Thailand<- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X232.DeleteFinalDemand_ind_Bangkok_Thailand",
             "X232.DeleteSupplysector_ind_Bangkok_Thailand",
             "X232.Supplysector_ind_Bangkok_Thailand",
             "X232.FinalEnergyKeyword_ind_Bangkok_Thailand",
             "X232.SubsectorLogit_ind_Bangkok_Thailand",
             "X232.SubsectorShrwtFllt_ind_Bangkok_Thailand",
             "X232.SubsectorInterp_ind_Bangkok_Thailand",
             "X232.FuelPrefElast_indenergy_Bangkok_Thailand",
             "X232.StubTech_ind_Bangkok_Thailand",
             "X232.StubTechInterp_ind_Bangkok_Thailand",
             "X232.StubTechCalInput_indenergy_Bangkok_Thailand",
             "X232.StubTechCalInput_indfeed_Bangkok_Thailand",
             "X232.StubTechProd_industry_Bangkok_Thailand",
             "X232.StubTechCoef_industry_Bangkok_Thailand",
             "X232.PerCapitaBased_ind_Bangkok_Thailand",
             "X232.IncomeElasticity_ind_gssp2_Bangkok_Thailand",
             "X232.PriceElasticity_ind_Bangkok_Thailand",
             "X232.BaseService_ind_Bangkok_Thailand",
             "X232.UnlimitRsrc_Bangkok_Thailand",
             "X232.UnlimitRsrcPrice_Bangkok_Thailand",
             "X232.Supplysector_urb_indproc_Bangkok_Thailand",
             "X232.SubsectorLogit_urb_ind_Bangkok_Thailand",
             "X232.SubsectorShrwtFllt_urb_ind_Bangkok_Thailand",
             "X232.SubsectorInterp_urb_ind_Bangkok_Thailand",
             "X232.StubTech_urb_ind_Bangkok_Thailand",
             "X232.StubTechCoef_indproc_Bangkok_Thailand",
             "X232.MAC_indproc_Bangkok_Thailand",
             "X232.fgas_all_units_ind_Bangkok_Thailand",
             "X232.nonghg_max_reduction_ind_Bangkok_Thailand",
             "X232.nonghg_steepness_ind_Bangkok_Thailand",
             "X232.hfc_future_ind_Bangkok_Thailand",
             "X232.nonco2_max_reduction_indproc_Bangkok_Thailand",
             "X232.nonco2_steepness_indproc_Bangkok_Thailand",
             "X232.pol_emissions_ind_Bangkok_Thailand",
             "X232.ghg_emissions_ind_Bangkok_Thailand",
             "X232.StubTechMarket_ind_Bangkok_Thailand",
             "X232.StubTechSecMarket_ind_Bangkok_Thailand",
             "X232.nonco2_indproc_Bangkok_Thailand"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_Bangkok_Thailand.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X232.DeleteFinalDemand_ind_Bangkok_Thailand<- get_data(all_data, "X232.DeleteFinalDemand_ind_Bangkok_Thailand")
    X232.DeleteSupplysector_ind_Bangkok_Thailand<- get_data(all_data, "X232.DeleteSupplysector_ind_Bangkok_Thailand")
    X232.Supplysector_ind_Bangkok_Thailand<- get_data(all_data, "X232.Supplysector_ind_Bangkok_Thailand")
    X232.FinalEnergyKeyword_ind_Bangkok_Thailand<- get_data(all_data, "X232.FinalEnergyKeyword_ind_Bangkok_Thailand")
    X232.SubsectorLogit_ind_Bangkok_Thailand<- get_data(all_data, "X232.SubsectorLogit_ind_Bangkok_Thailand")
    X232.SubsectorShrwtFllt_ind_Bangkok_Thailand<- get_data(all_data, "X232.SubsectorShrwtFllt_ind_Bangkok_Thailand")
    X232.SubsectorInterp_ind_Bangkok_Thailand<- get_data(all_data, "X232.SubsectorInterp_ind_Bangkok_Thailand")
    X232.FuelPrefElast_indenergy_Bangkok_Thailand<- get_data(all_data, "X232.FuelPrefElast_indenergy_Bangkok_Thailand")
    X232.StubTech_ind_Bangkok_Thailand<- get_data(all_data, "X232.StubTech_ind_Bangkok_Thailand")
    X232.StubTechInterp_ind_Bangkok_Thailand<- get_data(all_data, "X232.StubTechInterp_ind_Bangkok_Thailand")
    X232.StubTechCalInput_indenergy_Bangkok_Thailand<- get_data(all_data, "X232.StubTechCalInput_indenergy_Bangkok_Thailand")
    X232.StubTechCalInput_indfeed_Bangkok_Thailand<- get_data(all_data, "X232.StubTechCalInput_indfeed_Bangkok_Thailand")
    X232.StubTechProd_industry_Bangkok_Thailand<- get_data(all_data, "X232.StubTechProd_industry_Bangkok_Thailand")
    X232.StubTechCoef_industry_Bangkok_Thailand<- get_data(all_data, "X232.StubTechCoef_industry_Bangkok_Thailand")
    X232.PerCapitaBased_ind_Bangkok_Thailand<- get_data(all_data, "X232.PerCapitaBased_ind_Bangkok_Thailand")
    X232.IncomeElasticity_ind_gssp2_Bangkok_Thailand<- get_data(all_data, "X232.IncomeElasticity_ind_gssp2_Bangkok_Thailand")
    X232.PriceElasticity_ind_Bangkok_Thailand<- get_data(all_data, "X232.PriceElasticity_ind_Bangkok_Thailand")
    X232.BaseService_ind_Bangkok_Thailand<- get_data(all_data, "X232.BaseService_ind_Bangkok_Thailand")
    X232.UnlimitRsrc_Bangkok_Thailand<- get_data(all_data, "X232.UnlimitRsrc_Bangkok_Thailand")
    X232.UnlimitRsrcPrice_Bangkok_Thailand<- get_data(all_data, "X232.UnlimitRsrcPrice_Bangkok_Thailand")
    X232.Supplysector_urb_indproc_Bangkok_Thailand<- get_data(all_data, "X232.Supplysector_urb_indproc_Bangkok_Thailand")
    X232.SubsectorLogit_urb_ind_Bangkok_Thailand<- get_data(all_data, "X232.SubsectorLogit_urb_ind_Bangkok_Thailand")
    X232.SubsectorShrwtFllt_urb_ind_Bangkok_Thailand<- get_data(all_data, "X232.SubsectorShrwtFllt_urb_ind_Bangkok_Thailand")
    X232.SubsectorInterp_urb_ind_Bangkok_Thailand<- get_data(all_data, "X232.SubsectorInterp_urb_ind_Bangkok_Thailand")
    X232.StubTech_urb_ind_Bangkok_Thailand<- get_data(all_data, "X232.StubTech_urb_ind_Bangkok_Thailand")
    X232.StubTechCoef_indproc_Bangkok_Thailand<- get_data(all_data, "X232.StubTechCoef_indproc_Bangkok_Thailand")
    X232.MAC_indproc_Bangkok_Thailand<- get_data(all_data, "X232.MAC_indproc_Bangkok_Thailand")
    X232.fgas_all_units_ind_Bangkok_Thailand<- get_data(all_data, "X232.fgas_all_units_ind_Bangkok_Thailand")
    X232.nonghg_max_reduction_ind_Bangkok_Thailand<- get_data(all_data, "X232.nonghg_max_reduction_ind_Bangkok_Thailand")
    X232.nonghg_steepness_ind_Bangkok_Thailand<- get_data(all_data, "X232.nonghg_steepness_ind_Bangkok_Thailand")
    X232.hfc_future_ind_Bangkok_Thailand<- get_data(all_data, "X232.hfc_future_ind_Bangkok_Thailand")
    X232.nonco2_max_reduction_indproc_Bangkok_Thailand<- get_data(all_data, "X232.nonco2_max_reduction_indproc_Bangkok_Thailand")
    X232.nonco2_steepness_indproc_Bangkok_Thailand<- get_data(all_data, "X232.nonco2_steepness_indproc_Bangkok_Thailand")
    X232.pol_emissions_ind_Bangkok_Thailand<- get_data(all_data, "X232.pol_emissions_ind_Bangkok_Thailand")
    X232.ghg_emissions_ind_Bangkok_Thailand<- get_data(all_data, "X232.ghg_emissions_ind_Bangkok_Thailand")
    X232.StubTechMarket_ind_Bangkok_Thailand<- get_data(all_data, "X232.StubTechMarket_ind_Bangkok_Thailand")
    X232.StubTechSecMarket_ind_Bangkok_Thailand<- get_data(all_data, "X232.StubTechSecMarket_ind_Bangkok_Thailand")
    X232.nonco2_indproc_Bangkok_Thailand<- get_data(all_data, "X232.nonco2_indproc_Bangkok_Thailand")

    # ===================================================

    # Produce outputs
    create_xml("industry_Bangkok_Thailand.xml") %>%
      add_xml_data(X232.DeleteFinalDemand_ind_Bangkok_Thailand, "DeleteFinalDemand") %>%
      add_xml_data(X232.DeleteSupplysector_ind_Bangkok_Thailand, "DeleteSupplysector") %>%
      add_logit_tables_xml(X232.Supplysector_ind_Bangkok_Thailand, "Supplysector") %>%
      add_xml_data(X232.FinalEnergyKeyword_ind_Bangkok_Thailand, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(X232.SubsectorLogit_ind_Bangkok_Thailand, "SubsectorLogit") %>%
      add_xml_data(X232.SubsectorShrwtFllt_ind_Bangkok_Thailand, "SubsectorShrwtFllt") %>%
      add_xml_data(X232.SubsectorInterp_ind_Bangkok_Thailand, "SubsectorInterp") %>%
      add_xml_data(X232.FuelPrefElast_indenergy_Bangkok_Thailand, "FuelPrefElast") %>%
      add_xml_data(X232.StubTech_ind_Bangkok_Thailand, "StubTech") %>%
      add_xml_data(X232.StubTechInterp_ind_Bangkok_Thailand, "StubTechInterp") %>%
      add_xml_data(X232.StubTechCalInput_indenergy_Bangkok_Thailand, "StubTechCalInput") %>%
      add_xml_data(X232.StubTechCalInput_indfeed_Bangkok_Thailand, "StubTechCalInput") %>%
      add_xml_data(X232.StubTechProd_industry_Bangkok_Thailand, "StubTechProd") %>%
      add_xml_data(X232.StubTechCoef_industry_Bangkok_Thailand, "StubTechCoef") %>%
      add_xml_data(X232.PerCapitaBased_ind_Bangkok_Thailand, "PerCapitaBased") %>%
      add_xml_data(X232.IncomeElasticity_ind_gssp2_Bangkok_Thailand, "IncomeElasticity") %>%
      add_xml_data(X232.PriceElasticity_ind_Bangkok_Thailand, "PriceElasticity") %>%
      add_xml_data(X232.BaseService_ind_Bangkok_Thailand, "BaseService") %>%
      add_xml_data(X232.UnlimitRsrc_Bangkok_Thailand, "UnlimitRsrc") %>%
      add_xml_data(X232.UnlimitRsrcPrice_Bangkok_Thailand, "UnlimitRsrcPrice") %>%
      add_logit_tables_xml(X232.Supplysector_urb_indproc_Bangkok_Thailand, "Supplysector") %>%
      add_logit_tables_xml(X232.SubsectorLogit_urb_ind_Bangkok_Thailand, "SubsectorLogit") %>%
      add_xml_data(X232.SubsectorShrwtFllt_urb_ind_Bangkok_Thailand, "SubsectorShrwtFllt") %>%
      add_xml_data(X232.SubsectorInterp_urb_ind_Bangkok_Thailand, "SubsectorInterp") %>%
      add_xml_data(X232.StubTech_urb_ind_Bangkok_Thailand, "StubTech") %>%
      add_xml_data(X232.StubTechCoef_indproc_Bangkok_Thailand, "StubTechCoef") %>%
      add_xml_data(X232.MAC_indproc_Bangkok_Thailand, "MAC") %>%
      add_xml_data(X232.fgas_all_units_ind_Bangkok_Thailand, "StubTechEmissUnits") %>%
      add_xml_data(X232.nonghg_max_reduction_ind_Bangkok_Thailand, "GDPCtrlMax") %>%
      add_xml_data(X232.nonghg_steepness_ind_Bangkok_Thailand, "GDPCtrlSteep") %>%
      add_xml_data(X232.hfc_future_ind_Bangkok_Thailand, "OutputEmissCoeff") %>%
      add_xml_data(X232.nonco2_max_reduction_indproc_Bangkok_Thailand, "GDPCtrlMax") %>%
      add_xml_data(X232.nonco2_steepness_indproc_Bangkok_Thailand, "GDPCtrlSteep") %>%
      add_xml_data(X232.pol_emissions_ind_Bangkok_Thailand, "InputEmissions") %>%
      add_xml_data(X232.ghg_emissions_ind_Bangkok_Thailand, "InputEmissions") %>%
      add_xml_data(X232.StubTechMarket_ind_Bangkok_Thailand, "StubTechMarket") %>%
      add_xml_data(X232.StubTechSecMarket_ind_Bangkok_Thailand, "StubTechSecMarket") %>%
      add_xml_data(X232.nonco2_indproc_Bangkok_Thailand, "StbTechOutputEmissions") %>%
      add_precursors("X232.DeleteFinalDemand_ind_Bangkok_Thailand",
                     "X232.DeleteSupplysector_ind_Bangkok_Thailand",
                     "X232.Supplysector_ind_Bangkok_Thailand",
                     "X232.FinalEnergyKeyword_ind_Bangkok_Thailand",
                     "X232.SubsectorLogit_ind_Bangkok_Thailand",
                     "X232.SubsectorShrwtFllt_ind_Bangkok_Thailand",
                     "X232.SubsectorInterp_ind_Bangkok_Thailand",
                     "X232.FuelPrefElast_indenergy_Bangkok_Thailand",
                     "X232.StubTech_ind_Bangkok_Thailand",
                     "X232.StubTechInterp_ind_Bangkok_Thailand",
                     "X232.StubTechCalInput_indenergy_Bangkok_Thailand",
                     "X232.StubTechCalInput_indfeed_Bangkok_Thailand",
                     "X232.StubTechProd_industry_Bangkok_Thailand",
                     "X232.StubTechCoef_industry_Bangkok_Thailand",
                     "X232.PerCapitaBased_ind_Bangkok_Thailand",
                     "X232.IncomeElasticity_ind_gssp2_Bangkok_Thailand",
                     "X232.PriceElasticity_ind_Bangkok_Thailand",
                     "X232.BaseService_ind_Bangkok_Thailand",
                     "X232.UnlimitRsrc_Bangkok_Thailand",
                     "X232.UnlimitRsrcPrice_Bangkok_Thailand",
                     "X232.Supplysector_urb_indproc_Bangkok_Thailand",
                     "X232.SubsectorLogit_urb_ind_Bangkok_Thailand",
                     "X232.SubsectorShrwtFllt_urb_ind_Bangkok_Thailand",
                     "X232.SubsectorInterp_urb_ind_Bangkok_Thailand",
                     "X232.StubTech_urb_ind_Bangkok_Thailand",
                     "X232.StubTechCoef_indproc_Bangkok_Thailand",
                     "X232.MAC_indproc_Bangkok_Thailand",
                     "X232.fgas_all_units_ind_Bangkok_Thailand",
                     "X232.nonghg_max_reduction_ind_Bangkok_Thailand",
                     "X232.nonghg_steepness_ind_Bangkok_Thailand",
                     "X232.hfc_future_ind_Bangkok_Thailand",
                     "X232.nonco2_max_reduction_indproc_Bangkok_Thailand",
                     "X232.nonco2_steepness_indproc_Bangkok_Thailand",
                     "X232.pol_emissions_ind_Bangkok_Thailand",
                     "X232.ghg_emissions_ind_Bangkok_Thailand",
                     "X232.StubTechMarket_ind_Bangkok_Thailand",
                     "X232.StubTechSecMarket_ind_Bangkok_Thailand",
                     "X232.nonco2_indproc_Bangkok_Thailand") ->
      industry_Bangkok_Thailand.xml

    return_data(industry_Bangkok_Thailand.xml)
  } else {
    stop("Unknown command")
  }
}
