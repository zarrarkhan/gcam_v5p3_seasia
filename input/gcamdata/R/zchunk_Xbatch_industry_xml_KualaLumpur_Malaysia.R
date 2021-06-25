# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_industry_xml_KualaLumpur_Malaysia
#'
#' Construct XML data structure for \code{industry_KualaLumpur_Malaysia}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_KualaLumpur_Malaysia}. The corresponding file in the
#' original data system was \code{batch_industry_KualaLumpur_Malaysia} (energy XML).
module_energy_Xbatch_industry_xml_KualaLumpur_Malaysia<- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X232.DeleteFinalDemand_ind_KualaLumpur_Malaysia",
             "X232.DeleteSupplysector_ind_KualaLumpur_Malaysia",
             "X232.Supplysector_ind_KualaLumpur_Malaysia",
             "X232.FinalEnergyKeyword_ind_KualaLumpur_Malaysia",
             "X232.SubsectorLogit_ind_KualaLumpur_Malaysia",
             "X232.SubsectorShrwtFllt_ind_KualaLumpur_Malaysia",
             "X232.SubsectorInterp_ind_KualaLumpur_Malaysia",
             "X232.FuelPrefElast_indenergy_KualaLumpur_Malaysia",
             "X232.StubTech_ind_KualaLumpur_Malaysia",
             "X232.StubTechInterp_ind_KualaLumpur_Malaysia",
             "X232.StubTechCalInput_indenergy_KualaLumpur_Malaysia",
             "X232.StubTechCalInput_indfeed_KualaLumpur_Malaysia",
             "X232.StubTechProd_industry_KualaLumpur_Malaysia",
             "X232.StubTechCoef_industry_KualaLumpur_Malaysia",
             "X232.PerCapitaBased_ind_KualaLumpur_Malaysia",
             "X232.IncomeElasticity_ind_gssp2_KualaLumpur_Malaysia",
             "X232.PriceElasticity_ind_KualaLumpur_Malaysia",
             "X232.BaseService_ind_KualaLumpur_Malaysia",
             "X232.UnlimitRsrc_KualaLumpur_Malaysia",
             "X232.UnlimitRsrcPrice_KualaLumpur_Malaysia",
             "X232.Supplysector_urb_indproc_KualaLumpur_Malaysia",
             "X232.SubsectorLogit_urb_ind_KualaLumpur_Malaysia",
             "X232.SubsectorShrwtFllt_urb_ind_KualaLumpur_Malaysia",
             "X232.SubsectorInterp_urb_ind_KualaLumpur_Malaysia",
             "X232.StubTech_urb_ind_KualaLumpur_Malaysia",
             "X232.StubTechCoef_indproc_KualaLumpur_Malaysia",
             "X232.MAC_indproc_KualaLumpur_Malaysia",
             "X232.fgas_all_units_ind_KualaLumpur_Malaysia",
             "X232.nonghg_max_reduction_ind_KualaLumpur_Malaysia",
             "X232.nonghg_steepness_ind_KualaLumpur_Malaysia",
             "X232.hfc_future_ind_KualaLumpur_Malaysia",
             "X232.nonco2_max_reduction_indproc_KualaLumpur_Malaysia",
             "X232.nonco2_steepness_indproc_KualaLumpur_Malaysia",
             "X232.pol_emissions_ind_KualaLumpur_Malaysia",
             "X232.ghg_emissions_ind_KualaLumpur_Malaysia",
             "X232.StubTechMarket_ind_KualaLumpur_Malaysia",
             "X232.StubTechSecMarket_ind_KualaLumpur_Malaysia",
             "X232.nonco2_indproc_KualaLumpur_Malaysia"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_KualaLumpur_Malaysia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X232.DeleteFinalDemand_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.DeleteFinalDemand_ind_KualaLumpur_Malaysia")
    X232.DeleteSupplysector_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.DeleteSupplysector_ind_KualaLumpur_Malaysia")
    X232.Supplysector_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.Supplysector_ind_KualaLumpur_Malaysia")
    X232.FinalEnergyKeyword_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.FinalEnergyKeyword_ind_KualaLumpur_Malaysia")
    X232.SubsectorLogit_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.SubsectorLogit_ind_KualaLumpur_Malaysia")
    X232.SubsectorShrwtFllt_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.SubsectorShrwtFllt_ind_KualaLumpur_Malaysia")
    X232.SubsectorInterp_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.SubsectorInterp_ind_KualaLumpur_Malaysia")
    X232.FuelPrefElast_indenergy_KualaLumpur_Malaysia<- get_data(all_data, "X232.FuelPrefElast_indenergy_KualaLumpur_Malaysia")
    X232.StubTech_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTech_ind_KualaLumpur_Malaysia")
    X232.StubTechInterp_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTechInterp_ind_KualaLumpur_Malaysia")
    X232.StubTechCalInput_indenergy_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTechCalInput_indenergy_KualaLumpur_Malaysia")
    X232.StubTechCalInput_indfeed_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTechCalInput_indfeed_KualaLumpur_Malaysia")
    X232.StubTechProd_industry_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTechProd_industry_KualaLumpur_Malaysia")
    X232.StubTechCoef_industry_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTechCoef_industry_KualaLumpur_Malaysia")
    X232.PerCapitaBased_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.PerCapitaBased_ind_KualaLumpur_Malaysia")
    X232.IncomeElasticity_ind_gssp2_KualaLumpur_Malaysia<- get_data(all_data, "X232.IncomeElasticity_ind_gssp2_KualaLumpur_Malaysia")
    X232.PriceElasticity_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.PriceElasticity_ind_KualaLumpur_Malaysia")
    X232.BaseService_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.BaseService_ind_KualaLumpur_Malaysia")
    X232.UnlimitRsrc_KualaLumpur_Malaysia<- get_data(all_data, "X232.UnlimitRsrc_KualaLumpur_Malaysia")
    X232.UnlimitRsrcPrice_KualaLumpur_Malaysia<- get_data(all_data, "X232.UnlimitRsrcPrice_KualaLumpur_Malaysia")
    X232.Supplysector_urb_indproc_KualaLumpur_Malaysia<- get_data(all_data, "X232.Supplysector_urb_indproc_KualaLumpur_Malaysia")
    X232.SubsectorLogit_urb_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.SubsectorLogit_urb_ind_KualaLumpur_Malaysia")
    X232.SubsectorShrwtFllt_urb_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.SubsectorShrwtFllt_urb_ind_KualaLumpur_Malaysia")
    X232.SubsectorInterp_urb_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.SubsectorInterp_urb_ind_KualaLumpur_Malaysia")
    X232.StubTech_urb_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTech_urb_ind_KualaLumpur_Malaysia")
    X232.StubTechCoef_indproc_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTechCoef_indproc_KualaLumpur_Malaysia")
    X232.MAC_indproc_KualaLumpur_Malaysia<- get_data(all_data, "X232.MAC_indproc_KualaLumpur_Malaysia")
    X232.fgas_all_units_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.fgas_all_units_ind_KualaLumpur_Malaysia")
    X232.nonghg_max_reduction_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.nonghg_max_reduction_ind_KualaLumpur_Malaysia")
    X232.nonghg_steepness_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.nonghg_steepness_ind_KualaLumpur_Malaysia")
    X232.hfc_future_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.hfc_future_ind_KualaLumpur_Malaysia")
    X232.nonco2_max_reduction_indproc_KualaLumpur_Malaysia<- get_data(all_data, "X232.nonco2_max_reduction_indproc_KualaLumpur_Malaysia")
    X232.nonco2_steepness_indproc_KualaLumpur_Malaysia<- get_data(all_data, "X232.nonco2_steepness_indproc_KualaLumpur_Malaysia")
    X232.pol_emissions_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.pol_emissions_ind_KualaLumpur_Malaysia")
    X232.ghg_emissions_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.ghg_emissions_ind_KualaLumpur_Malaysia")
    X232.StubTechMarket_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTechMarket_ind_KualaLumpur_Malaysia")
    X232.StubTechSecMarket_ind_KualaLumpur_Malaysia<- get_data(all_data, "X232.StubTechSecMarket_ind_KualaLumpur_Malaysia")
    X232.nonco2_indproc_KualaLumpur_Malaysia<- get_data(all_data, "X232.nonco2_indproc_KualaLumpur_Malaysia")

    # ===================================================

    # Produce outputs
    create_xml("industry_KualaLumpur_Malaysia.xml") %>%
      add_xml_data(X232.DeleteFinalDemand_ind_KualaLumpur_Malaysia, "DeleteFinalDemand") %>%
      add_xml_data(X232.DeleteSupplysector_ind_KualaLumpur_Malaysia, "DeleteSupplysector") %>%
      add_logit_tables_xml(X232.Supplysector_ind_KualaLumpur_Malaysia, "Supplysector") %>%
      add_xml_data(X232.FinalEnergyKeyword_ind_KualaLumpur_Malaysia, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(X232.SubsectorLogit_ind_KualaLumpur_Malaysia, "SubsectorLogit") %>%
      add_xml_data(X232.SubsectorShrwtFllt_ind_KualaLumpur_Malaysia, "SubsectorShrwtFllt") %>%
      add_xml_data(X232.SubsectorInterp_ind_KualaLumpur_Malaysia, "SubsectorInterp") %>%
      add_xml_data(X232.FuelPrefElast_indenergy_KualaLumpur_Malaysia, "FuelPrefElast") %>%
      add_xml_data(X232.StubTech_ind_KualaLumpur_Malaysia, "StubTech") %>%
      add_xml_data(X232.StubTechInterp_ind_KualaLumpur_Malaysia, "StubTechInterp") %>%
      add_xml_data(X232.StubTechCalInput_indenergy_KualaLumpur_Malaysia, "StubTechCalInput") %>%
      add_xml_data(X232.StubTechCalInput_indfeed_KualaLumpur_Malaysia, "StubTechCalInput") %>%
      add_xml_data(X232.StubTechProd_industry_KualaLumpur_Malaysia, "StubTechProd") %>%
      add_xml_data(X232.StubTechCoef_industry_KualaLumpur_Malaysia, "StubTechCoef") %>%
      add_xml_data(X232.PerCapitaBased_ind_KualaLumpur_Malaysia, "PerCapitaBased") %>%
      add_xml_data(X232.IncomeElasticity_ind_gssp2_KualaLumpur_Malaysia, "IncomeElasticity") %>%
      add_xml_data(X232.PriceElasticity_ind_KualaLumpur_Malaysia, "PriceElasticity") %>%
      add_xml_data(X232.BaseService_ind_KualaLumpur_Malaysia, "BaseService") %>%
      add_xml_data(X232.UnlimitRsrc_KualaLumpur_Malaysia, "UnlimitRsrc") %>%
      add_xml_data(X232.UnlimitRsrcPrice_KualaLumpur_Malaysia, "UnlimitRsrcPrice") %>%
      add_logit_tables_xml(X232.Supplysector_urb_indproc_KualaLumpur_Malaysia, "Supplysector") %>%
      add_logit_tables_xml(X232.SubsectorLogit_urb_ind_KualaLumpur_Malaysia, "SubsectorLogit") %>%
      add_xml_data(X232.SubsectorShrwtFllt_urb_ind_KualaLumpur_Malaysia, "SubsectorShrwtFllt") %>%
      add_xml_data(X232.SubsectorInterp_urb_ind_KualaLumpur_Malaysia, "SubsectorInterp") %>%
      add_xml_data(X232.StubTech_urb_ind_KualaLumpur_Malaysia, "StubTech") %>%
      add_xml_data(X232.StubTechCoef_indproc_KualaLumpur_Malaysia, "StubTechCoef") %>%
      add_xml_data(X232.MAC_indproc_KualaLumpur_Malaysia, "MAC") %>%
      add_xml_data(X232.fgas_all_units_ind_KualaLumpur_Malaysia, "StubTechEmissUnits") %>%
      add_xml_data(X232.nonghg_max_reduction_ind_KualaLumpur_Malaysia, "GDPCtrlMax") %>%
      add_xml_data(X232.nonghg_steepness_ind_KualaLumpur_Malaysia, "GDPCtrlSteep") %>%
      add_xml_data(X232.hfc_future_ind_KualaLumpur_Malaysia, "OutputEmissCoeff") %>%
      add_xml_data(X232.nonco2_max_reduction_indproc_KualaLumpur_Malaysia, "GDPCtrlMax") %>%
      add_xml_data(X232.nonco2_steepness_indproc_KualaLumpur_Malaysia, "GDPCtrlSteep") %>%
      add_xml_data(X232.pol_emissions_ind_KualaLumpur_Malaysia, "InputEmissions") %>%
      add_xml_data(X232.ghg_emissions_ind_KualaLumpur_Malaysia, "InputEmissions") %>%
      add_xml_data(X232.StubTechMarket_ind_KualaLumpur_Malaysia, "StubTechMarket") %>%
      add_xml_data(X232.StubTechSecMarket_ind_KualaLumpur_Malaysia, "StubTechSecMarket") %>%
      add_xml_data(X232.nonco2_indproc_KualaLumpur_Malaysia, "StbTechOutputEmissions") %>%
      add_precursors("X232.DeleteFinalDemand_ind_KualaLumpur_Malaysia",
                     "X232.DeleteSupplysector_ind_KualaLumpur_Malaysia",
                     "X232.Supplysector_ind_KualaLumpur_Malaysia",
                     "X232.FinalEnergyKeyword_ind_KualaLumpur_Malaysia",
                     "X232.SubsectorLogit_ind_KualaLumpur_Malaysia",
                     "X232.SubsectorShrwtFllt_ind_KualaLumpur_Malaysia",
                     "X232.SubsectorInterp_ind_KualaLumpur_Malaysia",
                     "X232.FuelPrefElast_indenergy_KualaLumpur_Malaysia",
                     "X232.StubTech_ind_KualaLumpur_Malaysia",
                     "X232.StubTechInterp_ind_KualaLumpur_Malaysia",
                     "X232.StubTechCalInput_indenergy_KualaLumpur_Malaysia",
                     "X232.StubTechCalInput_indfeed_KualaLumpur_Malaysia",
                     "X232.StubTechProd_industry_KualaLumpur_Malaysia",
                     "X232.StubTechCoef_industry_KualaLumpur_Malaysia",
                     "X232.PerCapitaBased_ind_KualaLumpur_Malaysia",
                     "X232.IncomeElasticity_ind_gssp2_KualaLumpur_Malaysia",
                     "X232.PriceElasticity_ind_KualaLumpur_Malaysia",
                     "X232.BaseService_ind_KualaLumpur_Malaysia",
                     "X232.UnlimitRsrc_KualaLumpur_Malaysia",
                     "X232.UnlimitRsrcPrice_KualaLumpur_Malaysia",
                     "X232.Supplysector_urb_indproc_KualaLumpur_Malaysia",
                     "X232.SubsectorLogit_urb_ind_KualaLumpur_Malaysia",
                     "X232.SubsectorShrwtFllt_urb_ind_KualaLumpur_Malaysia",
                     "X232.SubsectorInterp_urb_ind_KualaLumpur_Malaysia",
                     "X232.StubTech_urb_ind_KualaLumpur_Malaysia",
                     "X232.StubTechCoef_indproc_KualaLumpur_Malaysia",
                     "X232.MAC_indproc_KualaLumpur_Malaysia",
                     "X232.fgas_all_units_ind_KualaLumpur_Malaysia",
                     "X232.nonghg_max_reduction_ind_KualaLumpur_Malaysia",
                     "X232.nonghg_steepness_ind_KualaLumpur_Malaysia",
                     "X232.hfc_future_ind_KualaLumpur_Malaysia",
                     "X232.nonco2_max_reduction_indproc_KualaLumpur_Malaysia",
                     "X232.nonco2_steepness_indproc_KualaLumpur_Malaysia",
                     "X232.pol_emissions_ind_KualaLumpur_Malaysia",
                     "X232.ghg_emissions_ind_KualaLumpur_Malaysia",
                     "X232.StubTechMarket_ind_KualaLumpur_Malaysia",
                     "X232.StubTechSecMarket_ind_KualaLumpur_Malaysia",
                     "X232.nonco2_indproc_KualaLumpur_Malaysia") ->
      industry_KualaLumpur_Malaysia.xml

    return_data(industry_KualaLumpur_Malaysia.xml)
  } else {
    stop("Unknown command")
  }
}
