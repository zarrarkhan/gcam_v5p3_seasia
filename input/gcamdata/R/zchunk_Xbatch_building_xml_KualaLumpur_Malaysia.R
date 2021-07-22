# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_building_xml_KualaLumpur_Malaysia
#'
#' Construct XML data structure for \code{building_KualaLumpur_Malaysia.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_KualaLumpur_Malaysia.xml}. The corresponding file in the
#' original data system was \code{batch_building_KualaLumpur_Malaysia.xml} (energy XML).
module_energy_Xbatch_building_xml_KualaLumpur_Malaysia <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X244.DeleteThermalService_bld_KualaLumpur_Malaysia",
             "X244.DeleteConsumer_bld_KualaLumpur_Malaysia",
             "X244.DeleteSupplysector_bld_KualaLumpur_Malaysia",
             "X244.SubregionalShares_KualaLumpur_Malaysia",
             "X244.PriceExp_IntGains_KualaLumpur_Malaysia",
             "X244.Floorspace_KualaLumpur_Malaysia",
             "X244.DemandFunction_serv_KualaLumpur_Malaysia",
             "X244.DemandFunction_flsp_KualaLumpur_Malaysia",
             "X244.Satiation_flsp_KualaLumpur_Malaysia",
             "X244.SatiationAdder_KualaLumpur_Malaysia",
             "X244.ThermalBaseService_KualaLumpur_Malaysia",
             "X244.GenericBaseService_KualaLumpur_Malaysia",
             "X244.ThermalServiceSatiation_KualaLumpur_Malaysia",
             "X244.GenericServiceSatiation_KualaLumpur_Malaysia",
             "X244.Intgains_scalar_KualaLumpur_Malaysia",
             "X244.ShellConductance_bld_KualaLumpur_Malaysia",
             "X244.Supplysector_bld_KualaLumpur_Malaysia",
             "X244.FinalEnergyKeyword_bld_KualaLumpur_Malaysia",
             "X244.SubsectorShrwtFllt_bld_KualaLumpur_Malaysia",
             "X244.SubsectorInterp_bld_KualaLumpur_Malaysia",
             "X244.FuelPrefElast_bld_KualaLumpur_Malaysia",
             "X244.StubTech_bld_KualaLumpur_Malaysia",
             "X244.StubTechEff_bld_KualaLumpur_Malaysia",
             "X244.StubTechCalInput_bld_KualaLumpur_Malaysia",
             "X244.SubsectorLogit_bld_KualaLumpur_Malaysia",
             "X244.StubTechIntGainOutputRatio_KualaLumpur_Malaysia",
             "X244.HDDCDD_constdd_no_GCM_KualaLumpur_Malaysia",
             "X244.fgas_all_units_bld_KualaLumpur_Malaysia",
             "X244.nonghg_max_reduction_bld_KualaLumpur_Malaysia",
             "X244.nonghg_steepness_bld_KualaLumpur_Malaysia",
             "X244.hfc_future_bld_KualaLumpur_Malaysia",
             "X244.pol_emissions_bld_KualaLumpur_Malaysia",
             "X244.ghg_emissions_bld_KualaLumpur_Malaysia"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_KualaLumpur_Malaysia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X244.DeleteThermalService_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.DeleteThermalService_bld_KualaLumpur_Malaysia")
    X244.DeleteConsumer_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.DeleteConsumer_bld_KualaLumpur_Malaysia")
    X244.DeleteSupplysector_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.DeleteSupplysector_bld_KualaLumpur_Malaysia")
    X244.SubregionalShares_KualaLumpur_Malaysia<- get_data(all_data, "X244.SubregionalShares_KualaLumpur_Malaysia")
    X244.PriceExp_IntGains_KualaLumpur_Malaysia<- get_data(all_data, "X244.PriceExp_IntGains_KualaLumpur_Malaysia")
    X244.Floorspace_KualaLumpur_Malaysia<- get_data(all_data, "X244.Floorspace_KualaLumpur_Malaysia")
    X244.DemandFunction_serv_KualaLumpur_Malaysia<- get_data(all_data, "X244.DemandFunction_serv_KualaLumpur_Malaysia")
    X244.DemandFunction_flsp_KualaLumpur_Malaysia<- get_data(all_data, "X244.DemandFunction_flsp_KualaLumpur_Malaysia")
    X244.Satiation_flsp_KualaLumpur_Malaysia<- get_data(all_data, "X244.Satiation_flsp_KualaLumpur_Malaysia")
    X244.SatiationAdder_KualaLumpur_Malaysia<- get_data(all_data, "X244.SatiationAdder_KualaLumpur_Malaysia")
    X244.ThermalBaseService_KualaLumpur_Malaysia<- get_data(all_data, "X244.ThermalBaseService_KualaLumpur_Malaysia")
    X244.GenericBaseService_KualaLumpur_Malaysia<- get_data(all_data, "X244.GenericBaseService_KualaLumpur_Malaysia")
    X244.ThermalServiceSatiation_KualaLumpur_Malaysia<- get_data(all_data, "X244.ThermalServiceSatiation_KualaLumpur_Malaysia")
    X244.GenericServiceSatiation_KualaLumpur_Malaysia<- get_data(all_data, "X244.GenericServiceSatiation_KualaLumpur_Malaysia")
    X244.Intgains_scalar_KualaLumpur_Malaysia<- get_data(all_data, "X244.Intgains_scalar_KualaLumpur_Malaysia")
    X244.ShellConductance_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.ShellConductance_bld_KualaLumpur_Malaysia")
    X244.Supplysector_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.Supplysector_bld_KualaLumpur_Malaysia")
    X244.FinalEnergyKeyword_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.FinalEnergyKeyword_bld_KualaLumpur_Malaysia")
    X244.SubsectorShrwtFllt_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.SubsectorShrwtFllt_bld_KualaLumpur_Malaysia")
    X244.SubsectorInterp_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.SubsectorInterp_bld_KualaLumpur_Malaysia")
    X244.FuelPrefElast_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.FuelPrefElast_bld_KualaLumpur_Malaysia")
    X244.StubTech_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.StubTech_bld_KualaLumpur_Malaysia")
    X244.StubTechEff_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.StubTechEff_bld_KualaLumpur_Malaysia")
    X244.StubTechCalInput_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.StubTechCalInput_bld_KualaLumpur_Malaysia")
    X244.SubsectorLogit_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.SubsectorLogit_bld_KualaLumpur_Malaysia")
    X244.StubTechIntGainOutputRatio_KualaLumpur_Malaysia<- get_data(all_data, "X244.StubTechIntGainOutputRatio_KualaLumpur_Malaysia")
    X244.HDDCDD_constdd_no_GCM_KualaLumpur_Malaysia<- get_data(all_data, "X244.HDDCDD_constdd_no_GCM_KualaLumpur_Malaysia")
    X244.fgas_all_units_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.fgas_all_units_bld_KualaLumpur_Malaysia")
    X244.nonghg_max_reduction_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.nonghg_max_reduction_bld_KualaLumpur_Malaysia")
    X244.nonghg_steepness_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.nonghg_steepness_bld_KualaLumpur_Malaysia")
    X244.hfc_future_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.hfc_future_bld_KualaLumpur_Malaysia")
    X244.pol_emissions_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.pol_emissions_bld_KualaLumpur_Malaysia")
    X244.ghg_emissions_bld_KualaLumpur_Malaysia<- get_data(all_data, "X244.ghg_emissions_bld_KualaLumpur_Malaysia")

    # ===================================================

    # Produce outputs
    create_xml("building_KualaLumpur_Malaysia.xml") %>%
      add_xml_data(X244.DeleteThermalService_bld_KualaLumpur_Malaysia, "DeleteThermalService") %>%
      add_xml_data(X244.DeleteConsumer_bld_KualaLumpur_Malaysia, "DeleteConsumer") %>%
      add_xml_data(X244.DeleteSupplysector_bld_KualaLumpur_Malaysia, "DeleteSupplysector") %>%
      add_logit_tables_xml(X244.Supplysector_bld_KualaLumpur_Malaysia, "Supplysector") %>%
      add_logit_tables_xml(X244.SubsectorLogit_bld_KualaLumpur_Malaysia, "SubsectorLogit") %>%
      add_xml_data(X244.SubregionalShares_KualaLumpur_Malaysia, "SubregionalShares") %>%
    add_xml_data(X244.PriceExp_IntGains_KualaLumpur_Malaysia, "PriceExp_IntGains") %>%
    add_xml_data(X244.Floorspace_KualaLumpur_Malaysia, "Floorspace") %>%
    add_xml_data(X244.DemandFunction_serv_KualaLumpur_Malaysia, "DemandFunction_serv") %>%
    add_xml_data(X244.DemandFunction_flsp_KualaLumpur_Malaysia, "DemandFunction_flsp") %>%
    add_xml_data(X244.Satiation_flsp_KualaLumpur_Malaysia, "Satiation_flsp") %>%
    add_xml_data(X244.SatiationAdder_KualaLumpur_Malaysia, "SatiationAdder") %>%
    add_xml_data(X244.ThermalBaseService_KualaLumpur_Malaysia, "ThermalBaseService") %>%
    add_xml_data(X244.GenericBaseService_KualaLumpur_Malaysia, "GenericBaseService") %>%
    add_xml_data(X244.ThermalServiceSatiation_KualaLumpur_Malaysia, "ThermalServiceSatiation") %>%
    add_xml_data(X244.GenericServiceSatiation_KualaLumpur_Malaysia, "GenericServiceSatiation") %>%
    add_xml_data(X244.Intgains_scalar_KualaLumpur_Malaysia, "Intgains_scalar") %>%
    add_xml_data(X244.ShellConductance_bld_KualaLumpur_Malaysia, "ShellConductance") %>%
    add_xml_data(X244.FinalEnergyKeyword_bld_KualaLumpur_Malaysia, "FinalEnergyKeyword") %>%
    add_xml_data(X244.SubsectorShrwtFllt_bld_KualaLumpur_Malaysia, "SubsectorShrwtFllt") %>%
    add_xml_data(X244.SubsectorInterp_bld_KualaLumpur_Malaysia, "SubsectorInterp") %>%
    add_xml_data(X244.FuelPrefElast_bld_KualaLumpur_Malaysia, "FuelPrefElast") %>%
    add_xml_data(X244.StubTech_bld_KualaLumpur_Malaysia, "StubTech") %>%
    add_xml_data(X244.StubTechEff_bld_KualaLumpur_Malaysia, "StubTechEff") %>%
    add_xml_data(X244.StubTechCalInput_bld_KualaLumpur_Malaysia, "StubTechCalInput") %>%
    add_xml_data(X244.StubTechIntGainOutputRatio_KualaLumpur_Malaysia, "StubTechIntGainOutputRatio") %>%
    add_xml_data(X244.HDDCDD_constdd_no_GCM_KualaLumpur_Malaysia, "HDDCDD") %>%
      add_xml_data(X244.fgas_all_units_bld_KualaLumpur_Malaysia, "StubTechEmissUnits") %>%
      add_xml_data(X244.nonghg_max_reduction_bld_KualaLumpur_Malaysia, "GDPCtrlMax") %>%
      add_xml_data(X244.nonghg_steepness_bld_KualaLumpur_Malaysia, "GDPCtrlSteep") %>%
      add_xml_data(X244.hfc_future_bld_KualaLumpur_Malaysia, "OutputEmissCoeff") %>%
      add_xml_data(X244.pol_emissions_bld_KualaLumpur_Malaysia, "InputEmissions") %>%
      add_xml_data(X244.ghg_emissions_bld_KualaLumpur_Malaysia, "InputEmissions") %>%
      add_precursors("X244.DeleteConsumer_bld_KualaLumpur_Malaysia",
                     "X244.DeleteSupplysector_bld_KualaLumpur_Malaysia",
                     "X244.SubregionalShares_KualaLumpur_Malaysia",
                     "X244.PriceExp_IntGains_KualaLumpur_Malaysia",
                     "X244.Floorspace_KualaLumpur_Malaysia",
                     "X244.DemandFunction_serv_KualaLumpur_Malaysia",
                     "X244.DemandFunction_flsp_KualaLumpur_Malaysia",
                     "X244.Satiation_flsp_KualaLumpur_Malaysia",
                     "X244.SatiationAdder_KualaLumpur_Malaysia",
                     "X244.ThermalBaseService_KualaLumpur_Malaysia",
                     "X244.GenericBaseService_KualaLumpur_Malaysia",
                     "X244.ThermalServiceSatiation_KualaLumpur_Malaysia",
                     "X244.GenericServiceSatiation_KualaLumpur_Malaysia",
                     "X244.Intgains_scalar_KualaLumpur_Malaysia",
                     "X244.ShellConductance_bld_KualaLumpur_Malaysia",
                     "X244.Supplysector_bld_KualaLumpur_Malaysia",
                     "X244.FinalEnergyKeyword_bld_KualaLumpur_Malaysia",
                     "X244.SubsectorShrwtFllt_bld_KualaLumpur_Malaysia",
                     "X244.SubsectorInterp_bld_KualaLumpur_Malaysia",
                     "X244.FuelPrefElast_bld_KualaLumpur_Malaysia",
                     "X244.StubTech_bld_KualaLumpur_Malaysia",
                     "X244.StubTechEff_bld_KualaLumpur_Malaysia",
                     "X244.StubTechCalInput_bld_KualaLumpur_Malaysia",
                     "X244.SubsectorLogit_bld_KualaLumpur_Malaysia",
                     "X244.StubTechIntGainOutputRatio_KualaLumpur_Malaysia",
                     "X244.HDDCDD_constdd_no_GCM_KualaLumpur_Malaysia",
                     "X244.fgas_all_units_bld_KualaLumpur_Malaysia",
                     "X244.nonghg_max_reduction_bld_KualaLumpur_Malaysia",
                     "X244.nonghg_steepness_bld_KualaLumpur_Malaysia",
                     "X244.hfc_future_bld_KualaLumpur_Malaysia",
                     "X244.pol_emissions_bld_KualaLumpur_Malaysia",
                     "X244.ghg_emissions_bld_KualaLumpur_Malaysia") ->
      building_KualaLumpur_Malaysia.xml

    return_data(building_KualaLumpur_Malaysia.xml)
  } else {
    stop("Unknown command")
  }
}
