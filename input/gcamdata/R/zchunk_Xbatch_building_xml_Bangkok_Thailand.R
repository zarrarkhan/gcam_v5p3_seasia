# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Xbatch_building_xml_Bangkok_Thailand
#'
#' Construct XML data structure for \code{building_Bangkok_Thailand.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_Bangkok_Thailand.xml}. The corresponding file in the
#' original data system was \code{batch_building_Bangkok_Thailand.xml} (energy XML).
module_energy_Xbatch_building_xml_Bangkok_Thailand <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("X244.DeleteConsumer_bld_Bangkok_Thailand",
             "X244.DeleteSupplysector_bld_Bangkok_Thailand",
             "X244.SubregionalShares_Bangkok_Thailand",
             "X244.PriceExp_IntGains_Bangkok_Thailand",
             "X244.Floorspace_Bangkok_Thailand",
             "X244.DemandFunction_serv_Bangkok_Thailand",
             "X244.DemandFunction_flsp_Bangkok_Thailand",
             "X244.Satiation_flsp_Bangkok_Thailand",
             "X244.SatiationAdder_Bangkok_Thailand",
             "X244.ThermalBaseService_Bangkok_Thailand",
             "X244.GenericBaseService_Bangkok_Thailand",
             "X244.ThermalServiceSatiation_Bangkok_Thailand",
             "X244.GenericServiceSatiation_Bangkok_Thailand",
             "X244.Intgains_scalar_Bangkok_Thailand",
             "X244.ShellConductance_bld_Bangkok_Thailand",
             "X244.Supplysector_bld_Bangkok_Thailand",
             "X244.FinalEnergyKeyword_bld_Bangkok_Thailand",
             "X244.SubsectorShrwtFllt_bld_Bangkok_Thailand",
             "X244.SubsectorInterp_bld_Bangkok_Thailand",
             "X244.FuelPrefElast_bld_Bangkok_Thailand",
             "X244.StubTech_bld_Bangkok_Thailand",
             "X244.StubTechEff_bld_Bangkok_Thailand",
             "X244.StubTechCalInput_bld_Bangkok_Thailand",
             "X244.SubsectorLogit_bld_Bangkok_Thailand",
             "X244.StubTechIntGainOutputRatio_Bangkok_Thailand",
             "X244.HDDCDD_constdd_no_GCM_Bangkok_Thailand",
             "X244.fgas_all_units_bld_Bangkok_Thailand",
             "X244.nonghg_max_reduction_bld_Bangkok_Thailand",
             "X244.nonghg_steepness_bld_Bangkok_Thailand",
             "X244.hfc_future_bld_Bangkok_Thailand",
             "X244.pol_emissions_bld_Bangkok_Thailand",
             "X244.ghg_emissions_bld_Bangkok_Thailand"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_Bangkok_Thailand.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    X244.DeleteConsumer_bld_Bangkok_Thailand<- get_data(all_data, "X244.DeleteConsumer_bld_Bangkok_Thailand")
    X244.DeleteSupplysector_bld_Bangkok_Thailand<- get_data(all_data, "X244.DeleteSupplysector_bld_Bangkok_Thailand")
    X244.SubregionalShares_Bangkok_Thailand<- get_data(all_data, "X244.SubregionalShares_Bangkok_Thailand")
    X244.PriceExp_IntGains_Bangkok_Thailand<- get_data(all_data, "X244.PriceExp_IntGains_Bangkok_Thailand")
    X244.Floorspace_Bangkok_Thailand<- get_data(all_data, "X244.Floorspace_Bangkok_Thailand")
    X244.DemandFunction_serv_Bangkok_Thailand<- get_data(all_data, "X244.DemandFunction_serv_Bangkok_Thailand")
    X244.DemandFunction_flsp_Bangkok_Thailand<- get_data(all_data, "X244.DemandFunction_flsp_Bangkok_Thailand")
    X244.Satiation_flsp_Bangkok_Thailand<- get_data(all_data, "X244.Satiation_flsp_Bangkok_Thailand")
    X244.SatiationAdder_Bangkok_Thailand<- get_data(all_data, "X244.SatiationAdder_Bangkok_Thailand")
    X244.ThermalBaseService_Bangkok_Thailand<- get_data(all_data, "X244.ThermalBaseService_Bangkok_Thailand")
    X244.GenericBaseService_Bangkok_Thailand<- get_data(all_data, "X244.GenericBaseService_Bangkok_Thailand")
    X244.ThermalServiceSatiation_Bangkok_Thailand<- get_data(all_data, "X244.ThermalServiceSatiation_Bangkok_Thailand")
    X244.GenericServiceSatiation_Bangkok_Thailand<- get_data(all_data, "X244.GenericServiceSatiation_Bangkok_Thailand")
    X244.Intgains_scalar_Bangkok_Thailand<- get_data(all_data, "X244.Intgains_scalar_Bangkok_Thailand")
    X244.ShellConductance_bld_Bangkok_Thailand<- get_data(all_data, "X244.ShellConductance_bld_Bangkok_Thailand")
    X244.Supplysector_bld_Bangkok_Thailand<- get_data(all_data, "X244.Supplysector_bld_Bangkok_Thailand")
    X244.FinalEnergyKeyword_bld_Bangkok_Thailand<- get_data(all_data, "X244.FinalEnergyKeyword_bld_Bangkok_Thailand")
    X244.SubsectorShrwtFllt_bld_Bangkok_Thailand<- get_data(all_data, "X244.SubsectorShrwtFllt_bld_Bangkok_Thailand")
    X244.SubsectorInterp_bld_Bangkok_Thailand<- get_data(all_data, "X244.SubsectorInterp_bld_Bangkok_Thailand")
    X244.FuelPrefElast_bld_Bangkok_Thailand<- get_data(all_data, "X244.FuelPrefElast_bld_Bangkok_Thailand")
    X244.StubTech_bld_Bangkok_Thailand<- get_data(all_data, "X244.StubTech_bld_Bangkok_Thailand")
    X244.StubTechEff_bld_Bangkok_Thailand<- get_data(all_data, "X244.StubTechEff_bld_Bangkok_Thailand")
    X244.StubTechCalInput_bld_Bangkok_Thailand<- get_data(all_data, "X244.StubTechCalInput_bld_Bangkok_Thailand")
    X244.SubsectorLogit_bld_Bangkok_Thailand<- get_data(all_data, "X244.SubsectorLogit_bld_Bangkok_Thailand")
    X244.StubTechIntGainOutputRatio_Bangkok_Thailand<- get_data(all_data, "X244.StubTechIntGainOutputRatio_Bangkok_Thailand")
    X244.HDDCDD_constdd_no_GCM_Bangkok_Thailand<- get_data(all_data, "X244.HDDCDD_constdd_no_GCM_Bangkok_Thailand")
    X244.fgas_all_units_bld_Bangkok_Thailand<- get_data(all_data, "X244.fgas_all_units_bld_Bangkok_Thailand")
    X244.nonghg_max_reduction_bld_Bangkok_Thailand<- get_data(all_data, "X244.nonghg_max_reduction_bld_Bangkok_Thailand")
    X244.nonghg_steepness_bld_Bangkok_Thailand<- get_data(all_data, "X244.nonghg_steepness_bld_Bangkok_Thailand")
    X244.hfc_future_bld_Bangkok_Thailand<- get_data(all_data, "X244.hfc_future_bld_Bangkok_Thailand")
    X244.pol_emissions_bld_Bangkok_Thailand<- get_data(all_data, "X244.pol_emissions_bld_Bangkok_Thailand")
    X244.ghg_emissions_bld_Bangkok_Thailand<- get_data(all_data, "X244.ghg_emissions_bld_Bangkok_Thailand")

    # ===================================================

    # Produce outputs
    create_xml("building_Bangkok_Thailand.xml") %>%
      add_xml_data(X244.DeleteConsumer_bld_Bangkok_Thailand, "DeleteConsumer") %>%
      add_xml_data(X244.DeleteSupplysector_bld_Bangkok_Thailand, "DeleteSupplysector") %>%
      add_logit_tables_xml(X244.Supplysector_bld_Bangkok_Thailand, "Supplysector") %>%
      add_logit_tables_xml(X244.SubsectorLogit_bld_Bangkok_Thailand, "SubsectorLogit") %>%
      add_xml_data(X244.SubregionalShares_Bangkok_Thailand, "SubregionalShares") %>%
    add_xml_data(X244.PriceExp_IntGains_Bangkok_Thailand, "PriceExp_IntGains") %>%
    add_xml_data(X244.Floorspace_Bangkok_Thailand, "Floorspace") %>%
    add_xml_data(X244.DemandFunction_serv_Bangkok_Thailand, "DemandFunction_serv") %>%
    add_xml_data(X244.DemandFunction_flsp_Bangkok_Thailand, "DemandFunction_flsp") %>%
    add_xml_data(X244.Satiation_flsp_Bangkok_Thailand, "Satiation_flsp") %>%
    add_xml_data(X244.SatiationAdder_Bangkok_Thailand, "SatiationAdder") %>%
    add_xml_data(X244.ThermalBaseService_Bangkok_Thailand, "ThermalBaseService") %>%
    add_xml_data(X244.GenericBaseService_Bangkok_Thailand, "GenericBaseService") %>%
    add_xml_data(X244.ThermalServiceSatiation_Bangkok_Thailand, "ThermalServiceSatiation") %>%
    add_xml_data(X244.GenericServiceSatiation_Bangkok_Thailand, "GenericServiceSatiation") %>%
    add_xml_data(X244.Intgains_scalar_Bangkok_Thailand, "Intgains_scalar") %>%
    add_xml_data(X244.ShellConductance_bld_Bangkok_Thailand, "ShellConductance") %>%
    add_xml_data(X244.FinalEnergyKeyword_bld_Bangkok_Thailand, "FinalEnergyKeyword") %>%
    add_xml_data(X244.SubsectorShrwtFllt_bld_Bangkok_Thailand, "SubsectorShrwtFllt") %>%
    add_xml_data(X244.SubsectorInterp_bld_Bangkok_Thailand, "SubsectorInterp") %>%
    add_xml_data(X244.FuelPrefElast_bld_Bangkok_Thailand, "FuelPrefElast") %>%
    add_xml_data(X244.StubTech_bld_Bangkok_Thailand, "StubTech") %>%
    add_xml_data(X244.StubTechEff_bld_Bangkok_Thailand, "StubTechEff") %>%
    add_xml_data(X244.StubTechCalInput_bld_Bangkok_Thailand, "StubTechCalInput") %>%
    add_xml_data(X244.StubTechIntGainOutputRatio_Bangkok_Thailand, "StubTechIntGainOutputRatio") %>%
    add_xml_data(X244.HDDCDD_constdd_no_GCM_Bangkok_Thailand, "HDDCDD") %>%
      add_xml_data(X244.fgas_all_units_bld_Bangkok_Thailand, "StubTechEmissUnits") %>%
      add_xml_data(X244.nonghg_max_reduction_bld_Bangkok_Thailand, "GDPCtrlMax") %>%
      add_xml_data(X244.nonghg_steepness_bld_Bangkok_Thailand, "GDPCtrlSteep") %>%
      add_xml_data(X244.hfc_future_bld_Bangkok_Thailand, "OutputEmissCoeff") %>%
      add_xml_data(X244.pol_emissions_bld_Bangkok_Thailand, "InputEmissions") %>%
      add_xml_data(X244.ghg_emissions_bld_Bangkok_Thailand, "InputEmissions") %>%
      add_precursors("X244.DeleteConsumer_bld_Bangkok_Thailand",
                     "X244.DeleteSupplysector_bld_Bangkok_Thailand",
                     "X244.SubregionalShares_Bangkok_Thailand",
                     "X244.PriceExp_IntGains_Bangkok_Thailand",
                     "X244.Floorspace_Bangkok_Thailand",
                     "X244.DemandFunction_serv_Bangkok_Thailand",
                     "X244.DemandFunction_flsp_Bangkok_Thailand",
                     "X244.Satiation_flsp_Bangkok_Thailand",
                     "X244.SatiationAdder_Bangkok_Thailand",
                     "X244.ThermalBaseService_Bangkok_Thailand",
                     "X244.GenericBaseService_Bangkok_Thailand",
                     "X244.ThermalServiceSatiation_Bangkok_Thailand",
                     "X244.GenericServiceSatiation_Bangkok_Thailand",
                     "X244.Intgains_scalar_Bangkok_Thailand",
                     "X244.ShellConductance_bld_Bangkok_Thailand",
                     "X244.Supplysector_bld_Bangkok_Thailand",
                     "X244.FinalEnergyKeyword_bld_Bangkok_Thailand",
                     "X244.SubsectorShrwtFllt_bld_Bangkok_Thailand",
                     "X244.SubsectorInterp_bld_Bangkok_Thailand",
                     "X244.FuelPrefElast_bld_Bangkok_Thailand",
                     "X244.StubTech_bld_Bangkok_Thailand",
                     "X244.StubTechEff_bld_Bangkok_Thailand",
                     "X244.StubTechCalInput_bld_Bangkok_Thailand",
                     "X244.SubsectorLogit_bld_Bangkok_Thailand",
                     "X244.StubTechIntGainOutputRatio_Bangkok_Thailand",
                     "X244.HDDCDD_constdd_no_GCM_Bangkok_Thailand",
                     "X244.fgas_all_units_bld_Bangkok_Thailand",
                     "X244.nonghg_max_reduction_bld_Bangkok_Thailand",
                     "X244.nonghg_steepness_bld_Bangkok_Thailand",
                     "X244.hfc_future_bld_Bangkok_Thailand",
                     "X244.pol_emissions_bld_Bangkok_Thailand",
                     "X244.ghg_emissions_bld_Bangkok_Thailand") ->
      building_Bangkok_Thailand.xml

    return_data(building_Bangkok_Thailand.xml)
  } else {
    stop("Unknown command")
  }
}
