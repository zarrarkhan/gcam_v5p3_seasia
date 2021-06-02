# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamseasia_batch_building_SEAsia_xml
#'
#' Construct XML data structure for \code{building_SEAsia.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_SEAsia.xml}. The corresponding file in the
#' original data system was \code{batch_building_SEAsia.xml} (gcamseasia XML).
module_gcamseasia_batch_building_SEAsia_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.DeleteConsumer_SEAbld",
             "L244.DeleteSupplysector_SEAbld",
             #"L244.SubregionalShares_gcamSEA",
             "L244.PriceExp_IntGains_gcamSEA",
             "L244.Floorspace_gcamSEA",
             "L244.DemandFunction_serv_gcamSEA",
             "L244.DemandFunction_flsp_gcamSEA",
             "L244.Satiation_flsp_gcamSEA",
             "L244.SatiationAdder_gcamSEA",
             "L244.ThermalBaseService_gcamSEA",
             "L244.GenericBaseService_gcamSEA",
             "L244.ThermalServiceSatiation_gcamSEA",
             "L244.GenericServiceSatiation_gcamSEA",
             "L244.Intgains_scalar_gcamSEA",
             "L244.ShellConductance_bld_gcamSEA",
             "L244.Supplysector_bld_gcamSEA",
             "L244.FinalEnergyKeyword_bld_gcamSEA",
             "L244.SubsectorShrwtFllt_bld_gcamSEA",
             "L244.SubsectorInterp_bld_gcamSEA",
             "L244.SubsectorInterpTo_bld_gcamSEA",
             "L244.SubsectorLogit_bld_gcamSEA",
             "L244.StubTech_bld_gcamSEA",
             "L244.StubTechCalInput_bld_gcamSEA",
             "L244.StubTechMarket_bld_gcamSEA",
             "L244.GlobalTechIntGainOutputRatio_gcamSEA",
             "L244.GlobalTechInterpTo_bld_gcamSEA",
             "L244.GlobalTechEff_bld_gcamSEA",
             "L244.GlobalTechShrwt_bld_gcamSEA",
             "L244.GlobalTechCost_bld_gcamSEA",
             "L244.GlobalTechSCurve_bld_gcamSEA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_SEAsia.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.DeleteConsumer_SEAbld <- get_data(all_data, "L244.DeleteConsumer_SEAbld")
    L244.DeleteSupplysector_SEAbld <- get_data(all_data, "L244.DeleteSupplysector_SEAbld")
    #L244.SubregionalShares <- get_data(all_data, "L244.SubregionalShares_gcamSEA")
    L244.PriceExp_IntGains <- get_data(all_data, "L244.PriceExp_IntGains_gcamSEA")
    L244.Floorspace <- get_data(all_data, "L244.Floorspace_gcamSEA")
    L244.DemandFunction_serv <- get_data(all_data, "L244.DemandFunction_serv_gcamSEA")
    L244.DemandFunction_flsp <- get_data(all_data, "L244.DemandFunction_flsp_gcamSEA")
    L244.Satiation_flsp <- get_data(all_data, "L244.Satiation_flsp_gcamSEA")
    L244.SatiationAdder <- get_data(all_data, "L244.SatiationAdder_gcamSEA")
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService_gcamSEA")
    L244.GenericBaseService <- get_data(all_data, "L244.GenericBaseService_gcamSEA")
    L244.ThermalServiceSatiation <- get_data(all_data, "L244.ThermalServiceSatiation_gcamSEA")
    L244.GenericServiceSatiation <- get_data(all_data, "L244.GenericServiceSatiation_gcamSEA")
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar_gcamSEA")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld_gcamSEA")
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld_gcamSEA")
    L244.FinalEnergyKeyword_bld <- get_data(all_data, "L244.FinalEnergyKeyword_bld_gcamSEA")
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_gcamSEA")
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld_gcamSEA")
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld_gcamSEA")
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld_gcamSEA")
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld_gcamSEA")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld_gcamSEA")
    L244.StubTechMarket_bld <- get_data(all_data, "L244.StubTechMarket_bld_gcamSEA")
    L244.GlobalTechIntGainOutputRatio <- get_data(all_data, "L244.GlobalTechIntGainOutputRatio_gcamSEA")
    L244.GlobalTechInterpTo_bld <- get_data(all_data, "L244.GlobalTechInterpTo_bld_gcamSEA")
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld_gcamSEA")
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamSEA")
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld_gcamSEA")
    L244.GlobalTechSCurve_bld <- get_data(all_data, "L244.GlobalTechSCurve_bld_gcamSEA")

    # ===================================================

    # Produce outputs
    create_xml("building_SEAsia.xml") %>%
      add_xml_data(L244.DeleteConsumer_SEAbld, "DeleteConsumer") %>%
      add_xml_data(L244.DeleteSupplysector_SEAbld, "DeleteSupplysector") %>%
      #add_xml_data(L244.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L244.PriceExp_IntGains, "PriceExp_IntGains") %>%
      add_xml_data(L244.Floorspace, "Floorspace") %>%
      add_xml_data(L244.DemandFunction_serv, "DemandFunction_serv") %>%
      add_xml_data(L244.DemandFunction_flsp, "DemandFunction_flsp") %>%
      add_xml_data(L244.Satiation_flsp, "Satiation_flsp") %>%
      add_xml_data(L244.SatiationAdder, "SatiationAdder") %>%
      add_xml_data(L244.ThermalBaseService, "ThermalBaseService") %>%
      add_xml_data(L244.GenericBaseService, "GenericBaseService") %>%
      add_xml_data(L244.ThermalServiceSatiation, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericServiceSatiation, "GenericServiceSatiation") %>%
      add_xml_data(L244.Intgains_scalar, "Intgains_scalar") %>%
      add_xml_data(L244.ShellConductance_bld, "ShellConductance") %>%
      add_logit_tables_xml(L244.Supplysector_bld, "Supplysector") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld, "FinalEnergyKeyword") %>%
      add_xml_data(L244.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.SubsectorInterp_bld, "SubsectorInterp") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld, "SubsectorLogit") %>%
      add_xml_data(L244.StubTech_bld, "StubTech") %>%
      add_xml_data(L244.StubTechCalInput_bld, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechMarket_bld, "StubTechMarket") %>%
      add_xml_data(L244.GlobalTechIntGainOutputRatio, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(L244.GlobalTechInterpTo_bld, "GlobalTechInterpTo") %>%
      add_xml_data(L244.GlobalTechEff_bld, "GlobalTechEff") %>%
      add_xml_data(L244.GlobalTechShrwt_bld, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechSCurve_bld, "GlobalTechSCurve")  %>%
      add_precursors("L244.DeleteConsumer_SEAbld",
                     "L244.DeleteSupplysector_SEAbld",
                     #"L244.SubregionalShares_gcamSEA",
                     "L244.PriceExp_IntGains_gcamSEA",
                     "L244.Floorspace_gcamSEA",
                     "L244.DemandFunction_serv_gcamSEA",
                     "L244.DemandFunction_flsp_gcamSEA",
                     "L244.Satiation_flsp_gcamSEA",
                     "L244.SatiationAdder_gcamSEA",
                     "L244.ThermalBaseService_gcamSEA",
                     "L244.GenericBaseService_gcamSEA",
                     "L244.ThermalServiceSatiation_gcamSEA",
                     "L244.GenericServiceSatiation_gcamSEA",
                     "L244.Intgains_scalar_gcamSEA",
                     "L244.ShellConductance_bld_gcamSEA",
                     "L244.Supplysector_bld_gcamSEA",
                     "L244.FinalEnergyKeyword_bld_gcamSEA",
                     "L244.SubsectorShrwtFllt_bld_gcamSEA",
                     "L244.SubsectorInterp_bld_gcamSEA",
                     "L244.SubsectorInterpTo_bld_gcamSEA",
                     "L244.SubsectorLogit_bld_gcamSEA",
                     "L244.StubTech_bld_gcamSEA",
                     "L244.StubTechCalInput_bld_gcamSEA",
                     "L244.StubTechMarket_bld_gcamSEA",
                     "L244.GlobalTechIntGainOutputRatio_gcamSEA",
                     "L244.GlobalTechInterpTo_bld_gcamSEA",
                     "L244.GlobalTechEff_bld_gcamSEA",
                     "L244.GlobalTechShrwt_bld_gcamSEA",
                     "L244.GlobalTechCost_bld_gcamSEA",
                     "L244.GlobalTechSCurve_bld_gcamSEA") ->
      building_SEAsia.xml

    # # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L244.SubsectorInterpTo_bld)) {

      building_SEAsia.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld, "SubsectorInterpTo") ->
        building_SEAsia.xml

      }

    return_data(building_SEAsia.xml)
  } else {
    stop("Unknown command")
  }
}
