func_get_var_spec <- function(variable = "ND",
                              rqst = "unit",
                              as_txt = FALSE,
                              parenthesis,
                              debug = FALSE) {
    make_compatible_filename <- function(name) {
        # Replace spaces and special characters with underscores
        name <- gsub("[^a-zA-Z0-9]", "_", name)
        # Remove any leading or trailing underscores
        name <- gsub("^_+|_+$", "", name)
        return(name)
    }

    if (!rqst %in% c(
        "unit", "name", "lim_abs", "lim_dif",
        "filename", "table", "variable"
    )) {
        return("request not supported")
    }

    variable_spec <- data.frame(
        # c("INPUT", "UNIT", "VARIABLE_NAME", "LIM_ABS", "LIM_DIF"),
        c("ND", 'NOT*" "*DEFINED', 'NOT*" "*DEFINED', -999, -999),
        c("CORR", "", "Corr", 0, 0),
        c("NMB", "%", "NMB", 0, 0),
        c("NME", "%", "NME", 0, 0),
        c("TEMPERATURE", "K", "Temperature", 300, 10),
        c("SFCTMP", '"\U00B0"*C', "Temperature", 40, 1),
        c("T2", '"\U00B0"*C', "Temperature", 40, 1),
        c("RH", "%", 'Relative*" "*Humidity', 100, 1),
        c("RH2", "%", 'Relative*" "*Humidity', 100, 1),
        c("WINDSPEED", 'm*" "*s^-1', 'Wind*" "*Speed', 8, 1),
        c("WSPD10", 'm*" "*s^-1', 'Wind*" "*Speed', 8, 1),
        c("WS10", 'm*" "*s^-1', 'Wind*" "*Speed', 8, 1),
        c("WS", 'm*" "*s^-1', 'Wind*" "*Speed', 8, 1),
        c("WINDDIRECTION", "deg", 'Wind*" "*Direction', 360, 360),
        c("WDIR10", "deg", 'Wind*" "*Direction', 360, 360),
        c("WDIR", "deg", 'Wind*" "*Direction', 360, 360),
        c("WD10", "deg", 'Wind*" "*Direction', 360, 360),
        c("WD", "deg", 'Wind*" "*Direction', 360, 360),
        c("PRECIP", "cm", "Precipitation", 0.2, 0.1),
        c("RT", "mm", "Precipitation", 0.2, 0.1),
        c("WATERVAPOR", "", 'Water*" "*Vapor', 0, 0),
        c("PBLH", "m", "PBLH", 1500, 15),
        c("SFCPR", "Pa", "Pressure", 105000, 10500),
        c("PR", "hPa", "Pressure", 1050, 1050),
        c("PM10", '"\U00B5"*g*" "*m^-3', "PM[10]", 40, 4),
        c("PM25", '"\U00B5"*g*" "*m^-3', "PM[2.5]", 15, 3),
        c("PM25TOT", '"\U00B5"*g*" "*m^-3', "PM[2.5]", 15, 3),
        c("SO4", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*SO[4]^"2-"', 1.5, 0.3),
        c("PM25SO4", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*SO[4]^"2-"', 1.5, 0.3),
        c("NO3", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*NO[3]^"-"', 1, 0.2),
        c("PM25NO3", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*NO[3]^"-"', 1, 0.2),
        c("NH4", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*NH[4]^"+"', 0.4, 0.1),
        c("PM25NH4", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*NH[4]^"+"', 0.4, 0.1),
        c("OA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*OA', 6, 6),
        c("PM25OA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*OA', 6, 6),
        c("OC", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*OC', 5, 5),
        c("PM25OC", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*OC', 5, 5),
        c("TC", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*TC', 5, 5),
        c("PM25TC", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*TC', 5, 5),
        c("EC", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*EC', 0.5, 0.1),
        c("PM25EC", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*EC', 0.5, 0.1),
        c("NA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Na', 0.5, 0.2),
        c("PM25NA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Na', 0.5, 0.2),
        c("CL", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Cl', 0.5, 0.2),
        c("PM25CL", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Cl', 0.5, 0.2),
        c("PM25SOIL", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Soil', 2, 0.5),
        c("PMFSOILIMPV", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Soil', 2, 0.5),
        c("PM25UNSPEC", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Unspeciated', 5, 1),
        c("PM25UNSPEC1", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Unspeciated', 5, 1),
        c("PMFPOA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*POA', 3, 1),
        c("PMFSOA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*SOA', 3, 1),
        c("PMFASOA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*ASOA', 3, 1),
        c("PMFBSOA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*BSOA', 3, 1),
        c("K", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*K', 0.5, 0.1),
        c("PM25K", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*K', 0.5, 0.1),
        c("MG", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Mg', 0.5, 0.1),
        c("PM25MG", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Mg', 0.5, 0.1),
        c("CA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Ca', 0.5, 0.1),
        c("PM25CA", '"\U00B5"*g*" "*m^-3', 'PM[2.5]*"_"*Ca', 0.5, 0.1),
        c("O3", "ppbV", "O[3]", 70, 7),
        c("O31HRMAXTIME", "ppbV", 'MDA1*" "*O[3]*" "*Time', 70, 7),
        c("O38HRMAX", "ppbV", 'MDA8*" "*O[3]', 70, 7),
        c("O38HRMAX9CELL", "ppbV", 'MDA8*" "*O[3]*" "*9*Cell', 70, 7),
        c("O38HRMAXTIME", "ppbV", 'MDA8*" "*O[3]*" "*Time', 70, 7),
        c("VOC", "ppbC", "VOC", 100, 20),
        c("SO2", "ppbV", "SO[2]", 2, 1),
        c("NOX", "ppbV", "NO[X]", 10, 4),
        c("NO", "ppbV", "NO", 10, 4),
        c("NO2", "ppbV", "NO[2]", 10, 4),
        c("NOY", "ppbV", "NO[Y]", 10, 4),
        c("NOZ", "ppbV", "NO[Z]", 10, 4),
        c("HNO3", "ppbV", "HNO[3]", 1.5, 0.5),
        c("NH3", "ppbV", "NH[3]", 50, 5),
        c("CO", "ppbV", "CO", 250, 25),
        c("OH", "ppbV", "OH", 0.0003, 0.0001),
        c("ISOPRENE", "ppbV", "Isoprene", 2, 0.5),
        c("ISOP", "ppbV", "Isoprene", 2, 0.5),
        c("ETH", "ppbV", "Ethene", 1.5, 0.5),
        c("ETHA", "ppbV", "Ethane", 6, 2),
        c("TOLUENE", "ppbV", "Toluene", 0.2, 0.1),
        c("TOL", "ppbV", "Toluene", 0.2, 0.1),
        c("ALD2", "ppbV", "Acetaldehyde", 2, 0.4),
        c("FORM", "ppbV", "Formaldehyde", 3.5, 0.7),
        c("BENZENE", "ppbV", "Benzene", 1.5, 0.15),
        c("H2O2", "ppbV", "H[2]*O[2]", 2.5, 0.25),
        c("ACETONE", "ppbV", "Acetone", -999, -999),
        c("HONO", "ppbV", "HONO", -999, -999),
        c("PAN", "ppbV", "PAN", -999, -999),
        c("N2O5", "ppbV", "N[2]*O[5]", -999, -999),
        c("C2H2", "ppbV", "C[2]*H[2]", -999, -999),
        c("C2H4", "ppbV", "C[2]*H[4]", -999, -999),
        c("C2H6", "ppbV", "C[2]*H[6]", -999, -999),
        c("HCHO", "ppbV", "HCHO", -999, -999),
        c("CH3CHO", "ppbV", "CH[3]*CHO", -999, -999),
        c("CH3OH", "ppbV", "CH[3]*OH", -999, -999),
        c("C2H5OH", "ppbV", "C[2]*H[5]*OH", 0, 0),
        c("HCOOH", "ppbV", "HCOOH", -999, -999),
        c("PHENOL", "ppbV", "Phenol", -999, -999),
        c("CHOCHO", "ppbV", "CHOCHO", -999, -999),
        c("AOE@550NM", "Mm^-1", 'AOE*"@"*550*nm', -999, -999),
        c("H2O2HNO3", "", 'H[2]*O[2]*"/"*HNO[3]', -999, -999),
        c("GR", "", "GR", 2, 1),
        c("ADJGR", "", "ADJGR", 2, 1),
        c("DSN", "", "DSN", 4, 1),
        c("FORMNOY", "", 'Formaldehyde*"/"*NO[Y]', 0.56, 1),
        c("FORMNOZ", "", 'Formaldehyde*"/"*NO[Z]', 2, 1),
        c("O3NOX", "", 'O[3]*"/"*NO[X]', 30, 15),
        c("O3NOY", "", 'O[3]*"/"*NO[Y]', 14, 7),
        c("O3NOZ", "", 'O[3]*"/"*NO[Z]', 40, 20),
        c("NIT", "ppbV", "NIT", -999, -999),
        c("RGN", "ppbV", "RGN", -999, -999),
        c("TPN", "ppbV", "TPN", -999, -999),
        c("NTR", "ppbV", "NTR", -999, -999),
        c("HN3", "ppbV", "HN3", -999, -999),
        c("PN4", '"\U00B5"*g*" "*m^-3', "PN4", -999, -999),
        c("O3N", "ppbV", "O3N", -999, -999),
        c("O3V", "ppbV", "O3V", -999, -999),
        c("OON", "ppbV", "OON", -999, -999),
        c("OOV", "ppbV", "OOV", -999, -999),
        c("PEC", '"\U00B5"*g*" "*m^-3', "PEC", -999, -999),
        c("POA", '"\U00B5"*g*" "*m^-3', "POA", -999, -999),
        c("PFC", '"\U00B5"*g*" "*m^-3', "PFC", -999, -999),
        c("PFN", '"\U00B5"*g*" "*m^-3', "PFN", -999, -999),
        c("PCC", '"\U00B5"*g*" "*m^-3', "PCC", -999, -999),
        c("PCS", '"\U00B5"*g*" "*m^-3', "PCS", -999, -999),
        c("ARO", "ppbV", "ARO", -999, -999),
        c("ISP", "ppbV", "ISP", -999, -999),
        c("TRP", "ppbV", "TRP", -999, -999),
        c("SQT", "ppbV", "SQT", -999, -999),
        c("CG1", "ppbV", "CG1", -999, -999),
        c("CG2", "ppbV", "CG2", -999, -999),
        c("CG3", "ppbV", "CG3", -999, -999),
        c("CG4", "ppbV", "CG4", -999, -999),
        c("PO1", '"\U00B5"*g*" "*m^-3', "PO1", -999, -999),
        c("PO2", '"\U00B5"*g*" "*m^-3', "PO2", -999, -999),
        c("PO3", '"\U00B5"*g*" "*m^-3', "PO3", -999, -999),
        c("PO4", '"\U00B5"*g*" "*m^-3', "PO4", -999, -999),
        c("PPA", '"\U00B5"*g*" "*m^-3', "PPA", -999, -999),
        c("PPB", '"\U00B5"*g*" "*m^-3', "PPB", -999, -999),
        c("PS4", '"\U00B5"*g*" "*m^-3', "PS4", -999, -999),
        c("DMS", "ppbV", "DMS", -999, -999),
        c("", "", "", -999, -999)
    )

    colnames(variable_spec) <- NULL
    colnames(variable_spec) <- variable_spec[1, ]
    variable_spec <- variable_spec[-1, ]
    row.names(variable_spec) <- c("unit", "name", "lim_abs", "lim_dif")

    if (rqst == "table") {
        return(variable_spec)
    }

    if (debug) {
        print("Table:")
        print(variable_spec)
        print(paste("input: ", variable))
    }

    if (missing(parenthesis)) {
        if (rqst == "unit") {
            parenthesis <- TRUE
        } else if (rqst == "name" || rqst == "lim_abs" || rqst == "lim_dif") {
            parenthesis <- FALSE
        }
    }

    variable <- toupper(gsub(pattern = "[ ._]", replacement = "", x = variable))

    if (debug) {
        print("output:")
        print(variable_spec[rqst, variable])
    }

    if (rqst == "filename") {
        return(make_compatible_filename(variable))
    }

    ### only for check health
    if (rqst == "variable") {
        return(variable)
    }

    if (!variable %in% colnames(variable_spec)) {
        variable <- "ND"
    }

    if (rqst == "lim_abs" || rqst == "lim_dif") {
        return(as.numeric(variable_spec[rqst, variable]))
    }

    if (variable == "" || variable_spec[rqst, variable] == "") {
        return("")
    }

    if (as_txt || variable_spec[rqst, variable] == "%") {
        if (!parenthesis) {
            return(variable_spec[rqst, variable])
        } else {
            return(paste0("(", variable_spec[rqst, variable], ")"))
        }
    }

    if (!parenthesis) {
        return(parse(text = variable_spec[rqst, variable]))
    } else {
        return(parse(text = paste0("(", variable_spec[rqst, variable], ")")))
    }
}

### Checks for errors
if (FALSE) {
    source(paste0(R.utils::getParent(this.path::this.path()), "/script_pkg_vars_spec.R"))
    vars <- colnames(func_get_var_spec(rqst = "table"))
    for (var in vars) {
        print(var)
        print(func_get_var_spec(variable = var))
        print(func_get_var_spec(variable = var, "name"))
        if (!var == func_get_var_spec(variable = var, "variable")) {
            print("error in naming")
            stop()
        }
    }
}

### Recycle bin
if (FALSE) {
    source(paste0(R.utils::getParent(this.path::this.path()), "/script_pkg_vars_spec.R"))
    func_get_var_spec(variable = "")
    func_get_var_spec(variable = "ND", "name")
    func_get_var_spec(variable = "PM25_TOT")
    func_get_var_spec(variable = "PM25_TOT", rqst = "lim_abs")
    func_get_var_spec(variable = "NMB", debug = TRUE)
    func_get_var_spec(variable = "NMB", parenthesis = TRUE)
    func_get_var_spec(variable = "PM25_TOT", parenthesis = TRUE)
    func_get_var_spec(variable = "PM25_TOT", parenthesis = FALSE)
    func_get_var_spec(variable = "PM25_XXX")
    func_get_var_spec(variable = "AOE@550nm", parenthesis = TRUE)
    func_get_var_spec(variable = "AOE@550nm", parenthesis = TRUE)
    func_get_var_spec(variable = "Wind speed", debug = TRUE)
    func_get_var_spec(variable = "PM2. 5")
    func_get_var_spec(variable = "PM2.5", rqst = "filename")
    func_get_var_spec(variable = "PM2.5", rqst = "ABC")
    func_get_var_spec(variable = "PM2.5", rqst = "ABC")

    x <- 1:10
    y <- x^2
    plot(x, y,
        type = "b", col = "blue", main = "Simple Plot with Expression",
        xlab = "X-axis", ylab = "Y-axis"
    )
    text(5, 50, func_get_var_spec(variable = "RH", rqst = "name"),
        cex = 1.5, col = "red"
    )
}
