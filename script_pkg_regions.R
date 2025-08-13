### Function: USA Regions
### Functions Called: none
### Developer: Kiarash Farzad, 20240205
###

# ### download all state names
# # data <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
#
# stateNameVector <- data[,1]
# stateNameVector <-  stateNameVector[stateNameVector!= "Alaska"]
# stateNameVector <-  stateNameVector[stateNameVector!= "Hawaii"]
#

### all states
# "maryland", "delaware", "pennsylvania", "new jersey", "new york"
# "connecticut", "rhode island", "massachusetts", "vermont", "new hampshire"
# "maine", "alabama", "florida", "georgia", "south carolina"
# "north carolina", "virginia","mississippi", "tennessee", "kentucky"
# "illinois", "indiana", "ohio", "west virginia", "wisconsin", "michigan"
# "minnesota", "iowa", "missouri", "arkansas", "louisiana", "washington"
# "oregon", "idaho", "california", "nevada", "montana", "wyoming", "utah"
# "colorado", "arizona", "new mexico", "north dakota", "south dakota"
# "nebraska", "texas", "oklahoma", "kansas"

func_reg_conv_name <- function(inp) {
    inp <- if (tolower(inp) == "all") inp else tolower(inp)
    meta <- t(
        data.frame(
            c("Alabama", "AL"),
            c("Alaska", "AK"),
            c("Arizona", "AZ"),
            c("Arkansas", "AR"),
            c("California", "CA"),
            c("Colorado", "CO"),
            c("Connecticut", "CT"),
            c("Delaware", "DE"),
            c("Florida", "FL"),
            c("Georgia", "GA"),
            c("Hawaii", "HI"),
            c("Idaho", "ID"),
            c("Illinois", "IL"),
            c("Indiana", "IN"),
            c("Iowa", "IA"),
            c("Kansas", "KS"),
            c("Kentucky", "KY"),
            c("Louisiana", "LA"),
            c("Maine", "ME"),
            c("Maryland", "MD"),
            c("Massachusetts", "MA"),
            c("Michigan", "MI"),
            c("Minnesota", "MN"),
            c("Mississippi", "MS"),
            c("Missouri", "MO"),
            c("Montana", "MT"),
            c("Nebraska", "NE"),
            c("Nevada", "NV"),
            c("New Hampshire", "NH"),
            c("New Jersey", "NJ"),
            c("New Mexico", "NM"),
            c("New York", "NY"),
            c("North Carolina", "NC"),
            c("North Dakota", "ND"),
            c("Ohio", "OH"),
            c("Oklahoma", "OK"),
            c("Oregon", "OR"),
            c("Pennsylvania", "PA"),
            c("Rhode Island", "RI"),
            c("South Carolina", "SC"),
            c("South Dakota", "SD"),
            c("Tennessee", "TN"),
            c("Texas", "TX"),
            c("Utah", "UT"),
            c("Vermont", "VT"),
            c("Virginia", "VA"),
            c("Washington", "WA"),
            c("West Virginia", "WV"),
            c("Wisconsin", "WI"),
            c("Wyoming", "WY")
        )
    )
    row.names(meta) <- NULL
    for (col in 1:dim(meta)[2]) {
        meta[, col] <- tolower(meta[, col])
    }

    if (inp %in% meta[, 1] && inp %in% meta[, 2] && tolower(inp) != "all") {
        print(paste("the input is in all columns, cannot distinguish:", inp))
        stop()
    } else if (!inp %in% meta[, 1] && !inp %in% meta[, 2] && tolower(inp) != "all") {
        print(paste("the input is not available:", inp))
        stop()
    }
    return(
        if (inp %in% meta[, 1]) {
            meta[meta[, 1] == inp, 2]
        } else if (inp %in% meta[, 2]) {
            meta[meta[, 2] == inp, 1]
        } else if (inp == "all") {
            meta[, 2]
        } else if (inp == "ALL" || inp == "All") {
            meta[, 1]
        }
    )
}

func_reg <- function(
    def = "", check_state = FALSE, state_name = "",
    return_region_states = FALSE, region_name = "", name_list = "",
    regions_list = "", silent = TRUE, num_regions = FALSE) {
    #### funcs ####
    func_reg_def <- function(def = "") {
        if (def == "") {
            return(print("zone_def not defined; defs are: ecw, ew, cr, epa, gr state"))
        }
        if (def == "ecw") {
            ## eastern central western USA
            name_list <- c("r_e", "r_c", "r_w")
            if (!silent) {
                print("regions: eastern central western (ecw) called")
                print("options:")
                print(name_list)
            }
            if (num_regions) {
                return(length(name_list))
            }
            r_e <- c(
                "maryland", "delaware", "pennsylvania", "new jersey",
                "new york", "connecticut",
                "rhode island", "massachusetts", "vermont",
                "new hampshire", "maine", "alabama", "florida", "georgia",
                "south carolina", "north carolina", "virginia", "mississippi",
                "tennessee", "kentucky", "illinois", "indiana", "ohio",
                "west virginia", "wisconsin", "michigan"
            )
            r_c <- c(
                "north dakota", "south dakota", "nebraska", "texas", "oklahoma",
                "kansas", "louisiana", "arkansas", "missouri", "minnesota",
                "iowa"
            )
            r_w <- c(
                "washington", "oregon", "idaho", "california", "nevada",
                "montana", "wyoming", "utah", "colorado", "arizona",
                "new mexico"
            )
            return(list(name_list, list(r_e, r_c, r_w)))
        } else if (def == "ew") {
            ### eastern western USA
            name_list <- c("r_e", "r_w")
            if (!silent) {
                print("regions: eastern western (ew) called")
                print("options:")
                print(name_list)
            }
            if (num_regions) {
                return(length(name_list))
            }
            r_e <- c(
                "maryland", "delaware", "pennsylvania",
                "new jersey", "new york",
                "connecticut", "rhode island", "massachusetts", "vermont",
                "new hampshire", "maine", "alabama", "florida", "georgia",
                "south carolina", "north carolina", "virginia", "mississippi",
                "tennessee", "kentucky", "illinois", "indiana", "ohio",
                "west virginia", "wisconsin", "michigan", "minnesota", "iowa",
                "missouri", "arkansas", "louisiana"
            )
            r_w <- c(
                "washington", "oregon", "idaho", "california",
                "nevada", "montana",
                "wyoming", "utah", "colorado", "arizona", "new mexico",
                "north dakota", "south dakota", "nebraska", "texas", "oklahoma",
                "kansas"
            )
            return(list(name_list, list(r_e, r_w)))
        } else if (def == "cr") {
            ### climate regions
            ### https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions
            ### https://www.ncdc.noaa.gov/cag/regional/mapping
            name_list <- c(
                "r_101_Northeast", "r_102_Upper_Midwest",
                "r_103_Ohio_Valley", "r_104_Southeast",
                # "r_105_Northern_Rockies_and_Plains", ## correct one
                "r_105_Northern_Rockies",
                "r_106_South",
                "r_107_Southwest", "r_108_Northwest", "r_109_West"
            )
            if (!silent) {
                print("climate regions by NOAA called")
                print("options:")
                print(name_list)
            }
            if (num_regions) {
                return(length(name_list))
            }
            r_101_Northeast <- c(
                "maryland", "delaware", "pennsylvania", "new jersey",
                "new york", "connecticut", "rhode island", "massachusetts",
                "vermont", "new hampshire", "maine"
            )
            r_102_Upper_Midwest <- c(
                "wisconsin", "minnesota",
                "iowa", "michigan"
            )
            r_103_Ohio_Valley <- c(
                "tennessee", "missouri", "kentucky", "illinois",
                "indiana", "ohio", "west virginia"
            )
            r_104_Southeast <- c(
                "alabama", "florida", "georgia", "south carolina",
                "north carolina", "virginia"
            )
            r_105_Northern_Rockies_and_Plains <- c(
                "montana", "wyoming", "north dakota", "south dakota",
                "nebraska"
            )
            r_106_South <- c(
                "texas", "oklahoma", "kansas", "louisiana", "arkansas",
                "mississippi"
            )
            r_107_Southwest <- c("utah", "colorado", "arizona", "new mexico")

            r_108_Northwest <- c("washington", "oregon", "idaho")
            r_109_West <- c("california", "nevada")
            return(list(name_list, list(
                r_101_Northeast, r_102_Upper_Midwest,
                r_103_Ohio_Valley, r_104_Southeast,
                r_105_Northern_Rockies_and_Plains, r_106_South, r_107_Southwest,
                r_108_Northwest, r_109_West
            )))
        } else if (def == "epa") {
            ### epa regions
            name_list <- c(
                "r_1", "r_2", "r_3", "r_4", "r_5", "r_6",
                "r_7", "r_8", "r_9", "r_10"
            )
            if (!silent) {
                print("EPA regions called")
                print("options:")
                print(name_list)
            }
            if (num_regions) {
                return(length(name_list))
            }
            r_1 <- c(
                "maine", "new hampshire", "vermont", "massachusetts",
                "rhode island", "connecticut"
            )
            r_2 <- c("new york", "new jersey")
            r_3 <- c(
                "virginia", "west virginia", "maryland", "delaware",
                "pennsylvania"
            )
            r_4 <- c(
                "kentucky", "tennessee", "north carolina", "mississippi",
                "alabama", "georgia", "south carolina", "florida"
            )
            r_5 <- c(
                "wisconsin", "minnesota", "michigan", "illinois",
                "indiana", "ohio"
            )
            r_6 <- c(
                "new mexico", "texas", "oklahoma", "louisiana",
                "arkansas"
            )
            r_7 <- c("nebraska", "kansas", "iowa", "missouri")
            r_8 <- c(
                "montana", "wyoming", "north dakota", "south dakota",
                "utah", "colorado"
            )
            r_9 <- c("california", "nevada", "arizona")
            r_10 <- c("washington", "oregon", "idaho")
            return(list(name_list, list(
                r_1, r_2, r_3, r_4, r_5, r_6, r_7, r_8, r_9, r_10
            )))
        } else if (def == "gr") {
            ### geographic regions
            name_list <- c(
                "r_1", "r_2", "r_3", "r_4", "r_5", "r_6",
                "r_7", "r_8", "r_9",
            )
            if (!silent) {
                print("Geographic regions called")
                print("options:")
                print(name_list)
            }
            if (num_regions) {
                return(length(name_list))
            }
            r_1 <- c("washington", "oregon", "california")
            r_2 <- c(
                "montana", "idaho", "wyoming", "nevada", "utah",
                "colorado", "arizona", "new mexico"
            )
            r_3 <- c(
                "north dakota", "south dakota", "minnesota", "iowa",
                "missouri", "nebraska", "kansas"
            )
            r_4 <- c("texas", "oklahoma", "arkansas", "louisiana")
            r_5 <- c("illinois", "indiana", "ohio", "wisconsin", "michigan")
            r_6 <- c("mississippi", "tennessee", "kentucky", "alabama")
            r_7 <- c(
                "maine", "connecticut", "rhode island", "massachusetts",
                "vermont", "new hampshire"
            )
            r_8 <- c("new york", "new jersey", "pennsylvania")
            r_9 <- c(
                "west virginia", "virginia", "south carolina",
                "north carolina", "georgia", "florida", "maryland", "delaware"
            )
        } else if (def == "state") {
            ### climate regions
            ### https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions
            ### https://www.ncdc.noaa.gov/cag/regional/mapping
            name_list_temp <- c(
                "maryland", "delaware", "pennsylvania", "new jersey", "new york",
                "connecticut", "rhode island", "massachusetts", "vermont", "new hampshire",
                "maine", "alabama", "florida", "georgia", "south carolina",
                "north carolina", "virginia", "mississippi", "tennessee", "kentucky",
                "illinois", "indiana", "ohio", "west virginia", "wisconsin", "michigan",
                "minnesota", "iowa", "missouri", "arkansas", "louisiana", "washington",
                "oregon", "idaho", "california", "nevada", "montana", "wyoming", "utah",
                "colorado", "arizona", "new mexico", "north dakota", "south dakota",
                "nebraska", "texas", "oklahoma", "kansas"
            )
            name_list <- c()
            for (i in name_list_temp) {
                name_list <- c(name_list, paste0("r_", func_reg_conv_name(i)))
            }
            if (!silent) {
                print("state regions called")
                print("options:")
                print(name_list)
            }
            if (num_regions) {
                return(length(name_list))
            }
            for (i in name_list) {
                ii <- func_reg_conv_name(stringr::str_remove(i, "r_"))
                assign(x = i, value = c(ii))
            }
            temp <- list()
            for (i in name_list) {
                temp <- c(temp, list(get(i)))
            }
            return(list(name_list, temp))
        }
    }

    func_check_state <- function(check_state = FALSE, state_name = "",
                                 name_list = "", region_name = "",
                                 regions_list = "") {
        for (region_names in name_list) {
            assign(region_names, regions_list[[which(name_list == region_names,
                arr.ind = TRUE
            )]])
        }
        if (check_state && state_name == "") {
            print("you did not provide state name")
            return(NA)
        } else {
            for (region_names in name_list) {
                if (state_name %in% get(region_names) ||
                    state_name %in% toupper(get(region_names)) ||
                    state_name %in% tools::toTitleCase(get(region_names))) {
                    print(paste(state_name, "is in", region_names))
                    return(region_names)
                }
            }
            print("check the state name please")
            return(NA)
        }
    }

    func_return_region_states <- function(return_region_states = FALSE,
                                          region_name = "",
                                          name_list = "",
                                          regions_list = "") {
        if (return_region_states && region_name == "") {
            print("you did not provide region name")
            return(NA)
        } else if (!region_name %in% name_list) {
            print("your provided region_name is not in the list")
            return(NA)
        } else {
            for (region_names in name_list) {
                assign(
                    region_names,
                    regions_list[[which(name_list == region_name,
                        arr.ind = TRUE
                    )]]
                )
            }
            return(get(paste0(region_name)))
        }
    }

    ##### main run #####
    needed_data <- func_reg_def(def = def)

    if (check_state) {
        return(func_check_state(
            check_state = check_state,
            state_name = state_name,
            name_list = needed_data[[1]],
            region_name = region_name,
            regions_list = needed_data[[2]]
        ))
    } else if (return_region_states) {
        return(func_return_region_states(
            return_region_states = return_region_states,
            region_name = region_name,
            name_list = needed_data[[1]],
            regions_list = needed_data[[2]]
        ))
    } else {
        return(needed_data[[1]])
    }
}

################################################################################
################################################################################
##################################### test #####################################
################################################################################
################################################################################

if (FALSE) {
    ### test1
    func_reg(def = "")
    func_reg(def = "", num_regions = TRUE)
    func_reg(def = "cr")
    func_reg(def = "cr", num_regions = TRUE)
    func_reg(def = "cr", check_state = TRUE)
    func_reg(def = "cr", check_state = TRUE, state_name = "California")
    x <- func_reg(def = "cr", check_state = TRUE, state_name = "California")
    print(x)
    func_reg(def = "cr", return_region_states = TRUE)
    func_reg(def = "cr", return_region_states = TRUE, region_name = "r_c")
    cr_regions <- func_reg(def = "cr", silent = TRUE)
    for (cr_region in cr_regions) {
        cr_region_states <- func_reg(
            def = "cr", return_region_states = TRUE,
            region_name = cr_region
        )
        for (state in cr_region_states) {
            print(state)
            print(func_reg_conv_name(state))
        }
    }
    func_reg(def = "state")
    func_reg(def = "state", num_regions = TRUE)
    func_reg(def = "state", check_state = TRUE, state_name = "California")
}
