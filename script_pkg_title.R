func_make_title <- function(
    title = "",
    line_2 = "",
    title_size = 1.6,
    title_location = 1,
    unit = "",
    unit_size = 1.2,
    unit_location = 0,
    l_unit = TRUE,
    specie = "",
    specie_size = 1.2,
    specie_location = 0,
    l_specie = TRUE,
    debug = FALSE) {
    if (debug) {
        print("making the title")
        print(paste("title size:", title_size))
        print(paste("title location:", title_location))
    }
    if (class(title) == "expression") {
        title(
            main = title,
            cex.main = title_size,
            line = title_location + 0.4
        )
        if (class(line_2) == "expression") {
            title(
                main = line_2,
                cex.main = title_size,
                line = title_location - 1.6,
                add = TRUE,
            )
        } else {
            title(
                main = paste0(line_2), font.main = 2,
                cex.main = title_size,
                line = title_location - 2,
                add = TRUE,
            )
        }
    } else {
        title(
            main = paste0(title), font.main = 2,
            cex.main = title_size,
            line = title_location
        )
        if (class(line_2) == "expression") {
            title(
                main = line_2,
                cex.main = title_size,
                line = title_location - 2,
                add = TRUE,
            )
        } else {
            title(
                main = line_2,
                cex.main = title_size,
                line = title_location - 2,
                add = TRUE,
            )
        }
    }

    if (l_unit) {
        if (class(unit) == "expression") {
            title(unit,
                adj = 1, cex.main = unit_size,
                line = unit_location + 1
            )
        } else {
            title(unit,
                adj = 1, cex.main = unit_size,
                line = unit_location + 0.5
            )
        }
    }
    if (l_specie) {
        if (class(specie) == "expression") {
            title(specie,
                adj = 0, cex.main = specie_size,
                line = specie_location + 1
            )
        } else {
            title(specie,
                adj = 0, cex.main = specie_size,
                line = specie_location + 0.5
            )
        }
    }
}
