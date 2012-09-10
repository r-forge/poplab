simped <-
function (baseyear, simyears, endyear, population.male,population.fem,fertility,
          incidence.male,incidence.fem,mortality.male,mortality.fem,healthy,sex.a,
          fam.rel,d.mod,famrisk, mortratio,print.option,seed, folder, name.base)
{
    if (missing(baseyear)) {
        baseyear <- endyear - simyears + 1
    }
    if (missing(endyear)) {
        endyear <- baseyear + simyears - 1
    }
    if (missing(sex.a)) {
        sex.a <- 2
    }
    if (missing(healthy)) {
        healthy <- 1
    }
    if (missing(famrisk)) {
        famrisk <- 1
    }
    if (missing(mortratio)) {
        mortratio <- 1
    }
    if (missing(print.option)) {
        print.option <- TRUE
    }
    if (missing(seed)) {
        seed <- NULL
    }
    if (missing(name.base)) {
        name.base <- ""
    }
    if ((!is.null(seed)) & (!is.character(seed)))
        set.seed(seed)
    fert <- readin(f.name = fertility, folder)
    femmort <- readin(f.name = mortality.fem, folder)
    malemort <- readin(f.name = mortality.male, folder)
    warn.old <- options()$warn
    options(warn = -1)
    if (name.base == "") {
        name <- try(scan(file = file.path(folder, paste("base_pop_", baseyear,
            ".txt", sep = "")), quiet = TRUE, what = "character",
            nlines = 1), silent = TRUE)
    }
    else {
        name <- try(scan(file = file.path(folder, paste("base_pop_", baseyear,
            "_", name.base, ".txt", sep = "")), quiet = TRUE,
            what = "character", nlines = 1), silent = TRUE)
    }

    if (name[1] %in% c("ID", "yob", "sex", "m", "f", "yod", "yoi")) {
        if (name.base == "") {
            basepop <- readin(f.name = paste("base_pop_", baseyear,
                ".txt", sep = ""), folder)
        }
        else {
            basepop <- readin(f.name = paste("base_pop_", baseyear,
                "_", name.base, ".txt", sep = ""), folder)
        }
        

        cat("Baseline population was read from", folder, "folder \n")
        class(basepop) <- "poplab"
        plot.poplab(basepop, "base", population.fem, population.male,
            baseyear, folder)
    }
    options(warn = warn.old)
    pedigree <- basepop
    todel <- which(pedigree[, "yob"] == baseyear)
    if (length(todel) != 0)
        pedigree <- pedigree[-todel, ]
    counter <- max(pedigree[, "ID"])
    cat("Start simulating pedigree from calendar year", baseyear,
        "to calendar year", endyear, ": \n")
    cat("Baseline population:", nrow(pedigree), "individuals \n")
    if (endyear > max(as.numeric(colnames(fert)[-1])))
        warning(paste("Fertility rates of the year", max(as.numeric(colnames(fert)[-1])),
            "will be used for the calendar years", max(as.numeric(colnames(fert)[-1])) +
                1, "-", endyear), call. = FALSE)
    if (endyear > max(as.numeric(colnames(femmort)[-1])))
        warning(paste("Female mortality rates of the year", max(as.numeric(colnames(femmort)[-1])),
            "will be used for the calendar years", max(as.numeric(colnames(femmort)[-1])) +
                1, "-", endyear), call. = FALSE)
    if (endyear > max(as.numeric(colnames(malemort)[-1])))
        warning(paste("Male mortality rates of the year", max(as.numeric(colnames(malemort)[-1])),
            "will be used for the calendar years", max(as.numeric(colnames(malemort)[-1])) +
                1, "-", endyear), call. = FALSE)
    if (healthy == 0) {
        if (sex.a == 1){ #male line aggregation
        inc <- readin(f.name = incidence.male, folder)
        if (endyear > max(as.numeric(colnames(inc)[-1])))
            warning(paste("Male incidence rates of the year", max(as.numeric(colnames(inc)[-1])),
                "will be used for the calendar years", max(as.numeric(colnames(inc)[-1])) +
                  1, "-", endyear), call. = FALSE)
        }
        else if (sex.a == 2){ #female line aggregation
        inc <- readin(f.name = incidence.fem, folder)
        if (endyear > max(as.numeric(colnames(inc)[-1])))
            warning(paste("Female incidence rates of the year", max(as.numeric(colnames(inc)[-1])),
                "will be used for the calendar years", max(as.numeric(colnames(inc)[-1])) +
                  1, "-", endyear), call. = FALSE)
        }
        else { #familial aggregation through either gender
            maleinc <- readin(f.name = incidence.male, folder)
            feminc <- readin(f.name = incidence.fem, folder)
        if (endyear > max(as.numeric(colnames(maleinc)[-1])))
            warning(paste("Male incidence rates of the year", max(as.numeric(colnames(maleinc)[-1])),
                "will be used for the calendar years", max(as.numeric(colnames(maleinc)[-1])) +
                  1, "-", endyear), call. = FALSE)
        if (endyear > max(as.numeric(colnames(feminc)[-1])))
            warning(paste("Female incidence rates of the year", max(as.numeric(colnames(feminc)[-1])),
                "will be used for the calendar years", max(as.numeric(colnames(feminc)[-1])) +
                  1, "-", endyear), call. = FALSE)
        }
        if (length(famrisk) > 1) {
            famrisk <- matrix(data = famrisk, byrow = FALSE,
                ncol = 2)
            colnames(famrisk) <- c("age", "val")
            if (!(d.mod %in% c("agesprr", "agespor")))
                stop(paste("Invalid risk value specification for the chosen disease model"))
        }
        else {
            if (!(d.mod %in% c("rr", "or")))
                stop(paste("Invalid risk value specification for the chosen disease model"))
        }
        if (!(d.mod %in% c("agespor", "agesprr", "or", "rr")))
            stop(paste("Invalid disease model specification for the evolved population"))
    }
    z <- baseyear
    while (z <= endyear) {
        ind <- ifelse(z > max(as.numeric(colnames(fert)[-1])),
            1 + which.max(as.numeric(colnames(fert)[-1])), which(colnames(fert) ==
                z, arr.ind = TRUE))
        childbc <- fert[, c(1, ind)]
        childbc <- childbc[childbc[, 2] != 0, ]
        colnames(childbc) <- c("age", "rate")
        indmort.f <- ifelse(z > max(as.numeric(colnames(femmort)[-1])),
            1 + which.max(as.numeric(colnames(femmort)[-1])),
            which(colnames(femmort) == z, arr.ind = TRUE))
        indmort.m <- ifelse(z > max(as.numeric(colnames(malemort)[-1])),
            1 + which.max(as.numeric(colnames(malemort)[-1])),
            which(colnames(malemort) == z, arr.ind = TRUE))
        if (nrow(femmort) != nrow(malemort))
            stop("Different age structure in the male and female mortality files. Organize files and try again.")
        tempmort <- cbind(femmort[, c(1, indmort.f)], malemort[,
            indmort.m])
        colnames(tempmort) <- c("age", "femrate", "malerate")
        pedigree <- givebirth(ped = pedigree, childbc = childbc,
            nr = counter, yr = z)
        counter <- max(pedigree[, "ID"])
        pedigree <- assigndeath(ped = pedigree, tempmort = tempmort,
            risk = mortratio, yr = z)
        if (healthy == 0) {
            if ((sex.a == 1) | (sex.a == 2)){
            ind.i <- ifelse(z > max(as.numeric(colnames(inc)[-1])),
                1 + which.max(as.numeric(colnames(inc)[-1])),
                which(colnames(inc) == z, arr.ind = TRUE))
            tempinc <- inc[, c(1, ind.i)]
            tempinc <- tempinc[tempinc[, 2] != 0, , drop = FALSE]
            colnames(tempinc) <- c("age", "rate")
            }
            else{
                indinc.f <- ifelse(z > max(as.numeric(colnames(feminc)[-1])),
                    1 + which.max(as.numeric(colnames(feminc)[-1])),
                    which(colnames(feminc) == z, arr.ind = TRUE))
                indinc.m <- ifelse(z > max(as.numeric(colnames(maleinc)[-1])),
                    1 + which.max(as.numeric(colnames(maleinc)[-1])),
                    which(colnames(maleinc) == z, arr.ind = TRUE))
                if (nrow(feminc) != nrow(maleinc))
                    stop("Different age structure in the male and female incidence files. Organize files and try again.")
                tempinc <- cbind(feminc[, c(1, indinc.f)], maleinc[, indinc.m])
                colnames(tempinc) <- c("age", "femrate", "malerate")
            }
            pedigree <- assignevent(ped = pedigree, inc = tempinc,
                risk = famrisk, yr = z, sex = sex.a, type = d.mod,
                fam.rel = fam.rel)
        }
        z <- z + 1
    }
    cat("Finished simulating the population for calendar time",
        baseyear, "-", endyear, "\n")
    class(pedigree) <- "poplab"
    res <- list(basepop, pedigree)
    class(res) <- "poplab"
    if (print.option)
        print.poplab(res, "current", endyear, folder)
    return(res)
}
