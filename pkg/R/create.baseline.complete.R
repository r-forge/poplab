create.baseline.complete <-
function (baseyear, base.scale, runintime, population.male,population.fem,fertility,
          incidence.male,incidence.fem,mortality.male,mortality.fem,healthy,sex.a,
          fam.rel,d.mod,risk, mortratio, print.option, seed, folder) 
{
    if (missing(healthy)) {
        healthy <- 1
    }
    if (missing(risk)) {
        risk <- 1
    }
    if (missing(sex.a)) {
        sex.a <- 2
    }
    if (missing(mortratio)) {
        mortratio <- 1
    }
    if (missing(base.scale)) {
        base.scale <- 500
    }
    if (missing(runintime)) {
        runintime <- 100
    }
    if (missing(print.option)) {
        print.option <- TRUE
    }
    if (missing(seed)) {
        seed <- NULL
    }
    if ((!is.null(seed)) & (!is.character(seed))) 
        set.seed(seed)
    if (base.scale <= 0) 
        stop("Something wrong with parameter base.scale. See help(create.baseline.complete)")
    if (runintime <= 0) 
        stop("Something wrong with parameter runintime. See help(create.baseline.complete)")
    baseyr <- baseyear - runintime
    fert <- readin(f.name = fertility, folder)
    if (!(baseyear %in% as.integer(colnames(fert)[-1]))) 
        stop(paste("No fertility rates for the year", baseyear, 
            "in data files"))
    childbc <- fert[, c(1, which(colnames(fert) == baseyear, 
        arr.ind = TRUE))]
    childbc <- childbc[childbc[, 2] != 0, ]
    colnames(childbc) <- c("age", "rate")
    femmort <- readin(f.name = mortality.fem, folder)
    malemort <- readin(f.name = mortality.male, folder)
    if ((!(baseyear %in% as.integer(colnames(femmort)[-1]))) | 
        (!(baseyear %in% as.integer(colnames(malemort)[-1])))) 
        stop(paste("No mortality rates either for males or females, for the year", 
            baseyear, "in data files"))
    if (nrow(femmort) != nrow(malemort)) 
        stop("Different age structure in the male and female mortality files. Organize files and try again.")
    tempmort <- cbind(femmort[, c(1, which(colnames(femmort) == 
        baseyear, arr.ind = TRUE))], malemort[, which(colnames(malemort) == 
        baseyear, arr.ind = TRUE)])
    colnames(tempmort) <- c("age", "femrate", "malerate")
    fempop <- readin(f.name = population.fem, folder)
    if (!(baseyear %in% as.integer(colnames(fempop)[-1]))) 
        stop(paste("No female population counts for the year", 
            baseyear, "in data files"))
    fempop <- fempop[, c(1, which(colnames(fempop) == baseyear, 
        arr.ind = TRUE))]
    colnames(fempop) <- c("age", "counts")
    malepop <- readin(f.name = population.male, folder)
    if (!(baseyear %in% as.integer(colnames(malepop)[-1]))) 
        stop(paste("No male population counts for the year", 
            baseyear, "in data files"))
    malepop <- malepop[, c(1, which(colnames(malepop) == baseyear, 
        arr.ind = TRUE))]
    colnames(malepop) <- c("age", "counts")
    if (healthy == 0) {
        if (sex.a == 1){
            tempinc <- readin(f.name = incidence.male, folder)
            if (!(baseyear %in% as.integer(colnames(tempinc)[-1]))) 
                stop(paste("No incidence rates for the year", baseyear, 
                    "in data files"))
            tempinc <- tempinc[, c(1, which(colnames(tempinc) == baseyear, arr.ind = TRUE))]
            colnames(tempinc) <- c("age", "rate")
        }
        else if (sex.a == 2){
            tempinc <- readin(f.name = incidence.fem, folder)
            if (!(baseyear %in% as.integer(colnames(tempinc)[-1]))) 
                stop(paste("No incidence rates for the year", baseyear, 
                    "in data files"))
            tempinc <- tempinc[, c(1, which(colnames(tempinc) == baseyear, arr.ind = TRUE))]
            colnames(tempinc) <- c("age", "rate")        
        }
        else{
        feminc <- readin(f.name = incidence.fem, folder)
        maleinc <- readin(f.name = incidence.male, folder)
        if ((!(baseyear %in% as.integer(colnames(feminc)[-1]))) | 
            (!(baseyear %in% as.integer(colnames(maleinc)[-1])))) 
        stop(paste("No incidence rates either for males or females, for the year", 
            baseyear, "in data files"))
        if (nrow(feminc) != nrow(maleinc)) 
            stop("Different age structure in the male and female incidence files. Organize files and try again.")
        tempinc <- cbind(feminc[, c(1, which(colnames(feminc) == 
            baseyear, arr.ind = TRUE))], maleinc[, which(colnames(maleinc) == 
            baseyear, arr.ind = TRUE)])
        colnames(tempinc) <- c("age", "femrate", "malerate")
        }
        if (length(risk) > 1) {
            risk <- matrix(data = risk, byrow = FALSE, ncol = 2)
            colnames(risk) <- c("age", "val")
            if (!(d.mod %in% c("agesprr", "agespor"))) 
                stop(paste("Invalid risk value specification for the chosen disease model"))
        }
        else {
            if (!(d.mod %in% c("rr", "or"))) 
                stop(paste("Invalid risk value specification for the chosen disease model"))
        }
        if (!(d.mod %in% c("agespor", "agesprr", "or", "rr"))) 
            stop(paste("Invalid disease model specification"))
    }
    baseyrtot.fem <- sum(fempop[, colnames(fempop) == "counts"])
    femprof <- cbind(fempop[, "age"], fempop[, colnames(fempop) == 
        "counts"]/baseyrtot.fem * 100, (baseyr - fempop[, "age"]))
    colnames(femprof) <- c("age", "counts", "yob")
    femprof[, "counts"] <- round(femprof[, "counts"] * base.scale)
    baseyrtot.male <- sum(malepop[, colnames(malepop) == "counts"])
    maleprof <- cbind(malepop[, "age"], malepop[, colnames(malepop) == 
        "counts"]/baseyrtot.male * 100, (baseyr - malepop[, "age"]))
    colnames(maleprof) <- c("age", "counts", "yob")
    maleprof[, "counts"] <- round(maleprof[, "counts"] * base.scale * 
        (sum(malepop[, "counts"])/sum(fempop[, "counts"])))
    femprof <- femprof[(femprof[, "age"] != 0) & (femprof[, "counts"] > 
        0), ]
    maleprof <- maleprof[(maleprof[, "age"] != 0) & (maleprof[, 
        "counts"] > 0), ]
    s.pedigree <- NULL
    counter <- 0
    s.pedigree <- rbind(cbind(c(mapply(seq, cumsum(maleprof[, 
        "counts"]) - maleprof[, "counts"] + 1, cumsum(maleprof[, 
        "counts"])), recursive = TRUE), c(mapply(rep, maleprof[, 
        "yob"], maleprof[, "counts"]), recursive = TRUE), rep(1, 
        length = sum(maleprof[, "counts"])), c(mapply(rep, 0, 
        maleprof[, "counts"]), recursive = TRUE)), cbind(c(mapply(seq, 
        cumsum(femprof[, "counts"]) - femprof[, "counts"] + 1, 
        cumsum(femprof[, "counts"])), recursive = TRUE), c(mapply(rep, 
        femprof[, "yob"], femprof[, "counts"]), recursive = TRUE), 
        rep(2, length = sum(femprof[, "counts"])), c(mapply(rep, 
            0, femprof[, "counts"]), recursive = TRUE)))
    s.pedigree <- cbind(s.pedigree, rep(0, length = nrow(s.pedigree)), 
        rep(0, length = nrow(s.pedigree)), rep(0, length = nrow(s.pedigree)))
    colnames(s.pedigree) <- c("ID", "yob", "sex", "m", "f", "yod", 
        "yoi")
    correct <- max(s.pedigree[s.pedigree[, "sex"] == 1, "ID"])
    s.pedigree[s.pedigree[, "sex"] == 2, "ID"] <- s.pedigree[s.pedigree[, 
        "sex"] == 2, "ID"] + correct
    counter <- max(s.pedigree[, "ID"])
    cat("Start simulating baseline population of year", baseyear, 
        "\n")
    for (z in 1:(runintime + 1)) {
        s.pedigree <- givebirth(ped = s.pedigree, childbc = childbc, 
            nr = counter, yr = baseyr)
        counter <- max(s.pedigree[, "ID"])
        if (z <= runintime) 
            s.pedigree <- assigndeath(ped = s.pedigree, tempmort = tempmort, 
                risk = mortratio, yr = baseyr)
        if ((healthy == 0) && (z <= runintime)) 
            s.pedigree <- assignevent(ped = s.pedigree, inc = tempinc, 
                risk = risk, yr = baseyr, sex = sex.a, type = d.mod, 
                fam.rel = fam.rel)
        baseyr <- baseyr + 1
    }
    s.ped <- s.pedigree[s.pedigree[, "yod"] == 0, ]
    pop <- rbind(decide(s.ped, sex = 1, malepop, baseyear), decide(s.ped, 
        sex = 2, fempop, baseyear))
    cat("Finished simulating the baseline population of year", 
        baseyear, "\n")
    notaround <- retrieve.parents(pop, s.pedigree)
    pop <- rbind(pop, notaround)
    class(pop) <- "poplab"
    if (print.option) 
        print.poplab(pop, "base", baseyear, folder)
    return(pop)
}
