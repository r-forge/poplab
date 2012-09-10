givebirth <-
function (ped, childbc, nr, yr) 
{
    probfar <- ped[ped[, "yod"] == 0 & ped[, "sex"] == 1 & ped[,"ID"] %in% setdiff(ped[ped[, "sex"] == 1, "ID"], unique(ped[ped[,"f"] != 0, "f"])), , drop = FALSE]
    probfar <- cbind(probfar, age = (yr - probfar[, "yob"]))
    probmor <- ped[ped[, "yod"] == 0 & ped[, "sex"] == 2, ,drop = FALSE]
    probmor <- cbind(probmor, age = (yr - probmor[, "yob"]))
    probmor <- cbind(probmor, rate = childbc[, "rate"][match(probmor[,"age"], childbc[, "age"])])
    probmor <- probmor[!is.na(probmor[, "rate"]), ]
    if (nrow(probmor) == 0) 
        stop(paste("No potential mothers in the population. Calendar year",yr))
    if (nrow(probmor[probmor[, "rate"] > 1, , drop = FALSE]) > 0) {
        warning("One (or more) probability of reproduction greater than 1 \n")
        probmor[probmor[, "rate"] > 1, "rate"] <- 1
    }
    already.mor <- probmor[probmor[, "ID"] %in% unique(ped[ped[,"m"] != 0, "m"]), c("ID", "rate")]
    already.couple <- cbind(m = already.mor[, "ID"], rate = already.mor[,"rate"], f = ped[, "f"][match(already.mor[, "ID"], ped[,"m"])])
    already.couple <- already.couple[!is.na(already.couple[,"f"]), ]
    already.couple <- cbind(already.couple, yod.f = ped[, "yod"][match(already.couple[,"f"], ped[, "ID"])])
    already.couple <- already.couple[!is.na(already.couple[, "yod.f"]) & already.couple[, "yod.f"] == 0, ]
    already.couple <- cbind(already.couple, chances = rbinom(nrow(already.couple),1, already.couple[, "rate"]))
    first.timer <- probmor[probmor[, "ID"] %in% setdiff(probmor[,"ID"], already.mor[, "ID"]), ]
    first.timer <- cbind(m = first.timer[, "ID"], age = first.timer[,"age"], chances = rbinom(nrow(first.timer), 1, 
                             first.timer[,"rate"]), f = rep(0, length = nrow(first.timer)))
    first.timer <- first.timer[first.timer[, "chances"] == 1,]
    ages <- unique(first.timer[, "age"])
    for (a in 1:length(ages)) {
        if (nrow(first.timer[first.timer[, "age"] == ages[a],, drop = FALSE]) <= nrow(probfar[probfar[, "age"] %in% 
            c((ages[a] - 1):(ages[a] + 4)), , drop = FALSE])) {
            first.timer[first.timer[, "age"] == ages[a], "f"] <- sample(probfar[probfar[, 
                "age"] %in% c((ages[a] - 1):(ages[a] + 4)), "ID", 
                drop = FALSE], size = nrow(first.timer[first.timer[, 
                "age"] == ages[a], , drop = FALSE]))
        }
        else {
            first.timer[first.timer[, "m"] %in% sample(first.timer[first.timer[, 
                "age"] == ages[a] & first.timer[, "f"] == 0, 
                "m"], size = nrow(probfar[probfar[, "age"] %in% 
                c((ages[a] - 1):(ages[a] + 4)), , drop = FALSE])), 
                "f"] <- probfar[probfar[, "age"] %in% c((ages[a] - 
                1):(ages[a] + 4)), "ID"]
        }
        probfar <- probfar[probfar[, "ID"] %in% setdiff(probfar[, 
            "ID"], first.timer[, "f"]), ]
    }
    mor.far <- rbind(already.couple[already.couple[, "chances"] == 
        1, c("m", "f"), drop = FALSE], first.timer[first.timer[, 
        "f"] != 0, c("m", "f"), drop = FALSE])
    mor.far <- cbind(mor.far, sex = rbinom(nrow(mor.far), 1, 
        0.5))
    ped <- rbind(ped, cbind(c((nr + 1):(nr + nrow(mor.far))), 
        rep(yr, length = nrow(mor.far)), ifelse(mor.far[, "sex"] == 
            1, 2, 1), mor.far[, c("m", "f")], rep(0, length = nrow(mor.far)), 
        rep(0, length = nrow(mor.far))))
    return(ped)
}
