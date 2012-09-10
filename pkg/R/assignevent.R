assignevent <-
function (ped, inc, risk, yr, sex, type, fam.rel) 
{
    if (!(sex %in% c(1, 2, 3)))
    # 1 for males only, 2 for females only and 3 for males or females aggregation
        stop("Gender not recognized")
    if (!(fam.rel %in% c("s", "p"))) 
        stop("Familial relationship for disease aggregation not recognized")        
    if (sex != 3){
        condped <- ped[ped[, "yod"] == 0 & ped[, "yoi"] == 0 & ped[,"sex"] == sex, ]
        condped <- cbind(condped, age = (yr - condped[, "yob"]))        
        condped <- cbind(condped, rate = inc[, "rate"][match(condped[,"age"], inc[, "age"])])
            } else{
        condped <- ped[ped[, "yod"] == 0 & ped[, "yoi"] == 0, ]
        condped <- cbind(condped, age = (yr - condped[, "yob"]))
        condped <- rbind(cbind(condped[condped[, "sex"] == 2, ], 
            rate = inc[, "femrate"][match(condped[condped[,"sex"] == 2, "age"], inc[, "age"])]), 
            cbind(condped[condped[,"sex"] == 1, ], rate = inc[, "malerate"][match(condped[condped[,"sex"] == 1, "age"], inc[, "age"])]))
        }
    
    condped <- condped[!is.na(condped[, "rate"]) & condped[,"rate"] != 0, ]

    if (fam.rel == "s") {
        condped <- condped[condped[, "m"] != 0, ]
        ped <- ped[order(ped[, "yoi"]), ]
        condped <- cbind(condped, yoi.rel = ped[ped[, "yoi"] != 0, "yoi"][match(condped[, "m"], ped[ped[, "yoi"] != 0, "m"])], 
                 age.i.rel = ped[ped[, "yoi"] != 0, "yob"][match(condped[,"m"], ped[ped[, "yoi"] != 0, "m"])])
    }
    else {
        if (sex == 1) {
            condped <- cbind(condped, yoi.rel = ped[, "yoi"][match(condped[,"f"], ped[, "ID"])], age.i.rel = ped[, "yob"][match(condped[, 
                "f"], ped[, "ID"])])
        }
        else if (sex == 2){
            condped <- cbind(condped, yoi.rel = ped[, "yoi"][match(condped[,"m"], ped[, "ID"])], age.i.rel = ped[, "yob"][match(condped[, 
                "m"], ped[, "ID"])])
        }
        else{
            condped <- cbind(condped, yoi.f = ped[, "yoi"][match(condped[, 
                "f"], ped[, "ID"])], age.i.f = ped[, "yob"][match(condped[, 
                "f"], ped[, "ID"])], yoi.m = ped[, "yoi"][match(condped[, 
                "m"], ped[, "ID"])], age.i.m = ped[, "yob"][match(condped[, 
                "m"], ped[, "ID"])])
            condped <- cbind(condped[, 1:(ncol(condped) - 4)],
                yoi.rel = pmax(condped[, "yoi.m"], condped[, "yoi.f"]),
                age.i.rel = ifelse(pmax(condped[, "yoi.m"], 
                condped[, "yoi.f"]) == condped[, "yoi.m"], 
                condped[, "age.i.m"], condped[, "age.i.f"]))
        }

    }
    condped[is.na(condped[, "yoi.rel"]), "yoi.rel"] <- 0
    condped[, "age.i.rel"] <- ifelse(condped[, "yoi.rel"] == 
        0, 0, condped[, "yoi.rel"] - condped[, "age.i.rel"])
    condped[(condped[, "yoi.rel"] != 0), "rate"] <- switch(type, 
        rr = condped[(condped[, "yoi.rel"] != 0), "rate"] * risk, 
        or = risk * condped[(condped[, "yoi.rel"] != 0), "rate"]/(condped[(condped[, 
            "yoi.rel"] != 0), "rate"] * (risk - 1) + 1), agesprr = condped[condped[, 
            "yoi.rel"] != 0, "rate"] * risk[cut(condped[condped[, 
            "yoi.rel"] != 0, "age.i.rel"], c(0, risk[, "age"]), 
            right = FALSE, include.lowest = TRUE), "val"], agespor = risk[cut(condped[condped[, 
            "yoi.p"] != 0, "age.i.rel"], c(0, risk[, "age"]), 
            right = FALSE, include.lowest = TRUE), "val"] * condped[(condped[, 
            "yoi.rel"] != 0), "rate"]/(condped[(condped[, "yoi.rel"] != 
            0), "rate"] * (risk[cut(condped[condped[, "yoi.rel"] != 
            0, "age.i.rel"], c(0, risk[, "age"]), right = FALSE, 
            include.lowest = TRUE), "val"] - 1) + 1), )
    if (nrow(condped[condped[, "rate"] > 1, , drop = FALSE]) > 
        0) {
        warning("One (or more) probability of disease incidence greater than 1 \n")
        condped[condped[, "rate"] > 1, "rate"] <- 1
    }
    condped <- cbind(condped, cancer = rbinom(nrow(condped), 
        1, condped[, "rate"]))
    ped[ped[, "ID"] %in% condped[condped[, "cancer"] == 1, "ID"], 
        "yoi"] <- yr
    return(ped)
}
