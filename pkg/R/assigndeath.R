assigndeath <-
function (ped, tempmort, risk, yr) 
{
    condped <- cbind(ped[ped[, "yod"] == 0, , drop = FALSE], 
        age = (yr - ped[ped[, "yod"] == 0, "yob"]))
    condped <- rbind(cbind(condped[condped[, "sex"] == 2, ], 
        rate = tempmort[, "femrate"][match(condped[condped[, 
            "sex"] == 2, "age"], tempmort[, "age"])]), cbind(condped[condped[, 
        "sex"] == 1, ], rate = tempmort[, "malerate"][match(condped[condped[, 
        "sex"] == 1, "age"], tempmort[, "age"])]))
    condped <- condped[!is.na(condped[, "rate"]) & condped[, 
        "rate"] != 0, ]
    condped[condped[, "yoi"] != 0, "rate"] <- condped[condped[, 
        "yoi"] != 0, "rate"] * risk
    if (nrow(condped[condped[, "rate"] > 1, , drop = FALSE]) > 
        0) {
        warning("One (or more) probability of death greater than 1 \n")
        condped[condped[, "rate"] > 1, "rate"] <- 1
    }
    condped <- cbind(condped, mortch = rbinom(nrow(condped), 
        1, condped[, "rate"]))
    ped[(ped[, "ID"] %in% condped[condped[, "mortch"] == 1, "ID"]) | 
        (ped[, "ID"] %in% condped[condped[, "age"] >= 100, "ID"]), 
        "yod"] <- yr
    return(ped)
}
