age.profile <-
function (ped, sex, year) 
{
    if (!(sex %in% c(1, 2))) 
        stop("Gender not recognized")
    ped <- ped[(ped[, "yod"] == 0 | ped[, "yod"] > year) & ped[, 
        "yob"] <= year & ped[, "sex"] == sex, ]
    ped <- cbind(ped, age = year - ped[, "yob"])
    runinprof <- cbind(as.integer(rownames(as.matrix(table(ped[, 
        "age"])))), as.matrix(table(ped[, "age"])))
    colnames(runinprof) <- c("age", "counts")
    return(runinprof)
}
