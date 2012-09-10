decide <-
function (ped, sex, pop, baseyr) 
{
    if (!(sex %in% c(1, 2))) 
        stop("Gender not recognized")
    ped <- ped[ped[, "sex"] == sex, ]
    pedpop <- age.profile(ped, sex, baseyr)
    pedpop <- pedpop[pedpop[, "age"] %in% pop[, "age"], ]
    pop <- pop[pop[, "age"] %in% pedpop[, "age"], ]
    this.age <- pop[pop[, "counts"] == max(pop[, "counts"]), 
        "age"]
    pop <- cbind(pop, ratio = pop[, "counts"]/pop[pop[, "counts"] == 
        max(pop[, "counts"]), "counts"])
    colnames(pop) <- c("age", "counts", "ratio")
    pedpop <- cbind(pedpop, ratio = pedpop[, "counts"]/pedpop[pedpop[, 
        "age"] == this.age, "counts"])
    colnames(pedpop) <- c("age", "counts", "ratio")
    old.value.this <- pedpop[pedpop[, "age"] == this.age, "counts"]
    compare <- cbind(pedpop[, "age"], pedpop[, "ratio"], pop[, 
        "ratio"])
    compare <- cbind(compare, ratio = compare[, 2]/compare[, 
        3])
    colnames(compare) <- c("age", "sim_ratio", "true_ratio", 
        "ratio")
    the.factor <- min(compare[!is.na(compare[, "ratio"]), "ratio"])
    age.prob <- compare[(compare[!is.na(compare[, "ratio"]), 
        "ratio"] == min(compare[!is.na(compare[, "ratio"]), "ratio"])), 
        "age"]
    new.pedpop <- cbind(compare[, "age"], compare[, "true_ratio"], 
        rep(0, length = nrow(pedpop)))
    colnames(new.pedpop) <- c("age", "true_ratio", "counts")
    new.pedpop[new.pedpop[, "age"] == this.age, "counts"] <- the.factor * 
        pedpop[pedpop[, "age"] == this.age, "counts"]
    new.pedpop[, "counts"] <- new.pedpop[, "true_ratio"] * new.pedpop[new.pedpop[, 
        "age"] == this.age, "counts"]
    probs <- cbind(new.pedpop[, "age"], p = new.pedpop[, "counts"]/pedpop[, 
        "counts"])
    colnames(probs) <- c("age", "p")
    probs[probs[, "age"] == this.age, "p"] <- new.pedpop[probs[, 
        "age"] == this.age, "counts"]/old.value.this
    ped <- cbind(ped, age = (baseyr - ped[, "yob"]))
    ped <- cbind(ped, prob = probs[, "p"][match(ped[, "age"], 
        probs[, "age"])])
    ped <- ped[!is.na(ped[, "prob"]), ]
    ped[ped[, "prob"] > 1, "prob"] <- 1
    ped <- cbind(ped, p = rbinom(nrow(ped), 1, ped[, "prob"]))
    ped <- ped[ped[, "p"] == 1, ]
    ped <- ped[, c(1:7)]
    return(as.matrix(ped))
}
