draw.profile <-
function (pop, year, realmale, realfem) 
{
    profile.male <- age.profile(pop, sex = 1, year)
    profile.fem <- age.profile(pop, sex = 2, year)
    profile.male <- profile.male[profile.male[, "age"] %in% realmale[, 
        "age"], ]
    realmale <- realmale[realmale[, "age"] %in% profile.male[, 
        "age"], ]
    profile.fem <- profile.fem[profile.fem[, "age"] %in% realfem[, 
        "age"], ]
    realfem <- realfem[realfem[, "age"] %in% profile.fem[, "age"], 
        ]
    realmale[, "counts"] <- (realmale[, "counts"]/sum(realmale[, 
        "counts"])) * 100
    realfem[, "counts"] <- (realfem[, "counts"]/sum(realfem[, 
        "counts"])) * 100
    profile.male[, "counts"] <- (profile.male[, "counts"]/sum(profile.male[, 
        "counts"])) * 100
    profile.fem[, "counts"] <- (profile.fem[, "counts"]/sum(profile.fem[, 
        "counts"])) * 100
    split.screen(c(1, 2))
    screen(1)
    par(cex = 0.7)
    plot(range(c(realmale[, "age"], profile.male[, "age"])), 
        range(c(realmale[, "counts"], profile.male[, "counts"])), 
        type = "n", xlab = "age", ylab = "percentage")
    title(paste("Age profiles for the real and simulated \n MALE population of year", 
        year))
    lines(realmale[, "age"], realmale[, "counts"], col = "red", 
        lwd = 3, lty = 3)
    lines(profile.male[, "age"], profile.male[, "counts"], col = "red", 
        lwd = 2)
    leg.txt <- c("Real population", "Simulated population")
    legend(0, 0.22, leg.txt, lwd = c(3, 2), lty = c(3, 1), col = c("red", 
        "red"), cex = 0.9, bty = "n")
    screen(2)
    par(cex = 0.7)
    plot(range(c(realfem[, "age"], profile.fem[, "age"])), range(c(realfem[, 
        "counts"], profile.fem[, "counts"])), type = "n", xlab = "age", 
        ylab = "percentage")
    title(paste("Age profiles for the real and simulated \n FEMALE population of year", 
        year))
    lines(realfem[, "age"], realfem[, "counts"], col = "darkblue", 
        lwd = 3, lty = 3)
    lines(profile.fem[, "age"], profile.fem[, "counts"], col = "darkblue", 
        lwd = 2)
    leg.txt <- c("Real population", "Simulated population")
    legend(0, 0.22, leg.txt, lwd = c(3, 2), lty = c(3, 1), col = c("darkblue", 
        "darkblue"), cex = 0.9, bty = "n")
}
