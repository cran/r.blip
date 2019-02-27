get_temp <- function() {
    return (tempfile())
}

get_temp <- function(ext) {
    return (tempfile(fileext = ext))
}

get_temp_jkl <- function() {
    return (get_temp(".jkl"))
}

get_temp_dat <- function() {
    return (get_temp(".dat"))
}

get_temp_arff <- function() {
    return (get_temp(".arff"))
}

get_temp_res <- function() {
    return (get_temp(".res"))
}

get_temp_net <- function() {
    return (get_temp(".net"))
}


#' Jkl reader
#'
#' Read a Jkl file (parent sets cache) 
#'
#' @param path Path of the file to load
#' @param names List of variable names
#' @return the cache of parent sets
#'
read.jkl <- function(path, names) {

#     print(path)
    con  <- file(path, open = "r")
    ln <- readLines(con, n=1)
    while (grepl("^[#]", ln, perl=T))
        ln <- readLines(con, n=1)
    n <- strtoi(ln)
    ln <- readLines(con, n=1)
    while (grepl("^[#]", ln, perl=T))
        ln <- readLines(con, n=1)
    jkl <- list()
    for (i in 1:n) {
        ln <- strsplit(ln, " ", fixed=T)[[1]]
        np <- strtoi(ln[2])
        psets <- character(np)
        for (j in 1:np) {
            ln <- readLines(con, n=1)
            ln <- strsplit(ln, " ", fixed=T)[[1]]
            sc = ln[1]
            sz = ln[2]
            pset = ln[-c(1,2)]
            au <- paste(sapply(pset, function(z) names[strtoi(z)+1]), collapse=", ")
            au <- paste(c(sc, " # ", au, ""), collapse="")
            psets[j] <- au[1]
        }
       jkl[[i]] <- list(name=names[i], psets=psets)
        ln <- readLines(con, n=1)
    }

close(con)
    return(jkl)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#' Jkl writer (with names)
#'
#' Write a Jkl file (parent sets cache) 
#'
#' @param path Path of the file to write
#' @param jkl parent sets cache to write
#'
write.jkl <- function(path, jkl) {

   con  <- file(path, open = "w")
    n <- length(jkl)
    
    names <- sapply (jkl, function(x) x$name)

    writeLines(as.character(n), con)
    for (i in 1:n) {
        nm <- jkl[[i]]$name
        psets <- jkl[[i]]$psets
        np <- length(psets)
        writeLines(paste(i - 1, np, sep=" "), con)
        
        for (j in 1:np) {
            ps <- psets[j]
            ln <- strsplit(ps, "#", fixed=T)[[1]]
            sc = trim(ln[1])
            pset <- strsplit(trim(ln[2]), ", ", fixed=T)[[1]]
            if(length(pset) == 0) {
                au <- paste(sc, 0, collapse=" ") 
                writeLines(au, con)
            } else {
                # cat (pset)
                au <- sapply(pset, function(z) which(z == names)[[1]] - 1)
                au <- paste(sc, length(au), paste(au, collapse=" "), collapse=" ") 
                writeLines(au, con)
            }
        }
    }

    close(con)
    
    return(names)
}


#' Structure reader
#'
#' Reads a str file (BN structure) 
#'
#' @param path Path of the file to load
#' @param names List of variable names
#' @return the BN structure
#'
read.str <- function(path, names) {

    con <- file(path, open = "r")
    n <- length(names)
    
    str <- ""
    for (i in 1:n) {
        ln <- readLines(con, n=1)
        ln <- strsplit(ln, ":", fixed=T)[[1]]
        v <- strtoi(ln[1])
        str <- paste(str, "[",names[v+1], sep="")

        ps <- strsplit(ln[2], "(", fixed=T)[[1]]

        if (length(ps) > 1) {
            str <- paste(str, "|", sep="")
            ps <- strsplit(ps[2], ")", fixed=T)[[1]]
            ps <- strsplit(ps[1], ",", fixed=T)[[1]]

            c <- sapply (ps, function(x) names[strtoi(x)+1])

            c <- paste(c, sep="", collapse=":")
            
            str <- paste(str, c, sep="")
        }

        str <- paste(str, "]", sep="")
    }

close(con)
    return(str)
}


