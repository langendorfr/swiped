#! swiped: symbolic english
#! Author: Ryan E. Langendorf
#! License: GPL v3

## Packages
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(ggforce))
suppressMessages(library(grid))

## Command line input arguments
args <- commandArgs(trailingOnly = TRUE)
input <- args[1]
named_input <- args[2]

## Whether to name each word
named <- FALSE
if (!is.na(named_input)) {
    if (named_input == "named") {
        named = TRUE
    }
}

## File to be swiped
text <- readChar(input, file.info(input)$size)
text = tolower(text)
text = gsub("[[:punct:] ]+", " ", text) #text = gsub("[[:punct:] ]+", " ", text)
text = gsub("[[:digit:]]+", "", text)
text = gsub("[\r\n]", " ", text)

## Digitized keyboard
letter_locations <- matrix(c(
    "q", 66, 214,
    "w", 191, 214,
    "e", 315, 213,
    "r", 435, 216,
    "t", 558, 219,
    "y", 683, 213,
    "u", 805, 211,
    "i", 928, 220,
    "o", 1052, 212,
    "p", 1176, 213,
    "a", 128, 386,
    "s", 257, 382,
    "d", 376, 381,
    "f", 498, 378,
    "g", 623, 383,
    "h", 745, 382,
    "j", 868, 381,
    "k", 987, 381,
    "l", 1116, 374,
    "z", 254, 549,
    "x", 376, 550,
    "c", 500, 550,
    "v", 623, 550,
    "b", 746, 549,
    "n", 868, 550,
    "m", 992, 549
), ncol = 3, byrow = TRUE)
colnames(letter_locations) = c("letter", "lat", "lon")
letter_locations = letter_locations %>% as_tibble()
letter_locations$lat = letter_locations$lat %>% as.numeric()
letter_locations$lon = letter_locations$lon %>% as.numeric()

## Keyboard digitized in the fourth quadrant, which is how Illustrator's ruler works
letter_locations$lon = -letter_locations$lon

## Split text into words
text_words <- strsplit(text, split = " ") %>% unlist()

## Remove extra spaces
text_words = text_words[!(text_words == "")]

## Number of points in each word
spline_length <- round(1e5 / length(text_words))
# spline_length <- round(1e3 / length(text_words))

text_positions <- tibble(
    word = rep(
        "MISSING",
        spline_length * length(text_words)
    ),
    lat = rep(
        0,
        spline_length * length(text_words)
    ),
    lon = rep(
        0,
        spline_length * length(text_words)
    )
)

text_thickness <- tibble(
    word = rep(
        "MISSING",
        spline_length * length(text_words)
    ),
    width = rep(
        0,
        spline_length * length(text_words)
    )
)

i_text_words <- seq_along(text_words)
max_text_words <- max(i_text_words)

print(paste0("Swiping ", input, " which has ", max_text_words, " words..."))

## Create each word's swiped symbol one at a time
for (word in i_text_words) {
    word_letters <- strsplit(text_words[word], split = "") %>% unlist()
    word_locations <- matrix(NA, 
                            nrow = length(word_letters),
                            ncol = 2)
    colnames(word_locations) = c("lat", "lon")

    for (wl in seq_along(word_letters)) {
        word_locations[wl,] = letter_locations %>% 
            dplyr::filter(letter == word_letters[wl]) %>% 
            dplyr::select(lat, lon) %>%
            unlist()
        word_locations[wl,] = word_locations[wl,] + rnorm(
            n = 2,
            sd = 1e-5 * abs(mean(word_locations[wl,]))
        )
    }

    spline_word <- apply(
        word_locations,
        2,
        function(u) {
            stats::spline(
                1:nrow(word_locations),
                u,
                xout = seq(
                    from = 1,
                    to = nrow(word_locations),
                    length = spline_length)
            )$y
        }
    ) %>% as_tibble()

    spline_word = spline_word %>% dplyr::transmute(
        word = paste0(word, "_", text_words[word]), across(everything())
    )
    text_positions[(1 + (word*spline_length) - spline_length) : (word*spline_length), c(1,2,3)] = spline_word

    ## Need to add two additional rows for the thickness calculation
    if (nchar(text_words[word]) == 1) {
        word_locations_jittered_1 <- word_locations + rnorm(n = 2, sd = 1e-5 * abs(mean(word_locations)))
        word_locations_jittered_2 <- word_locations + rnorm(n = 2, sd = 1e-5 * abs(mean(word_locations)))
        word_locations = rbind(word_locations, word_locations_jittered_1, word_locations_jittered_2)
    }

    ## Thickness calculations
    word_thickness <- matrix(
        NA,
        nrow = length(word_letters),
        ncol = 2
    )
    colnames(word_thickness) = c("distance", "angle")
    word_thickness = word_thickness %>% as_tibble()

    ## Handle single letter words first
    if (nchar(text_words[word]) == 1) {
        wl <- 1
        word_thickness$distance[wl] = 1e-5
        word_thickness$angle[wl] = 1e-5
        word_thickness_jittered <- word_thickness + rnorm(n = 2, sd = 1e-50)
        word_thickness = rbind(word_thickness, word_thickness_jittered)
    } else {
        for (wl in 1:nrow(word_locations)) {
            if (wl == nrow(word_locations)) {
                word_thickness$distance[wl] = 1e-5
            } else {
                word_thickness$distance[wl] = sqrt((word_locations[wl, 1] - word_locations[wl+1, 1])^2 + (word_locations[wl, 2] - word_locations[wl+1, 2])^2)
            }

            ## No change in direction possible for first letter
            if (wl %in% c(1, nrow(word_locations))) {
                word_thickness$angle[wl] = 1e-5
            } else {
                word_thickness$angle[wl] = LearnGeom::Angle(word_locations[wl-1,], word_locations[wl,], word_locations[wl+1,])
            }
        }

    }

        ## Need to normalize distance and angle by the max possible
        word_thickness[, 1] = word_thickness[, 1] / max(word_thickness[, 1])
        max_angle <- 180
        word_thickness[, 2] = word_thickness[, 2] / max_angle

        spline_thickness <- apply(
            word_thickness,
            2,
            function(u) {
                stats::spline(
                    1:nrow(word_thickness),
                    u,
                    xout = seq(
                        from = 1,
                        to = nrow(word_thickness),
                        length = spline_length))$y
            }
        ) %>% as_tibble()

        spline_thickness = spline_thickness %>% dplyr::transmute(
            word = paste0(word, "_", text_words[word]), across(everything())
        )

        ## Collapse distance and angle between letters
        spline_thickness = tibble(
            word = spline_thickness$word,
            width = log10(1 + (abs(spline_thickness$angle) / abs(spline_thickness$distance)))
        )

    text_thickness[(1 + (word*spline_length) - spline_length) : (word*spline_length),] = spline_thickness
}

text_positions$word = factor(
    text_positions$word,
    levels = text_positions$word %>% 
        unique() %>% 
        as.character()
)

## Merge thickness into coordinates
text_thickness = text_thickness %>% dplyr::select(width)
text_positions = dplyr::bind_cols(text_positions, text_thickness)

## The last width will be infinite
text_positions$width[is.na(text_positions$width)] = 1e-5

## Some widths will be infinite
text_positions$width[is.infinite(text_positions$width)] = 1

## Rescale thickness
text_positions$width = text_positions$width/max(text_positions$width)
text_positions$width = ((1+text_thickness$width)^(1/5))

## Remove any file extension from the name of the file
save_name <- strsplit(input, "[.]") %>% unlist()
save_name = paste(save_name[-length(save_name)], collapse = "")

## Save as "png" (raster) or "pdf" (vector)
format = "png"
if (format == "png") {
    grDevices::png(
        filename = paste0(
            "Swiped_",
            save_name,
            "_",
            gsub(
                pattern = "-",
                replacement = "",
                gsub(
                    pattern = ":",
                    replacement = "-",
                    gsub(
                        pattern = " ",
                        replacement = "-",
                        x = Sys.time()
                    )
                )
            ),
            ".png"
        ),
        width = 8.5,
        height = 11,
        # onefile = TRUE
        units = "in",
        res = 500
    )
} else if (format == "pdf") {
    grDevices::cairo_pdf(
        filename = paste0(
            "Swiped_",
            save_name,
            "_",
            gsub(
                pattern = "-",
                replacement = "",
                gsub(
                    pattern = ":",
                    replacement = "-",
                    gsub(
                        pattern = " ",
                        replacement = "-",
                        x = Sys.time()
                    )
                )
            ),
            ".pdf"
        ),
        width = 8.5,
        height = 11,
        onefile = TRUE
    )
} else {
    stop("Unknown format. Currently only 'pdf' and 'png' are supported.")
}

if (named) {
    text_positions = text_positions %>% dplyr::mutate(word_titles = as.character(text_positions$word))
    text_positions$word_titles = do.call(rbind, strsplit(text_positions$word_titles, "_"))[,2]
    text_positions$word_titles = factor(text_positions$word_titles, levels = text_positions$word_titles %>% unique())

    ##!! FIX: facet_wrap(~world_titles) incorrectly combines words that repeat
    page_print <- ggplot(text_positions) +
        theme_void() +
        facet_wrap(
            ~word_titles,
            ncol = 2 + round(length(text_words)^(8.5/(8.5 + 11)))
        ) +
        geom_point(
            aes(
                x = lat,
                y = lon
            ),
            size = 10/length(text_words)
        ) +
        coord_fixed() +
        guides(size = "none") +
        theme(
            # strip.text = element_blank(),
            plot.title = element_text(size=0.2),
            panel.spacing.x = unit(1, "mm"),
            panel.spacing.y = unit(5, "mm"),
            plot.margin = unit(c(3, 3, 3, 3), "mm")
        )
} else {
    page_print <- ggplot(text_positions) +
        theme_void() +
        facet_wrap(
            ~word,
            ncol = 2 + round(length(text_words)^(8.5/(8.5 + 11)))
        ) +
        geom_point(
            aes(
                x = lat,
                y = lon
            ),
            size = 10/length(text_words)
        ) +
        coord_fixed() +
        guides(size = "none") +
        theme(
            strip.text = element_blank(),
            panel.spacing.x = unit(1, "mm"),
            panel.spacing.y = unit(5, "mm"),
            plot.margin = unit(c(3, 3, 3, 3), "mm")
        )
}

print(page_print)
invisible(dev.off())

print("Swiped.")