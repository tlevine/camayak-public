song <- function() {
  df <- features(read.four.orgs())
  f <- function(season) verse(df, season)

  # Nothing happens in the first two seasons.
  lapply(unique(df$season)[-(1:2)], f)
}

#' Organizations ordered by average activity
order.orgs <- function(df = read.four.orgs()) {
  f <- function(org) mean(df[df$organization == org,'Activity.generated'])
  x <- sapply(levels(df$organization), f)
  names(sort(x))
}

#' Season
verse <- function(df.super, season) {
  df <- df.super[df.super$season == season,]
  org.order <- order.orgs(df.super)

  summer <- all(df$summer)
  if (summer) {
    # Picardy
    pitch.progression <- c(1,5,1,3)
    scale <- scales$natural.minor
    chord.type <- c('m', 'm', 'm', 'M')
  } else {
    pitch.progression <- c(1, 5, 6, 4)
    scale <- scales$major + intervals$P5
    chord.type <- c('M', 'M', 'm', 'M')
  }


  phrase.breaks <- data.frame(start = c(1, cumsum(rep(floor(nrow(df) / 4), 3))))
  phrase.breaks$end <- c(phrase.breaks$start[-1] + 1, nrow(df))

  f <- function(rowname)
    df[order(df$date,df$organization)[phrase.breaks[rowname,'start']:phrase.breaks[rowname,'end']],]
  phrase.dfs <- lapply(rownames(phrase.breaks), f)
# print(phrase.breaks)
# for (d in phrase.dfs)
#   print(range(d$date))

  g <- function(org, var) df[df$organization == org,var]


  inversion <- sapply(org.order, function(org)
    round(max(0, mean(g(org, 'Assignments.created'))/6)))

# chord.type <- sapply(org.order, function(org)
# # if (sd(g(org, 'Assignments.created') < 100)) 'M' else 'm'),
#   if (sum(g(org, 'Comments.written')) > 0) 'M' else 'm')

  # For the next measure
  pickup <- if (all(df$summer)) c(4) else c(2,4)


# cat('\n\n')
# print(pitch.progression)
# print(inversion)


  x <- list(phrases = lapply(phrase.dfs, phrase),
            summer = summer,
            drums = !summer,
            verse.number = as.numeric(season),
            chord.rhythm = 2^floor(log(mean(df$Comments.written) + 1, 3)),
            melody.rhythm = 2^floor(log(mean(df$Assignments.created) + 1, 2)))
  x$phrases[[4]]$measures[[4]]$end.of.verse <- TRUE
  h <- function(phrase) {
    for (i in 1:4) {
      phrase$measures[[i]]$organization <- org.order[i]
      phrase$measures[[i]]$base.pitch <- scale[pitch.progression[i]]
      phrase$measures[[i]]$chord.type <- chord.type[i]
      phrase$measures[[i]]$pickup.at.end <- i %in% pickup
      phrase$measures[[i]]$inversion <- inversion[i]
    }
    phrase
  }
  x$phrases <- lapply(x$phrases, h)
  for (h in 1:4) {
    x$phrases[[h]]$phrase.number <- h

    for (i in 1:4) {
      x$phrases[[h]]$measures[[i]]$end.dates <- rep(x$phrases[[h]]$start.date - 1, 4)
      names(x$phrases[[h]]$measures[[i]]$end.dates) <- org.order
      x$phrases[[h]]$measures[[i]]$end.dates[[i]] <- x$phrases[[h]]$end.date
    }

    x$phrases[[h]]$end.date <- x$phrases[[h]]$start.date <- NULL
  }
  x
}

phrase <- function(df) {
  w <- list(n.pickup.notes = round(max(0,log(df$Pitches.submitted,4))),
            pitches.submitted = sum(df$Pitches.submitted),
            start.date = min(df$date),
            end.date = max(df$date),
        
            # For calibration: hist(rollsum(four.orgs$Users.registered, 60))
            triplet.beats = (4:1)[0:round(max(0,log(sum(df$Users.registered),10)))],
            users.registered = sum(df$Users.registered),
        
            # Change this setting elsewhere.
            measures = lapply(rep(FALSE, 4),
                              function(x) list(end.of.verse = FALSE)))
  for (i in 1:4)
    w$measures[[i]]$measure.number <- i
  w
}

#' Define instruments based on signal generators and overtones.
