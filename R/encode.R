#' To add: triplets and n.pickup.notes
library(tuneR)
library(doParallel)
library(foreach)

orgs <- c('PSFK.com', 'The State Press', 'The Duke Chronicle', 'Daily Bruin')

org.colors <- c('#683496', '#9A0032', '#021B3F', '#0080C6')
names(org.colors) <- orgs

test <- function()
  play(normalize(compose.song(song(), bpm = 240), unit = '16'), 'play')

plot.measure.video <- function(four.orgs, end.dates,
                               verse.number, phrase.number, measure.number, 
                               verse, phrase, measure,
                               drums,
                               present.org, chord.rhythm,
                               users.registered, pitches.submitted,
                               ...) {
  if (verse$summer) { fg <- 'black'; bg <- 'grey20' }
  else { fg <- 'grey60'; bg <- 'black' }
  four.orgs$log.activity.generated <- log(four.orgs$Activity.generated + 1)
  for (submeasure in 1:2) {

    fn <- sprintf('/tmp/camayak-%02d-%02d-%02d-%02d.png',
                  verse.number, phrase.number, measure.number, submeasure)
    png(fn, width = 1600, height = 900, type = 'cairo-png')

    par(fg = fg, col = fg, col.axis = fg,
        col.lab = fg, col.main = fg, col.sub = fg,
        cex.axis = 2,
        las = 1, bg = bg, ...)
    plot(log.activity.generated ~ date, type = 'n', data = four.orgs,
         xlab = '', ylab = '', bty = 'n', axes = FALSE)



    text(x = as.Date('2012-05-01'), y = log(600), pos = 4,
         font = 2, family = 'Helvetica',
         label = 'Activity generated\nby day',
         cex = 2, col = fg)
    if (verse.number == 12)
      text(x = as.Date('2013-04-01'), y = log(c(500, 450)),
       labels = c('Journalism-Driven Data', 'Thomas Levine\nSpecial thanks to Stanton Burton\nhttp://thomaslevine.com'),
       pos = c(3, 1), cex = 1.5, font = c(2, 1),
       family = 'Helvetica', col = 'white')

    breaks <- seq.Date(as.Date('2012-04-01'), as.Date('2015-01-01'), by = '3 months')
    labels <- strftime(breaks, '%b')
    januaries <- months(breaks) == 'January'
    labels[januaries] <- strftime(breaks[januaries], '%Y\n%b')
    axis(1, breaks, labels, lwd = 0)

    labels <- c(0, 1, 5, 10, 50, 100, 500, 1000)
    breaks <- log(labels + 1)
    axis(2, breaks, labels, lwd = 0)

    # So the present org is on top
    reordered.orgs <- c(orgs[orgs != present.org], present.org)

    for (org in reordered.orgs) {
      df <- subset(four.orgs, organization == org & date <= end.dates[[org]])

      if (nrow(df) > 0) {
        lines(df$log.activity.generated ~ df$date, col = org.colors[[org]],
              lwd = 5)
        if (org == present.org) {
          selector <- end.dates[[org]] == df$date
  
          cex <- 2 + if (submeasure == 2) users.registered
                     else pitches.submitted
  
          points(df[selector,'log.activity.generated'] ~ df[selector,'date'],
                 cex = cex, lwd = 5, col = org.colors[[present.org]],
                 pch = submeasure)
        }
      }
      if (org == present.org) {
        label <- if (nrow(df) > 0 && df[selector,'log.activity.generated'] > 0) present.org
                 else paste0('(', present.org, ')')
        text(x = as.Date('2012-09-01'), y = log(8.5), cex = 3,
             font = 2, family = 'Helvetica',
             label = label, col = org.colors[[present.org]])
      }
    }

    season <- c('Winter', 'Spring', 'Summer', 'Autumn')[1 + (verse.number %% 4)]
    text(x = as.Date('2012-09-01'), y = log(6), cex = 2,
         font = 1, family = 'Helvetica',
         label = season, col = fg)

    dev.off()
    cat(paste0('Plotted ', fn, '\n'))
  }
}


compose.song <- function(four.orgs, s, bpm = 180) {
  registerDoParallel()

# verse <- s[[1]]
  waves <- foreach (verse = s) %dopar% {

    wave <- empty.wave
    for (phrase in verse$phrases)
      for (measure in phrase$measures) {
        wave <- bind(wave, compose.measure(verse, phrase, measure, bpm = bpm))
        plot.measure.video(four.orgs, measure$end.dates,
                           verse$verse.number, phrase$phrase.number, measure$measure.number, 
                           verse, phrase, measure,
                           verse$drums,
                           measure$organization, verse$chord.rhythm,
                           phrase$users.registered, phrase$pitches.submitted)
      }
    wave

  }
  Reduce(bind, waves)
}

#' This will clash if you try to combine measures with different bpm.
compose.measure <- function(verse, phrase, measure, bpm = 180) {
  chord <- measure$base.pitch + invert.chord(chords[[measure$chord.type]],
                                             measure$inversion)

  melody <- rep(c(chord[2], NA), verse$melody.rhythm) # spaces for possible pickups
  if (measure$pickup.at.end) {
    n <- phrase$n.pickup.notes
    while (n > 0 && any(is.na(melody))) {
      n <- n - 1
      old <- melody[is.na(melody)]
      melody[is.na(melody)] <- c(old[1:(length(old)-1)], chord[1])
    }
  }


  pitch.tracks <- drum.tracks <- list()
  pitch.tracks$melody <- melody
  if (!measure$end.of.verse) {
    pitch.tracks$chord1 <- rep(chord[1], verse$chord.rhythm)
    pitch.tracks$chord3 <- rep(chord[3], verse$chord.rhythm)
    if (verse$drums) {
      drum.tracks$snare.drum <- list(sample = normalize(roland$SD1, unit = '1'),
                                     rhythm = c(FALSE, FALSE, TRUE, FALSE))
      if (verse$chord.rhythm >= 2)
        drum.tracks$main.drums <- list(sample = normalize(roland$BD0, unit = '1'),
                                       rhythm = c(TRUE, FALSE, FALSE, FALSE))
      if (verse$melody.rhythm >= 8)
        drum.tracks$hi.hat <- list(sample = normalize(roland$HHC, unit = '1'),
                                   rhythm = c(FALSE, TRUE, FALSE, TRUE))
    }
  }

  seconds.per.measure <- 60 * 4 / bpm
  measure.duration.raw <- seconds.per.measure * samp.rate # in samples

  # for nice division
  measure.duration <- measure.duration.raw - (measure.duration.raw %% 2^8)

  waves <- append(lapply(pitch.tracks, generate.pitch, measure.duration = measure.duration),
                  lapply(drum.tracks, generate.drum, measure.duration = measure.duration))

# waves <- lapply(pitch.tracks, generate.pitch, measure.duration = measure.duration)

  result <- waves[[1]]
  for (wave in waves[-1])
    result <- result + wave
  result
}

samp.rate <- 44100
empty.wave <- Wave(numeric(0), numeric(0), samp.rate, 16)
empty.wave@stereo <- TRUE

generate.pitch <- function(vec, measure.duration = NULL) {
  note.duration <- measure.duration / length(vec)
  no.sound <- silence(note.duration, xunit = 'samples', stereo = TRUE)

  frequencies <- P.n(vec, a = 12)

  # Rewrite this more cleanly with a reduce.
  result <- empty.wave
  for (freq in frequencies) {
    if (is.na(freq)) result <- bind(result, no.sound)
    else {
      tone <- compose.note(freq, note.duration)
      result <- bind(result, tone)
    }
  }

  result
}

generate.drum <- function(drum, measure.duration = NULL) {
  note.duration <- measure.duration / length(drum$rhythm)
  extracted.wave <- extractWave(drum$sample, from = 0, to = note.duration, xunit = 'samples')
  sound <- bind(extracted.wave,
                silence(note.duration - length(extracted.wave),
                        xunit = 'samples', stereo = TRUE))
  no.sound <- silence(note.duration, xunit = 'samples', stereo = TRUE)
  sequence <- list(no.sound, no.sound, no.sound, no.sound)
  sequence[drum$rhythm] <- sound

  Reduce(bind, sequence, empty.wave)
} 

polynomial.fade <- function(n)
  function(n.samples) ((n.samples:1)/n.samples) ^ n
no.fade <- polynomial.fade(0)
linear.fade <- polynomial.fade(1)
quadratic.fade <- polynomial.fade(2)
sqrt.fade <- polynomial.fade(1/2)

compose.note <- function(freqs, duration, fade = sqrt.fade) {
  relative.amplitudes <- (2^-(2/3)*(0:(length(freqs))))[-1]

  x <- NULL
  for (i in 1:length(freqs)) {
    y <- sine(freqs[i], duration, xunit = 'samples', stereo = TRUE) * fade(duration)
    if (is.null(x)) x <- y
    else x <- x + y * relative.amplitudes[i]
  }

  x
}
