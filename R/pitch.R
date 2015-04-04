#' 12-tone equal temperment (12-TET)
#' @example
#'   P.n(40)
#'   P.n(c(40, 42, 43))
P.n <- absolute.frequency <- function(n, P.a = 440, a = 49)
  P.a * (2^(1/12))^(n - a)

intervals <- list(P1=0, P4=5, P5=7, P8=12,
                  M2=2, M3=4, M6=9, M7=11,
                  m2=1, m3=3, m6=8, m7=10)
attach(intervals)

chords <- list(M = cumsum(c(P1, M3, P5)),
               m = cumsum(c(P1, m3, P5)),
               M7 = cumsum(c(P1, M3, P5, M7)),
               m7 = cumsum(c(P1, M3, P5, m7)))


scales <- list(natural.minor = c(P1, m2, m3, P4, P5, m6, m7),
               major = c(P1, M2, M3, P4, P5, M6, M7))

#' Invert a chord
#' @param inversion 0 for base, 1 for first inversion, 2 for second inversion
invert.chord <- function(chord, full.inversion) {
  inversion <- full.inversion %% 3
  octave.shift <- (full.inversion - inversion) / 3

  bottom <- chord[(1+inversion):length(chord)]
  top <- chord[0:inversion] + intervals$P8
  octave <- intervals$P8 * octave.shift

  # Notes stay in the same order but are shifted.
  # This makes it easy to select the root note.
  c(top, bottom) + octave
}

detach(intervals)
