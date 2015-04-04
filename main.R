library(devtools)
unloadNamespace('devtools')
devtools::load_all()

BPM <- 240
if (!('four.orgs' %in% ls())) four.orgs <- read.four.orgs()

score <- song()
# score <- score[1:3]
wave <- compose.song(four.orgs, score, bpm = BPM)

sixteenths.per.second <- BPM * 4 / 60

wav.file <- '/tmp/camayak.wav'
writeWave(normalize(wave, unit = '16'), wav.file)
system('./video.sh')
