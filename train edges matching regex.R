regex <- "R'? E'? R2 E'?"

algs <- algiKrawedzie$nazwa[grep(regex,algiKrawedzie$alg)]

cat(gsub('(.{10,12}) ','\\1\n',paste(sample(algs),collapse = ' ')))
