op_pos <- scan('opinions_en/positive-words.txt', what='character', comment.char=';')
op_neg <- scan('opinions_en/negative-words.txt', what='character', comment.char=';')

hindi_pos<-scan('opinions_en/hindi_positive.txt', what='character', comment.char=';')
hindi_neg<-scan('opinions_en/hindi_negative.txt', what='character', comment.char=';')
pos.words <- c(op_pos,hindi_pos, 'upgrade', 'fleek')
neg.words <- c(op_neg,hindi_neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical', 'bs', 'bullshit', 'b.s.')
pos.words<-sort(pos.words)
neg.words<-sort(neg.words)