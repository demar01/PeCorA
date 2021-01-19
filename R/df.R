df$loc<-c("nuc","cyt","nuc")
set.seed(4321)
spec = c(train = .7, test = .15, validate = .15)

g = sample(cut(
  seq(nrow(df)),
  nrow(df)*cumsum(c(0,spec)),
  labels = names(spec)
))


res = split(df, g)


rf <- caret::train(Loc ~ ., data = res$train[ ,-1], method = "rf",
                   allowParralel = TRUE,
                   tuneLength = 6,
                   ntree = 101)
