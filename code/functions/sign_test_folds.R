sign_test_folds = function(x1, x2, n, split = 9/10) { # 9/10 split for 10-fold cv
  J = length(x1)
  N_train = n * split
  N_test = n * (1-split)
  d = x2 - x1
  m = mean(d)
  v = var(d)
  
  t = m / sqrt(v * (1/J + N_test/N_train))
  df = J - 1
  
  # one-sided p-value
  if (m >= 0){
    p = pt(abs(t), df, lower.tail = FALSE) # one.sided p-value for correct direction
  } else {
    p = 1-pt(abs(t), df, lower.tail = FALSE) # one.sided p-value for incorrect direction
  }
  
  return(p)
}

