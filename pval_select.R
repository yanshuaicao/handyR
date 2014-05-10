############################################################
### pvalue based automatic variable elimination/selection
### Author: Yanshuai Cao, 2014 May
############################################################

form.term <- function(s1, s2, sep=':') paste(s1, s2, sep=sep, collapse = '')
parse.term <- function(t) unlist(strsplit(t, "[*:]"))
term.equal <- setequal

term.in <- function(t, terms) {
  t <- parse.term(t)
  for (tt in terms) {
    if (term.equal(t, parse.term(tt))) return(TRUE)
  }
  return(FALSE)
}

form.cross.terms <- function(vars1, vars2=NULL, sep=':') {
  terms <- character()
  
  if (is.null(vars2)) {
    for (ind in 1:(length(vars1) - 1)) {
      for (ind2 in (ind+1):length(vars1)) {
        terms <- c(terms, form.term(vars1[ind], vars1[ind2]))
      }
    }
  } else {
    for (v1 in vars1) {
      for (v2 in vars2) {        
          terms <- c(terms, form.term(v1, v2))
        }
      }
    }
  return(terms)
}


pval.select <- function(data, regressor, independ.vars, depend.var,
                        cross.terms=FALSE,
                        cutoff.p.value=0.01,
                        p.value.name=NULL,
                        verbose=TRUE)
{
  ### helper funcs
  has.converged <- function(value, cutoff, verbose) {
    converged <- (value < cutoff)
    if (converged && verbose) {
      print(paste('The least significant variable,',
            names(value),
            ',is significant enough, we are done.'))
    }
    return(converged)
  }

  ### form regression terms
  if (typeof(cross.terms) == "character") {
    used.independ.vars <- c(independ.vars, cross.terms)
  } else if (cross.terms) {
    used.independ.vars <- c(independ.vars, form.cross.terms(independ.vars))
  } else {
    used.independ.vars <- independ.vars
  }
  
  print(used.independ.vars)
  
  ### main loop
  count <- 0
  repeat{
    count <- count + 1
    
    base.name <- paste(depend.var,
                       '_on_',
                       paste(used.independ.vars,
                             collapse = '_'),
                       sep = '')

    this.formula <- as.formula(paste(depend.var,'~',
                                     paste(used.independ.vars, collapse = '+'),
                                     sep = ''))

    if (verbose) {
      print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
      print(paste('Iteration #', count))
      print(this.formula)
      print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    }

    model <- regressor(this.formula, data=data)
    
    if (verbose) {
      print('*---------------------------------------------*')
      print('model:')
      print(base.name)
      print('*---------------------------------------------*')
      print(summary(model, which='mle'))
    }

    coefs <- coef(summary(model))
    if (is.null(p.value.name)) {
      sigs <- coefs[,ncol(coefs)]
    } else {
      sigs <- coef(summary(model))[,p.value.name]
    }

    
    sorted.sigs <- sort(sigs, decreasing = TRUE)
    worst.value <- sorted.sigs[1]
    
    if (has.converged(worst.value, cutoff.p.value, verbose)) break
    
    if (!(term.in(names(worst.value), used.independ.vars))) {
      
      msg <- paste('Least significant variable to drop:',
                   names(worst.value),
                   'is not an independent variable,',
                   'this could imply model misspecification.')
      
      warning(msg)

      if (verbose) print(paste('WARNING:', msg))
      
      #find the next least significant indep var
      sigs <- sigs[Filter(function(x) term.in(x, used.independ.vars),
                          names(sigs))]
      
      sorted.sigs <- sort(sigs, decreasing = TRUE)
      worst.value <- sorted.sigs[1]
      
      if (has.converged(worst.value, cutoff.p.value, verbose)) break
    }
    
    worst.term <- parse.term(names(worst.value))
    used.independ.vars <- Filter(function(x) !(term.equal(parse.term(x), worst.term)),
                                               used.independ.vars)

    #just a sanity check
    stopifnot(!(term.in(names(worst.value), used.independ.vars)))
    
    if (verbose) {
      print(paste('Dropping the least significant variable:', names(worst.value)))
    }
  } # end of main loop

  ### finish
  if (verbose) {
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    print('End of p-value based automatic variable elimination, final model:')
    print(summary(model, which='mle'))
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
  }
  return(model)
}


