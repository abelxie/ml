# compute the price using binomial model

compute.lattice = function(S0,u,nperiod) {
    m = matrix(0, nperiod+1, nperiod+1)
    m[1,1] = S0
    d = 1/u
    for(i in 1:nperiod){
        m[,i+1] = m[,i] * u
        m[i+1,i+1] = m[i,i] * d
    }
    m
}

risk.neutral.q = function(u,r) {
    d = 1/u
    (r - d)/(u-d)
}

create.model = function(S0, u, K, r, 
                        nperiod, d = 1/u, 
                        type=c('EUR.call')) {
    if (missing(u) && !missing(d)) u = 1/d
    re = list(S0 = S0, # time 0 price
         u = u  , # the upper direction rate
         d = d  , # typically 1/u
         K = K  , # strike price
         r = r  , # one period rate
         nperiod = nperiod,  # period
         type = type
    )
    re$s.lattice = compute.lattice(S0,u,nperiod)
    class(re) = 'simple.binomial.model'
    re
}

'[<-.simple.binomial.model' = function(obj, nm, value) {
    obj[[ nm ]] = value
    obj$s.lattice = compute.lattice(obj$S0, obj$u, obj$nperiod)
    obj
}

update.simple.binomial.model = function(obj, ...) {
    o = list(...)
    for(nm in names(o)) {
        obj[[ nm ]] = o[[ nm ]]
    }
    obj$s.lattice = compute.lattice(obj$S0, obj$u, obj$nperiod)
    obj
}

compute.payoff.EUR.call = function(v) {
    pmax(v$s.lattice[,v$nperiod+1] -  v$K, 0)
}

compute.payoff.AME.call = function(v) {
    compute.payoff.EUR.call(v)
}

compute.payoff.EUR.put = function(v) {
    pmax(v$K - v$s.lattice[,v$nperiod+1] , 0)
}

compute.payoff.AME.put = function(v) {
    compute.payoff.EUR.put(v)
}

compute.payoff = function(v) {
    f = sprintf('compute.payoff.%s', v$type)
    m = match.call()
    m[[1]] = as.symbol(f)
    eval(m)
}

payoff.lattice.EUR.call = function(v) {
    nperiod = v$nperiod
    m = matrix(0, nperiod+1, nperiod+1)
    m[, nperiod+1] = compute.payoff(v)
    q = risk.neutral.q(v$u, v$r)
    for(i in nperiod:1){
        m[1:i, i] = (q*m[1:i, i+1] + (1-q)*m[2:(i+1),i+1]) / v$r
    }
    m
}

payoff.lattice.EUR.put = function(v) {
    # exactly same way, using risk neutral measure
    payoff.lattice.EUR.call(v)
}

payoff.lattice.AME.call = function(v) {
    nperiod = v$nperiod
    m = matrix(0, nperiod+1, nperiod+1)
    m[, nperiod+1] = compute.payoff(v)
    q = risk.neutral.q(v$u, v$r)
    for(i in nperiod:1){
        m[1:i, i] = pmax( pmax(v$s.lattice[1:i,i] - v$K,0) , (q*m[1:i, i+1] + (1-q)*m[2:(i+1),i+1])/v$r )
    }
    m
}

payoff.lattice.AME.put = function(v) {
    nperiod = v$nperiod
    m = matrix(0, nperiod+1, nperiod+1)
    m[, nperiod+1] = compute.payoff(v)
    q = risk.neutral.q(v$u, v$r)
    for(i in nperiod:1){
        m[1:i, i] = pmax( pmax(v$K - v$s.lattice[1:i,i], 0) , (q*m[1:i, i+1] + (1-q)*m[2:(i+1),i+1])/v$r )
    }
    m
}

compute.price = function(v){
    stopifnot(is(v, 'simple.binomial.model'))
    f = sprintf('payoff.lattice.%s', v$type) 
    m = match.call()
    m[[1]] = as.symbol(f)
    m$v = v
    eval(m)[1,1]
}

print.simple.binomial.model = function(x, ...){
    cat(
    sprintf('%s period %s option with strike %s',x$nperiod, x$type, x$K),
    '\n',
    sprintf('Stock Model: u=%s%%, r=%s%%, S0=%s', round(100*(x$u-1),2), 
     round(100*(x$r-1),2), x$S0),
    '\n',
    sprintf('Price  =  %s', compute.price(x)),
    '\n'
    )
}
## Example

m1 = create.model(S0=100, u = 1.03, K = 110, r = 1.02, nperiod = 10)
m2 = update(m1,'type'='EUR.put')
m3 = update(m1,'type'='AME.call')
m4 = update(m1,'type'='AME.put')
