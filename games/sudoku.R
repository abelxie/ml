pmap = function(map, solution=NULL, title=if(is.null(solution)) 'Original' else 'Solution') {
    colsetting = c('green','red')
    plot(1:9, type = 'n', axes=F, xlab='', ylab='', main=title)
    axis(c(1,3), at=1:9, labels = 1:9, tick=F, xpd=NA)
    axis(c(2,4), at=1:9, labels = 9:1, tick=F, las = 1, xpd=NA)
    sol = !is.null(solution)
    for(i in 1:9){
        for(j in 1:9){
            x = j
            y = 9 - i + 1
            points(x,y, pch=22,cex=5,col=colsetting[ (map[i,j]==0) + 1], bg=colsetting[ (map[i,j]==0) + 1])
            if (map[i,j]!=0) text(x,y,label=map[i,j])
            if (sol) {
                text(x,y,label=solution[i,j])
            }
        }
    }
    abline(h=c(3.5,6.5), lty='dotted')
    abline(v=c(3.5,6.5), lty='dotted')
}

initialize.inds = function() {
    ind0 = matrix(1:81,9,9)
    row.inds = col.inds = mil.inds = vector(9,mode='list')
    ind2row = ind2col = ind2mil = integer(81)
    for(i in 1:9){
        row.inds[[i]] = ind0[i,]
        col.inds[[i]] = ind0[,i]
    }
    for(i in 1:3){
        for(j in 1:3){
            xind = 1:3 + (i-1)*3
            yind = 1:3 + (j-1)*3
            mil.inds[[ (i-1)*3 + j ]] = ind0[xind, yind]
        }
    }
    for(i in 1:9){
        ind2row[ row.inds[[i]] ] = i
        ind2col[ col.inds[[i]] ] = i
        ind2mil[ mil.inds[[i]] ] = i
    }
    assign('row.inds',row.inds,env=.GlobalEnv)
    assign('col.inds',col.inds,env=.GlobalEnv)
    assign('mil.inds',mil.inds,env=.GlobalEnv)
    assign('ind2row',ind2row,env=.GlobalEnv)
    assign('ind2col',ind2col,env=.GlobalEnv)
    assign('ind2mil',ind2mil,env=.GlobalEnv)
}

simplify.00 = function(map) {
    Rs = Cs = Ms = list()
    holes = map == 0
    filled = which(!holes)
    for(i in 1:9){
        foo = map[ row.inds[[i]] ]
        foo = foo[foo!=0]
        if(any(duplicated(foo))) return(FALSE)
        Rs[[i]] = setdiff(1:9, foo)
        foo = map[ col.inds[[i]] ]
        foo = foo[foo!=0]
        if(any(duplicated(foo))) return(FALSE)
        Cs[[i]] = setdiff(1:9, foo)
        foo = map[ mil.inds[[i]] ]
        foo = foo[foo!=0]
        if(any(duplicated(foo))) return(FALSE)
        Ms[[i]] = setdiff(1:9, foo)
    }
    #crme
    holes = which(holes)
    re = vector(length(holes),mode='list')
    Hs = integer(length(holes))
    no = 0
    for(h in holes){
        i = ind2row[h]
        j = ind2col[h]
        m = ind2mil[h]
        p = intersect(intersect(Rs[[i]], Cs[[j]]), Ms[[m]])
        if (length(p)==0) {
        # if a hole can not fill anything
            return(FALSE)
        }
        if (length(p)==1) {
        # only one possibility
            map[i,j] = p
            next()
        }
        no = no + 1
        # more than one
        re[[ no ]] = p
        Hs[[ no ]] = h
    }
    if (no==0) {
        Hs = integer(0)
        Re = list()
    }else{
        Hs = Hs[1:no]
        Re = simplify.Re(Hs,re[1:no])
        if (any(sapply(Re,length)==0)) return(FALSE)
    }
    # need recompute the Rs,Ms,Cs
    for(i in 1:9){
        foo = map[ row.inds[[i]] ]
        foo = foo[foo!=0]
        if(any(duplicated(foo))) return(FALSE)
        Rs[[i]] = setdiff(1:9, foo)
        #
        hinds = which(Hs %in% row.inds[[i]])
        kk = Rs[[i]]
        for(hind in hinds){
            kk = setdiff(kk, Re[[ hind ]])
        }
        if (length(kk)>0) return(FALSE)
        #
        foo = map[ col.inds[[i]] ]
        foo = foo[foo!=0]
        if(any(duplicated(foo))) return(FALSE)
        Cs[[i]] = setdiff(1:9, foo)
        #
        hinds = which(Hs %in% col.inds[[i]])
        kk = Cs[[i]]
        for(hind in hinds){
            kk = setdiff(kk, Re[[ hind ]])
        }
        if (length(kk)>0) return(FALSE)
        #
        foo = map[ mil.inds[[i]] ]
        foo = foo[foo!=0]
        if(any(duplicated(foo))) return(FALSE)
        Ms[[i]] = setdiff(1:9, foo)
        #
        hinds = which(Hs %in% mil.inds[[i]])
        kk = Ms[[i]]
        for(hind in hinds){
            kk = setdiff(kk, Re[[ hind ]])
        }
        if (length(kk)>0) return(FALSE)
        #
    }
    attr(map,'poss') = list(Rs=Rs,Cs=Cs,Ms=Ms,Hs=Hs,Re=Re)
    map
}

simplify.01 = function(map){
    last.map = map
    while(T){
        map = simplify.00(last.map)
        if (is.logical(map)) {
        # failed
            return(map)
        }
        poss = attr(map,'poss')
        Rs = poss$Rs
        Cs = poss$Cs
        Ms = poss$Ms
        Hs = poss$Hs
        if (length(Hs)==0) {
            attr(map,'success') = TRUE
            return(map)
        }
        Re = poss$Re
        for(i in 1:9){
            foo = which(Hs %in% row.inds[[i]])
            if(length(foo)>0){
                for(num in Rs[[i]]){
                    tmp = sapply(Re[foo], function(x) any(num == x))
                    if(sum(tmp)==1) {
                    # num must be at tmp position
                        pos = Hs[foo[tmp]]
                        map[ ind2row[pos], ind2col[pos] ] = num
                    }
                }
            }
            foo = which(Hs %in% col.inds[[i]])
            if(length(foo)>0){
                for(num in Cs[[i]]){
                    tmp = sapply(Re[foo], function(x) any(num == x))
                    if(sum(tmp)==1) {
                    # num must be at tmp position
                        pos = Hs[foo[tmp]]
                        map[ ind2row[pos], ind2col[pos] ] = num
                    }
                }
            }
            foo = which(Hs %in% mil.inds[[i]])
            if(length(foo)>0){
                for(num in Ms[[i]]){
                    tmp = sapply(Re[foo], function(x) any(num == x))
                    if(sum(tmp)==1) {
                    # num must be at tmp position
                        pos = Hs[foo[tmp]]
                        map[ ind2row[pos], ind2col[pos] ] = num
                    }
                }
            }
        }
        if (all(map==last.map)) break()
        last.map = map
    }
    map = simplify.00(map)
    if (length(attr(map,'poss')$Hs)==0) {
        attr(map, 'success') = TRUE
    } else {
        attr(map, 'success') = FALSE
    }
    map
}

initialize.inds()

simplify.Re = function(Hs, Re){
    any.simplified = FALSE
    for(i0 in seq_along(Hs)){
        h = Hs[i0]
        x = ind2row[h]
        y = ind2col[h]
        m = ind2mil[h]
        left.choice = try(Re[[ i0 ]],silent=T)
        if (is(left.choice,'try-error')) browser()
        # pair
        if (length(left.choice)==2) {
            # hole in the same row
            foo = which(Hs %in% row.inds[[x]])
            if (length(foo)>2) {
                foo = setdiff(foo, i0)
                for(i in seq_along(foo)){
                    if (length(Re[[foo[i]]])!=2) next()
                    if (all(left.choice==Re[[foo[i]]])) {
                    # found, elliminate other possibility
                        left.inds = foo[ -i ]
                        for(j in left.inds){
                            Re[[j]] = setdiff(Re[[j]], left.choice)
                        }
                        any.simplified = T
                        break()
                    }
                }
            }
            # hole in the same col
            foo = which(Hs %in% col.inds[[y]])
            if (length(foo)>2) {
                foo = setdiff(foo, i0)
                for(i in seq_along(foo)){
                    if (length(Re[[foo[i]]])!=2) next()
                    if (all(left.choice==Re[[foo[i]]])) {
                    # found, elliminate other possibility
                        left.inds = foo[ -i ]
                        for(j in left.inds){
                            Re[[j]] = setdiff(Re[[j]], left.choice)
                        }
                        any.simplified = T
                        break()
                    }
                }
            }
            # hole in the same mil
            foo = which(Hs %in% mil.inds[[m]])
            if (length(foo)>2) {
                foo = setdiff(foo, i0)
                for(i in seq_along(foo)){
                    if (length(Re[[foo[i]]])!=2) next()
                    if (all(left.choice==Re[[foo[i]]])) {
                    # found, elliminate other possibility
                        left.inds = foo[ -i ]
                        for(j in left.inds){
                            Re[[j]] = setdiff(Re[[j]], left.choice)
                        }
                        any.simplified = T
                        break()
                    }
                }
            }
        }
        # triple
        if (length(left.choice)==3) {
            # hole in the same row
            foo = which(Hs %in% row.inds[[x]])
            if(length(foo)>4) {
                foo = setdiff(foo, i0)
                cnt = c()
                for(i in seq_along(foo)){
                    if (length(Re[[foo[i]]])!=3) next()
                    if (all(left.choice==Re[[foo[i]]])) {
                    # found, elliminate other possibility
                        cnt = c(cnt, i)
                        if (length(cnt)==2) {
                            left.inds = foo[ -cnt ]
                            for(j in left.inds){
                                Re[[j]] = setdiff(Re[[j]], left.choice)
                            }
                            any.simplified = T
                            break()
                        }
                    }
                }
            }
            # hole in the same row
            foo = which(Hs %in% col.inds[[y]])
            if(length(foo)>4) {
                foo = setdiff(foo, i0)
                cnt = c()
                for(i in seq_along(foo)){
                    if (length(Re[[foo[i]]])!=3) next()
                    if (all(left.choice==Re[[foo[i]]])) {
                    # found, elliminate other possibility
                        cnt = c(cnt, i)
                        if (length(cnt)==2) {
                            left.inds = foo[ -cnt ]
                            for(j in left.inds){
                                Re[[j]] = setdiff(Re[[j]], left.choice)
                            }
                            any.simplified = T
                            break()
                        }
                    }
                }
            }
            # hole in the same row
            foo = which(Hs %in% mil.inds[[m]])
            if(length(foo)>4) {
                foo = setdiff(foo, i0)
                cnt = c()
                for(i in seq_along(foo)){
                    if (length(Re[[foo[i]]])!=3) next()
                    if (all(left.choice==Re[[foo[i]]])) {
                    # found, elliminate other possibility
                        cnt = c(cnt, i)
                        if (length(cnt)==2) {
                            left.inds = foo[ -cnt ]
                            for(j in left.inds){
                                Re[[j]] = setdiff(Re[[j]], left.choice)
                            }
                            any.simplified = T
                            break()
                        }
                    }
                }
            }
        }
    }
    attr(Re,'simplified')=any.simplified
    Re
}

solve.sudoku0 = function(map){
    m1 = simplify.01(map)
    if (is.logical(m1)) {
    # failed, backtrack
        COUNTS <<- COUNTS + 1
        return(FALSE)
    }
    if (attr(m1,'success')) {
        return(m1)
    }
    poss = attr(m1,'poss')
    Hs = poss$Hs
    Re = poss$Re
    l = sapply(Re, length)
    Hs = Hs[order(l)]
    Re = Re[order(l)]

    for(i in seq_along(Hs)){
        for(j in Re[[i]]){
            m1[ ind2row[Hs[i]], ind2col[Hs[i]] ] = j
            m2 = Recall(m1)
            if (is.logical(m2)) next()
            if (attr(m2,'success')) return(m2)
        }
    }
    COUNTS <<- COUNTS + 1
    FALSE
}

solve.sudoku = function(map){
    initialize.inds()
    assign('COUNTS',0,env=.GlobalEnv)
    timestamp()
    print(unix.time(re <- solve.sudoku0(map)))
    cat(sprintf('Backtrack COUNTS : %s \n', COUNTS))
    timestamp()
    if (is.logical(re)) {
        return(re)
    }
    pmap(map,re)
}


map.expert2 = rbind(
c(6,0,7,3,0,2,0,0,4),
c(0,8,0,0,0,0,0,0,0),
c(0,0,4,0,0,1,0,0,0),
c(0,5,0,0,0,7,0,8,0),
c(0,0,0,0,0,3,0,0,6),
c(0,0,0,0,2,0,9,4,1),
c(3,0,0,0,0,0,1,0,0),
c(0,0,0,0,0,0,0,0,0),
c(0,2,0,0,9,8,5,0,0))

map.test3 = rbind(
c(0,0,4,0,2,0,6,1,8),
c(0,0,3,0,1,0,7,9,5),
c(0,0,0,0,0,5,0,0,0),
c(3,2,0,1,7,6,0,0,9),
c(5,9,0,4,0,0,2,0,0),
c(0,0,0,0,9,0,0,0,7),
c(9,3,0,2,0,0,1,0,0),
c(0,0,0,0,5,0,0,0,2),
c(1,7,0,6,8,3,0,0,4))

map.easy = rbind(
c(9,0,6,2,3,5,0,0,4),
c(7,2,8,0,4,0,0,1,0),
c(4,0,5,7,1,0,0,6,2),
c(0,0,0,4,5,0,0,9,0),
c(0,4,0,0,0,3,6,5,0),
c(0,5,0,9,2,0,1,0,0),
c(0,9,1,3,7,2,0,0,6),
c(0,7,4,0,6,0,5,2,9),
c(8,6,0,5,0,4,0,3,0))

map.hard = rbind(
c(5,0,0,0,0,0,9,0,0),
c(0,0,0,0,0,0,5,3,0),
c(0,0,0,0,7,2,0,0,0),
c(0,0,7,0,0,0,0,4,0),
c(0,0,0,0,0,5,0,9,3),
c(0,4,2,0,0,0,0,8,0),
c(0,2,1,0,0,0,0,0,0),
c(4,0,0,0,0,8,0,0,0),
c(0,0,0,0,9,0,0,0,7))

map.expert = rbind(
c(rep(0,5),c(8,6,0,3)),
c(1,rep(0,6),7,4),
c(9,rep(0,8)),
c(0,0,5,0,0,0,8,4,0),
c(rep(0,3),8,0,0,1,0,0),
c(0,6,3,0,0,2,rep(0,3)),
c(rep(0,3),7,0,0,9,0,0),
c(rep(0,3),0,2,5,0,0,0),
c(rep(0,3),9,0,0,0,8,0))

map.test2 = matrix(c(
0,9,0,0,0,2,6,0,0,
1,0,0,0,8,0,4,7,0,
0,0,0,4,0,0,0,3,0,
0,0,2,0,0,0,8,0,0,
0,4,0,0,7,0,0,6,0,
9,0,0,0,0,1,0,2,0,
7,5,0,1,0,0,0,0,0,
0,8,9,0,3,5,0,0,0,
0,0,0,9,0,0,0,0,0),9,9,byrow=T)


map.test5 = matrix(c(
0,0,0,0,0,0,1,2,3,
0,0,9,0,0,0,0,0,0,
0,0,0,0,0,9,0,0,0,
0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0),9,9,byrow=T)

map.test6 = matrix(c(
8, 0, 0, 0, 0, 0, 0, 0, 0 , 
 0, 0, 3, 6, 0, 0, 0, 0, 0 , 
 0, 7, 0, 0, 9, 0, 2, 0, 0 , 
 0, 5, 0, 0, 0, 7, 0, 0, 0 , 
 0, 0, 0, 0, 4, 5, 7, 0, 0 , 
 0, 0, 0, 1, 0, 6, 0, 3, 0 , 
 0, 0, 1, 0, 0, 0, 0, 6, 8 ,  
 0, 0, 8, 5, 0, 0, 0, 1, 0 , 
 0, 9, 0, 0, 0, 0, 4, 0, 0 ),9,9)

map.test7 = matrix(c(
8, 0, 0, 0, 0, 0, 0, 0, 0 , 
 0, 0, 3, 6, 0, 0, 0, 0, 0 , 
 0, 7, 0, 0, 9, 0, 2, 0, 0 , 
 0, 5, 0, 0, 0, 7, 0, 0, 0 , 
 0, 0, 0, 0, 4, 5, 7, 0, 0 , 
 0, 0, 0, 1, 0, 0, 0, 3, 0 , 
 0, 0, 1, 0, 0, 0, 0, 6, 8 ,  
 0, 0, 8, 5, 0, 0, 0, 1, 0 , 
 0, 9, 0, 0, 0, 0, 4, 0, 0 ),9,9,byrow=T)

