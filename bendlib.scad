
function take_first(a,l) = [ for (i = [0:min(len(a),l)-1]) a[i] ];
function take_first_or_default(a,b,l) = let(k = min(l,len(a))) concat([for(i=[0:k-1]) a[i]], [for(i=[k:l-1]) b[i]]);
function reverse(a, do_reverse = true) = do_reverse ? [for (i = [len(a) - 1 : -1 : 0]) a[i]] : a;

function m_flat(m) = [ for (k = m) for (n = k) n ];
function m_move(vec) = let(l = len(vec)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? 1 : j == l ? vec[i] : 0 ] ];
function m_scale(vec) = let(l = len(vec)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? i == l ? 1 : vec[i] : 0 ] ];
function m_scale_uniform(factor, dims = 3) = m_scale([for (i = [0:dims-1]) factor]);
function m_rot_2(angle) = [[cos(angle),-sin(angle),0],[sin(angle),cos(angle),0],[0,0,1]];
function m_rot_x(angle) = [[1,0,0,0],[0,cos(angle),-sin(angle),0],[0,sin(angle),cos(angle),0],[0,0,0,1]];
function m_rot_y(angle) = [[cos(angle),0,-sin(angle),0],[0,1,0,0],[sin(angle),0,cos(angle),0],[0,0,0,1]];
function m_rot_z(angle) = [[cos(angle),-sin(angle),0,0],[sin(angle),cos(angle),0,0],[0,0,1,0],[0,0,0,1]];
function m_id(dim = 3) = [ for (i = [0:dim]) [ for (j = [0:dim]) i == j ? 1 : 0 ] ];
function m_order(m_seq, i = 0) = i >= len(m_seq)-1 ? m_seq[i] : m_order(m_seq, i + 1) * m_seq[i];
function m_normalize(mt) = len(mt[0][0]) == undef ? mt : m_order(mt);

function p_3(pt, z = 0, y = 0, x = 0) = len(pt[0]) == undef 
    ? take_first_or_default(pt, [x,y,z], 3)
    : [ for (p = pt) p_3(p, z, y, x) ];
        
function p_tr(v, m) = let(l = len(m), mn = m_normalize(m)) 
    len(v[0]) == undef
    ? take_first(mn * [ for (i = [0:l-1]) i < len(v) ? v[i] : i == l-1 ? 1 : 0 ], len(v))
    : [ for (x = v) p_tr(x, mn) ];


//function p_arc(r, s, e, last = true) = 
function p_ngon(r, n) = [ for (i = [0:n-1]) let (a = i * 360 / n) [ cos(a) * r, sin(a) * r ] ];
function p_star(r1, r2, n) = let(m = n * 2) [ for (i = [0:m-1]) let(r = i % 2 == 0 ? r1 : r2) [ cos(i * 360 / m) * r, sin(i * 360 / m) * r ] ];