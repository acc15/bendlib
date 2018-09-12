
/** Computes factorial */
function fac(n) = n <= 1 ? 1 : n * fac(n - 1);

/** Normalizes array such that if `a` is array then return `a`, otherwise generates array with values `a` */
function a_cast(a, l) = len(a) == undef ? [ for (i = [0:l]) a ] : a;

/** Computes sum of all elements in array */
function a_sum(a, i = 0) = i >= len(a) ? 0 : a[i] + a_sum(a, i);

/** Computes squares vector norm */
function a_norm_sq(v) = a_sum([ for (e = v) e * e ]);

/** Computes unit vector */
function a_unit(v) = v / norm(v);

/** Takes first `l` elements from `a` */
function a_head(a,l) = [ for (i = [0:min(len(a),l)-1]) a[i] ];

/** Takes first `l` elements from `a` then from `b` */
function a_head_def(a,b,l) = let(k = min(l,len(a))) concat([for(i=[0:k-1]) a[i]], [for(i=[k:l-1]) b[i]]);

/** Reverses array */
function a_reverse(a, do_reverse = true) = do_reverse ? [for (i = [len(a) - 1 : -1 : 0]) a[i]] : a;


    


/** Creates offsets array from matrix (or any 2 dim array) */
function m_off(m, i = 0, sum = 0) = i > len(m) ? [] : concat([sum], m_off(m, i + 1, sum + len(m[i])));

/** Flatten matrix to array */
function m_flat(m) = [ for (k = m) for (n = k) n ];

/** Creates translation matrix */
function m_move(vec, dims = 3) = let(a = a_cast(vec, dims), l = len(vec)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? 1 : j == l ? vec[i] : 0 ] ];

/** Creates scale matrix. If vec is scalar - then creates uniform scale matrix */
function m_scale(vec, dims = 3) = let(a = a_cast(vec, dims), l = len(a)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? i == l ? 1 : a[i] : 0 ] ];

/** Creates 2d rotation matrix */
function m_rot_2(angle) = [[cos(angle),-sin(angle),0],[sin(angle),cos(angle),0],[0,0,1]];

/** Creates rotation matrix around X axis */
function m_rot_x(angle) = [[1,0,0,0],[0,cos(angle),-sin(angle),0],[0,sin(angle),cos(angle),0],[0,0,0,1]];

/** Creates rotation matrix around Y axis */
function m_rot_y(angle) = [[cos(angle),0,-sin(angle),0],[0,1,0,0],[sin(angle),0,cos(angle),0],[0,0,0,1]];

/** Creates rotation matrix around Z axis */
function m_rot_z(angle) = [[cos(angle),-sin(angle),0,0],[sin(angle),cos(angle),0,0],[0,0,1,0],[0,0,0,1]];

/** Creates rotation matrix from quaternion */
function m_rot_q(q) = [[q[0],-q[1],-q[2],-q[3]], [q[1],q[0],-q[3],q[2]], [q[2],q[3],q[0],-q[1]],[q[3],-q[2],q[1],q[0]]];

/** Creates rotation matrix from plane normal */
function m_rot_v(v) = let(x = a_unit(v), y = a_unit([-x[1],x[0],0]), z = a_unit(cross(x, y))) 
    [[x[0],y[0],z[0],0],[x[1],y[1],z[1],0],[x[2],y[2],z[2],0],[0,0,0,1]];

/** Creates identity matrix */
function m_id(dim = 3) = [ for (i = [0:dim]) [ for (j = [0:dim]) i == j ? 1 : 0 ] ];

/** If m_seq is matrix then return it, if m_seq is array of matricies then premultiplies all of them  */
function m_order(m_seq, i = 0) = i >= len(m_seq)-1 ? m_seq[i] : m_order(m_seq, i + 1) * m_seq[i];

/** If `mt` is matrix then return it, if `mt` is array of matricies then premultiplies all of them and returns result */
function m_normalize(mt) = len(mt[0][0]) == undef ? mt : m_order(mt);

/** Converts 2d point (or array of points) to 3d */
function p_3(pt, z = 0, y = 0, x = 0) = len(pt[0]) == undef 
    ? a_head_def(pt, [x,y,z], 3)
    : [ for (p = pt) p_3(p, z, y, x) ];
        
/** Transforms point (or array of points) using supplied matrix (or set of matrix - see `m_normalize`) */
function p_tr(v, m) = let(l = len(m), mn = m_normalize(m)) 
    len(v[0]) == undef
    ? a_head(mn * [ for (i = [0:l-1]) i < len(v) ? v[i] : i == l-1 ? 1 : 0 ], len(v))
    : [ for (x = v) p_tr(x, mn) ];

function p_ellipse_perimeter(r) = let(ra = a_cast(r, 2)) 
    2 * PI * ((rx == ry) ? ra[0] : sqrt(norm_s(ra[0]*ra[0] + ra[1]*ra[1]) / 2));
function p_arc_steps(r, a) = $fn > 0 ? $fn : ceil(max(min(a / $fa, p_ellipse_perimeter(r) / $fs), 5));

function p_arc_pt(r, a, p = [0,0]) = let(ra = a_cast(r, 2)) [cos(a) * ra[0] + p[0], sin(a) * ra[1] + p[1]];
function p_arc_loop(r, s, e, c, last = true, p = [0,0]) = r == 0 ? [p] : [ for(i = [0 : last ? c : c - 1]) p_arc_pt(r, s + i * (e - s) / c, p) ];
function p_arc(r, s, e, last = true, p = [0,0]) = arc_loop(r, s, e, p_arc_steps(r, abs(e - s)), last, p);

/** Generates NGON shape */
function p_ngon(r, n, p = [0,0]) = p_arc_loop(r, 0, 360, n, false, p);

/** Generates star shape */
function p_star(r1, r2, n) = let(ra1 = a_cast(r1, 2), ra2 = a_cast(r2, 2), m = n * 2) 
    [ for (i = [0:m-1]) [ cos(i * 360 / m) * (i % 2 == 0 ? ra1[0] : ra2[0]), sin(i * 360 / m) * (i % 2 == 0 ? ra1[1] : ra2[1]) ] ];
        
/** Creates 2d rectangle shape with specified rounded corner radiuses */
function p_rect(dim, r = 0) = let(rv = (len(r) == 4) ? [r[0],r[1],r[2],r[3]] : [r, r, r, r])
    concat(
        arc(rv[0], 0, 90, [dim[0] - rv[0], dim[1] - rv[0]]),
        arc(rv[1], 90, 180, [rv[1], dim[1] - rv[1]]),
        arc(rv[2], 180, 270, [rv[2], rv[2]]),
        arc(rv[3], 270, 360, [dim[0] - rv[3], rv[3]])
        );
    
/** Generates array of triangles required to connect two profiles */
function f_faces(a_off, a_len, b_off, b_len) = a_len > b_len
    ? f_faces_minmax(b_off, b_len, a_off, a_len, false)
    : f_faces_minmax(a_off, a_len, b_off, b_len, true);

/** Generates surface */
function f_surf(off, l, order = false) = l > 2 ? [a_reverse([for (i = [0:l-1]) off + (i % l)], order)] : [];

function f_faces_minmax(min_off, min_len, max_off, max_len, order = false) = let(k = floor(max_len / min_len)) concat(
    [ for (i = [0:max_len-1]) a_reverse([
            max_off + i % max_len, 
            min_off + floor(i / k) % min_len, 
            max_off + (i + 1) % max_len
        ], order)
    ],
    min_len > 1 ? [ for (i = [0:min_len-1]) a_reverse([
            min_off + i % min_len,
            max_off + floor((i + 1) * k) % max_len,
            min_off + (i + 1) % min_len
        ], !order)
    ] : []
);

/** Skins specified profiles forming 3d shape */
module p_skin(profiles, loop = false) {
    
    edges = m_flat(profiles);
    offsets = m_off(profiles);
    
    faces = concat([ for (i = [0:len(profiles)-2]) for (f = f_faces(
        offsets[i], offsets[i + 1] - offsets[i],
        offsets[i + 1], offsets[i + 2] - offsets[i + 1])) f ],
        loop ? [] : concat(
            f_surf(offsets[0], offsets[1] - offsets[0]), 
            f_surf(offsets[len(offsets)-2],offsets[len(offsets)-1] - offsets[len(offsets)-2], true)
        )
    );//, make_face(profiles[-1])];
    polyhedron(edges, faces);
    
}


/*

function ngon_poly(n, r, p = [0, 0]) = arc_loop(r, 0, 360, p, n, false);

module ngon(n, r) 
    polygon(ngon_poly(n, r));

module arc_profile(r, s, e, thickness) {
    inner = arc(r - thickness, s, e);
    outer = arc(r, e, s);
    polygon(concat(inner, outer));
}*/


function b_combination(i, n) = fac(n) / (fac(i) * fac(n - i));
function b_polynome(i, n, t) = b_combination(i, n) * pow(t, i) * pow(1 - t, n - i);
function b_component(i, n, t, p) = let (b = b_polynome(i, n, t)) [ for (c = p) c * b ];
function b_steps() = $fn > 0 ? $fn : 12;
function b_point(points, t, sum = [0,0], i = 0) = let(n = len(points) - 1) 
    i > n ? sum : b_point(points, t, b_component(i, n, t, points[i]) + sum, i + 1);
function b_curve(points, last = true) = len(points <= 2) ? points :
    let(segments = b_steps()) [ for (i = [0 : last ? segments : segments - 1]) b_point(points, i / segments) ];
    
module p_line_2(p1, p2, d) {   
    vec = p2 - p1;
    a = atan2(vec[1], vec[0]);
    
    l = [cos(a - 90) * d / 2, sin(a - 90) * d / 2];
    r = [cos(a + 90) * d / 2, sin(a + 90) * d / 2];
    
    polygon([
        p1 + r,
        p1 + l,
        p2 + l,
        p2 + r
    ]);
}

module p_lines(points, d) {
    for (i = [0:len(points)-1]) 
        p_line_2(points[i], points[(i+1) % len(points)], d);
}
    
module b_debug_point(pt, f, d) {
    translate([pt[0],pt[1],0]) 
        % color([f, (1 - f), 0]) circle(d = d);
}

module b_debug_line(p1, p2, f, d) {
    % color([f, (1 - f), 0]) line(p1, p2, d);
}

module b_debug(points, d = 2) {
    n = len(points);
    for (i = [0:n-2]) {
        f = i / (n - 1);
        b_debug_point(points[i], i / (n - 1), d);
        b_debug_line(points[i], points[i + 1], f, d / 2);
    }
    b_debug_point(points[n-1], 1, d);
}
 

