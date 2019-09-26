
/** Computes factorial */
function bl_fac(n) = n <= 1 ? 1 : n * fac(n - 1);

/** Normalizes array such that if `a` is array then return `a`, otherwise generates array with values `a` */
function bl_cast(a, l) = len(a) == undef ? [ for (i = [0:l-1]) a ] : a;

/** Computes sum of all elements in array */
function bl_sum(a, i = 0) = i >= len(a) ? 0 : a[i] + bl_sum(a, i + 1);

/** Checks whether v is zero or not */
function bl_zero(v, i = 0) = i >= len(v) ? true: v[i] == 0 && bl_zero(v, i + 1);

function bl_fill(v, l) = [ for (i = [0:l-1]) v ];

/** Computes squares vector norm */
function bl_norm_sq(v) = bl_sum([ for (e = v) e * e ]);

/** Computes unit vector */
function bl_unit(v) = v / norm(v);

/** Takes first `l` elements from `a` */
function bl_head(a,l) = [ for (i = [0:min(len(a),l)-1]) a[i] ];

/** Takes first `l` elements from `a` then from `b` */
function bl_head_def(a,b,l) = let(k = min(l,len(a))) concat([for(i=[0:k-1]) a[i]], [for(i=[k:l-1]) b[i]]);

/** Reverses array */
function bl_reverse(a, do_reverse = true) = do_reverse ? [for (i = [len(a) - 1 : -1 : 0]) a[i]] : a;

/** Creates offsets array from matrix (or any 2 dim array) */
function bl_off(m, i = 0, sum = 0) = i > len(m) ? [] : concat([sum], bl_off(m, i + 1, sum + len(m[i])));

/** Flatten matrix to array */
function bl_flat(m) = [ for (k = m) for (n = k) n ];

/** Creates translation matrix */
function bl_move(vec, dims = 3) = let(a = bl_cast(vec, dims), l = len(vec)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? 1 : j == l ? vec[i] : 0 ] ];

/** Creates scale matrix. If vec is scalar - then creates uniform scale matrix */
function bl_scale(vec, dims = 3) = let(a = bl_cast(vec, dims), l = len(a)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? i == l ? 1 : a[i] : 0 ] ];

/** Creates 2d rotation matrix */
function bl_rot_2(angle) = [[cos(angle),-sin(angle),0],[sin(angle),cos(angle),0],[0,0,1]];

/** Creates rotation matrix around X axis */
function bl_rot_x(angle) = [[1,0,0,0],[0,cos(angle),-sin(angle),0],[0,sin(angle),cos(angle),0],[0,0,0,1]];

/** Creates rotation matrix around Y axis */
function bl_rot_y(angle) = [[cos(angle),0,sin(angle),0],[0,1,0,0],[-sin(angle),0,cos(angle),0],[0,0,0,1]];

/** Creates rotation matrix around Z axis */
function bl_rot_z(angle) = [[cos(angle),-sin(angle),0,0],[sin(angle),cos(angle),0,0],[0,0,1,0],[0,0,0,1]];

/** Creates axis-angle rotation matrix */
function bl_rot_a(axis, angle) = let(n = bl_unit(axis), s = sin(angle), c = cos(angle), ac = 1 - c) [
    [c + ac*n[0]*n[0], ac*n[0]*n[1] - s*n[2], ac*n[0]*n[2] + s*n[1], 0],
    [ac*n[1]*n[0] + s*n[2], c + ac*n[1]*n[1], ac*n[1]*n[2] - s*n[0], 0],
    [ac*n[2]*n[0] - s*n[1], ac*n[2]*n[1] + s*n[0], c + ac*n[2]*n[2], 0],
    [0,0,0,1]
];

/** Creates rotation matrix from quaternion */
function bl_rot_q(q) = let(x = q[1], y = q[2], z = q[3], w = q[0], xw = 2*x*w, yw = 2*y*w, zw = 2*z*w) [
    [1-2*y*y-2*z*z, 2*x*y - zw, 2*x*z + yw, 0],
    [2*x*y+zw, 1-2*x*x-2*z*z, 2*y*z - xw, 0],
    [2*x*z-yw, 2*y*z+xw, 1-2*x*x-2*y*y, 0],
    [0,0,0,1]
];

function bl_ort(v) = [-v[1],v[2],-v[0]];

/** Creates rotation matrix from plane normal */
function bl_rot_v(v) = let(
    up = [0,0,1],
    f = bl_unit(v),
    s = bl_unit(cross(f, up)),
    u = bl_unit(cross(s, f))
)
    [
        [s[0],u[0],f[0],0],
        [s[1],u[1],f[1],0],
        [s[2],u[2],f[2],0],
        [0,0,0,1]
    ];

/** Creates identity matrix */
function bl_id(dim = 3) = [ for (i = [0:dim]) [ for (j = [0:dim]) i == j ? 1 : 0 ] ];

/** If m_seq is matrix then return it, if m_seq is array of matricies then premultiplies all of them  */
function bl_order(m_seq, i = 0) = i >= len(m_seq)-1 ? m_seq[i] : bl_order(m_seq, i + 1) * m_seq[i];

/** If `mt` is matrix then return it, if `mt` is array of matricies then premultiplies all of them and returns result */
function bl_normalize(mt) = len(mt[0][0]) == undef ? mt : bl_order(mt);

/** Converts 2d point (or array of points) to 3d points */
function bl_to3d(pt, z = 0, y = 0, x = 0) = len(pt[0]) == undef 
    ? bl_head_def(pt, [x,y,z], 3)
    : [ for (p = pt) bl_to3d(p, z, y, x) ];
        
/** Transforms point (or array of points) using supplied matrix (or set of matrix - see `bl_normalize`) */
function bl_tr(v, m) = let(l = len(m), mn = bl_normalize(m)) 
    len(v[0]) == undef
    ? bl_head(mn * [ for (i = [0:l-1]) i < len(v) ? v[i] : i == l-1 ? 1 : 0 ], len(v))
    : [ for (x = v) bl_tr(x, mn) ];

/** Approximation of ellipse perimeter (about 5% error) */
function bl_ellipse_perimeter(r) = let(ra = bl_cast(r, 2)) 
    2 * PI * ((ra[0] == ra[1]) ? ra[0] : sqrt((ra[0]*ra[0] + ra[1]*ra[1]) / 2));
function bl_arc_steps(r, a) = $fn > 0 ? $fn : ceil(max(min(a / $fa, bl_ellipse_perimeter(r) / $fs), 5));

function bl_arc_pt(r, a, p = [0,0]) = let(ra = bl_cast(r, 2)) [cos(a) * ra[0] + p[0], sin(a) * ra[1] + p[1]];
function bl_arc_loop(r, s, e, c, last = true, p = [0,0]) = r == 0 ? [p] : [ for(i = [0 : last ? c : c - 1]) bl_arc_pt(r, s + i * (e - s) / c, p) ];
function bl_arc(r, s, e, last = true, p = [0,0]) = bl_arc_loop(r, s, e, bl_arc_steps(r, abs(e - s)), last, p);

/** Generates NGON shape */
function bl_ngon(r, n, p = [0,0]) = bl_arc_loop(r, 0, 360, n, false, p);

/** Generates star shape */
function bl_star(r1, r2, n) = let(ra1 = bl_cast(r1, 2), ra2 = bl_cast(r2, 2), m = n * 2) 
    [ for (i = [0:m-1]) [ cos(i * 360 / m) * (i % 2 == 0 ? ra1[0] : ra2[0]), sin(i * 360 / m) * (i % 2 == 0 ? ra1[1] : ra2[1]) ] ];
        
/** Creates 2d rectangle shape with specified rounded corner radiuses */
function bl_rect(dim, r = 0, center = false) = let(
    ltrb = center ? [-dim[0]/2, dim[1]/2, dim[0]/2, -dim[1]/2] : [0,dim[1],dim[0],0], 
    left = ltrb[0], top = ltrb[1], right = ltrb[2], bottom = ltrb[3],
    rv = (len(r) == 4) ? [r[0],r[1],r[2],r[3]] : [r, r, r, r])
    concat(
        bl_arc(rv[0], 0, 90, true, [right - rv[0], top - rv[0]]),
        bl_arc(rv[1], 90, 180, true, [left + rv[1], top - rv[1]]),
        bl_arc(rv[2], 180, 270, true, [left + rv[2], bottom + rv[2]]),
        bl_arc(rv[3], 270, 360, true, [right - rv[3], bottom + rv[3]])
        );
    
/** Generates array of triangles required to connect two profiles */
function bl_faces(a_off, a_len, b_off, b_len) = a_len > b_len
    ? bl_faces_minmax(b_off, b_len, a_off, a_len, false)
    : bl_faces_minmax(a_off, a_len, b_off, b_len, true);

/** Generates surface */
function bl_surf(off, l, order = false) = l > 2 ? [bl_reverse([for (i = [0:l-1]) off + (i % l)], order)] : [];

function bl_faces_minmax(min_off, min_len, max_off, max_len, order = false) = let(k = floor(max_len / min_len)) concat(
    [ for (i = [0:max_len-1]) bl_reverse([
            max_off + i % max_len, 
            min_off + floor(i / k) % min_len, 
            max_off + (i + 1) % max_len
        ], order)
    ],
    min_len > 1 ? [ for (i = [0:min_len-1]) bl_reverse([
            min_off + i % min_len,
            max_off + floor((i + 1) * k) % max_len,
            min_off + (i + 1) % min_len
        ], !order)
    ] : []
);

/** Skins specified profiles forming 3d shape */
module bl_skin(profiles, loop = false) {
    
    edges = bl_flat(profiles);
    offsets = bl_off(profiles);
    
    faces = concat([ for (i = [0:len(profiles)-2]) for (f = bl_faces(
        offsets[i], offsets[i + 1] - offsets[i],
        offsets[i + 1], offsets[i + 2] - offsets[i + 1])) f ],
        loop ? [] : concat(
            bl_surf(offsets[0], offsets[1] - offsets[0]), 
            bl_surf(offsets[len(offsets)-2],offsets[len(offsets)-1] - offsets[len(offsets)-2], true)
        )
    );//, make_face(profiles[-1])];
    polyhedron(edges, faces);
    
}


function bl_combination(i, n) = bl_fac(n) / (bl_fac(i) * bl_fac(n - i));
function bl_polynome(i, n, t) = bl_combination(i, n) * pow(t, i) * pow(1 - t, n - i);
function bl_component(i, n, t, p) = let (b = bl_polynome(i, n, t)) [ for (c = p) c * b ];
function bl_steps() = $fn > 0 ? $fn : 12;
function bl_point(points, t, sum, i = 0) = let(n = len(points) - 1) 
    i > n ? sum : bl_point(points, t, bl_component(i, n, t, points[i]) + sum, i + 1);
function bl_curve(points, last = true) = len(points <= 2) ? points :
    let(segments = bl_steps()) [ for (i = [0 : last ? segments : segments - 1]) bl_point(points, i / segments, bl_fill(0, len(points[0]))) ];
    
module bl_line_3(p1, p2, d = 1) {
    v = p2 - p1;
    m = bl_rot_v(v);
    translate(p1)
        multmatrix(m)
            translate([-d/2,-d/2,0])
                cube([d,d,norm(v)]);
}
   
module bl_lines_3(points, d, loop = false) {
    for (i = [0:len(points)-2]) 
        bl_line_3(points[i], points[i+1], d);
    if (loop) {
        bl_line_3(points[len(points)-1], points[0], d);
    }
}

module bl_line_2(p1, p2, d) {   
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

module bl_lines(points, d) {
    for (i = [0:len(points)-1]) 
        bl_line_2(points[i], points[(i+1) % len(points)], d);
}
    
module bl_debug_point(pt, f, d) {
    translate([pt[0],pt[1],0]) 
        % color([f, (1 - f), 0]) circle(d = d);
}

module bl_debug_line(p1, p2, f, d) {
    % color([f, (1 - f), 0]) bl_line_2(p1, p2, d);
}

module bl_debug(points, d = 2) {
    n = len(points);
    for (i = [0:n-2]) {
        f = i / (n - 1);
        bl_debug_point(points[i], i / (n - 1), d);
        bl_debug_line(points[i], points[i + 1], f, d / 2);
    }
    bl_debug_point(points[n-1], 1, d);
}
 

