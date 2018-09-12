
/** Takes first `l` elements from `a` */
function a_head(a,l) = [ for (i = [0:min(len(a),l)-1]) a[i] ];

/** Takes first `l` elements from `a` then from `b` */
function a_head_def(a,b,l) = let(k = min(l,len(a))) concat([for(i=[0:k-1]) a[i]], [for(i=[k:l-1]) b[i]]);

/** Reverses array */
function a_reverse(a, do_reverse = true) = do_reverse ? [for (i = [len(a) - 1 : -1 : 0]) a[i]] : a;

/** Normalizes array such that if `a` is array then return `a`, otherwise generates array with values `a` */
function a_norm(a, l) = len(a) == undef ? [ for (i = [0:l]) a ] : a;

/** Creates offsets array from matrix (or any 2 dim array) */
function m_off(m, i = 0, sum = 0) = i > len(m) ? [] : concat([sum], m_off(m, i + 1, sum + len(m[i])));

/** Flatten matrix to array */
function m_flat(m) = [ for (k = m) for (n = k) n ];

/** Creates translation matrix */
function m_move(vec, dims = 3) = let(a = a_norm(vec, dims), l = len(vec)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? 1 : j == l ? vec[i] : 0 ] ];

/** Creates scale matrix. If vec is scalar - then creates uniform scale matrix */
function m_scale(vec, dims = 3) = let(a = a_norm(vec, dims), l = len(a)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? i == l ? 1 : a[i] : 0 ] ];

/** Creates 2d rotation matrix */
function m_rot_2(angle) = [[cos(angle),-sin(angle),0],[sin(angle),cos(angle),0],[0,0,1]];

/** Creates rotation matrix around X axis */
function m_rot_x(angle) = [[1,0,0,0],[0,cos(angle),-sin(angle),0],[0,sin(angle),cos(angle),0],[0,0,0,1]];

/** Creates rotation matrix around Y axis */
function m_rot_y(angle) = [[cos(angle),0,-sin(angle),0],[0,1,0,0],[sin(angle),0,cos(angle),0],[0,0,0,1]];

/** Creates rotation matrix around Z axis */
function m_rot_z(angle) = [[cos(angle),-sin(angle),0,0],[sin(angle),cos(angle),0,0],[0,0,1,0],[0,0,0,1]];

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

/** Generates NGON shape */
function p_ngon(r, n) = let(ra = a_norm(r, 2)) 
    [ for (i = [0:n-1]) let (a = i * 360 / n) [ cos(a) * ra[0], sin(a) * ra[1] ] ];

/** Generates star shape */
function p_star(r1, r2, n) = let(ra1 = a_norm(r1, 2), ra2 = a_norm(r2, 2), m = n * 2) 
    [ for (i = [0:m-1]) [ cos(i * 360 / m) * (i % 2 == 0 ? ra1[0] : ra2[0]), sin(i * 360 / m) * (i % 2 == 0 ? ra1[1] : ra2[1]) ] ];
        
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