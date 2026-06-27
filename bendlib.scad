/** Expands or shrinks value to required amount of dimensions. Can be used to convert:
    
    1. scalar to vector of dimension `nd` (v must be scalar, e.g. non-list)
    2. vector to `nd` dimension - useful for converting 2d to 3d point, or vice-versa
    3. points in vector `v` to `nd` dimensions - useful for polygon processing (to convert 3d points to 2d or vice versa)
        
    - `v` value to convert (scalar, list of scalars, or list of points - list of lists (in terms of openscad))
    - `nd` can be a number of dimensions or list with default values for additional dimensions, it length specifies target count of dimensions
    
*/
function bl_nd(v, nd) = let(
    count = is_list(nd) ? len(nd) : nd
) is_list(v) ? let(
    default = is_list(nd) ? nd : bl_nd(0, count)
) is_list(v[0])
    ? [ for (p = v) bl_nd(p, default) ]
    : [ for (i = [0 : count-1]) i < len(v) ? v[i] : default[i] ]
: v != undef ? [ for (i = [0 : count-1]) v ] : undef;

function bl_2d(v) = bl_nd(v,2);
function bl_3d(v) = bl_nd(v,3);


function bl_repeat(n, v) = [ for (i = [0:n-1]) v ];

/** Returns property value from list of pairs where left is a string key and right is value */
function bl_get(props, key, optional = false) = let(v = search([key], props)) is_num(v[0]) ? props[v[0]][1] : assert(optional, concat("No property found by key ", key)) undef;


function bl_set_default(props, key, value) = let(v = search([key], props)) is_num(v[0]) ? props : is_list(props) ? concat(props, [key, value]) : [[key,value]];

/** Returns first parameter if not undef, otherwise returns second paramater */
function bl_def(v, v_def) = v != undef ? v : v_def;

/** Multiplies vectors elementwise (hadamard product) */
function bl_mul(a, b) = is_list(a) 
    ? is_list(b) ? [ for (i = [0:max(len(a),len(b))-1]) bl_def(a[i], 1) * bl_def(b[i], 1) ] : [ for (v = a) v * b ] 
    : is_list(b) ? [ for (v = b) v * a ] : a * b;


/** Computes factorial */
function bl_fac(n) = n <= 1 ? 1 : n * bl_fac(n - 1);

/** Computes sum of all elements in array */
function bl_sum(a, start = 0, end = -1) = let(a_end = end < 0 ? len(a) : min(len(a),end)) start >= a_end ? 0 : a[start] + bl_sum(a, start + 1, a_end);

/** Checks whether each element of v is zero or not (zero vector with zero norm) */
function bl_zero(v, i = 0) = i >= len(v) ? true: v[i] == 0 && bl_zero(v, i + 1);

/** Computes squares vector norm */
function bl_norm_sq(v) = bl_sum([ for (e = v) e * e ]);

/** Computes unit vector */
function bl_unit(v) = v / norm(v);

function bl_unit_identity(v, i) = bl_zero(v) ? [ for (j = [0:len(v)-1]) j == i ? 1 : 0 ] : bl_unit(v);

/** Extracts elements from `l` to `r`, not including `r` */
function bl_sub(a, l, r = undef) = let (
    ol = max(0, l < 0 ? len(a) + l : l), 
    or = max(ol, min(len(a), r == undef ? len(a) : r < 0 ? len(a) + r : r))
) [ for (i = [ol: or - 1]) a[i] ];

/** Takes first `l` elements from `a` */
function bl_head(a,l) = bl_sub(a, 0, l);

/** Takes last `l` elements from `a` */
function bl_tail(a, l) = bl_sub(a, -l);

/** Takes first `l` elements from `a` then from `b` */
function bl_head_def(a,b,l) = let(k = min(l,len(a))) concat([for(i=[0:k-1]) a[i]], [for(i=[k:l-1]) b[i]]);

/** Reverses array */
function bl_reverse(a, do_reverse = true) = do_reverse ? [for (i = [len(a) - 1 : -1 : 0]) a[i]] : a;

/** Creates offsets array from matrix (or any 2 dim array) */
function bl_off(m, i = 0, sum = 0) = i > len(m) ? [] : concat([sum], bl_off(m, i + 1, sum + len(m[i])));

/** Flatten matrix to array */
function bl_flat(m) = [ for (k = m) for (n = k) n ];

/** Creates translation matrix */
function bl_translate(vec, dims = 3) = let(a = bl_nd(vec, dims), l = len(vec)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? 1 : j == l ? vec[i] : 0 ] ];
    
function bl_translation(matrix) = [ for (i=[0:len(matrix)-2]) matrix[i][len(matrix[i])-1] ];

/** Creates scale matrix. If vec is scalar - then creates uniform scale matrix */
function bl_scale(vec, dims = 3) = let(a = bl_nd(vec, dims), l = len(a)) [ for (i = [0:l]) [ for (j = [0:l]) i == j ? i == l ? 1 : a[i] : 0 ] ];

/** Creates 2d rotation matrix */
function bl_rotate_2(angle) = [[cos(angle),-sin(angle),0],[sin(angle),cos(angle),0],[0,0,1]];

/** Creates rotation matrix around X axis */
function bl_rotate_x(angle) = [[1,0,0,0],[0,cos(angle),-sin(angle),0],[0,sin(angle),cos(angle),0],[0,0,0,1]];

/** Creates rotation matrix around Y axis */
function bl_rotate_y(angle) = [[cos(angle),0,sin(angle),0],[0,1,0,0],[-sin(angle),0,cos(angle),0],[0,0,0,1]];

/** Creates rotation matrix around Z axis */
function bl_rotate_z(angle) = [[cos(angle),-sin(angle),0,0],[sin(angle),cos(angle),0,0],[0,0,1,0],[0,0,0,1]];

function bl_rotate(xyz) = bl_rotate_z(xyz[2]) * bl_rotate_y(xyz[1]) * bl_rotate_x(xyz[0]);

/** Creates axis-angle rotation matrix */
function bl_rotate_a(axis, angle) = let(n = bl_unit(axis), s = sin(angle), c = cos(angle), ac = 1 - c) [
    [c + ac*n[0]*n[0], ac*n[0]*n[1] - s*n[2], ac*n[0]*n[2] + s*n[1], 0],
    [ac*n[1]*n[0] + s*n[2], c + ac*n[1]*n[1], ac*n[1]*n[2] - s*n[0], 0],
    [ac*n[2]*n[0] - s*n[1], ac*n[2]*n[1] + s*n[0], c + ac*n[2]*n[2], 0],
    [0,0,0,1]
];

/** Creates rotation matrix from quaternion */
function bl_rotate_q(q) = let(x = q[1], y = q[2], z = q[3], w = q[0], xw = 2*x*w, yw = 2*y*w, zw = 2*z*w) [
    [1-2*y*y-2*z*z, 2*x*y - zw, 2*x*z + yw, 0],
    [2*x*y+zw, 1-2*x*x-2*z*z, 2*y*z - xw, 0],
    [2*x*z-yw, 2*y*z+xw, 1-2*x*x-2*y*y, 0],
    [0,0,0,1]
];

function bl_ort(v) = [-v[1],v[2],-v[0]];

/** Creates rotation matrix from plane normal */
function bl_rotate_v(v) = let(
    up = [0,0,1],
    f = bl_unit(v),
    s = bl_unit_identity(cross(f, up), 0),
    u = bl_unit_identity(cross(s, f), 1)
)
    [
        [s[0],u[0],f[0],0],
        [s[1],u[1],f[1],0],
        [s[2],u[2],f[2],0],
        [0,0,0,1]
    ];

/** Creates identity matrix */
function bl_id(dim = 3) = [ for (i = [0:dim]) [ for (j = [0:dim]) i == j ? 1 : 0 ] ];

/** 
    Normalizes radius[] values to [x_radius, y_radius] array
 */
function bl_radius_cast(radius) = is_list(radius) ? len(radius) >= 2 ? radius : [radius[0], radius[0]] : [radius, radius];

/** 
    Normalizes [start, end] angle array, 
    if `a` is scalar or single element array then angles is [0, a], 
    otherwise first two elements are [start, end] angles
 */
function bl_angle_cast(angle) = is_list(angle) ? len(angle) >= 2 ? angle : [0, angle[0]] : [0, angle];

/** Approximation of ellipse perimeter (about 5% error) */
function bl_ellipse_perimeter(radius) = let(ra = bl_radius_cast(radius)) 
    2 * PI * ((ra[0] == ra[1]) ? ra[0] : sqrt((ra[0]*ra[0] + ra[1]*ra[1]) / 2));

/** 
    
    Computes number of points to draw arc. 
    Uses original OpenSCAD formula, but extends it to support ellipsis arcs:
    
        int get_fragments_from_r(double r, double fn, double fs, double fa)
        {
             if (r < GRID_FINE) return 3;
             if (fn > 0.0) return (int)(fn >= 3 ? fn : 3);
             return (int)ceil(fmax(fmin(360.0 / fa, r*2*M_PI / fs), 5));
        }
    
 */
function bl_arc_steps(radius, angle) = let(aa = bl_angle_cast(angle)) $fn > 0 ? $fn : ceil(max(min(abs(aa[1] - aa[0]) / $fa, bl_ellipse_perimeter(radius) / $fs), 5));

/** Computes single arc point at given angle */
function bl_polar(radius, angle) = let(ra = bl_radius_cast(radius)) bl_mul([cos(angle), sin(angle)], ra);

/** 
    Computes arc 
        
    radius - either single number or array of 2 numbers (radius x, radius y - useful for ellipsis shapes)
    angle - either single angle arc is computed as [0, angle], or array of start and end angles
    position - fixed offset of arc applied to each arc point
    slice - allow to slice (drop) some points of arc.
        for example to remove last arc point set slice = [0,-1] or first one [1,0]
        useful for joining multiple arcs to avoid duplicate start / end points
        
    It takes usual $fa, $fs, $fn variables into account and uses them to compute total arc point count.
    If you need to set fixed amount of points use $fn = x
        
*/
function bl_arc(radius, angle, position = [0,0], slice = [0,0]) = let(
    steps = bl_arc_steps(radius, angle) - 1,
    ra = bl_radius_cast(radius), 
    aa = bl_angle_cast(angle)
) ra[0] == 0 || ra[1] == 0 
    ? [ position ] 
    : [ for(i = [slice[0] : steps + slice[1]]) bl_polar(ra, aa[0] + i * (aa[1] - aa[0]) / steps) + position ];

/** Generates NGON shape */
function bl_ngon_points(radius, sides = 3, position = [0,0]) = bl_arc_loop(radius, 360, sides, position);

/** Creates 2d rectangle shape with specified rounded corner radiuses (radius parameter can be list with 1, 2, 4 length)*/
module bl_ngon(radius, sides = 3, position = [0,0]) {
    polygon(bl_ngon_points(radius, sides, position));
}

/** Generates star shape */
function bl_star_points(radius, sides = 5, position = [0,0]) = let(ra = bl_radius_cast(radius), m = sides * 2) 
    [ for (i = [0:m-1]) let(r = i % 2 == 0 ? ra[0] : ra[1]) [ cos(i * 360 / m) * r, sin(i * 360 / m) * r ] + position ];
       
/** Generates star shape */
module bl_star(radius, sides = 5, position = [0,0]) {
    polygon(bl_star_points(radius, sides, position));
}
    
/** Creates 2d rectangle shape with specified rounded corner radiuses */
function bl_square_points(dim, radius = 0, center = false) = let(
    left = center ? -dim[0]/2 : 0,
    bottom = center ? -dim[1]/2 : 0,
    right = center ? dim[0]/2 : dim[0],
    top = center ? dim[1]/ 2 : dim[1],
    rv = [ for(r = (is_list(radius)
        ? len(radius) >= 4 ? radius : len(radius) >= 2 
            ? [radius[0], radius[0], radius[1], radius[1]] 
            : [radius[0], radius[0], radius[0], radius[0]]
        : [radius, radius, radius, radius])) bl_radius_cast(r) ]
) concat(
    bl_arc(radius = rv[0], angle = [0, 90], position = [right - rv[0].x, top - rv[0].y]),
    bl_arc(radius = rv[1], angle = [90, 180], position = [left + rv[1].x, top - rv[1].y]),
    bl_arc(radius = rv[2], angle = [180, 270], position = [left + rv[2].x, bottom + rv[2].y]),
    bl_arc(radius = rv[3], angle = [270, 360], position = [right - rv[3].x, bottom + rv[3].y])
);
    
/** 
    Creates 2d rectangle shape with specified rounded corner radiuses (radius parameter can be list with 1, 2, 4 length)
    This also allows to create chamfered corners just set $fn = 1
*/
module bl_square(dim, radius = 0, center = false) {
    polygon(bl_square_points(dim, radius, center));
}
    
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


function bl_bezier_combination(i, n) = bl_fac(n) / (bl_fac(i) * bl_fac(n - i));
function bl_bezier_polynome(i, n, t) = bl_bezier_combination(i, n) * pow(t, i) * pow(1 - t, n - i);
function bl_bezier_component(i, n, t, p) = let (b = bl_bezier_polynome(i, n, t)) [ for (c = p) c * b ];
function bl_bezier_steps() = $fn > 0 ? $fn : 12;
function bl_bezier_point(points, t, sum, i = 0) = let(n = len(points) - 1) 
    i > n ? sum : bl_bezier_point(points, t, bl_bezier_component(i, n, t, points[i]) + sum, i + 1);
function bl_bezier(points, last = true) = len(points) <= 2 ? points :
    let(segments = bl_bezier_steps()) [ for (i = [0 : last ? segments : segments - 1]) bl_bezier_point(points, i / segments, bl_nd(0, len(points[0]))) ];
    
module bl_line_3(p1, p2, d = 1) {
    v = p2 - p1;
    m = bl_rotate_v(v);
    translate(p1) {
        multmatrix(m) {
            linear_extrude(norm(v)) {
                children();
            }
        }
    }
}
   
module bl_lines_3(points, d, loop = false) {
    for (i = [0:len(points)-2]) 
        bl_line_3(points[i], points[i+1], d) {
            children();
        }
    if (loop) {
        bl_line_3(points[len(points)-1], points[0], d) {
            children();
        }
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
 
function bl_offset_line(l, off) = let(v = l[1] - l[0], ov = bl_unit([v[1], -v[0]]) * off) [ l[0] + ov, l[1] + ov ];

function bl_line_intersection(lines) = let(
    l1 = lines[0], 
    l2 = lines[1], 
    v1 = l1[0] - l1[1],
    v2 = l2[0] - l2[1],
    q1 = l1[0].x*l1[1].y - l1[0].y*l1[1].x,
    q2 = l2[0].x*l2[1].y - l2[0].y*l2[1].x,
    d = v1.x*v2.y - v1.y*v2.x
) abs(d) < 0.0001 ? undef : [ 
(q1*v2.x - v1.x*q2) / d, 
(q1*v2.y - v1.y*q2) / d
];

function bl_offset_poly(poly, off) = let(n = len(poly)) [ for (i = [0:n-1]) let(
    p1 = poly[i > 0 ? i - 1 : n - 1],
    p2 = poly[i],
    p3 = poly[(i + 1) % n],
    l1 = bl_offset_line([p1, p2], off),
    l2 = bl_offset_line([p2, p3], off),
    pi = bl_line_intersection([l1, l2]),
    pt = pi == undef ? l1[1] : pi
) pt ];

/*
    
    Creates mirrors of children() item
    
    dim - dimension of box
    count_or_offsets - either count of mirrors [1 .. 4 (2d) .. 8 (3d) ], or direct array of offsets for each mirror copy
        each elemet in this array can be undef - in this case mirror copy will be ignored, so you can skip some mirror copies
    center - whether mirrors should be centered against main axes (x,y,z)
    move - whether mirrors should be moved by theirs offsets or this is handled in children() call

    Examples of usages:

    bl_quad_mirror([20,20], 4) {
        translate([4,4])
        circle(d = 3);
    }

*/
module bl_quad_mirror(dim, count_or_offsets, center=false, move=true) {
    d = bl_3d(dim);
    o = is_list(count_or_offsets) ? bl_3d(count_or_offsets) : [ for (i=[0:count_or_offsets-1]) bl_3d(0) ];
    
    for ($index = [0:len(o)-1]) {
        $offset = o[$index];
        if ($offset != undef) {
            translate(center ? bl_3d(0) : d/2)
            mirror([0,0,floor($index / 4) % 2])
            mirror([0,floor($index / 2) % 2,0])
            mirror([$index % 2,0,0])
            translate(-d/2 + (move ? $offset : bl_3d(0)))
            children();
        }
    }
}

module bl_ring(inner_d = undef, thickness = undef, outer_d = undef) {
    inner_d = inner_d != undef ? inner_d : outer_d - thickness*2;
    outer_d = outer_d != undef ? outer_d : inner_d + thickness*2;
    difference() {
        circle(d = outer_d);
        circle(d = inner_d);
    }
}

module bl_hull_circle(d = undef, length = undef, r = undef, distance = undef, center = false) {
    r = r != undef ? r : d / 2;
    d = d != undef ? d : r * 2;
    distance = distance != undef ? distance : length-d;
    translate(center ? [-distance/2,0] : [r,r])
    hull() {
        circle(d = d);
        translate([distance,0])
        circle(d = d);
    }
}

module bl_hull_ring(inner_d = undef, thickness = undef, length = undef, outer_d = undef, distance = undef, center = false) {
    inner_d = inner_d != undef ? inner_d : outer_d - thickness*2;
    outer_d = outer_d != undef ? outer_d : inner_d + thickness*2;
    distance = distance != undef ? distance : length - outer_d; 
    translate(center ? [0,0] : [outer_d/2+distance/2,outer_d/2])
    difference() {
        bl_hull_circle(d = outer_d, distance = distance, center=true);
        bl_hull_circle(d = inner_d, distance = distance, center=true);
    }
}

module bl_half_circle_square(d, l=-1) {
    sl = l < 0 ? d/2 : l - d/2;
    intersection() {
        circle(d = d);
        translate([-d/2,0])
        square([d, d/2]);
    }
    translate([-d/2,-sl])
        square([d, sl]);
}

module bl_grid(counts, dim) {
    c = bl_nd(counts,[1,1,1]);
    d = bl_nd(dim,[0,0,0]);
    for (z = [0:c[2]-1]) {
        for (y = [0:c[1]-1]) {
            for (x = [0:c[0]-1]) {
                translate([x*dim[0], y*dim[1],z*dim[0]])
                    children();
            }
        }
    }
}

module bl_box(xyz_dim, xyz_thickness, center = false) {
    t_norm = [for (t = xyz_thickness) let(tl = bl_nd(t,2)) [for (tv = tl) tv <= 0 ? -1 : tv]];
    translate(center ? -xyz_dim/2 : [0,0,0] )
    difference() {
        cube(xyz_dim);
        translate([ for (t = t_norm) t[0] ])
        cube(xyz_dim - [ for (t = t_norm) bl_sum(t) ]);
    }
}

module bl_offset_clone(offsets) {
    for (off = offsets) {
        translate(off)
        children();
    }
}

module bl_tower(heights, axis = [0,0,1]) {
    for (i = [0 : len(heights)]) {
        $index = i;
        $height = heights[i];
        $offset = bl_sum(heights, 0, i);
        translate(axis * $offset)
        children();
    }
}

module bl_extrude_tower(heights, axis = [0,0,1]) {
    for (i = [0 : $children - 1]) {
        $index = i;
        $height = heights[i];
        $offset = bl_sum(heights, 0, i);
        translate(axis * $offset) {
            linear_extrude($height)
            children(i);
        }
    }
}
