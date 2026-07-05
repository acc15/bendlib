include <bendlib.scad>;

module axis(l = 10, t = 1) {
    translate(bl_cast(-t/2, 3)) {
        color("blue")
        cube([t, t, l]);

        color("green")
        cube([t, l, t]);

        color("red")
        cube([l, t, t]);
    }
}

module bl_rot_a_test(axis) {
    multmatrix(bl_rot_a(axis, i * ainc))
            children();
}

module bl_rot_tests() {    


    translate([0,30,0])
    rotate([$t * 360, 0, 0])
        axis(10, 1);

    translate([15,30,0])
    rotate([0, $t * 360, 0])
        axis(10, 1);

    translate([30,30,0])
    rotate([0, 0, $t * 360])
        axis(10, 1);


    translate([0,15,0])
    multmatrix(bl_rot_x($t * 360))
        axis(10, 1);

    translate([15,15,0])
    multmatrix(bl_rot_y($t * 360))
        axis(10, 1);

    translate([30,15,0])
    multmatrix(bl_rot_z($t * 360))
        axis(10, 1);


    multmatrix(bl_rot_a([1,0,0], $t * 360))
        axis(10, 1);

    translate([15,0,0])
    multmatrix(bl_rot_a([0,1,0], $t * 360))
        axis(10, 1);

    translate([30,0,0])
    multmatrix(bl_rot_a([0,0,1], $t * 360))
        axis(10, 1);
    
}

//bl_rot_tests();


module mm(a, b) {
    multmatrix(make_mt(a,b))
        brick();
}

function make_mt(a, b) = bl_rot_z(a) * bl_rot_y(b) * bl_rot_z(-a);

function m_test(v) = let(uv = a_unit(v)) (uv[0] == 0 && uv[1] == 0)
    ? uv[2] > 0 ? make_mt(0,0) : make_mt(90,180) 
    : make_mt(atan2(uv[1],uv[0]), atan2(uv[2],uv[0]));

module arc_test() {
    
    //$fn = 12;
    
    #circle(r=10);
    
    r = 10;
    a = [90,-90];
    
    echo(bl_arc_steps(r=r, a=a));
    echo(bl_arc(r=r, a=a, l = false));
    //v = 
    
    polygon(concat([[0,0]], bl_arc(r=r, a=a)));
    
}

module bl_rot_v_test() {

    

    echo(u = bl_unit_identity([0,0,0], 1), m = bl_rot_v([0,0,-10]));
    echo(m = bl_rot_v([0,0,10]));
    



    bl_lines_3([[0,0,0], [10,10,10], [15,0,0], [15,0,-30], [40,5,0], [0,20,15], [0,20,30]], 2) {
        circle(d = 3, $fn = 16);
    }

}

bl_rot_v_test();



