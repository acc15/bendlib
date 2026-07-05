use <bendlib.scad>;

function button(
    body_dim = undef, 
    mount_dim = undef, 
    pin_dim = undef,
    contact_dim = undef, 
    contact_offset = undef,
    contact_thickness = undef
 ) = let(
    body_dim = bl_def(body_dim, [12, 8.2, 4]),
    mount_dim = bl_def(mount_dim, [6.8, 2]),
    pin_dim = bl_def(pin_dim, [4.5, 2]),
    contact_dim = bl_def(contact_dim, [[13, 4, 0.5], [26, 2, 1]]),
    contact_offset = bl_def(contact_offset, [0,0,3]),
    contact_thickness = bl_def(contact_thickness, 0.2)
    
) [
    body_dim, 
    mount_dim,
    pin_dim,
    contact_dim,
    contact_offset,
    contact_thickness
];

function button_body_dim(type) = type[0];
function button_mount_dim(type) = type[1];
function button_pin_dim(type) = type[2];
function button_contact_dim(type) = type[3];
function button_contact_offset(type) = type[4];
function button_contact_thickness(type) = type[5];


module button_body_shape(type) {
    body_dim = button_body_dim(type);
    intersection() {
        circle(d = body_dim[0]);
        square(bl_2d(body_dim), center=true);
    }
}

module button_mount_diff(type, thickness, tolerance, thickness_tolerance = undef, part_offset = undef) {
    body_dim = button_body_dim(type);
    mount_dim = button_mount_dim(type);
    thickness_tolerance = bl_def(thickness_tolerance, tolerance);
    part_offset = bl_def(part_offset, thickness_tolerance);
    translate([0,0,body_dim[2]+part_offset-thickness_tolerance])
    cylinder(d = mount_dim[0] + tolerance*2, h = thickness + thickness_tolerance*2);
}

module button(type = button()) {
    
    body_dim = button_body_dim(type);
    mount_dim = button_mount_dim(type);
    pin_dim = button_pin_dim(type);
    
    color("black")
    linear_extrude(body_dim[2])
    button_body_shape(type);
    
    color("#111111")
    cylinder(d = mount_dim[0], h = mount_dim[1] + body_dim[2]);
    color("#222222")
    cylinder(d = pin_dim[0], h = pin_dim[1] + mount_dim[1] + body_dim[2]);
    
    
    translate(button_contact_offset(type))
    linear_extrude(button_contact_thickness(type))
    for (c = button_contact_dim(type)) {
        bl_square(c, radius = c[2], center=true);
    }
    
}

$fa = 0.2;
$fs = 0.2;

function push_button(dim = undef, dia = undef, height = undef) = [
    bl_def(dim, [6,6,4,0.2]), 
    bl_def(dia, [3.5,3,1.2]), 
    bl_def(height, 2)
];

function push_button_dim(type) = type[0];
function push_button_dia(type) = type[1];
function push_button_height(type) = type[2];

module push_button(type = push_button()) {
    
    dim = push_button_dim(type);
    dia = push_button_dia(type);
    height = push_button_height(type);
    
    mount_offsets = bl_repeat(4, [1,1]);
    
    color("gray")
    translate([0,0,dim[2]-dim[3]])
    linear_extrude(dim[3])
    difference() {
        square([dim[0],dim[1]]);
        bl_quad_mirror(dim, mount_offsets) {
            circle(d = dia[2]);
        }
        translate(bl_2d(dim)/2)
        circle(d = dia[0]);
    }
    
    color("black") {
        cube([dim[0],dim[1],dim[2]-dim[3]]);
    
        translate([0,0,dim[2]-dim[3]])
        bl_quad_mirror(dim, mount_offsets) {
            cylinder(d = dia[2], h = dim[3]*2);
        }
    
        color("black")
        translate([dim[0]/2,dim[1]/2,dim[2]])
        cylinder(d1 = dia[0], d2 = dia[1], h = height);
    }
}

push_button(push_button(height = 9.5));


/*
button = button();
button(button);
*#button_mount_diff(button, 1.5, 0.2);*/