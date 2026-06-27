use <bendlib.scad>;

function battery(dim = undef, plus_dim = undef, minus_dim = undef) = [
    bl_def(dim, [18,65]), 
    bl_def(plus_dim, [0,0]), 
    bl_def(minus_dim, [0,0])
];
function battery_18650(flat=false) = battery([18,65], flat ? undef : [5.5, 1.5]);
function battery_32700() = battery([32,70]);

function battery_dim(type) = type[0];
function battery_plus_dim(type) = type[1];
function battery_minus_dim(type) = type[2];
function battery_full_dim(type) = [
    max(battery_dim(type)[0], battery_plus_dim(type)[0], battery_minus_dim(type)[0]), 
    battery_minus_dim(type)[1] + battery_dim(type)[1] + battery_plus_dim(type)[1]
];
function battery_flat(type) = type[1] == [0,0] && type[2] == [0,0];

module battery(type = battery_18650()) {
    dim = battery_dim(type);
    plus_dim = battery_plus_dim(type);    
    minus_dim = battery_minus_dim(type);
    colors = ["black", "black","gray","red", "red"];
    bl_tower([minus_dim[1], dim[1]*0.1, dim[1]*0.8, dim[1]*0.1, plus_dim[1]]) {
        color(colors[$index])
        cylinder(d = $index == 0 ? minus_dim[0] : $index == 4 ? plus_dim[0] : dim[0], h = $height);
    }
}

function battery_contact(
    dim = undef, 
    length = undef, 
    dia = undef
) = let(
    dim = bl_def(dim, [9.5, 9, 0.2]),
    length = bl_def(length, 1.5),
    dia = bl_def(dia, 6)
) [ dim, length, dia ];

function battery_contact_plus() = battery_contact();
function battery_contact_minus() = battery_contact(length = 9);
function battery_contact_dim(type) = type[0];
function battery_contact_length(type) = type[1];
function battery_contact_dia(type) = type[2];

module battery_contact(type = battery_contact_plus()) {
    dim = battery_contact_dim(type);
    length = battery_contact_length(type); 
    dia = battery_contact_dia(type);
    union() {
        translate([-dim[0]/2,-dim[1]/2,0])
        linear_extrude(dim[2]) {
            bl_square(dim, [3,3,1,1]);
            translate([(dim[0]-2.4)/2,0])
            bl_square([2.4,15],[1.2,1.2,0,0]);
        }
        cylinder(d = dia, h = length);
    }
}

function battery_contact_diff(
    contact = undef, 
    height = undef, 
    tolerance = undef, 
    wall = undef
) = let (
    contact = bl_def(contact, battery_contact_plus()),
    height = bl_def(height, battery_contact_dim(contact)[1]/2),
    tolerance = bl_def(tolerance, 0.2),
    wall = bl_def(wall, 0.8)
) [ 
    contact, 
    height, 
    tolerance, 
    wall
];

function battery_contact_diff_contact(type) = type[0];
function battery_contact_diff_height(type) = type[1];
function battery_contact_diff_tolerance(type) = type[2];
function battery_contact_diff_wall(type) = type[3];

function battery_contact_diff_dim(type) = bl_mul(battery_contact_dim(battery_contact_diff_contact(type)),[1,0.5,1]) + 
    [0,battery_contact_diff_height(type),0] + 
    bl_3d(battery_contact_diff_tolerance(type)*2);
    
function battery_contact_diff_full_dim(type) = battery_contact_diff_dim(type) + [0,0,battery_contact_diff_wall(type)];

module battery_contact_diff(type = battery_contact_diff()) {
    contact = battery_contact_diff_contact(type);
    
    contact_dim = battery_contact_dim(contact);
    contact_dia = battery_contact_dia(contact);
    height = battery_contact_diff_height(type);
    tolerance = battery_contact_diff_tolerance(type);
    wall = battery_contact_diff_wall(type);
    
    diff_dim = battery_contact_diff_dim(type);
    union() {
        translate([-diff_dim[0]/2,-contact_dim[1]/2-tolerance,0])
        cube(diff_dim);
        
        d = contact_dia + tolerance*2;
        h = diff_dim[2] + wall + tolerance;
        
        cylinder(d = d, h = h);
        translate([-d/2,0,0])
        cube([d, height + tolerance, h]);
    }
}

function battery_box(
    battery = undef,
    thickness = undef,
    battery_tolerance = undef,
    contact_diff = undef,
    minus_length = undef
) = let(
    battery = bl_def(battery, battery_18650()),
    thickness = bl_3d(bl_def(thickness, 1.5)),
    battery_tolerance = bl_3d(bl_def(battery_tolerance, 0.5)),
    contact_diff = bl_def(contact_diff, battery_contact_diff()),
    minus_length = bl_def(minus_length, battery_contact_length(battery_contact_minus()))
) [
    battery,
    thickness,
    battery_tolerance,
    contact_diff,
    minus_length
];

function battery_box_battery(type) = type[0];

function battery_box_thickness(type) = type[1];
function battery_box_battery_tolerance(type) = type[2];
function battery_box_contact_diff(type) = type[3];
function battery_box_minus_length(type) = type[4];

function battery_box_battery_dim(type) = battery_dim(battery_box_battery(type));
function battery_box_contact(type) = battery_contact_diff_contact(battery_box_contact_diff(type));
function battery_box_contact_thickness(type) = battery_contact_diff_full_dim(battery_box_contact_diff(type))[2];
function battery_box_contact_offset(type) = let(
    thickness = battery_box_thickness(type),
    battery_tolerance = battery_box_battery_tolerance(type),
    battery_dim = battery_box_battery_dim(type)
) [
    thickness[0],
    thickness[1] + battery_tolerance[1] + battery_dim[0]/2,
    thickness[2] + battery_tolerance[2] + battery_dim[0]/2,
];

function battery_box_battery_offset(type) = battery_box_contact_offset(type) + 
    [battery_box_contact_thickness(type)+battery_box_inner_dim(type)[0]-battery_box_battery_dim(type)[1]-battery_box_battery_tolerance(type)[0],0,0];
    
function battery_box_inner_dim(type) = let(
    contact_diff = battery_box_contact_diff(type),
    battery_dim = battery_dim(battery_box_battery(type)),
    battery_tolerance = battery_box_battery_tolerance(type),
    minus_length = battery_box_minus_length(type)
) bl_mul(battery_tolerance, [2,2,1]) + [
    battery_dim[1] + minus_length/2 - battery_box_contact_thickness(type),
    battery_dim[0],
    battery_dim[0]/2 + battery_contact_diff_height(contact_diff)
];

function battery_box_outer_dim(type) = let(
    box_thickness = battery_box_thickness(type), 
    contact_diff = battery_box_contact_diff(type)
) battery_box_inner_dim(type) + [
    battery_contact_diff_full_dim(contact_diff)[2]*2 + box_thickness[0]*2,
    box_thickness[1]*2,
    box_thickness[2]
];

module battery_box_contact_placement(type) {
    contact_offset = battery_box_contact_offset(type);
    outer_dim = battery_box_outer_dim(type);
    translate(contact_offset)
    rotate([90,0,90]) {
        $contact_type = true;
        children();
    }
    translate([outer_dim[0],0,0])
    mirror([-1,0,0])
    translate(contact_offset)
    rotate([90,0,90]) {
        $contact_type = false; 
        children();
    }
}

module battery_box_contact(type, contact_type) {
    contact = battery_box_contact(type);
    use_contact = contact_type ? battery_contact(
        dim = battery_contact_dim(contact), 
        dia = battery_contact_dia(contact), 
        length = battery_box_minus_length(type)
    ) : contact;
    battery_contact(use_contact);
}

module battery_box(type = battery_box(), with_battery = false, with_contacts = false) {
    thickness = battery_box_thickness(type);
    diff = battery_box_contact_diff(type);
    battery_tolerance = battery_box_battery_tolerance(type);
    outer_dim = battery_box_outer_dim(type);
    inner_dim = battery_box_inner_dim(type);
    difference() {
        cube(outer_dim);
        translate(bl_mul(outer_dim - inner_dim, [0.5,0.5,1]))
        cube(inner_dim + [0,0,battery_contact_diff_tolerance(diff)]);
        battery_box_contact_placement(type)
            battery_contact_diff(diff);
    }
    if (with_battery) {
        translate(battery_box_battery_offset(type))
        rotate([0,90,0])
        battery(battery_box_battery(type));
    }
    if (with_contacts) {
        battery_box_contact_placement(type) {
            battery_box_contact(type, $contact_type);
        }
    }
}




$fa = 0.2;
$fs = 0.2;

*battery();
*battery_contact();
//battery_contact_diff(contact=battery_contact_minus()));
battery_box();

*battery_contact_diff();


*battery(battery_32700());
