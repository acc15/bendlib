use <./bendlib.scad>;

function led(
    dia = undef, 
    height = undef, 
    ring_dia = undef, 
    ring_height = undef
 ) = let(
    dia = bl_def(dia, 3), 
    height = bl_def(height, dia * 1.8),
    ring_dia = dia * 1.2,
    ring_height = bl_def(ring_height, 1) 
) [ dia, height, ring_dia, ring_height ];

function led_dia(type) = type[0];
function led_height(type) = type[1];
function led_ring_dia(type) = type[2];
function led_ring_height(type) = type[3];


module led(type) {
    dia = led_dia(type);
    height = led_height(type);
    ring_dia = led_ring_dia(type);
    ring_height = led_ring_height(type);
    
    translate([0,0,height-dia/2])
    rotate_extrude()
    polygon(concat([[0,0]], bl_arc(dia/2, [0,90])));
    cylinder(d = dia, h = height - dia/2);
   
    cylinder(d = ring_dia, h = ring_height);
}

$fa = 0.2;
$fs = 0.2;
led(led(dia = 3));
