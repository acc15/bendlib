use <bendlib.scad>;

function cms4056t(
    board_dim = undef,
    typec_dim = undef,
    typec_offset = undef,
    led_dim = undef,
    led_offsets = undef,
    pad_dim = undef,
    pad_hole_dia = undef,
    pad_offsets = undef
) = let(
    board_dim = bl_def(board_dim, [24, 18, 1.2]),
    typec_dim = bl_def(typec_dim, [9, 7, 3.2]),
    dim = board_dim + [0,0,typec_dim[2]],
    typec_offset = bl_def(typec_offset, [2, board_dim[1] - typec_dim[1] + 2, board_dim[2]]),
    led_dim = bl_def(led_dim, [1,2,1]),
    led_offsets = bl_def(led_offsets, [
        [13, board_dim[1]-led_dim[1], board_dim[2]],
        [14.3, board_dim[1]-led_dim[1], board_dim[2]]
    ]),
    pad_dim = bl_def(pad_dim, [2,2]),
    pad_hole_dia = bl_def(pad_hole_dia, 1),
    pad_offsets = bl_def(pad_offsets, [
        [0,0], 
        [8,0], 
        [board_dim[0]-pad_dim[0], 0], 
        [board_dim[0]-pad_dim[0] - 8, 0]
    ])
) [
    board_dim,
    typec_dim,
    dim,
    typec_offset,
    led_dim,
    led_offsets,
    pad_dim,
    pad_hole_dia,
    pad_offsets
];

function cms4056t_board_dim(type = cms4056t()) = type[0];
function cms4056t_typec_dim(type = cms4056t()) = type[1];
function cms4056t_dim(type = cms4056t()) = type[2];
function cms4056t_typec_offset(type = cms4056t()) = type[3];
function cms4056t_led_dim(type = cms4056t()) = type[4];
function cms4056t_led_offsets(type = cms4056t()) = type[5];
function cms4056t_pad_dim(type = cms4056t()) = type[6];
function cms4056t_pad_hole_dia(type = cms4056t()) = type[7];
function cms4056t_pad_offsets(type = cms4056t()) = type[8];

module cms4056t_typec_shape(type) {
    typec_dim = cms4056t_typec_dim(type);
    bl_hull_circle(typec_dim[2], typec_dim[0]);
}

module cms4056t_typec_diff(type, thickness, tolerance, axial_tolerance = undef, part_offset = undef) {
    axial_tolerance = bl_def(axial_tolerance, tolerance);
    part_offset = bl_def(part_offset, axial_tolerance);
    
    typec_offset = cms4056t_typec_offset(type);
    board_dim = cms4056t_board_dim(type);
    
    translate([typec_offset[0], board_dim[1] + part_offset, typec_offset[2]])
    rotate([90,0,0])
    translate([0,0,-thickness-axial_tolerance])
    linear_extrude(thickness + axial_tolerance*2)
    offset(tolerance)
    cms4056t_typec_shape(type);
}

module cms4056t(type = cms4056t()) {
    
    board_dim = cms4056t_board_dim(type);
    typec_dim = cms4056t_typec_dim(type);
    typec_offset = cms4056t_typec_offset(type);
    led_dim = cms4056t_led_dim(type);
    led_offsets = cms4056t_led_offsets(type);
    pad_dim = cms4056t_pad_dim(type);
    pad_hole_dia = cms4056t_pad_hole_dia(type);
    pad_offsets = cms4056t_pad_offsets(type);

    color("black")
    difference() {
        cube(board_dim);
        translate([0,-1,-1])
        for (off = pad_offsets) {
            translate(off)
            cube([pad_dim[0], pad_dim[1]+1, board_dim[2]+2]);
        }
    }
    
    translate(typec_offset)
    translate([0,0,typec_dim[2]])
    rotate([-90,0,0])
    linear_extrude(typec_dim[1])
    cms4056t_typec_shape(type);
    
    color("blue")
    for (off = led_offsets) {
        translate(off)
        cube(led_dim);
    }
    
    color("lightgray")
    for (off = pad_offsets) {
        translate(off)
        linear_extrude(board_dim[2])
        translate(pad_dim/2)
        difference() {
            square(pad_dim, center=true);
            circle(d = pad_hole_dia);
        }
    }

}

$fa = 0.2;
$fs = 0.2;
cms4056t(cms4056t());

#cms4056t_typec_diff(cms4056t(), 1.5, 0.2);
