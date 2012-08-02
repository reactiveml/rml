var caml_gr_x_size = 500;
var caml_gr_y_size = 400;

var caml_gr_x_loc = 10;
var caml_gr_y_loc = 25;

var caml_gr_current_x_pos = 0;
var caml_gr_current_y_pos = 0;

var canvas_elt;
var context;

var gr_color_r = 0;
var gr_color_g = 0;
var gr_color_b = 0;
var gr_font_name = "Arial";
var gr_font_size = 20;

function local_gr_draw_arc(centerX, centerY, width, height) {
    context.beginPath();
    context.moveTo(centerX, centerY - height/2); // A1
    context.bezierCurveTo(
        centerX + width/2, centerY - height/2, // C1
        centerX + width/2, centerY + height/2, // C2
        centerX, centerY + height/2); // A2
    context.bezierCurveTo(
        centerX - width/2, centerY + height/2, // C3
        centerX - width/2, centerY - height/2, // C4
        centerX, centerY - height/2); // A1
    context.closePath();
    context.stroke();
}

function local_gr_init_font() {
    // Choosing "px" is easier to compute caml_gr_text_size
    context.font = gr_font_size + "px " + gr_font_name;
}

//Provides: caml_gr_clear_graph
function caml_gr_clear_graph() {
    context.clearRect(0, 0, caml_gr_x_size, caml_gr_y_size);
    return 0;
}

//Provides: caml_gr_size_x
function caml_gr_size_x() {
    return caml_gr_x_size;
}

//Provides: caml_gr_size_y
function caml_gr_size_y() {
    return caml_gr_y_size;
}

//Provides: caml_gr_set_font
function caml_gr_set_font(font_name) {
    gr_font_name = font_name;
    local_gr_init_font();
    return 0;
}

//Provides: caml_gr_draw_string
function caml_gr_draw_string(text) {
    context.fillText(text,
                    caml_gr_current_x_pos,
                    caml_gr_y_size - caml_gr_current_y_pos);
    return 0;
}

//Provides: caml_gr_draw_char
function caml_gr_draw_char(text) {
    caml_gr_draw_string(text);
    return 0;
}

//Provides: caml_gr_set_color
function caml_gr_set_color(color) {
    gr_color_r = (color & 0xFF0000) >> 16;
    gr_color_g = (color & 0x00FF00) >> 8;
    gr_color_b = color & 0x0000FF;
    var new_style = "rgba("+gr_color_r+","+gr_color_g+","+gr_color_b+",255)";
    context.fillStyle = new_style;
    context.strokeStyle = new_style;
    return 0;
}

//Provides: caml_gr_point_color
function caml_gr_point_color(x_coor, y_coor) {
    var y_real_coor = caml_gr_y_size - y_coor;
    index = (x_coor + y_real_coor * caml_gr_x_size) * 4;
    var imageData =
        context.getImageData(x_coor, y_real_coor, 1, 1);
    var r = imageData.data[0];
    var g = imageData.data[1];
    var b = imageData.data[2];
    var color = (r << 16) + (g << 8) + b;
    return (r << 16) + (g << 8) + b;
}
