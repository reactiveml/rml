var caml_gr_x_size = 500;
var caml_gr_y_size = 400;

var caml_gr_x_loc = 10;
var caml_gr_y_loc = 100;

var caml_gr_current_x_pos = 0;
var caml_gr_current_y_pos = 0;

var canvas_elt;
var context;

var gr_color_r = 0;
var gr_color_g = 0;
var gr_color_b = 0;
var gr_font_name = "Arial";
var gr_font_size = 20;

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

//Provides: caml_gr_text_size
function caml_gr_text_size(text) {
    var msr = context.measureText(text);
    return [0, msr.width, gr_font_size]; // FIXME height is incorrect
}

//Provides: caml_gr_draw_string
function caml_gr_draw_string(text) {
    context.fillText(text,
                    caml_gr_current_x_pos,
                    caml_gr_y_size - caml_gr_current_y_pos - 1);
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
    var y_real_coor = caml_gr_y_size - y_coor - 1;
    index = (x_coor + y_real_coor * caml_gr_x_size) * 4;
    var imageData =
        context.getImageData(x_coor, y_real_coor, 1, 1);
    var r = imageData.data[0];
    var g = imageData.data[1];
    var b = imageData.data[2];
    return (r << 16) + (g << 8) + b;
}

//Provides: caml_gr_fill_poly
function caml_gr_fill_poly(points) {
    context.beginPath();
    for (var i = 1; i < points.length; i++) {
        if (i == 1) caml_gr_moveto(points[i][1], points[i][2]);
        caml_gr_lineto(points[i][1], points[i][2]);
    }
    context.closePath();
    context.fill();
    return 0;
}

//Provides: caml_gr_draw_image
function caml_gr_draw_image(image, x, y) {
    context.putImageData(image, x, caml_gr_y_size - y - 1);
    return 0;
}

//Provides: caml_gr_make_image
function caml_gr_make_image(colors) {
    var w = colors.length;
    var h = 0;
    if (w > 1) h = colors[1].length;
    var im = context.createImageData(w, h);
    var idx;
    var color;
    for (var x = 1; x <= w; x++)
        for (var y = 1; y <= h; y++) {
            idx = (x + (h - y) * w) * 4;
            color = colors[x][y];
            im.data[idx+0] = (color & 0xFF0000) >> 16; // r
            im.data[idx+1] = (color & 0x00FF00) >> 8;  // g
            im.data[idx+2] = (color & 0x0000FF);       // b
            im.data[idx+3] = 255;                      // a
        }
    return im;
}

//Provides: caml_gr_dump_image
function caml_gr_dump_image(im) {
    var colors = [];
    colors.length = im.width;
    colors[0] = 0;
    for (var i = 0, j = 0; i < im.data.length; i += 4, j++) {
        var x = 1 + (j % im.width);
        var y = im.height - Math.floor(j / im.height);
        if (typeof colors[x] == "undefined") {
            colors[x] = [];
            colors[x].length = im.height;
            colors[x][0] = 0;
        }
        colors[x][y] = (im.data[i] << 16) + (im.data[i+1] << 8) + im.data[i+2];
    }
    return colors;
}

// Events — really, not a sane implementation for caml_gr_wait_event

var gr_mouse_x = 0;
var gr_mouse_y = 0;
var gr_button = 0;
var gr_key_pressed = 0;
var gr_keys = $("#canvas").queue();

$("#canvas").mousemove(function(ev) {
    var offset = $("#canvas").offset();
    gr_mouse_x = Math.round(ev.pageX - offset.left);
    gr_mouse_y = Math.round(caml_gr_y_size - ev.pageY + offset.top);
});

$("#canvas").on("mousedown, mouseup", function (ev) {
    gr_button = 1;
});

$(document).keypress(function (ev) {
    gr_key_pressed = 1;
    gr_keys.push(ev.keyCode ? ev.keyCode : ev.which);
});
