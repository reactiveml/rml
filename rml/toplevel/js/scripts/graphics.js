//Provides: caml_gr_open_graph
function caml_gr_open_graph(geometry) {
    var matches;
    if (matches = /[^ ]* (\d+)x(\d+)(?:\+(\d+)-(\d+))?/.exec(geometry)) {
        caml_gr_x_size = matches[1];
        caml_gr_y_size = matches[2];
        if (matches[3]) { caml_gr_x_loc = matches[3]; }
        if (matches[4]) { caml_gr_y_loc = matches[4]; }
    }
    $("#graphics").show();
    $("#graphics").draggable();
    $("#graphics").height(parseInt(caml_gr_y_size,10)+23);
    $("#graphics").width(parseInt(caml_gr_x_size,10)+3);
    $("#graphics").css({ "top": caml_gr_y_loc+"px",
                         "left": caml_gr_x_loc+"px"
                       });
    canvas_elt = document.getElementById("canvas");
    var support = !!(canvas_elt.getContext && canvas_elt.getContext('2d'));
    if (support) {
        canvas_elt.setAttribute("width", caml_gr_x_size);
        canvas_elt.setAttribute("height", caml_gr_y_size);
        canvas_elt.setAttribute("style", "position: absolute; x:0; y:0;");
        context = canvas_elt.getContext("2d");
        caml_gr_set_line_width(1);
        local_gr_init_font();
    } else {
        console.log ("Your browser doesn't support HTML5 Canvas element");
        alert("Your browser doesn't support HTML5 Canvas element");
    }
    return 0;
}

//Provides: caml_gr_close_graph
function caml_gr_close_graph() {
    $("#graphics").hide();
    return 0;
}

//Provides: caml_gr_sigio_handler
function caml_gr_sigio_handler(sig) {
    return 0; // dummy value
}

//Provides: caml_gr_sigio_signal
function caml_gr_sigio_signal() {
    return 0; // dummy value
}

//Provides: caml_install_signal_handler
function caml_install_signal_handler(sig_num, sig_beh) {
    return 0; // return unit
}

//Provides: caml_gr_create_image
function caml_gr_create_image(x_size, y_size) {
    var im = context.createImageData(x_size, y_size);
    return im;
}

//Provides: caml_gr_blit_image
function caml_gr_blit_image(im1, x, y) {
    var y_real = caml_gr_y_size - y - im1.height;
    var im2 = context.getImageData(x, y_real, im1.width, im1.height);
    var tmp;
    for (var i = 0; i < im2.data.length; i += 4) {
        tmp= im2.data[i] | im2.data[i+1] | im2.data[i+2] | im2.data[i+3];
        if (tmp != 0)
            for (var j = 0; j < 4; j++)
                im1.data[i+j] = im2.data[i+j];
    }
    return 0;
}

//Provides: caml_gr_current_x
function caml_gr_current_x() {
    return caml_gr_current_x_pos;
}

//Provides: caml_gr_current_y
function caml_gr_current_y() {
    return caml_gr_current_y_pos;
}

//Provides: caml_gr_display_mode
function caml_gr_display_mode(mode) {
    console.log("caml_gr_display_mode not implemented");
    return 0;
}

//Provides: caml_gr_draw_rect
function caml_gr_draw_rect(x, y, w, h) {
    context.strokeRect(x, caml_gr_y_size - y - 1, w, -h);
    return 0;
}

//Provides: caml_gr_fill_rect
function caml_gr_fill_rect(x, y, w, h) {
    context.fillRect(x, caml_gr_y_size - y - 1, w, -h);
    return 0;
}

//Provides: caml_gr_draw_arc
function caml_gr_draw_arc(x, y, rx, ry, a1, a2) {
    context.beginPath();
    context.scale(1, ry/rx);
    context.arc(x, caml_gr_y_size - y - 1, rx, a1, a2);
    context.scale(1, rx/ry);
    context.closePath();
    context.stroke();
    return 0;
}

//Provides: caml_gr_fill_arc
function caml_gr_fill_arc(x, y, rx, ry, a1, a2) {
    context.beginPath();
    context.scale(1, ry/rx);
    context.arc(x, caml_gr_y_size - y - 1, rx, a1, a2);
    context.scale(1, rx/ry);
    context.closePath();
    context.fill();
    return 0;
}

//Provides: caml_gr_lineto
function caml_gr_lineto(x_coor, y_coor) {
    context.lineTo(x_coor, caml_gr_y_size - y_coor - 1);
    context.stroke();
    caml_gr_moveto(x_coor, y_coor);
    return 0;
}

//Provides: caml_gr_moveto
function caml_gr_moveto(new_x, new_y) {
    caml_gr_current_x_pos = new_x;
    caml_gr_current_y_pos = new_y;
    context.moveTo(new_x, caml_gr_y_size - new_y - 1);
    return 0;
}

//Provides: caml_gr_plot
function caml_gr_plot(x_coor, y_coor) {
    var y_real_coor = caml_gr_y_size - y_coor - 1;
    var imageData =
        context.getImageData(x_coor, y_real_coor, 1, 1);
    imageData.data[0] = gr_color_r;
    imageData.data[1] = gr_color_g;
    imageData.data[2] = gr_color_b;
    imageData.data[3] = 255; // Set alpha to opaque
    context.putImageData(imageData, x_coor, y_real_coor);
    return 0;
}

//Provides: caml_gr_remember_mode
function caml_gr_remember_mode(mode) {
    console.log("caml_gr_remember_mode not implemented");
    return 0;
}

//Provides: caml_gr_resize_window
function caml_gr_resize_window(new_x, new_y) {
    caml_gr_x_size = new_x;
    caml_gr_y_size = new_y;
    $("#graphics").width(parseInt(new_x,10)+3);
    $("#graphics").height(parseInt(new_y,10)+23);
    canvas_elt.setAttribute("width", new_x);
    canvas_elt.setAttribute("height", new_y);
    return 0;
}

//Provides: caml_gr_set_line_width
function caml_gr_set_line_width(width) {
    context.lineWidth = width;
    return 0;
}

//Provides: caml_gr_set_text_size
function caml_gr_set_text_size(font_size) {
    gr_font_size = font_size;
    local_gr_init_font();
    return 0;
}

//Provides: caml_gr_set_window_title
function caml_gr_set_window_title(title) {
    $("#graphicstitle").html(title+"");
    return 0;
}

//Provides: caml_gr_synchronize
function caml_gr_synchronize() {
    console.log("caml_gr_synchronize not implemented");
    return 0;
}

//Provides: caml_gr_wait_event
function caml_gr_wait_event(events) {
    var local_mouse_x = 0;
    var local_mouse_y = 0;
    var local_button = 0;
    var local_key_pressed = 0;
    var local_key = 35; // key = '#'
    var array = events;
    while (array.length > 2) {
        switch(array[1]) {
        case 0: // Button_down
            local_button = gr_button;
            break;
        case 1: // Button_up
            local_button = gr_button;
            break;
        case 2: // Key_pressed
            local_key_pressed = gr_key_pressed;
            if (gr_keys.length > 0) local_key = gr_keys.pop();
            break;
        case 3: // Mouse_motion
            local_mouse_x = gr_mouse_x;
            local_mouse_y = gr_mouse_y;
            break;
        case 4: // Poll
            local_button = gr_button;
            local_key_pressed = gr_key_pressed;
            local_mouse_x = gr_mouse_x;
            local_mouse_y = gr_mouse_y;
            local_key_pressed = gr_key_pressed;
            break;
        }
        // reached end of list (i.e. tl l == nil)
        if (array[2] instanceof Array) {
            array = array[2];
        } else {
            break;
        }
    }
    var result = [0,
                  local_mouse_x,
                  local_mouse_y,
                  local_button,
                  local_key_pressed,
                  local_key
                 ];
    gr_button = 0;
    gr_key_pressed = 0;
    return result;
}
