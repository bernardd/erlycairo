// File:      erlycairo.c
// author:    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
// copyright: 2007 Roberto Saccon
//  
// This module contains functions for starting a c-node which binds a subset
// of the cairo 2D graphics functions
// 
// The MIT License
// 
// Copyright (c) 2007 Roberto Saccon
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
// 
// since 2007-11-29 by Roberto Saccon


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "erl_interface.h"
#include "ei.h"
#include "cairo.h"


#define BUFSIZE 1000
#define KEY_LENGTH 100
#define ERR_CONTEXT "cairo_context error"

typedef unsigned char byte;

typedef struct _cairo_context {
    byte *cbuf;                /* Pointer to cairo buffer */
    cairo_surface_t *sf;       /* Pointer to cairo surface */
    cairo_t *cr;               /* Pointer to cairo structure */
} cairo_context;


ETERM * new_image_blank(ETERM *arg, int c_node);
ETERM * write_to_png(ETERM* arg, int c_node); 
ETERM * close_image(ETERM* arg, int c_node);   
ETERM * save(ETERM* arg, int c_node);
ETERM * restore(ETERM* arg, int c_node);
ETERM * set_line_width(ETERM* arg, int c_node);
ETERM * set_source_rgba(ETERM* arg, int c_node);
ETERM * set_operator(ETERM* arg, int c_node);
ETERM * move_to(ETERM* arg, int c_node);
ETERM * line_to(ETERM* arg, int c_node);
ETERM * curve_to(ETERM* arg, int c_node);
ETERM * rel_move_to(ETERM* arg, int c_node);
ETERM * rel_line_to(ETERM* arg, int c_node);
ETERM * rel_curve_to(ETERM* arg, int c_node);
ETERM * rectangle(ETERM* arg, int c_node);
ETERM * arc(ETERM* arg, int c_node);
ETERM * arc_negative(ETERM* arg, int c_node);
ETERM * close_path(ETERM* arg, int c_node);
ETERM * paint(ETERM* arg, int c_node);
ETERM * fill(ETERM* arg, int c_node);  
ETERM * fill_preserve(ETERM* arg, int c_node);   
ETERM * stroke(ETERM* arg, int c_node);
ETERM * stroke_preserve(ETERM* arg, int c_node);   
ETERM * translate(ETERM* arg, int c_node);
ETERM * scale(ETERM* arg, int c_node);
ETERM * rotate(ETERM* arg, int c_node);
ETERM * select_font_face(ETERM* arg, int c_node);
ETERM * set_font_size(ETERM* arg, int c_node);
ETERM * show_text(ETERM* arg, int c_node);
ETERM * text_extents(ETERM* arg, int c_node);
ETERM * surface_create_from_png(ETERM* arg, int c_node);
ETERM * surface_get_width(ETERM* arg, int c_node);
ETERM * surface_get_height(ETERM* arg, int c_node);
ETERM * surface_destroy(ETERM* arg, int c_node);
ETERM * set_source_surface(ETERM* arg, int c_node);
ETERM * write_to_png_stream(ETERM* arg, int c_node);


int main(int argc, char *argv[]) {     
    int fd;                      /* fd to Erlang node */
    unsigned char buf[BUFSIZE];  /* Buffer for incoming message */
    ErlMessage emsg;             /* Incoming message */
    int c_node;                   /* C-Node number */
    char *cookie;                /* Shared cookie */
    short creation;              /* ?? */
    char *erlang_node;           /* Erlang node to connect to */
    ETERM *fromp, *msgp, *fnp, *argp, *resp;
    int received, loop = 1;
    
    if (argc < 4) {
        erl_err_quit("invalid_args");
    }
    
    c_node = atoi(argv[1]);
    cookie = argv[2];
    creation = 0;
    erlang_node = argv[3];
    
    erl_init(NULL, 0);
    
    if (!erl_connect_init(c_node, cookie, creation)) {
        erl_err_quit("erl_connect_init");
    }
    
    if ((fd = erl_connect(erlang_node)) < 0) {
        erl_err_quit("erl_connect"); 
    }
       
    while (loop) {
        received = erl_receive_msg(fd, buf, BUFSIZE, &emsg);

    if (received == ERL_TICK) {
        /* ignore */    
    } else if (received == ERL_ERROR) {
        loop = 0;
    } else {
        if (emsg.type == ERL_REG_SEND) {          
            fromp = erl_element(2, emsg.msg);
            msgp = erl_element(3, emsg.msg);
            fnp = erl_element(1, msgp);
            argp = erl_element(2, msgp);  
            
            if (is_function(fnp, "stop")) {
                loop = 0;
                resp = erl_format("{c_node, ~i, ok}", c_node);
            } else if (is_function(fnp, "new_image_blank")) { 
                resp = new_image_blank(argp, c_node); 
            } else if (is_function(fnp, "write_to_png")) {
                resp = write_to_png(argp, c_node);
            } else if (is_function(fnp, "close_image")) {
                resp = close_image(argp, c_node);
            } else if (is_function(fnp, "save")) {
                resp = save(argp, c_node);
            } else if (is_function(fnp, "restore")) {
                resp = restore(argp, c_node);
            } else if (is_function(fnp, "set_line_width")) {
                resp = set_line_width(argp, c_node);
            } else if (is_function(fnp, "set_source_rgba")) {
                resp = set_source_rgba(argp, c_node);	
	        } else if (is_function(fnp, "set_operator")) {
                resp = set_operator(argp, c_node);
            } else if (is_function(fnp, "move_to")) {
                resp = move_to(argp, c_node);
            } else if (is_function(fnp, "line_to")) {
                resp = line_to(argp, c_node);
            } else if (is_function(fnp, "curve_to")) {
                resp = curve_to(argp, c_node);
            } else if (is_function(fnp, "rel_move_to")) {
                resp = rel_move_to(argp, c_node);
            } else if (is_function(fnp, "rel_line_to")) {
                resp = rel_line_to(argp, c_node);
            } else if (is_function(fnp, "rel_curve_to")) {
                resp = rel_curve_to(argp, c_node);
            } else if (is_function(fnp, "rectangle")) {
                resp = rectangle(argp, c_node);
            } else if (is_function(fnp, "arc")) {
                resp = arc(argp, c_node);
            } else if (is_function(fnp, "arc_negative")) {
                resp = arc_negative(argp, c_node);
            } else if (is_function(fnp, "close_path")) {
                resp = close_path(argp, c_node);
            } else if (is_function(fnp, "paint")) {
                resp = paint(argp, c_node);
            } else if (is_function(fnp, "fill")) {
                resp = fill(argp, c_node);
            } else if (is_function(fnp, "fill_preserve")) {
                resp = fill_preserve(argp, c_node);
            } else if (is_function(fnp, "stroke")) {
                resp = stroke(argp, c_node);
            } else if (is_function(fnp, "stroke_preserve")) {
                resp = stroke_preserve(argp, c_node);
            } else if (is_function(fnp, "translate")) {
                resp = translate(argp, c_node);
            } else if (is_function(fnp, "scale")) {
                resp = scale(argp, c_node);
            } else if (is_function(fnp, "rotate")) {
                resp = rotate(argp, c_node);
            } else if (is_function(fnp, "select_font")) {
                resp = select_font_face(argp, c_node);
            } else if (is_function(fnp, "set_font_size")) {
                resp = set_font_size(argp, c_node);
            } else if (is_function(fnp, "show_text")) {
                resp = show_text(argp, c_node);
            } else if (is_function(fnp, "text_extents")) {
                resp = text_extents(argp, c_node);          
            } else if (is_function(fnp, "surface_create_from_png")) {
                resp = surface_create_from_png(argp, c_node);          
            } else if (is_function(fnp, "surface_get_width")) {
                resp = surface_get_width(argp, c_node);          
            } else if (is_function(fnp, "surface_get_height")) {
                resp = surface_get_height(argp, c_node);          
            } else if (is_function(fnp, "surface_destroy")) {
                resp = surface_destroy(argp, c_node);          
            } else if (is_function(fnp, "set_source_surface")) {
                resp = set_source_surface(argp, c_node);          
            } else if (is_function(fnp, "write_to_png_stream")) {
                resp = write_to_png_stream(argp, c_node);          
            } else {
                resp = erl_format("{c_node, ~i, {error, '~s'}}", c_node, "unknown command");
            }         
            erl_send(fd, fromp, resp);           
            erl_free_term(emsg.from); 
            erl_free_term(emsg.msg);
            erl_free_term(fromp); 
            erl_free_term(msgp);
            erl_free_term(fnp); 
            erl_free_term(argp);
            erl_free_term(resp);
      }
    }
  }
  exit(EXIT_SUCCESS);
}


int is_function(ETERM *fn, char *fn_name) {
    return (strcmp((char *)ERL_ATOM_PTR(fn), fn_name) == 0);
}


double val(ETERM *arg) {
    if (ERL_IS_INTEGER(arg))
        return ERL_INT_VALUE(arg);
    else if (ERL_IS_FLOAT(arg))
        return ERL_FLOAT_VALUE(arg);
    else {
        exit(EXIT_FAILURE);
        return 0;
    }
}

void *ptr(ETERM *arg) {
	if (ERL_IS_INTEGER(arg))
		return (void*)ERL_INT_VALUE(arg);
	return NULL;
}


cairo_context *get_cairo_context(ETERM* arg) {
    ETERM* ctx = erl_element(1, arg); 
    cairo_context *ret = (cairo_context*)ptr(ctx);
    erl_free_term(ctx);
    FILE *f = fopen("/tmp/ec.txt", "a+");
    fprintf(f, "Ctx: %d\n", ret);
    fclose(f);
    return ret;
}


ETERM * new_image_blank(ETERM *arg, int c_node) { 
    int stride, cbufsize, status, key_length;
    ETERM *width, *height;
    cairo_context *ctx = NULL;             
    width = erl_element(1, arg);
    height = erl_element(2, arg);
    stride = ERL_INT_VALUE(width) * 4;
    cbufsize = ERL_INT_VALUE(height) * stride;
    ctx = malloc(sizeof(cairo_context));
    if (ctx) {
        ctx->cbuf = (byte *)malloc(cbufsize);
        if (ctx->cbuf) {
            memset(ctx->cbuf, 0, cbufsize);
            ctx->sf = cairo_image_surface_create_for_data(ctx->cbuf, 
                    CAIRO_FORMAT_ARGB32, ERL_INT_VALUE(width), 
                    ERL_INT_VALUE(height), stride);
            ctx->cr = cairo_create(ctx->sf);
            return erl_format("{c_node, ~i, {ok, ~i}}", c_node, ctx);
        } else {
            free(ctx);
            return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
        }
    } else {
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
    erl_free_term(width);
    erl_free_term(height);
}


ETERM * write_to_png(ETERM* arg, int c_node) {
    int status;
    ETERM *file;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) { 
        file = erl_element(2, arg); 
        status = cairo_surface_write_to_png(ctx->sf, (char *)ERL_ATOM_PTR(file));
        erl_free_term(file);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * close_image(ETERM* arg, int c_node) {
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        cairo_destroy(ctx->cr);
        cairo_surface_destroy(ctx->sf); 
        free(ctx->cbuf);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * save(ETERM* arg, int c_node) {
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        cairo_save(ctx->cr);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * restore(ETERM* arg, int c_node) {
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        cairo_restore(ctx->cr);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * set_line_width(ETERM* arg, int c_node) {
    ETERM *width;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        width = erl_element(2, arg);   
        cairo_set_line_width(ctx->cr, ERL_INT_VALUE(width));
        erl_free_term(width);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * set_source_rgba(ETERM* arg, int c_node) {
    ETERM *r, *g, *b, *a;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        r = erl_element(2, arg); 
        g = erl_element(3, arg);
        b = erl_element(4, arg);
        a = erl_element(5, arg);  
        cairo_set_source_rgba(ctx->cr, ERL_FLOAT_VALUE(r), val(g), val(b), val(a));
        erl_free_term(r);
        erl_free_term(g);
        erl_free_term(b);
        erl_free_term(a);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * set_operator(ETERM* arg, int c_node) {
    ETERM *operator;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        operator = erl_element(2, arg);    
        cairo_set_operator(ctx->cr, ERL_INT_VALUE(operator));
        erl_free_term(operator);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * move_to(ETERM* arg, int c_node) {
    ETERM *x, *y;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        x = erl_element(2, arg);  
        y = erl_element(3, arg); 
        cairo_move_to(ctx->cr, val(x), val(y));
        erl_free_term(x);
        erl_free_term(y);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * line_to(ETERM* arg, int c_node) {
    ETERM *x, *y;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        x = erl_element(2, arg);  
        y = erl_element(3, arg); 
        cairo_line_to(ctx->cr, ERL_INT_VALUE(x), ERL_INT_VALUE(y));
        erl_free_term(x);
        erl_free_term(y);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * curve_to(ETERM* arg, int c_node) {
    ETERM *c1x, *c1y, *c2x, *c2y, *x, *y;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        c1x = erl_element(2, arg);  
        c1y = erl_element(3, arg); 
        c2x = erl_element(4, arg);  
        c2y = erl_element(5, arg);
        x = erl_element(6, arg);  
        y = erl_element(7, arg);
        cairo_curve_to(ctx->cr, ERL_INT_VALUE(c1x), 
            val(c1y),
            val(c2x),
            val(c2y),
            val(x),
            val(y));
        erl_free_term(c1x);
        erl_free_term(c1y);
        erl_free_term(c2x);
        erl_free_term(c2y);
        erl_free_term(x);
        erl_free_term(y);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * rel_move_to(ETERM* arg, int c_node) {
    ETERM *x, *y;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        x = erl_element(2, arg);  
        y = erl_element(3, arg); 
        cairo_rel_move_to(ctx->cr, val(x), val(y));
        erl_free_term(x);
        erl_free_term(y);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * rel_line_to(ETERM* arg, int c_node) {
    ETERM *x, *y;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        x = erl_element(2, arg);  
        y = erl_element(3, arg); 
        cairo_rel_line_to(ctx->cr, val(x), val(y));
        erl_free_term(x);
        erl_free_term(y);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * rel_curve_to(ETERM* arg, int c_node) {
    ETERM *c1x, *c1y, *c2x, *c2y, *x, *y;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        c1x = erl_element(2, arg);  
        c1y = erl_element(3, arg); 
        c2x = erl_element(4, arg);  
        c2y = erl_element(5, arg);
        x = erl_element(6, arg);  
        y = erl_element(7, arg);
        cairo_rel_curve_to(ctx->cr, val(c1x), 
            val(c1y),
            val(c2x),
            val(c2y),
            val(x),
            val(y));
        erl_free_term(c1x);
        erl_free_term(c1y);
        erl_free_term(c2x);
        erl_free_term(c2y);
        erl_free_term(x);
        erl_free_term(y);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * rectangle(ETERM* arg, int c_node) {
  ETERM *x1, *y1, *x2, *y2;
  cairo_context *ctx = get_cairo_context(arg);
  if (ctx) {
    x1 = erl_element(2, arg); 
    y1 = erl_element(3, arg);
    x2 = erl_element(4, arg);
    y2 = erl_element(5, arg);  
    cairo_rectangle(ctx->cr, val(x1),
      val(y1),
      val(x2),
      val(y2));
    erl_free_term(x1);
    erl_free_term(y1);
    erl_free_term(x2);
    erl_free_term(y2);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * arc(ETERM* arg, int c_node) {
    ETERM *x, *y, *r, *a1, *a2;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        x = erl_element(2, arg); 
        y = erl_element(3, arg);
        r = erl_element(4, arg);
        a1 = erl_element(5, arg);  
        a2 = erl_element(6, arg); 
        cairo_arc(ctx->cr, val(x),
            val(y),
            val(r),
            val(a1),
            val(a2));
        erl_free_term(x);
        erl_free_term(y);
        erl_free_term(r);
        erl_free_term(a1);
        erl_free_term(a2);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * arc_negative(ETERM* arg, int c_node) {
  ETERM *x, *y, *r, *a1, *a2;
  cairo_context *ctx = get_cairo_context(arg);
  if (ctx) {
        x = erl_element(2, arg); 
        y = erl_element(3, arg);
        r = erl_element(4, arg);
        a1 = erl_element(5, arg);  
        a2 = erl_element(6, arg); 
        cairo_arc_negative(ctx->cr, val(x),
            val(y),
            val(r),
            val(a1),
            val(a2));
        erl_free_term(x);
        erl_free_term(y);
        erl_free_term(r);
        erl_free_term(a1);
        erl_free_term(a2);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * close_path(ETERM* arg, int c_node) { 
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) { 
        cairo_close_path(ctx->cr);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * paint(ETERM* arg, int c_node) {   
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        cairo_paint(ctx->cr);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * fill(ETERM* arg, int c_node) {  
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        cairo_fill(ctx->cr);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * fill_preserve(ETERM* arg, int c_node) {   
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        cairo_fill_preserve(ctx->cr);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * stroke(ETERM* arg, int c_node) { 
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) { 
        cairo_stroke(ctx->cr);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * stroke_preserve(ETERM* arg, int c_node) {   
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        cairo_stroke_preserve(ctx->cr);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * translate(ETERM* arg, int c_node) {
    ETERM *tx, *ty;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        tx = erl_element(2, arg);  
        ty = erl_element(3, arg); 
        cairo_translate(ctx->cr, val(tx), val(ty));
        erl_free_term(tx);
        erl_free_term(ty);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * scale(ETERM* arg, int c_node) {
    ETERM *sx, *sy;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        sx = erl_element(2, arg);  
        sy = erl_element(3, arg); 
        cairo_scale(ctx->cr, val(sx), val(sy));
        erl_free_term(sx);
        erl_free_term(sy);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * rotate(ETERM* arg, int c_node) {
    ETERM *angle;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        angle = erl_element(2, arg);   
        cairo_rotate(ctx->cr, val(angle));
        erl_free_term(angle);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * select_font_face(ETERM* arg, int c_node) {
    ETERM *family, *slant, *weight;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        family = erl_element(2, arg);  
        slant = erl_element(3, arg); 
        weight = erl_element(4, arg);
        cairo_select_font_face(ctx->cr, (char *)ERL_ATOM_PTR(family), ERL_INT_VALUE(slant), val(weight));
        erl_free_term(family);
        erl_free_term(slant);
        erl_free_term(weight);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * set_font_size(ETERM* arg, int c_node) {
    ETERM *size;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
      size = erl_element(2, arg);   
        cairo_set_font_size(ctx->cr, val(size));
        erl_free_term(size);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * show_text(ETERM* arg, int c_node) {
    ETERM *text;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        text = erl_element(2, arg);   
        cairo_show_text(ctx->cr, (char *)ERL_ATOM_PTR(text));
        erl_free_term(text);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * text_extents(ETERM* arg, int c_node) {
    ETERM *text;
    cairo_text_extents_t extents;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        text = erl_element(2, arg);
        cairo_text_extents (ctx->cr, (char *)ERL_ATOM_PTR(text), &extents);        
        erl_free_term(text);
        return erl_format("{c_node, ~i, {~f, ~f}}", c_node, extents.width, extents.height);
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


ETERM * surface_create_from_png(ETERM* arg, int c_node) {
    ETERM *file;
    file = erl_element(1, arg); 
    cairo_surface_t *surface = cairo_image_surface_create_from_png((char *)ERL_ATOM_PTR(file));
    erl_free_term(file);
    return erl_format("{c_node, ~i, {ok, ~i}}", c_node, surface);
}


ETERM * surface_get_width(ETERM* arg, int c_node) {
    ETERM *surface;
    surface = erl_element(1, arg); 
    int w = cairo_image_surface_get_width(ptr(surface));
    erl_free_term(surface);
    return erl_format("{c_node, ~i, {ok, ~i}}", c_node, w);
}

ETERM * surface_get_height(ETERM* arg, int c_node) {
    ETERM *surface;
    surface = erl_element(1, arg); 
    int h = cairo_image_surface_get_height(ptr(surface));
    erl_free_term(surface);
    return erl_format("{c_node, ~i, {ok, ~i}}", c_node, h);
}


ETERM * surface_destroy(ETERM* arg, int c_node) {
    ETERM *surface;
    surface = erl_element(1, arg); 
    cairo_surface_destroy(ptr(surface));
    erl_free_term(surface);
    return erl_format("{c_node, ~i, ok}", c_node);
}


ETERM * set_source_surface(ETERM* arg, int c_node) {
    ETERM *surface, *x, *y;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) {
        surface = erl_element(2, arg); 
        x = erl_element(3, arg);
        y = erl_element(4, arg);
        cairo_set_source_surface(ctx->cr, ptr(surface), val(x), val(y));
        erl_free_term(surface);
        erl_free_term(x);
        erl_free_term(y);
        return erl_format("{c_node, ~i, ok}", c_node);
    } else {
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}


struct png_data
{
    unsigned int size;
    unsigned int written;
    unsigned char *buf;
};

cairo_status_t write_cb(void *closure, const unsigned char *data, unsigned int length) {
    struct png_data *out = (struct png_data*)closure;

    // TODO: smarter allocation...
    if (length + out->written > out->size) {
        out->size = length + out->written;
        out->buf = realloc(out->buf, out->size);
    }

    memcpy(out->buf + out->written, data, length);
    out->written = out->size;
    return CAIRO_STATUS_SUCCESS;
}

ETERM * write_to_png_stream(ETERM* arg, int c_node) {
    int status;
    cairo_context *ctx = get_cairo_context(arg);
    if (ctx) { 
        struct png_data out = {};
        status = cairo_surface_write_to_png_stream(ctx->sf, write_cb, &out);
        ETERM *term = NULL;

        ei_x_buff req;
        ei_x_new_with_version(&req);
        ei_x_encode_tuple_header(&req, 3);
        ei_x_encode_atom(&req, "c_node");
        ei_x_encode_long(&req, c_node);
        ei_x_encode_tuple_header(&req, 2);
        ei_x_encode_atom(&req, "ok");
        ei_x_encode_binary(&req, out.buf, out.written);

        int index = 0;
        ei_decode_term(req.buff, &index, &term);
        ei_x_free(&req);
        free(out.buf);
        return term;
    } else { 
        return erl_format("{c_node, ~i, {error, '~s'}}", c_node, ERR_CONTEXT);
    }
}
