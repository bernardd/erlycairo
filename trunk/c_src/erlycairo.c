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
#include "uthash.h"
#include "cairo.h"


#define BUFSIZE 1000
#define KEY_LENGTH 100
#define ERLYCAIRO_STATUS_OK 0
#define ERLYCAIRO_STATUS_ERROR -1

typedef unsigned char byte;

typedef struct _cairo_context {
  char key[KEY_LENGTH]; 
  byte *cbuf;           /* Pointer to cairo buffer */
  cairo_surface_t *sf;  /* Pointer to cairo surface */
  cairo_t *cr;          /* Pointer to cairo structure */
  UT_hash_handle hh;    /* makes this structure hashable */
} cairo_context;

cairo_context *cairo_context_hash = NULL;

int main(int argc, char *argv[]) {
     
  int fd;                      /* fd to Erlang node */
  unsigned char buf[BUFSIZE];  /* Buffer for incoming message */
  ErlMessage emsg;             /* Incoming message */
  int cnode_number;            /* C-Node number */
  char *cookie;                /* Shared cookie */
  short creation;              /* ?? */
  char *erlang_node;           /* Erlang node to connect to */
  ETERM *fromp, *msgp, *fnp, *argp, *resp;
  int received, status, loop = 1;

  if (argc < 4) {
    erl_err_quit("invalid_args");
  }
  
  cnode_number = atoi(argv[1]);
  cookie = argv[2];
  creation = 0;
  erlang_node = argv[3];
  
  erl_init(NULL, 0);
  
  if (!erl_connect_init(cnode_number, cookie, creation)) {
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
          status = ERLYCAIRO_STATUS_OK;
        } else if (is_function(fnp, "new_image_blank")) {
          status = new_image_blank(fromp, argp);
        } else if (is_function(fnp, "write_to_png")) {
          status = write_to_png(fromp, argp);
        } else if (is_function(fnp, "close_image")) {
          status = close_image(fromp);
        } else if (is_function(fnp, "save")) {
          status = save(fromp);
        } else if (is_function(fnp, "restore")) {
          status = restore(fromp);
        } else if (is_function(fnp, "set_line_width")) {
          status = set_line_width(fromp, argp);
        } else if (is_function(fnp, "set_source_rgba")) {
          status = set_source_rgba(fromp, argp);	
	    } else if (is_function(fnp, "set_operator")) {
          status = set_operator(fromp, argp);
        } else if (is_function(fnp, "move_to")) {
          status = move_to(fromp, argp);
        } else if (is_function(fnp, "line_to")) {
          status = line_to(fromp, argp);
        } else if (is_function(fnp, "curve_to")) {
          status = curve_to(fromp, argp);
        } else if (is_function(fnp, "rel_move_to")) {
          status = rel_move_to(fromp, argp);
        } else if (is_function(fnp, "rel_line_to")) {
          status = rel_line_to(fromp, argp);
        } else if (is_function(fnp, "rel_curve_to")) {
          status = rel_curve_to(fromp, argp);
        } else if (is_function(fnp, "rectangle")) {
          status = rectangle(fromp, argp);
        } else if (is_function(fnp, "arc")) {
          status = arc(fromp, argp);
        } else if (is_function(fnp, "arc_negative")) {
          status = arc_negative(fromp, argp);
        } else if (is_function(fnp, "close_path")) {
          status = close_path(fromp);
        } else if (is_function(fnp, "paint")) {
          status = paint(fromp);
        } else if (is_function(fnp, "fill")) {
          status = fill(fromp);
        } else if (is_function(fnp, "fill_preserve")) {
          status = fill_preserve(fromp);
        } else if (is_function(fnp, "stroke")) {
          status = stroke(fromp, argp);
        } else if (is_function(fnp, "stroke_preserve")) {
          status = stroke_preserve(fromp, argp);
        } else if (is_function(fnp, "translate")) {
          status = translate(fromp, argp);
        } else if (is_function(fnp, "scale")) {
          status = scale(fromp, argp);
        } else if (is_function(fnp, "rotate")) {
          status = rotate(fromp, argp);
        } else if (is_function(fnp, "select_font")) {
          status = select_font_face(fromp, argp);
        } else if (is_function(fnp, "set_font_size")) {
          status = set_font_size(fromp, argp);
        } else if (is_function(fnp, "show_text")) {
          status = show_text(fromp, argp);
        } else {
          fprintf(stderr, "unknown command: %s\n\r", ERL_ATOM_PTR(fnp));
        }
        
        if (status == 0) {
          resp = erl_format("{cnode, ~i, ok}", cnode_number);
        } else if (status > 0) {
          resp = erl_format("{cnode, ~i, {error, '~s'}}", cnode_number, cairo_status_to_string(status));
        } else {
          resp = erl_format("{cnode, ~i, {error, '~s'}}", cnode_number, "status_not_implemented_yet");
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
  return (strncmp((char *)ERL_ATOM_PTR(fn), fn_name, strlen(fn_name)) == 0);
}

char *erlycairo_status_to_string(int status) {
  return "erlycairo error";
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


cairo_context *get_cairo_context(ETERM* from) {
  cairo_context *ctx = NULL;
  char key[KEY_LENGTH];
 
  sprintf(key, "%s%i%i%i", ERL_PID_NODE(from), 
    ERL_PID_NUMBER(from), ERL_PID_SERIAL(from), ERL_PID_CREATION(from));
  HASH_FIND_STR(cairo_context_hash, key, ctx);
  return ctx;
}


int new_image_blank(ETERM* from, ETERM *arg) {
  int stride, cbufsize, status, key_length;
  ETERM *width, *height;
  cairo_context *ctx = NULL;
             
  width = erl_element(1, arg);
  height = erl_element(2, arg);
  stride = ERL_INT_VALUE(width) * 4;
  cbufsize = ERL_INT_VALUE(height) * stride;
  key_length = strlen((char *)ERL_PID_NODE(from)) + 3*sizeof(int);
  if (key_length <= KEY_LENGTH) {
    ctx = malloc(sizeof(cairo_context));
    if (ctx) {
      sprintf(ctx->key, "%s%i%i%i", ERL_PID_NODE(from), 
        ERL_PID_NUMBER(from), ERL_PID_SERIAL(from), ERL_PID_CREATION(from)); 
      ctx->cbuf = (byte *)malloc(cbufsize);
      if (ctx->cbuf) {
        memset(ctx->cbuf, 0, cbufsize);
        ctx->sf = cairo_image_surface_create_for_data(ctx->cbuf, 
                                                      CAIRO_FORMAT_ARGB32,
                                                      ERL_INT_VALUE(width), 
                                                      ERL_INT_VALUE(height), 
                                                      stride);
        ctx->cr = cairo_create(ctx->sf);
        HASH_ADD_STR(cairo_context_hash, key, ctx);
        status = ERLYCAIRO_STATUS_OK;
      } else status = ERLYCAIRO_STATUS_ERROR;
    } else status = ERLYCAIRO_STATUS_ERROR;
  } else status = ERLYCAIRO_STATUS_ERROR;
  erl_free_term(width);
  erl_free_term(height);
  return status;
}

int write_to_png(ETERM* from, ETERM* arg) {
  int status;
  ETERM *file;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) { 
    file = erl_element(1, arg); 
    status = cairo_surface_write_to_png(ctx->sf, (char *)ERL_ATOM_PTR(file));
    erl_free_term(file);
    status = ERLYCAIRO_STATUS_OK;
  } else status = ERLYCAIRO_STATUS_ERROR;
  return status;
}

int close_image(ETERM* from) {
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    cairo_destroy(ctx->cr);
    cairo_surface_destroy(ctx->sf); 
    free(ctx->cbuf);
    HASH_DEL(cairo_context_hash, ctx);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int save(ETERM* from) {
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    cairo_save(ctx->cr);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int restore(ETERM* from) {
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    cairo_restore(ctx->cr);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int set_line_width(ETERM* from, ETERM* arg) {
  ETERM *width;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    width = erl_element(1, arg);   
    cairo_set_line_width(ctx->cr, ERL_INT_VALUE(width));
    erl_free_term(width);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int set_source_rgba(ETERM* from, ETERM* arg) {
  ETERM *r, *g, *b, *a;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    r = erl_element(1, arg); 
    g = erl_element(2, arg);
    b = erl_element(3, arg);
    a = erl_element(4, arg);  
    cairo_set_source_rgba(ctx->cr, ERL_FLOAT_VALUE(r), val(g), val(b), val(a));
    erl_free_term(r);
    erl_free_term(g);
    erl_free_term(b);
    erl_free_term(a);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int set_operator(ETERM* from, ETERM* arg) {
  ETERM *operator;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    operator = erl_element(1, arg);   
fprintf(stderr, "OPerator to %i\n\r", ERL_INT_VALUE(operator));  
    cairo_set_operator(ctx->cr, ERL_INT_VALUE(operator));
    erl_free_term(operator);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int move_to(ETERM* from, ETERM* arg) {
  ETERM *x, *y;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    x = erl_element(1, arg);  
    y = erl_element(2, arg); 
    cairo_move_to(ctx->cr, val(x), val(y));
    erl_free_term(x);
    erl_free_term(y);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int line_to(ETERM* from, ETERM* arg) {
  ETERM *x, *y;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    x = erl_element(1, arg);  
    y = erl_element(2, arg); 
    cairo_line_to(ctx->cr, ERL_INT_VALUE(x), ERL_INT_VALUE(y));
    erl_free_term(x);
    erl_free_term(y);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int curve_to(ETERM* from, ETERM* arg) {
  ETERM *c1x, *c1y, *c2x, *c2y, *x, *y;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    c1x = erl_element(1, arg);  
    c1y = erl_element(2, arg); 
    c2x = erl_element(3, arg);  
    c2y = erl_element(4, arg);
    x = erl_element(5, arg);  
    y = erl_element(6, arg);
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
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int rel_move_to(ETERM* from, ETERM* arg) {
  ETERM *x, *y;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    x = erl_element(1, arg);  
    y = erl_element(2, arg); 
    cairo_rel_move_to(ctx->cr, val(x), val(y));
    erl_free_term(x);
    erl_free_term(y);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int rel_line_to(ETERM* from, ETERM* arg) {
  ETERM *x, *y;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    x = erl_element(1, arg);  
    y = erl_element(2, arg); 
    cairo_rel_line_to(ctx->cr, val(x), val(y));
    erl_free_term(x);
    erl_free_term(y);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int rel_curve_to(ETERM* from, ETERM* arg) {
  ETERM *c1x, *c1y, *c2x, *c2y, *x, *y;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    c1x = erl_element(1, arg);  
    c1y = erl_element(2, arg); 
    c2x = erl_element(3, arg);  
    c2y = erl_element(4, arg);
    x = erl_element(5, arg);  
    y = erl_element(6, arg);
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
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int rectangle(ETERM* from, ETERM* arg) {
  ETERM *x1, *y1, *x2, *y2;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    x1 = erl_element(1, arg); 
    y1 = erl_element(2, arg);
    x2 = erl_element(3, arg);
    y2 = erl_element(4, arg);  
    cairo_rectangle(ctx->cr, val(x1),
      val(y1),
      val(x2),
      val(y2));
    erl_free_term(x1);
    erl_free_term(y1);
    erl_free_term(x2);
    erl_free_term(y2);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int arc(ETERM* from, ETERM* arg) {
  ETERM *x, *y, *r, *a1, *a2;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    x = erl_element(1, arg); 
    y = erl_element(2, arg);
    r = erl_element(3, arg);
    a1 = erl_element(4, arg);  
    a2 = erl_element(5, arg); 
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
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int arc_negative(ETERM* from, ETERM* arg) {
  ETERM *x, *y, *r, *a1, *a2;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    x = erl_element(1, arg); 
    y = erl_element(2, arg);
    r = erl_element(3, arg);
    a1 = erl_element(4, arg);  
    a2 = erl_element(5, arg); 
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
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int close_path(ETERM* from) { 
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) { 
    cairo_close_path(ctx->cr);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int paint(ETERM* from) {   
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    cairo_paint(ctx->cr);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int fill(ETERM* from) {  
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    cairo_fill(ctx->cr);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int fill_preserve(ETERM* from) {   
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    cairo_fill_preserve(ctx->cr);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int stroke(ETERM* from) { 
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) { 
    cairo_stroke(ctx->cr);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int stroke_preserve(ETERM* from) {   
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    cairo_stroke_preserve(ctx->cr);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int translate(ETERM* from, ETERM* arg) {
  ETERM *tx, *ty;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    tx = erl_element(1, arg);  
    ty = erl_element(2, arg); 
    cairo_translate(ctx->cr, val(tx), val(ty));
    erl_free_term(tx);
    erl_free_term(ty);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int scale(ETERM* from, ETERM* arg) {
  ETERM *sx, *sy;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    sx = erl_element(1, arg);  
    sy = erl_element(2, arg); 
    cairo_scale(ctx->cr, val(sx), val(sy));
    erl_free_term(sx);
    erl_free_term(sy);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int rotate(ETERM* from, ETERM* arg) {
  ETERM *angle;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    angle = erl_element(1, arg);   
    cairo_rotate(ctx->cr, val(angle));
    erl_free_term(angle);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}


int select_font_face(ETERM* from, ETERM* arg) {
  ETERM *family, *slant, *weight;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    family = erl_element(1, arg);  
    slant = erl_element(2, arg); 
    weight = erl_element(3, arg);
    cairo_select_font_face(ctx->cr, (char *)ERL_ATOM_PTR(family), ERL_INT_VALUE(slant), val(weight));
    erl_free_term(family);
    erl_free_term(slant);
    erl_free_term(weight);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int set_font_size(ETERM* from, ETERM* arg) {
  ETERM *size;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    size = erl_element(1, arg);   
    cairo_set_font_size(ctx->cr, val(size));
    erl_free_term(size);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}

int show_text(ETERM* from, ETERM* arg) {
  ETERM *text;
  cairo_context *ctx = get_cairo_context(from);
  if (ctx) {
    text = erl_element(1, arg);   
    cairo_show_text(ctx->cr, (char *)ERL_ATOM_PTR(text));
    erl_free_term(text);
    return ERLYCAIRO_STATUS_OK;
  } else return ERLYCAIRO_STATUS_ERROR;
}
