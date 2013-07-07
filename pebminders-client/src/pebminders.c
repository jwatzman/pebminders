#include "pebble_os.h"
#include "pebble_app.h"
#include "pebble_fonts.h"

#define MY_UUID {0x40, 0x5A, 0x82, 0xF9, 0x7E, 0xF2, 0x43, 0x6F, 0xA3, 0x8B, 0x59, 0x35, 0x68, 0xD0, 0x0C, 0xD7}
PBL_APP_INFO(MY_UUID, "PebMinders", "Josh Watzman", 3, 0 /* App version */, RESOURCE_ID_IMAGE_MENU_ICON, APP_INFO_WATCH_FACE);

Window window;

TextLayer text_pebminder_layer;
TextLayer text_date_layer;
TextLayer text_time_layer;

Layer line_layer;


void line_layer_update_callback(Layer *me, GContext* ctx) {

  graphics_context_set_stroke_color(ctx, GColorWhite);

  graphics_draw_line(ctx, GPoint(8, 97), GPoint(131, 97));
  graphics_draw_line(ctx, GPoint(8, 98), GPoint(131, 98));

}


void handle_init(AppContextRef ctx) {

  window_init(&window, "PebMinders");
  window_stack_push(&window, true /* Animated */);
  window_set_background_color(&window, GColorBlack);

  resource_init_current_app(&APP_RESOURCES);

  // TODO factor out this setup?
  // TODO consts for the frame
  text_layer_init(&text_pebminder_layer, window.layer.frame);
  text_layer_set_text_color(&text_pebminder_layer, GColorWhite);
  text_layer_set_background_color(&text_pebminder_layer, GColorClear);
  layer_set_frame(&text_pebminder_layer.layer, GRect(1, 1, 144-1, 168-68));
  text_layer_set_font(&text_pebminder_layer, fonts_get_system_font(FONT_KEY_GOTHIC_14));
  layer_add_child(&window.layer, &text_pebminder_layer.layer);

  text_layer_init(&text_date_layer, window.layer.frame);
  text_layer_set_text_color(&text_date_layer, GColorWhite);
  text_layer_set_background_color(&text_date_layer, GColorClear);
  layer_set_frame(&text_date_layer.layer, GRect(8, 68, 144-8, 168-68));
  text_layer_set_font(&text_date_layer, fonts_load_custom_font(resource_get_handle(RESOURCE_ID_FONT_ROBOTO_CONDENSED_21)));
  layer_add_child(&window.layer, &text_date_layer.layer);


  text_layer_init(&text_time_layer, window.layer.frame);
  text_layer_set_text_color(&text_time_layer, GColorWhite);
  text_layer_set_background_color(&text_time_layer, GColorClear);
  layer_set_frame(&text_time_layer.layer, GRect(7, 92, 144-7, 168-92));
  text_layer_set_font(&text_time_layer, fonts_load_custom_font(resource_get_handle(RESOURCE_ID_FONT_ROBOTO_BOLD_SUBSET_49)));
  layer_add_child(&window.layer, &text_time_layer.layer);


  layer_init(&line_layer, window.layer.frame);
  line_layer.update_proc = &line_layer_update_callback;
  layer_add_child(&window.layer, &line_layer);

  static char hello_text[] = "• Hello world!";
  text_layer_set_text(&text_pebminder_layer, hello_text);

  // TODO: Update display here to avoid blank display on launch?
}


void handle_minute_tick(AppContextRef ctx, PebbleTickEvent *t) {

  // Need to be static because they're used by the system later.
  static char time_text[] = "00:00";
  static char date_text[] = "Xxxxxxxxx 00";

  char *time_format;


  // TODO: Only update the date when it's changed.
  string_format_time(date_text, sizeof(date_text), "%B %e", t->tick_time);
  text_layer_set_text(&text_date_layer, date_text);


  if (clock_is_24h_style()) {
    time_format = "%R";
  } else {
    time_format = "%I:%M";
  }

  string_format_time(time_text, sizeof(time_text), time_format, t->tick_time);

  // Kludge to handle lack of non-padded hour format string
  // for twelve hour clock.
  if (!clock_is_24h_style() && (time_text[0] == '0')) {
    memmove(time_text, &time_text[1], sizeof(time_text) - 1);
  }

  text_layer_set_text(&text_time_layer, time_text);

}


void pbl_main(void *params) {
  PebbleAppHandlers handlers = {
    .init_handler = &handle_init,

    .tick_info = {
      .tick_handler = &handle_minute_tick,
      .tick_units = MINUTE_UNIT
    }

  };
  app_event_loop(params, &handlers);
}
