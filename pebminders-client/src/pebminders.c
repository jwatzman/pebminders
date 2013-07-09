#include "pebble_os.h"
#include "pebble_app.h"
#include "pebble_fonts.h"
#include "http.h"

PBL_APP_INFO(
	HTTP_UUID,
	"PebMinders",
	"Josh Watzman",
	1,
	0 /* App version */,
	RESOURCE_ID_IMAGE_MENU_ICON,
	APP_INFO_WATCH_FACE
);

#define SCREEN_WIDTH 144
#define SCREEN_HEIGHT 168

#define PEBMINDER_X 1
#define PEBMINDER_Y 1
#define DATE_X 8
#define DATE_Y 68
#define TIME_X 7
#define TIME_Y 92

#define APP_ID 1499260626

Window window;

TextLayer text_pebminder_layer;
TextLayer text_date_layer;
TextLayer text_time_layer;

Layer line_layer;

void handle_failure(int32_t cookie, int http_status, void *context) {
	text_layer_set_text(&text_pebminder_layer, "• Failure");
}

void handle_success(
		int32_t cookie,
		int http_status,
		DictionaryIterator *iter,
		void *context) {
	text_layer_set_text(&text_pebminder_layer, "• Success");
}

void handle_reconnect(void *context) {
	text_layer_set_text(&text_pebminder_layer, "• Reconnect");
}

void line_layer_update_callback(Layer *me, GContext* ctx) {
	graphics_context_set_stroke_color(ctx, GColorWhite);
	graphics_draw_line(ctx, GPoint(8, 97), GPoint(131, 97));
	graphics_draw_line(ctx, GPoint(8, 98), GPoint(131, 98));
}

void init_text_layer(TextLayer *tlayer, GRect frame, GFont font) {
	text_layer_init(tlayer, window.layer.frame);
	text_layer_set_text_color(tlayer, GColorWhite);
	text_layer_set_background_color(tlayer, GColorClear);
	layer_set_frame(&(tlayer->layer), frame);
	text_layer_set_font(
		tlayer,
		font
	);
	layer_add_child(&window.layer, &(tlayer->layer));
}

void handle_init(AppContextRef ctx) {
	window_init(&window, "PebMinders");
	window_stack_push(&window, true /* Animated */);
	window_set_background_color(&window, GColorBlack);

	resource_init_current_app(&APP_RESOURCES);

	init_text_layer(
		&text_pebminder_layer,
		GRect(
			PEBMINDER_X,
			PEBMINDER_Y,
			SCREEN_WIDTH - PEBMINDER_X,
			DATE_Y - PEBMINDER_Y
		),
		fonts_get_system_font(FONT_KEY_GOTHIC_14)
	);

	init_text_layer(
		&text_date_layer,
		GRect(DATE_X, DATE_Y, SCREEN_WIDTH-DATE_X, SCREEN_HEIGHT-DATE_Y),
		fonts_load_custom_font(
			resource_get_handle(RESOURCE_ID_FONT_ROBOTO_CONDENSED_21)
		)
	);

	init_text_layer(
		&text_time_layer,
		GRect(TIME_X, TIME_Y, SCREEN_WIDTH-TIME_X, SCREEN_HEIGHT-TIME_Y),
		fonts_load_custom_font(
			resource_get_handle(RESOURCE_ID_FONT_ROBOTO_BOLD_SUBSET_49)
		)
	);

	layer_init(&line_layer, window.layer.frame);
	line_layer.update_proc = &line_layer_update_callback;
	layer_add_child(&window.layer, &line_layer);

	text_layer_set_text(&text_pebminder_layer, "• Init");
	http_set_app_id(APP_ID);
	http_register_callbacks(
		(HTTPCallbacks){
			.failure=handle_failure,
			.success=handle_success,
			.reconnect=handle_reconnect
		},
		NULL
	);

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


	static int http_test = 0;
	if (http_test) {
		return;
	}
	http_test = 1;

	DictionaryIterator *body;
	HTTPResult result = http_out_get(
		"http://charybdis.local:8000/test.json",
		APP_ID,
		&body
	);
	if (result != HTTP_OK) {
		text_layer_set_text(&text_pebminder_layer, "• http_out_get failed");
		return;
	}
	if (http_out_send() != HTTP_OK) {
		text_layer_set_text(&text_pebminder_layer, "• http_out_send failed");
		return;
	}
	text_layer_set_text(&text_pebminder_layer, "• Sent");
}

void pbl_main(void *params) {
	PebbleAppHandlers handlers = {
		.init_handler = &handle_init,
		.tick_info = {
			.tick_handler = &handle_minute_tick,
			.tick_units = MINUTE_UNIT
		},
		.messaging_info = {
			.buffer_sizes = {
				// Cargo cult from WeatherWatch
				.inbound = 124,
				.outbound = 256
			}
		}
	};

	app_event_loop(params, &handlers);
}
