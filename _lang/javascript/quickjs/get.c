#include "quickjs-libc.h"
#include <stdio.h>

JSValue print_hello(JSContext *ctx, JSValueConst this_val,
        int argc, JSValueConst *argv) {
    printf("Hello from C!\n");
    return JS_UNDEFINED;
}

int main(int argc, char **argv) {
    JSRuntime *rt = JS_NewRuntime();
    JSContext *ctx = JS_NewContext(rt);

    quickjs_init_libc(ctx);

    JSValue global_obj = JS_GetGlobalObject(ctx);
    JS_SetPropertyStr(ctx, global_obj, "printHello",
            JS_NewCFunction(ctx, print_hello, "printHello", 0));
    JS_FreeValue(ctx, global_obj);

    const char *script = "printHello();";
    JSValue result = JS_Eval(ctx, script, strlen(script), "<cmd>", JS_EVAL_TYPE_GLOBAL);

    if (JS_IsException(result)) {
        js_std_dump_error(ctx);
    }
    JS_FreeValue(ctx, result);

    JS_FreeContext(ctx);
    JS_FreeRuntime(rt);
    return 0;
}

/* This example from Google AI 2025-05 */
