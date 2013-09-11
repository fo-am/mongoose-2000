#include <jni.h>
#include <sys/time.h>
#include <time.h>
#include <android/log.h>
#include <stdint.h>
#include <stdio.h>
#include "scheme/scheme.h"

int app_alive = 1;
scheme *sc=NULL;
FILE *log_file=NULL;

static long _getTime(void)
{
    struct timeval  now;
    gettimeofday(&now, NULL);
    return (long)(now.tv_sec*1000 + now.tv_usec/1000);
}

void Java_foam_mongoose_Scheme_nativeInit(JNIEnv* env)
{
    app_alive = 1;
    sc=scheme_init_new();
    FILE *log_file=fopen("/sdcard/mongoose/starwisp-log.txt","w");
    if (log_file!=NULL) scheme_set_output_port_file(sc, log_file);
}

void Java_foam_mongoose_Scheme_nativeDone(JNIEnv* env)
{
    app_alive = 0;
    fclose(log_file);
}

jstring Java_foam_mongoose_Scheme_nativeEval(JNIEnv* env, jobject thiz, jstring code)
{
   const char *native_code = (*env)->GetStringUTFChars(env, code, 0);
   scheme_load_string(sc,native_code);
   fflush(log_file);
   (*env)->ReleaseStringUTFChars(env, code, native_code);
   if (starwisp_data!=NULL) return (*env)->NewStringUTF(env,starwisp_data);
   return (*env)->NewStringUTF(env,"");
}
