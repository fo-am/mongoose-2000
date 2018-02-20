#include <jni.h>
#include <sys/time.h>
#include <time.h>
#include <android/log.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "app.h"
#include "scheme/scheme.h"

void Java_foam_starwisp_Scheme_nativeInit(JNIEnv* env)
{
    appInit();
}

void Java_foam_starwisp_Scheme_nativeDone(JNIEnv* env)
{
    appDeinit();
}

jstring Java_foam_starwisp_Scheme_nativeEval(JNIEnv* env, jobject thiz, jstring code)
{
   const char *native_code = (*env)->GetStringUTFChars(env, code, 0);
   appEval(native_code);
   (*env)->ReleaseStringUTFChars(env, code, native_code);
   if (starwisp_data!=NULL) {
       jstring ret = (*env)->NewStringUTF(env,starwisp_data);
       free(starwisp_data);
       starwisp_data=NULL;
       return ret;
   }
   return (*env)->NewStringUTF(env,"");
}

