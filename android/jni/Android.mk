LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := starwisp-core

LOCAL_CFLAGS := -DANDROID_NDK -O3 --fast-math -Wno-write-strings

LOCAL_SRC_FILES := \
	scheme/scheme.cpp \
    app-android.c

LOCAL_LDLIBS := -lGLESv1_CM -ldl -llog

include $(BUILD_SHARED_LIBRARY)
