LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
#include ${ANDROID_NDK_ROOT}\sources\cxx-stl\stlport\stlport

LOCAL_MODULE := starwisp-core

LOCAL_CFLAGS := -DANDROID_NDK -Wswitch -fpermissive -DDISABLE_IMPORTGL -O3 -Wno-write-strings -DNDEBUG
APP_OPTIM := release

LOCAL_SRC_FILES := \
	core/list.cpp \
	core/db.cpp \
	core/db_container.cpp \
	core/fixed.cpp \
	core/idmap.cpp \
	sqlite/sqlite3.c \
	scheme/scheme.cpp \
	app.cpp \
	app-android.c

LOCAL_LDLIBS := -lGLESv1_CM -lOpenSLES -ldl -llog

include $(BUILD_SHARED_LIBRARY)
