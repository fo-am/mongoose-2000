LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := starwisp-core

LOCAL_CFLAGS := -DANDROID_NDK -O3 -Wno-write-strings -DNDEBUG
APP_OPTIM := release

LOCAL_SRC_FILES := \
	core/list.cpp \
	core/db.cpp \
	core/db_container.cpp \
	sqlite/sqlite3.c \
	scheme/scheme.cpp \
	core/idmap.cpp \
    app-android.c

LOCAL_LDLIBS := -lGLESv1_CM -ldl -llog

include $(BUILD_SHARED_LIBRARY)
