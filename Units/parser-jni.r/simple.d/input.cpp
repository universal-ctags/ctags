// Taken from base/libs/hwui/jni/android_graphics_DisplayListCanvas.cpp
// in https://android.googlesource.com/platform/frameworks/base
/*
 * Copyright (C) 2010 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

static JNINativeMethod gMethods[] = {
        {"nGetMaximumTextureWidth", "()I", (void*)android_view_DisplayListCanvas_getMaxTextureSize},
        {"nGetMaximumTextureHeight", "()I",
         (void*)android_view_DisplayListCanvas_getMaxTextureSize},
        // ------------ @CriticalNative --------------
        {"nCreateDisplayListCanvas", "(JII)J",
         (void*)android_view_DisplayListCanvas_createDisplayListCanvas},
        {"nResetDisplayListCanvas", "(JJII)V",
         (void*)android_view_DisplayListCanvas_resetDisplayListCanvas},
        {"nEnableZ", "(JZ)V", (void*)android_view_DisplayListCanvas_enableZ},
        {"nFinishRecording", "(JJ)V", (void*)android_view_DisplayListCanvas_finishRecording},
        {"nDrawRenderNode", "(JJ)V", (void*)android_view_DisplayListCanvas_drawRenderNode},
        {"nDrawTextureLayer", "(JJ)V", (void*)android_view_DisplayListCanvas_drawTextureLayer},
        {"nDrawCircle", "(JJJJJ)V", (void*)android_view_DisplayListCanvas_drawCircleProps},
        {"nDrawRoundRect", "(JJJJJJJJ)V", (void*)android_view_DisplayListCanvas_drawRoundRectProps},
        {"nDrawWebViewFunctor", "(JI)V", (void*)android_view_DisplayListCanvas_drawWebViewFunctor},
        {"nDrawRipple", "(JJJJJJJIJ)V", (void*)android_view_DisplayListCanvas_drawRippleProps},
};


static Lanaguage languages [] = {
  {"C", 1},
  {"D", 1},
  {"Lisp", 0},
};
