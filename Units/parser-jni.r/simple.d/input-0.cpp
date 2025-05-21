static void foo()
{
  static JNINativeMethod LocalMethods[] = {{"aLocalMethod", "()I", (void*)localMethod},};
}
// The line: and the end: of LocalMethods are the same.
