public class C {
 @NonNull
 @CheckReturnValue(explanation = "When this function returns, the caller can get the Cipher name selected by the user in the Dialog.")
 public static DefaultCipherDialog newInstance() {
  DefaultCipherDialog instance = new DefaultCipherDialog();
  instance.init();
  return instance;
 }

 @Override
 public String toString() { return "hello"; }
}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.SOURCE)
public @interface CustomAnnotation {
}
