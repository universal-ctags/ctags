import javax.annotation.Nonnull;

public class bug2049723 {
  public void m1(String arg) {}
  public void m2(@Nonnull String arg) {}
}
