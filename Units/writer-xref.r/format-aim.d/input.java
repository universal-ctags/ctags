public class input extends Object
{
    private static String getTarget ()
    {
	return "world";
    }

    abstract protected static int life ();

    public static void main(String[] args)
    {
        System.out.println("Hello, "+input.getTarget()+".");
    }
}
