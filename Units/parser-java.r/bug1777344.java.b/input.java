import java.util.*;
public class bug1777344<K, V> {
    private HashMap<K,V> map1;
    private HashMap<K,V> map2 = new HashMap<K,V>(), map3;
    private boolean b = (1 < 3), c;
    private boolean d = 1 < 3, e;
}
