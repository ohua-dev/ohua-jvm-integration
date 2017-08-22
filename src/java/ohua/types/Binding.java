package ohua.types;


public final class Binding {
    public final String value;

    private Binding(String value) {
        this.value = value;
    }

    public static Binding mk(String value) {
        if (value.contains("/") || value.contains(" ")) // not comprehensive yet!
            return null;
        else
            return new Binding(value);
    }
}
