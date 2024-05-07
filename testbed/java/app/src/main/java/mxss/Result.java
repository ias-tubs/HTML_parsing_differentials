package mxss;

public class Result {
    private final String cleaned;
    private final String serialized;

    public Result(String cleaned, String serialized) {
        this.cleaned = cleaned;
        this.serialized = serialized;
    }

    public String getCleaned() {
        return this.cleaned;
    }

    public String getSerialized() {
        return this.serialized;
    }
}
