package mxss;

public class CodeFragment {
    private final long id;
    private final long tsid;
    private final String payload;

    public CodeFragment(long id, long tsid, String payload) {
        this.id = id;
        this.tsid = tsid;
        this.payload = payload;
    }

    @Override
    public String toString() {
        return "CodeFragment{" +
                "id=" + id +
                ", tsid=" + tsid +
                ", payload='" + payload + '\'' +
                '}';
    }

    public String getPayload() {
        return payload;
    }

    public long getTsId() {
        return tsid;
    }

    public long getId() {
        return id;
    }
}
