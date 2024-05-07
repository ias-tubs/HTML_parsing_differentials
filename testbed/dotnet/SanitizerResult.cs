namespace mxssy;

public class SanitizerResult
{
    public SanitizerResult(string clean, string? serialized)
    {
        this.Clean = clean;
        this.Serialized = serialized;
    }

    public string Clean { get; }
    public string? Serialized { get; }
}