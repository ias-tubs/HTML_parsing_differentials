namespace mxssy;

public static class StringExtensions
{
    public static string FixForSerialization(this string str)
    {
        return str.Replace("\\", "\\\\").Replace("\"", "\\\"");
    }
}