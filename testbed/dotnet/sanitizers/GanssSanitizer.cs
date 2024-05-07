using Ganss.Xss;

namespace mxssy;

public class GanssSanitizer : ISanitizer
{
    private HtmlSanitizer _sanitizer;
    public GanssSanitizer()
    {
        this.name = "ganss";
        this._sanitizer = new HtmlSanitizer();
        this._sanitizer.KeepChildNodes = true;
        this._sanitizer.AllowDataAttributes = true;
    }

    public string name { get; }
    public SanitizerResult sanitize(string payload)
    {
        return this._sanitizer.SanitizeWithDom(payload);
    }
    
    public static void GanssTest()
    {
        var payloads = new[]
        {
            "<a id='<math id='<math id=<!-- <title><noscript>--!><ul>/*<img src=x onerror=mxss(1)></ul></header></p></form></noscript></ul><div>-->>'>'>",
            "<a><br><div id=a foo='b'></div><hr><img src=x>",
            "<!-- abc --> <!-- <img src=x onerror=mxss(1)>",
            "<select><template><style><!--</style><a rel=\"--></style></template></select><img id=a src onerror=alert(1)>\">",
            "<IMG \"\"\"><img src=x onerror=alert(\"XSS\")></SCRIPT>\">",
            "<noscript><img src=x onerror=mxss(1)>"
        };
        var sanitizer = new HtmlSanitizer();
        sanitizer.KeepChildNodes = true;
        //sanitizer.AllowedTags.UnionWith(new []{"style", "svg", "template", "math", "title", "noscript"});
        foreach (var payload in payloads)
        {
            Console.Out.WriteLine($"Payload: {payload}:");
    
            var result = sanitizer.SanitizeWithDom(payload);
            Console.Out.WriteLine($"{result.Clean} -> {result.Serialized}");
        }
    }
}