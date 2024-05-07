using Ganss.Xss;

namespace mxssy;

public class GanssSanitizerLax : ISanitizer
{
    private HtmlSanitizer _sanitizer;
    public GanssSanitizerLax()
    {
        this.name = "ganss-lax";
        this._sanitizer = new HtmlSanitizer();
        this._sanitizer.KeepChildNodes = true;
        this._sanitizer.AllowDataAttributes = true;
        this._sanitizer.AllowedTags.UnionWith(ISanitizer.LaxTags);
        this._sanitizer.AllowedAttributes.UnionWith(ISanitizer.Attributes);

    }

    public string name { get; }
    public SanitizerResult sanitize(string payload)
    {
        return this._sanitizer.SanitizeWithDom(payload);
    }
    
    public static void GanssFixLaxTest()
    {
        var payloads = new[]
        {
            "<a id='<math id='<math id=<!-- <title><noscript>--!><ul>/*<img src=x onerror=mxss(1)></ul></header></p></form></noscript></ul><div>-->>'>'>",
            "<a><br><div id=a foo='b'></div><hr><img src=x>",
            "<!-- abc --> <!-- <img src=x onerror=mxss(1)>",
            "<select><template><style><!--</style><a rel=\"--></style></template></select><img id=a src onerror=alert(1)>\">",
            "<IMG \"\"\"><img src=x onerror=alert(\"XSS\")></SCRIPT>\">",
            "<noembed><img src=x onerror=mxss(1)>",
            "<script><img src=x onerror=mxss(1)>",
            "<xmp><img src=x onerror=mxss(1)>",
            "<noframes><img src=x onerror=mxss(1)>",
            "<iframe><img src=x onerror=mxss(1)>",
            "<keygen>&lt;/keygen&gt;<img src=x onerror=mxss(1)>",
            "<noscript>&lt;/noscript&gt&lt;img src=x onerror=mxss(1)&gt;",
            "<iframe><div foo=\"</iframe><img src=x onerror=alert(1)>",
            "<script><img src=x onerror=alert(1)></script>",
            "/*<noscript>--!><![CDATA[<td><script><!--<form></noscript><img src=x onerror=mxss(1)>--!></font></style></form>--!></script><span><noframes></td>]]>*/"
        };
        var sanitizer = new HtmlSanitizer();
        sanitizer.KeepChildNodes = true;
        sanitizer.AllowDataAttributes = true;
        sanitizer.AllowedTags.UnionWith(ISanitizer.LaxTags);
        sanitizer.AllowedAttributes.UnionWith(ISanitizer.Attributes);
        foreach (var payload in payloads)
        {
            Console.Out.WriteLine($"Payload: {payload}:");
    
            var result = sanitizer.SanitizeWithDom(payload);
            Console.Out.WriteLine($"{result.Clean} -> {result.Serialized}");
        }
    }
}