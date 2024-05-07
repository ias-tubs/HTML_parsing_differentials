using Vereyon.Web;

namespace mxssy;

public class HtmlRule : ISanitizer
{
    private HtmlSanitizer _sanitizer;
    public HtmlRule()
    {
        this.name = "html-rule";
        this._sanitizer = HtmlSanitizer.SimpleHtml5Sanitizer();
        this._sanitizer.RemoveComments = false;
        this._sanitizer.EncodeHtmlEntities = false;
    }
    public string name { get; }
    public SanitizerResult sanitize(string payload)
    {
        var result = this._sanitizer.SanitizeWithDom(payload);
        return result;
    }

    public static void HtmlRuleTest()
    {
        var payloads = new[]
        {
            "<a><br><div id=a foo='b'></div><hr><img src=x>",
            "<!-- abc --!> <img src=x onerror=mxss(1)>",
            "<a id='<math id='<math id=<!-- <title><noscript>--!><ul>/*<img src=x onerror=mxss(1)></ul></header></p></form></noscript></ul><div>-->>'>'>",
            "<select><template><style><!--</style><a rel=\"--></style></template></select><img id=a src onerror=alert(1)>\">",
        };
        var sanitizer = HtmlSanitizer.SimpleHtml5Sanitizer();
        foreach (var payload in payloads)
        {
            Console.Out.WriteLine($"Payload: {payload}:");
            var result = sanitizer.SanitizeWithDom(payload);
            Console.Out.WriteLine($"{result.Clean} -> {result.Serialized}");
        }
    }

}