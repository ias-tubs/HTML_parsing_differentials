namespace mxssy;

public interface ISanitizer
{
    public string name { get;  }
    public SanitizerResult sanitize(string payload);

    public static string[] Attributes = new[]
    {
        "id",
        "name",
        "title",
        "encoding",
        "foo",
        "data-foo"
    };
    
    public static string[] LaxTags = new[]
    {
            "div",
            "span",
            "title",
            "form",
            "dfn",
            "header",
            "p",
            "br",
            "a",
            "style",
            "table",
            "td",
            "tr",
            "colgroup",
            "svg",
            "foreignobject",
            "desc",
            "path",
            "math",
            "mtext",
            "mglyph",
            "mi",
            "mo",
            "mn",
            "ms",
            "annotation-xml",
            "noscript",
            "select",
            "input",
            "textarea",
            "keygen",
            "xmp",
            "noembed",
            "listing",
            "li",
            "ul",
            "pre",
            "var",
            "dl",
            "dt",
            "iframe",
            "noframes",
            "frameset",
            "plaintext",
            "font",
            "option",
            "object",
            "embed",
    };
}
