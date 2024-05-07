using AngleSharp;
using AngleSharp.Css.Parser;
using AngleSharp.Dom;
using AngleSharp.Html.Dom;
using AngleSharp.Html.Parser;
using Ganss.Xss;

namespace mxssy;

public static class GanssExtensionMethods
{
    private static readonly IConfiguration defaultConfiguration = Configuration.Default.WithCss(new CssParserOptions
    {
        IsIncludingUnknownDeclarations = true,
        IsIncludingUnknownRules = true,
        IsToleratingInvalidSelectors = true,
    });
    public static SanitizerResult SanitizeWithDom(this HtmlSanitizer sanitizer, string payload)
    {
        payload = payload.Trim();
        if (payload.Length < 1)
            return new SanitizerResult(string.Empty, string.Empty);
        var parser = sanitizer.HtmlParserFactory();
        var dom = parser.ParseDocument("<!doctype html><html><body>" + payload);
        var serialized = dom.Body.Serialize();
        sanitizer.SanitizeDom(dom, dom.Body, "https://ias-lab.de");
        var clean = dom.Body.ChildNodes.ToHtml(sanitizer.OutputFormatter);
        
        return new SanitizerResult(clean, serialized);
    }
    
    public static SanitizerResult SanitizeWithDomFix(this HtmlSanitizer sanitizer, string payload)
    {
        payload = payload.Trim();
        if (payload.Length < 1)
            return new SanitizerResult(string.Empty, string.Empty);
        var parser = new HtmlParser(new HtmlParserOptions { IsScripting = true }, BrowsingContext.New(defaultConfiguration));
        var dom = parser.ParseDocument("<!doctype html><html><body>" + payload);
        var serialized = dom.Body.Serialize();
        sanitizer.SanitizeDom(dom, dom.Body, "https://ias-lab.de");
        var clean = dom.Body.ChildNodes.ToHtml(sanitizer.OutputFormatter);
        
        return new SanitizerResult(clean, serialized);
    }

    private static string Serialize(this INode node)
    {
        switch (node.NodeType)
        {
            case NodeType.Text:
                return SerializeText((ICharacterData)node);
            case NodeType.Comment:
                return SerializeComment((IComment)node);
            case NodeType.Element:
                return SerializeTag((IElement)node);
            case NodeType.Document:
                var tags = node.ChildNodes.Select(child => child.Serialize()).ToList();

                return $"(#document [{string.Join(", ", tags)}])";
            default:
                throw new NotImplementedException("can't happen");
        }
    }

    private static string SerializeFragment(IDocumentFragment fragment)
    {
        var tags = fragment.ChildNodes.Select(child => child.Serialize()).ToList();
        return $"(#document-fragment [{string.Join(", ", tags)}])";
    }

    private static string SerializeTag(IElement tag)
    {

        var attrs = new List<string>();
        foreach (var attribute in tag.Attributes)
        {
            var attrName = attribute.Name.FixForSerialization();
            var attrValue = attribute.Value.FixForSerialization();
            attrs.Add($"(#attr \"{attrName}\" \"{attrValue}\")");
        }
        if (tag.NodeName == "TEMPLATE")
        {
            return $"(#tag \"{tag.NodeName.ToLower().FixForSerialization()}\" [{string.Join(", ", attrs)}] [{SerializeFragment(((IHtmlTemplateElement) tag).Content)})])";
        }
        else
        {
            var tags = tag.ChildNodes.Select(child => child.Serialize()).ToList();
            return $"(#tag \"{tag.NodeName.ToLower().FixForSerialization()}\" [{string.Join(", ", attrs)}] [{string.Join(", ", tags)}])";
        }
    }
    
    private static string SerializeText(ICharacterData text)
    {
        var value = text.NodeValue.FixForSerialization();
        return $"(#text \"{value}\")";
    }
    
    private static string SerializeComment(IComment comment)
    {
        var value = comment.NodeValue.FixForSerialization();
        return $"(#comment \"{value}\")";
    }
}
