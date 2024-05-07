using HtmlAgilityPack;

namespace mxssy;

public static class ExtensionMethods
{
        
    public static SanitizerResult SanitizeWithDom(this Vereyon.Web.HtmlSanitizer sanitizer, string payload)
    {
        payload = payload.Trim();
        if (payload.Length < 1)
            return new SanitizerResult(string.Empty, null);
        var htmlDocument = new HtmlDocument();
        htmlDocument.LoadHtml(payload);
        var serialized = htmlDocument.DocumentNode.Serialize();
        sanitizer.SanitizeNode(htmlDocument.DocumentNode);
        var clean = htmlDocument.DocumentNode.WriteTo();
        
        return new SanitizerResult(clean, serialized);
    }

    private static string Serialize(this HtmlNode node)
    {
        switch (node.NodeType)
        {
            case HtmlNodeType.Text:
                return SerializeText((HtmlTextNode)node);
            case HtmlNodeType.Comment:
                return SerializeComment((HtmlCommentNode)node);
            case HtmlNodeType.Element:
                return SerializeTag(node);
            case HtmlNodeType.Document:
                var tags = node.ChildNodes.Select(child => child.Serialize()).ToList();
                return $"(#document [{string.Join(", ", tags)}])";
            default:
                throw new NotImplementedException("can't happen");
        }
    }

    private static string SerializeTag(HtmlNode tag)
    {

        var attributes = tag.Attributes;
        var attrs = new List<string>();
        foreach (var attribute in tag.Attributes)
        {
            var attrName = attribute.Name.FixForSerialization();
            var attrValue = attribute.Value.FixForSerialization();
            attrs.Add($"(#attr \"{attrName}\" \"{attrValue}\")");
        }
        var tags = tag.ChildNodes.Select(child => child.Serialize()).ToList();
        return $"(#tag \"{tag.Name.FixForSerialization()}\" [{string.Join(", ", attrs)}] [{string.Join(", ", tags)}])";
    }
    
    private static string SerializeText(HtmlTextNode text)
    {
        var value = text.Text.FixForSerialization();
        return $"(#text \"{value}\")";
    }
    
    private static string SerializeComment(HtmlCommentNode comment)
    {
        var value = comment.Comment.FixForSerialization();
        return $"(#comment \"{value}\")";
    }
}