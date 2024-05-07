package mxss.sanitizer;

import mxss.Result;
import org.jsoup.Jsoup;
import org.jsoup.nodes.*;
import org.jsoup.safety.Cleaner;
import org.jsoup.safety.Safelist;

import java.util.ArrayList;
import java.util.List;

import static mxss.sanitizer.Sanitizer.fix;

public abstract class JsoupSanitizer implements Sanitizer {

    public Result sanitize(String input, Safelist safelist)
    {
        String baseUri = "";
        Document dirty = Jsoup.parseBodyFragment(input, baseUri);
        String serialized = JsoupSanitizer.serialize(dirty);
        //System.out.println("{|" + serialized + "|};");

        Cleaner cleaner = new Cleaner(safelist);
        Document clean = cleaner.clean(dirty);
        return new Result(clean.body().html(), serialized);
    }

    private static String serializeComment(Comment comment) {
        return String.format("(#comment \"%s\")", fix(comment.getData()));
    }
    private static String serializeCData(CDataNode cdata) {
        return String.format("(#cdata \"%s\")", fix(cdata.text()));
    }
    private static String serializeText(TextNode text) {
        return String.format("(#text \"%s\")", fix(text.text()));
    }
    private static String serializeData(DataNode data) {
        return String.format("(#text \"%s\")", fix(data.getWholeData()));
    }

    private static String serializeAttribute(Attribute attribute) {
        return String.format("(#attr \"%s\" \"%s\")", fix(attribute.getKey()), fix(attribute.getValue()));
    }
    private static String serializeTag(Element tag) {
        List<String> attributes = new ArrayList<>(tag.attributesSize());
        for(Attribute attribute : tag.attributes()) {
            attributes.add(serializeAttribute(attribute));
        }
        List<String> children = new ArrayList<>(tag.childrenSize());

        for(Node child : tag.childNodes()) {
            children.add(serializeNode(child));
        }
        String childrenOut = String.join(", ", children);
        String attributesOut = String.join(", ", attributes);

        return String.format("(#tag \"%s\" [%s] [%s])", fix(tag.tagName().toLowerCase()), attributesOut, childrenOut);
    }

    private static String serializeNode(Node node) {
            if(node instanceof Comment) {
                return serializeComment((Comment) node);
            } else if(node instanceof CDataNode) {
                return serializeCData((CDataNode) node);
            } else if(node instanceof  TextNode) {
                return serializeText((TextNode) node);
            } else if(node instanceof  DataNode) {
                return serializeData((DataNode) node);
            } else if(node instanceof Element) {
                return serializeTag((Element) node);
            }
            throw new IllegalStateException(String.format("Node: %s is invalid!!!", node));
    }
    private static String serialize(Document doc) {
        Element body = doc.body();
        return serializeNode(body);
    }
}
