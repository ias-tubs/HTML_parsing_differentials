package mxss.sanitizer;

import mxss.Result;
import mxss.Testing;
import org.htmlunit.cyberneko.parsers.DOMFragmentParser;
import org.htmlunit.cyberneko.xerces.dom.DocumentImpl;
import org.owasp.validator.html.Policy;
import org.owasp.validator.html.PolicyException;
import org.owasp.validator.html.ScanException;
import org.w3c.dom.*;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;

import java.io.IOException;
import java.io.StringReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AntiSamy implements Sanitizer {
    private final String name;
    private Matcher invalidXmlCharsMatcher = Pattern.compile("[\\u0000-\\u001F\\uD800-\\uDFFF\\uFFFE-\\uFFFF&&[^\\u0009\\u000A\\u000D]]").matcher("");
    public AntiSamy(String config) {
        this.name = config;
    }
    @Override
    public Result sanitize(String input) throws Exception {
        Policy policy = Policy.getInstance(Testing.class.getClassLoader().getResourceAsStream(this.name + ".xml"));
        String serialized = serialize(input);
        //System.out.println("{|" + serialized + "|};");
        org.owasp.validator.html.AntiSamy as = new org.owasp.validator.html.AntiSamy();
        return new Result(as.scan(input, policy).getCleanHTML(), serialized);
    }

    private String serialize(String html) throws SAXException, IOException {
        DOMFragmentParser parser = new DOMFragmentParser();
        Document document = new DocumentImpl();
        DocumentFragment dom = document.createDocumentFragment();
        parser.setProperty("http://cyberneko.org/html/properties/names/elems", "lower");

        parser.setFeature("http://cyberneko.org/html/features/scanner/style/strip-cdata-delims", false);
        parser.setFeature("http://cyberneko.org/html/features/scanner/cdata-sections", true);
        parser.parse(new InputSource(new StringReader(this.stripNonValidXMLCharacters(html))), dom);

        return serialize(dom);
    }
    private static String serialize(Node node) {
        switch (node.getNodeType()) {
            case Node.DOCUMENT_FRAGMENT_NODE:
                return serialize((DocumentFragment) node);
            case Node.DOCUMENT_NODE:
                return serialize((Document) node);
            case Node.TEXT_NODE:
                return serialize((Text) node);
            case Node.ELEMENT_NODE:
                return serialize((Element) node);
            case Node.COMMENT_NODE:
                return serialize((Comment) node);
            case Node.CDATA_SECTION_NODE:
                return serialize((CDATASection) node);
            default:
                throw new IllegalArgumentException(String.format("NodeType %d unhandled!%n", node.getNodeType()));
        }
    }
    private static String serialize(Text text) {
        return String.format("(#text \"%s\")", Sanitizer.fix(text.getNodeValue()));
    }
    private static String serialize(Comment comment) {
        return String.format("(#comment \"%s\")", Sanitizer.fix(comment.getNodeValue()));
    }
    private static String serialize(CDATASection cdataSection) {
        return String.format("(#cdata \"%s\")", Sanitizer.fix(cdataSection.getNodeValue()));
    }
    private static String serialize(Attr attribute) {
        return String.format("(#attr \"%s\" \"%s\")", Sanitizer.fix(attribute.getName()), Sanitizer.fix(attribute.getValue()));
    }
    private static String serialize(Element tag) {
        String tagName = Sanitizer.fix(tag.getTagName());
        NamedNodeMap attributes = tag.getAttributes();
        String[] attrs = new String[attributes.getLength()];
        for(int i = 0; i < attributes.getLength(); i++) {
            Node a = attributes.item(i);
            Attr attribute = (Attr) a;
            attrs[i] = serialize(attribute);
        }
        NodeList children = tag.getChildNodes();
        String[] cs = new String[children.getLength()];
        for(int i = 0; i < children.getLength(); i++) {
            Node node = children.item(i);
            cs[i] = serialize(node);
        }
        return String.format("(#tag \"%s\" [%s] [%s])", tagName, String.join(", ", attrs), String.join(", ", cs));
    }

    private static String serialize(DocumentFragment df) {
        NodeList children = df.getChildNodes();
        String[] cs = new String[children.getLength()];
        for(int i = 0; i < children.getLength(); i++) {
            Node node = children.item(i);
            cs[i] = serialize(node);
        }
        return String.format("(#document-fragment [%s])", String.join(", ", cs));
    }

    private static String serialize(Document document) {
        NodeList children = document.getChildNodes();
        String[] cs = new String[children.getLength()];
        for(int i = 0; i < children.getLength(); i++) {
            Node node = children.item(i);
            cs[i] = serialize(node);
        }
        return String.format("(#document [%s])", String.join(", ", cs));
    }

    private String stripNonValidXMLCharacters(String in) {

        if (in == null || (in.isEmpty())) {
            return ""; // vacancy test.
        }
        this.invalidXmlCharsMatcher.reset(in);
        return this.invalidXmlCharsMatcher.matches() ? this.invalidXmlCharsMatcher.replaceAll("") : in;
    }

    @Override
    public String getName() {
        return this.name;
    }
}
