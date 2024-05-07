package mxss.sanitizer;

import mxss.Result;

public interface Sanitizer {

    static final String[] TAGS = {
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


    Result sanitize(String input) throws Exception;
    String getName();

    static String fix(String input) {
        return input.replace("\\", "\\\\").replace("\"", "\\\"");
    }

}
