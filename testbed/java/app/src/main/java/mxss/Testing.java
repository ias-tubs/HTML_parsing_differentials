package mxss;

import mxss.sanitizer.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

public class Testing {
    public static void main(String[] args) throws Exception {
        Sanitizer[] sanitizers = {
                new JsoupBasic(),
                new JsoupLax(),
                new AntiSamy("antisamy"),

                new AntiSamy("antisamy-lax"),
        };

        String[] payloads = {
//                "<svg>/*\"<img src=x onerror=mxss(1)><select>*/</svg>",
//                "<svg><svg>'<style><noscript><img src=x onerror=mxss(1)>/*</style></svg><input><select></svg><style><div>",
//                "<keygen></br><svg></br>/*<noscript><img src=x onerror=mxss(1)></noscript>*/<style></svg></keygen>",
//                "<svg>/*<p><img src=x onerror=mxss(1)></p>*/</svg>",
//                "<svg title=<p>/*<textarea><form><math></p></style><form>*/<style></select><img src=x onerror=mxss(1)></form></mglyph></textarea>*/</p>\"><span>",
//                "<mglyph>/*</style><svg><input data-foo=<div></br><noscript></p><mglyph><img src=x onerror=mxss(1)></mglyph></noscript></div>'><br></span></svg>\"<math>*/</mglyph>",
//                "<svg><div id=<div><div><![CDATA[<!--<!--<style>--><img id=a src=x onerror=mxss(1)></pre>--!><ul><dl>]]><mo></style></div>>",
//                "<!--<a> <style>><c>",
//                "<noscript><!--</noscript>><div><img src=x onerror=alert(1)>--></template></div>",
//                "<p><a href='http://example.com/' onclick='stealCookies()'>Link</a></p>",
//                "<select><template><style><!--</style><a rel=\"--></style></template></select><img id=a src onerror=alert(1)>\">",
//                "<form><math><mtext></form><form><mglyph><style></math><img id=a src onerror=alert(1)>",
//                "<x/><title>&amp;lt;/title&amp;gt;&amp;lt;img id=a src=1 onerror=alert(1)&amp;gt;",
//                "<noscript><a title=\"</noscript><img id=a src=x onerror=alert(1)>\">",
//                "<noscript><style></noscript><img id=a src=x onerror=alert(1)>",
//                "<svg><style><img id=a src=x onerror=alert(1)></style></svg>",
//                "<svg><style>/*</style><img id=a src onerror=alert(1)",
//                "<svg><textarea></textarea><img id=a src onerror=alert(1)",
//                "<style><mglyph><textarea title='<!--<!--</style><img id=a src=x onerror=mxss(1)></mtext><!---->-->'></mglyph></style>",
//                "<div id='--!><style><!--</form><form id='<select><img id=a src=x onerror=mxss(1)></select>--!>'>--!>--!><!---->--!></style></style>'>",
//                "<span><mglyph><span foo='<!--<keygen><math><style title='<a><img id=a src=x onerror=mxss(1)></a></noscript>'></math></keygen>-->'></mglyph><!--<style></span>--!>",
//                "<a foo='<!--<span foo='/*<!--<select><img id=a src=x onerror=mxss(1)></select><br></mglyph>--><textarea>'>-->'>",
//                "<textarea></noscript><mtext></textarea><!----!><img id=a src=x onerror=mxss(1)><input></select>/*--></mtext></mglyph>",
//                "</span><form>/*<svg></p>/*<form></form><img id=a src=x onerror=mxss(1)><div></form></div></br>--!></svg><p></style></form>",
//                "<form foo=<a id=<!--<mglyph title=/*<style><form><img id=a src=x onerror=mxss(1)></form></style>'>-->>><div>",
//                "</form><math><style><img id=a src=x onerror=mxss(1)><math></style><style></math><div>",
//                "<a id=/*<!--</div><img id=a src=x onerror=mxss(1)>-->><style>",
//                "<mglyph></select><mglyph><svg><img id=a src=x onerror=mxss(1)></svg></mglyph><select></mglyph>",
//                "<div><math>\"<br><a><a id='<!--</svg>--!><span id='<math><img id=a src=x onerror=mxss(1)></math>></style>--><!--'></a></br></math></div>",
//                "<style id=<span><mtext><style data-foo=/*<input></a></style><mglyph><mtext title=<script>mxss(1)</script>'></input>\"></mtext></span>><!--\"",
//                "<a id=<span><svg data-foo=\"<mglyph title=\"</style><style>&lt;select&gt;&lt;img id=a src=x onerror=mxss(1)&gt;&lt;div&gt;&lt;/select&gt;<keygen>\"><p><!--></span></title></span>>",
//                "<select><var><div foo=<!--</math><!--<iframe><select><keygen></mi><img id=a src=x onerror=mxss(1)></keygen></select></plaintext></iframe>--!></td><!--</xmp>--!>></select>",
//                "<noembed>--!><select><iframe><mglyph><select><img id=a src=x onerror=mxss(1)></select></mglyph></iframe></select>/*</noembed>",
//                "<svg><style id=<div><style><![CDATA[<!--<!--<style>--><img id=a src=x onerror=mxss(1)></pre>--!><ul><dl>]]><mo></style></div>>",
//                "<var><svg></foreignobject><style></br><![CDATA[</plaintext>/*</ms><div>\"<img id=a src=x onerror=mxss(1)></div><script>]]></style></br></svg></var>",
                  "<svg><style><div id=\"</style><div id='\"><img src=x onerror=mxss(1)>'>",
                "<!--<div/>--!><img src=x onerror=mxss(1)> <li>--></p><input/>"

        };
        for(String payload : payloads) {
            System.out.printf("Working on '%s'%n", payload);
            for(Sanitizer s : sanitizers) {
                Result r =  s.sanitize(payload);
                String cleaned = r.getCleaned();
                System.out.printf("%s: '%s'%n", s.getName(), cleaned);
                System.out.printf("\t%s%n", r.getSerialized());
            }
        }
    }
}
