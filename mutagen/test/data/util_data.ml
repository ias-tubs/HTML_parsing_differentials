(** Generated by scripts/gen_util_tests.js *)
let data =
  [
    ( "<p><a href='http://example.com/' onclick='stealCookies()'>Link</a></p>",
      "%3Cp%3E%3Ca%20href='http://example.com/'%20onclick='stealCookies()'%3ELink%3C/a%3E%3C/p%3E",
      "%3Cp%3E%3Ca%20href%3D'http%3A%2F%2Fexample.com%2F'%20onclick%3D'stealCookies()'%3ELink%3C%2Fa%3E%3C%2Fp%3E" );
    ( "<select><template><style><!--</style><a rel=\"--></style></template></select><img id=a src onerror=alert(1)>\">",
      "%3Cselect%3E%3Ctemplate%3E%3Cstyle%3E%3C!--%3C/style%3E%3Ca%20rel=%22--%3E%3C/style%3E%3C/template%3E%3C/select%3E%3Cimg%20id=a%20src%20onerror=alert(1)%3E%22%3E",
      "%3Cselect%3E%3Ctemplate%3E%3Cstyle%3E%3C!--%3C%2Fstyle%3E%3Ca%20rel%3D%22--%3E%3C%2Fstyle%3E%3C%2Ftemplate%3E%3C%2Fselect%3E%3Cimg%20id%3Da%20src%20onerror%3Dalert(1)%3E%22%3E"
    );
    ( "<form><math><mtext></form><form><mglyph><style></math><img id=a src onerror=alert(1)>",
      "%3Cform%3E%3Cmath%3E%3Cmtext%3E%3C/form%3E%3Cform%3E%3Cmglyph%3E%3Cstyle%3E%3C/math%3E%3Cimg%20id=a%20src%20onerror=alert(1)%3E",
      "%3Cform%3E%3Cmath%3E%3Cmtext%3E%3C%2Fform%3E%3Cform%3E%3Cmglyph%3E%3Cstyle%3E%3C%2Fmath%3E%3Cimg%20id%3Da%20src%20onerror%3Dalert(1)%3E"
    );
    ( "<x/><title>&amp;lt;/title&amp;gt;&amp;lt;img id=a src=1 onerror=alert(1)&amp;gt;",
      "%3Cx/%3E%3Ctitle%3E&amp;lt;/title&amp;gt;&amp;lt;img%20id=a%20src=1%20onerror=alert(1)&amp;gt;",
      "%3Cx%2F%3E%3Ctitle%3E%26amp%3Blt%3B%2Ftitle%26amp%3Bgt%3B%26amp%3Blt%3Bimg%20id%3Da%20src%3D1%20onerror%3Dalert(1)%26amp%3Bgt%3B"
    );
    ( "<noscript><a title=\"</noscript><img id=a src=x onerror=alert(1)>\">",
      "%3Cnoscript%3E%3Ca%20title=%22%3C/noscript%3E%3Cimg%20id=a%20src=x%20onerror=alert(1)%3E%22%3E",
      "%3Cnoscript%3E%3Ca%20title%3D%22%3C%2Fnoscript%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dalert(1)%3E%22%3E" );
    ( "<noscript><style></noscript><img id=a src=x onerror=alert(1)>",
      "%3Cnoscript%3E%3Cstyle%3E%3C/noscript%3E%3Cimg%20id=a%20src=x%20onerror=alert(1)%3E",
      "%3Cnoscript%3E%3Cstyle%3E%3C%2Fnoscript%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dalert(1)%3E" );
    ( "<svg><style><img id=a src=x onerror=alert(1)></style></svg>",
      "%3Csvg%3E%3Cstyle%3E%3Cimg%20id=a%20src=x%20onerror=alert(1)%3E%3C/style%3E%3C/svg%3E",
      "%3Csvg%3E%3Cstyle%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dalert(1)%3E%3C%2Fstyle%3E%3C%2Fsvg%3E" );
    ( "<svg><style>/*</style><img id=a src onerror=alert(1)",
      "%3Csvg%3E%3Cstyle%3E/*%3C/style%3E%3Cimg%20id=a%20src%20onerror=alert(1)",
      "%3Csvg%3E%3Cstyle%3E%2F*%3C%2Fstyle%3E%3Cimg%20id%3Da%20src%20onerror%3Dalert(1)" );
    ( "<svg><textarea></textarea><img id=a src onerror=alert(1)",
      "%3Csvg%3E%3Ctextarea%3E%3C/textarea%3E%3Cimg%20id=a%20src%20onerror=alert(1)",
      "%3Csvg%3E%3Ctextarea%3E%3C%2Ftextarea%3E%3Cimg%20id%3Da%20src%20onerror%3Dalert(1)" );
    ( "<style><mglyph><textarea title='<!--<!--</style><img id=a src=x \
       onerror=mxss(1)></mtext><!---->-->'></mglyph></style>",
      "%3Cstyle%3E%3Cmglyph%3E%3Ctextarea%20title='%3C!--%3C!--%3C/style%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/mtext%3E%3C!----%3E--%3E'%3E%3C/mglyph%3E%3C/style%3E",
      "%3Cstyle%3E%3Cmglyph%3E%3Ctextarea%20title%3D'%3C!--%3C!--%3C%2Fstyle%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fmtext%3E%3C!----%3E--%3E'%3E%3C%2Fmglyph%3E%3C%2Fstyle%3E"
    );
    ( "<div id='--!><style><!--</form><form id='<select><img id=a src=x \
       onerror=mxss(1)></select>--!>'>--!>--!><!---->--!></style></style>'>",
      "%3Cdiv%20id='--!%3E%3Cstyle%3E%3C!--%3C/form%3E%3Cform%20id='%3Cselect%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/select%3E--!%3E'%3E--!%3E--!%3E%3C!----%3E--!%3E%3C/style%3E%3C/style%3E'%3E",
      "%3Cdiv%20id%3D'--!%3E%3Cstyle%3E%3C!--%3C%2Fform%3E%3Cform%20id%3D'%3Cselect%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fselect%3E--!%3E'%3E--!%3E--!%3E%3C!----%3E--!%3E%3C%2Fstyle%3E%3C%2Fstyle%3E'%3E"
    );
    ( "<span><mglyph><span foo='<!--<keygen><math><style title='<a><img id=a src=x \
       onerror=mxss(1)></a></noscript>'></math></keygen>-->'></mglyph><!--<style></span>--!>",
      "%3Cspan%3E%3Cmglyph%3E%3Cspan%20foo='%3C!--%3Ckeygen%3E%3Cmath%3E%3Cstyle%20title='%3Ca%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/a%3E%3C/noscript%3E'%3E%3C/math%3E%3C/keygen%3E--%3E'%3E%3C/mglyph%3E%3C!--%3Cstyle%3E%3C/span%3E--!%3E",
      "%3Cspan%3E%3Cmglyph%3E%3Cspan%20foo%3D'%3C!--%3Ckeygen%3E%3Cmath%3E%3Cstyle%20title%3D'%3Ca%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fa%3E%3C%2Fnoscript%3E'%3E%3C%2Fmath%3E%3C%2Fkeygen%3E--%3E'%3E%3C%2Fmglyph%3E%3C!--%3Cstyle%3E%3C%2Fspan%3E--!%3E"
    );
    ( "<a foo='<!--<span foo='/*<!--<select><img id=a src=x onerror=mxss(1)></select><br></mglyph>--><textarea>'>-->'>",
      "%3Ca%20foo='%3C!--%3Cspan%20foo='/*%3C!--%3Cselect%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/select%3E%3Cbr%3E%3C/mglyph%3E--%3E%3Ctextarea%3E'%3E--%3E'%3E",
      "%3Ca%20foo%3D'%3C!--%3Cspan%20foo%3D'%2F*%3C!--%3Cselect%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fselect%3E%3Cbr%3E%3C%2Fmglyph%3E--%3E%3Ctextarea%3E'%3E--%3E'%3E"
    );
    ( "<textarea></noscript><mtext></textarea><!----!><img id=a src=x \
       onerror=mxss(1)><input></select>/*--></mtext></mglyph>",
      "%3Ctextarea%3E%3C/noscript%3E%3Cmtext%3E%3C/textarea%3E%3C!----!%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3Cinput%3E%3C/select%3E/*--%3E%3C/mtext%3E%3C/mglyph%3E",
      "%3Ctextarea%3E%3C%2Fnoscript%3E%3Cmtext%3E%3C%2Ftextarea%3E%3C!----!%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3Cinput%3E%3C%2Fselect%3E%2F*--%3E%3C%2Fmtext%3E%3C%2Fmglyph%3E"
    );
    ( "</span><form>/*<svg></p>/*<form></form><img id=a src=x \
       onerror=mxss(1)><div></form></div></br>--!></svg><p></style></form>",
      "%3C/span%3E%3Cform%3E/*%3Csvg%3E%3C/p%3E/*%3Cform%3E%3C/form%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3Cdiv%3E%3C/form%3E%3C/div%3E%3C/br%3E--!%3E%3C/svg%3E%3Cp%3E%3C/style%3E%3C/form%3E",
      "%3C%2Fspan%3E%3Cform%3E%2F*%3Csvg%3E%3C%2Fp%3E%2F*%3Cform%3E%3C%2Fform%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3Cdiv%3E%3C%2Fform%3E%3C%2Fdiv%3E%3C%2Fbr%3E--!%3E%3C%2Fsvg%3E%3Cp%3E%3C%2Fstyle%3E%3C%2Fform%3E"
    );
    ( "<form foo=<a id=<!--<mglyph title=/*<style><form><img id=a src=x onerror=mxss(1)></form></style>'>-->>><div>",
      "%3Cform%20foo=%3Ca%20id=%3C!--%3Cmglyph%20title=/*%3Cstyle%3E%3Cform%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/form%3E%3C/style%3E'%3E--%3E%3E%3E%3Cdiv%3E",
      "%3Cform%20foo%3D%3Ca%20id%3D%3C!--%3Cmglyph%20title%3D%2F*%3Cstyle%3E%3Cform%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fform%3E%3C%2Fstyle%3E'%3E--%3E%3E%3E%3Cdiv%3E"
    );
    ( "</form><math><style><img id=a src=x onerror=mxss(1)><math></style><style></math><div>",
      "%3C/form%3E%3Cmath%3E%3Cstyle%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3Cmath%3E%3C/style%3E%3Cstyle%3E%3C/math%3E%3Cdiv%3E",
      "%3C%2Fform%3E%3Cmath%3E%3Cstyle%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3Cmath%3E%3C%2Fstyle%3E%3Cstyle%3E%3C%2Fmath%3E%3Cdiv%3E"
    );
    ( "<a id=/*<!--</div><img id=a src=x onerror=mxss(1)>-->><style>",
      "%3Ca%20id=/*%3C!--%3C/div%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E--%3E%3E%3Cstyle%3E",
      "%3Ca%20id%3D%2F*%3C!--%3C%2Fdiv%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E--%3E%3E%3Cstyle%3E" );
    ( "<mglyph></select><mglyph><svg><img id=a src=x onerror=mxss(1)></svg></mglyph><select></mglyph>",
      "%3Cmglyph%3E%3C/select%3E%3Cmglyph%3E%3Csvg%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/svg%3E%3C/mglyph%3E%3Cselect%3E%3C/mglyph%3E",
      "%3Cmglyph%3E%3C%2Fselect%3E%3Cmglyph%3E%3Csvg%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fsvg%3E%3C%2Fmglyph%3E%3Cselect%3E%3C%2Fmglyph%3E"
    );
    ( "<div><math>\"<br><a><a id='<!--</svg>--!><span id='<math><img id=a src=x \
       onerror=mxss(1)></math>></style>--><!--'></a></br></math></div>",
      "%3Cdiv%3E%3Cmath%3E%22%3Cbr%3E%3Ca%3E%3Ca%20id='%3C!--%3C/svg%3E--!%3E%3Cspan%20id='%3Cmath%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/math%3E%3E%3C/style%3E--%3E%3C!--'%3E%3C/a%3E%3C/br%3E%3C/math%3E%3C/div%3E",
      "%3Cdiv%3E%3Cmath%3E%22%3Cbr%3E%3Ca%3E%3Ca%20id%3D'%3C!--%3C%2Fsvg%3E--!%3E%3Cspan%20id%3D'%3Cmath%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fmath%3E%3E%3C%2Fstyle%3E--%3E%3C!--'%3E%3C%2Fa%3E%3C%2Fbr%3E%3C%2Fmath%3E%3C%2Fdiv%3E"
    );
    ( "<style id=<span><mtext><style data-foo=/*<input></a></style><mglyph><mtext \
       title=<script>mxss(1)</script>'></input>\"></mtext></span>><!--\"",
      "%3Cstyle%20id=%3Cspan%3E%3Cmtext%3E%3Cstyle%20data-foo=/*%3Cinput%3E%3C/a%3E%3C/style%3E%3Cmglyph%3E%3Cmtext%20title=%3Cscript%3Emxss(1)%3C/script%3E'%3E%3C/input%3E%22%3E%3C/mtext%3E%3C/span%3E%3E%3C!--%22",
      "%3Cstyle%20id%3D%3Cspan%3E%3Cmtext%3E%3Cstyle%20data-foo%3D%2F*%3Cinput%3E%3C%2Fa%3E%3C%2Fstyle%3E%3Cmglyph%3E%3Cmtext%20title%3D%3Cscript%3Emxss(1)%3C%2Fscript%3E'%3E%3C%2Finput%3E%22%3E%3C%2Fmtext%3E%3C%2Fspan%3E%3E%3C!--%22"
    );
    ( "<a id=<span><svg data-foo=\"<mglyph title=\"</style><style>&lt;select&gt;&lt;img id=a src=x \
       onerror=mxss(1)&gt;&lt;div&gt;&lt;/select&gt;<keygen>\"><p><!--></span></title></span>>",
      "%3Ca%20id=%3Cspan%3E%3Csvg%20data-foo=%22%3Cmglyph%20title=%22%3C/style%3E%3Cstyle%3E&lt;select&gt;&lt;img%20id=a%20src=x%20onerror=mxss(1)&gt;&lt;div&gt;&lt;/select&gt;%3Ckeygen%3E%22%3E%3Cp%3E%3C!--%3E%3C/span%3E%3C/title%3E%3C/span%3E%3E",
      "%3Ca%20id%3D%3Cspan%3E%3Csvg%20data-foo%3D%22%3Cmglyph%20title%3D%22%3C%2Fstyle%3E%3Cstyle%3E%26lt%3Bselect%26gt%3B%26lt%3Bimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%26gt%3B%26lt%3Bdiv%26gt%3B%26lt%3B%2Fselect%26gt%3B%3Ckeygen%3E%22%3E%3Cp%3E%3C!--%3E%3C%2Fspan%3E%3C%2Ftitle%3E%3C%2Fspan%3E%3E"
    );
    ( "<select><var><div foo=<!--</math><!--<iframe><select><keygen></mi><img id=a src=x \
       onerror=mxss(1)></keygen></select></plaintext></iframe>--!></td><!--</xmp>--!>></select>",
      "%3Cselect%3E%3Cvar%3E%3Cdiv%20foo=%3C!--%3C/math%3E%3C!--%3Ciframe%3E%3Cselect%3E%3Ckeygen%3E%3C/mi%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/keygen%3E%3C/select%3E%3C/plaintext%3E%3C/iframe%3E--!%3E%3C/td%3E%3C!--%3C/xmp%3E--!%3E%3E%3C/select%3E",
      "%3Cselect%3E%3Cvar%3E%3Cdiv%20foo%3D%3C!--%3C%2Fmath%3E%3C!--%3Ciframe%3E%3Cselect%3E%3Ckeygen%3E%3C%2Fmi%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fkeygen%3E%3C%2Fselect%3E%3C%2Fplaintext%3E%3C%2Fiframe%3E--!%3E%3C%2Ftd%3E%3C!--%3C%2Fxmp%3E--!%3E%3E%3C%2Fselect%3E"
    );
    ( "<noembed>--!><select><iframe><mglyph><select><img id=a src=x \
       onerror=mxss(1)></select></mglyph></iframe></select>/*</noembed>",
      "%3Cnoembed%3E--!%3E%3Cselect%3E%3Ciframe%3E%3Cmglyph%3E%3Cselect%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/select%3E%3C/mglyph%3E%3C/iframe%3E%3C/select%3E/*%3C/noembed%3E",
      "%3Cnoembed%3E--!%3E%3Cselect%3E%3Ciframe%3E%3Cmglyph%3E%3Cselect%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fselect%3E%3C%2Fmglyph%3E%3C%2Fiframe%3E%3C%2Fselect%3E%2F*%3C%2Fnoembed%3E"
    );
    ( "<svg><style id=<div><style><![CDATA[<!--<!--<style>--><img id=a src=x \
       onerror=mxss(1)></pre>--!><ul><dl>]]><mo></style></div>>",
      "%3Csvg%3E%3Cstyle%20id=%3Cdiv%3E%3Cstyle%3E%3C!%5BCDATA%5B%3C!--%3C!--%3Cstyle%3E--%3E%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/pre%3E--!%3E%3Cul%3E%3Cdl%3E%5D%5D%3E%3Cmo%3E%3C/style%3E%3C/div%3E%3E",
      "%3Csvg%3E%3Cstyle%20id%3D%3Cdiv%3E%3Cstyle%3E%3C!%5BCDATA%5B%3C!--%3C!--%3Cstyle%3E--%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fpre%3E--!%3E%3Cul%3E%3Cdl%3E%5D%5D%3E%3Cmo%3E%3C%2Fstyle%3E%3C%2Fdiv%3E%3E"
    );
    ( "<var><svg></foreignobject><style></br><![CDATA[</plaintext>/*</ms><div>\"<img id=a src=x \
       onerror=mxss(1)></div><script>]]></style></br></svg></var>",
      "%3Cvar%3E%3Csvg%3E%3C/foreignobject%3E%3Cstyle%3E%3C/br%3E%3C!%5BCDATA%5B%3C/plaintext%3E/*%3C/ms%3E%3Cdiv%3E%22%3Cimg%20id=a%20src=x%20onerror=mxss(1)%3E%3C/div%3E%3Cscript%3E%5D%5D%3E%3C/style%3E%3C/br%3E%3C/svg%3E%3C/var%3E",
      "%3Cvar%3E%3Csvg%3E%3C%2Fforeignobject%3E%3Cstyle%3E%3C%2Fbr%3E%3C!%5BCDATA%5B%3C%2Fplaintext%3E%2F*%3C%2Fms%3E%3Cdiv%3E%22%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fdiv%3E%3Cscript%3E%5D%5D%3E%3C%2Fstyle%3E%3C%2Fbr%3E%3C%2Fsvg%3E%3C%2Fvar%3E"
    );
    ( "<svg title=\"<annotation-xml foo=<annotation-xml encoding=\"application/xhtml+xml\">--><style \
       title=''</annotation-xml>/*&lt;img id=a src=x onerror=mxss(1)&gt;\"<noembed>''>--!>>\">\"",
      "%3Csvg%20title=%22%3Cannotation-xml%20foo=%3Cannotation-xml%20encoding=%22application/xhtml+xml%22%3E--%3E%3Cstyle%20title=''%3C/annotation-xml%3E/*&lt;img%20id=a%20src=x%20onerror=mxss(1)&gt;%22%3Cnoembed%3E''%3E--!%3E%3E%22%3E%22",
      "%3Csvg%20title%3D%22%3Cannotation-xml%20foo%3D%3Cannotation-xml%20encoding%3D%22application%2Fxhtml%2Bxml%22%3E--%3E%3Cstyle%20title%3D''%3C%2Fannotation-xml%3E%2F*%26lt%3Bimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%26gt%3B%22%3Cnoembed%3E''%3E--!%3E%3E%22%3E%22"
    );
    ( "<svg><style foo=<noframes>-->&lt;img id=a src=x onerror=mxss(1)&gt;--&gt;&lt;!--]]></pre></xmp><annotation-xml \
       encoding=\"application/xhtml+xml\">\">]]></svg><mglyph></dfn>",
      "%3Csvg%3E%3Cstyle%20foo=%3Cnoframes%3E--%3E&lt;img%20id=a%20src=x%20onerror=mxss(1)&gt;--&gt;&lt;!--%5D%5D%3E%3C/pre%3E%3C/xmp%3E%3Cannotation-xml%20encoding=%22application/xhtml+xml%22%3E%22%3E%5D%5D%3E%3C/svg%3E%3Cmglyph%3E%3C/dfn%3E",
      "%3Csvg%3E%3Cstyle%20foo%3D%3Cnoframes%3E--%3E%26lt%3Bimg%20id%3Da%20src%3Dx%20onerror%3Dmxss(1)%26gt%3B--%26gt%3B%26lt%3B!--%5D%5D%3E%3C%2Fpre%3E%3C%2Fxmp%3E%3Cannotation-xml%20encoding%3D%22application%2Fxhtml%2Bxml%22%3E%22%3E%5D%5D%3E%3C%2Fsvg%3E%3Cmglyph%3E%3C%2Fdfn%3E"
    );
    ( "<noscript xmlns=\"http://www.w3.org/1998/Math/MathML\"><a title=\"\"></noscript><img id=a src=x \
       onerror=alert(1)>\">",
      "%3Cnoscript%20xmlns=%22http://www.w3.org/1998/Math/MathML%22%3E%3Ca%20title=%22%22%3E%3C/noscript%3E%3Cimg%20id=a%20src=x%20onerror=alert(1)%3E%22%3E",
      "%3Cnoscript%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F1998%2FMath%2FMathML%22%3E%3Ca%20title%3D%22%22%3E%3C%2Fnoscript%3E%3Cimg%20id%3Da%20src%3Dx%20onerror%3Dalert(1)%3E%22%3E"
    );
    ( "<select><var><div foo=<!--</math><!--<iframe><select><keygen></mi><img src=x \
       onerror=mxss(1)></keygen></select></plaintext></iframe>--!></td><!--</xmp>--!>></select>",
      "%3Cselect%3E%3Cvar%3E%3Cdiv%20foo=%3C!--%3C/math%3E%3C!--%3Ciframe%3E%3Cselect%3E%3Ckeygen%3E%3C/mi%3E%3Cimg%20src=x%20onerror=mxss(1)%3E%3C/keygen%3E%3C/select%3E%3C/plaintext%3E%3C/iframe%3E--!%3E%3C/td%3E%3C!--%3C/xmp%3E--!%3E%3E%3C/select%3E",
      "%3Cselect%3E%3Cvar%3E%3Cdiv%20foo%3D%3C!--%3C%2Fmath%3E%3C!--%3Ciframe%3E%3Cselect%3E%3Ckeygen%3E%3C%2Fmi%3E%3Cimg%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fkeygen%3E%3C%2Fselect%3E%3C%2Fplaintext%3E%3C%2Fiframe%3E--!%3E%3C%2Ftd%3E%3C!--%3C%2Fxmp%3E--!%3E%3E%3C%2Fselect%3E"
    );
    ( "<desc data-foo=<br><select><mglyph><iframe>/*<style>-->--!><keygen><img src=x \
       onerror=mxss(1)><title></keygen></style></path><noembed><path></mglyph></br>>",
      "%3Cdesc%20data-foo=%3Cbr%3E%3Cselect%3E%3Cmglyph%3E%3Ciframe%3E/*%3Cstyle%3E--%3E--!%3E%3Ckeygen%3E%3Cimg%20src=x%20onerror=mxss(1)%3E%3Ctitle%3E%3C/keygen%3E%3C/style%3E%3C/path%3E%3Cnoembed%3E%3Cpath%3E%3C/mglyph%3E%3C/br%3E%3E",
      "%3Cdesc%20data-foo%3D%3Cbr%3E%3Cselect%3E%3Cmglyph%3E%3Ciframe%3E%2F*%3Cstyle%3E--%3E--!%3E%3Ckeygen%3E%3Cimg%20src%3Dx%20onerror%3Dmxss(1)%3E%3Ctitle%3E%3C%2Fkeygen%3E%3C%2Fstyle%3E%3C%2Fpath%3E%3Cnoembed%3E%3Cpath%3E%3C%2Fmglyph%3E%3C%2Fbr%3E%3E"
    );
    ( "<select>\"</plaintext></listing><iframe><ms><ul><input title=<annotation-xml \
       encoding=\"application/xhtml+xml\"><input>--><annotation-xml encoding=\"application/xhtml+xml\"><mtext><img \
       src=x onerror=mxss(1)></mtext></annotation-xml></input></annotation-xml>></ms>/*</select>",
      "%3Cselect%3E%22%3C/plaintext%3E%3C/listing%3E%3Ciframe%3E%3Cms%3E%3Cul%3E%3Cinput%20title=%3Cannotation-xml%20encoding=%22application/xhtml+xml%22%3E%3Cinput%3E--%3E%3Cannotation-xml%20encoding=%22application/xhtml+xml%22%3E%3Cmtext%3E%3Cimg%20src=x%20onerror=mxss(1)%3E%3C/mtext%3E%3C/annotation-xml%3E%3C/input%3E%3C/annotation-xml%3E%3E%3C/ms%3E/*%3C/select%3E",
      "%3Cselect%3E%22%3C%2Fplaintext%3E%3C%2Flisting%3E%3Ciframe%3E%3Cms%3E%3Cul%3E%3Cinput%20title%3D%3Cannotation-xml%20encoding%3D%22application%2Fxhtml%2Bxml%22%3E%3Cinput%3E--%3E%3Cannotation-xml%20encoding%3D%22application%2Fxhtml%2Bxml%22%3E%3Cmtext%3E%3Cimg%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Fmtext%3E%3C%2Fannotation-xml%3E%3C%2Finput%3E%3C%2Fannotation-xml%3E%3E%3C%2Fms%3E%2F*%3C%2Fselect%3E"
    );
    ( "<svg title=\"<annotation-xml foo=<annotation-xml encoding=\"application/xhtml+xml\">--><style \
       title=''</annotation-xml>/*&lt;img src=x onerror=mxss(1)&gt;\"<noembed>''>--!>>\">\"",
      "%3Csvg%20title=%22%3Cannotation-xml%20foo=%3Cannotation-xml%20encoding=%22application/xhtml+xml%22%3E--%3E%3Cstyle%20title=''%3C/annotation-xml%3E/*&lt;img%20src=x%20onerror=mxss(1)&gt;%22%3Cnoembed%3E''%3E--!%3E%3E%22%3E%22",
      "%3Csvg%20title%3D%22%3Cannotation-xml%20foo%3D%3Cannotation-xml%20encoding%3D%22application%2Fxhtml%2Bxml%22%3E--%3E%3Cstyle%20title%3D''%3C%2Fannotation-xml%3E%2F*%26lt%3Bimg%20src%3Dx%20onerror%3Dmxss(1)%26gt%3B%22%3Cnoembed%3E''%3E--!%3E%3E%22%3E%22"
    );
    ( "<select><keygen><iframe title=<table><mi><keygen>'<mglyph><img src=x \
       onerror=mxss(1)>--!></mglyph></keygen>--!></mtext><var><pre></mi></table></xmp>></keygen></select>",
      "%3Cselect%3E%3Ckeygen%3E%3Ciframe%20title=%3Ctable%3E%3Cmi%3E%3Ckeygen%3E'%3Cmglyph%3E%3Cimg%20src=x%20onerror=mxss(1)%3E--!%3E%3C/mglyph%3E%3C/keygen%3E--!%3E%3C/mtext%3E%3Cvar%3E%3Cpre%3E%3C/mi%3E%3C/table%3E%3C/xmp%3E%3E%3C/keygen%3E%3C/select%3E",
      "%3Cselect%3E%3Ckeygen%3E%3Ciframe%20title%3D%3Ctable%3E%3Cmi%3E%3Ckeygen%3E'%3Cmglyph%3E%3Cimg%20src%3Dx%20onerror%3Dmxss(1)%3E--!%3E%3C%2Fmglyph%3E%3C%2Fkeygen%3E--!%3E%3C%2Fmtext%3E%3Cvar%3E%3Cpre%3E%3C%2Fmi%3E%3C%2Ftable%3E%3C%2Fxmp%3E%3E%3C%2Fkeygen%3E%3C%2Fselect%3E"
    );
    ( "<br/><hr><br><p></br></div><div> <!-- foo -->",
      "%3Cbr/%3E%3Chr%3E%3Cbr%3E%3Cp%3E%3C/br%3E%3C/div%3E%3Cdiv%3E%20%3C!--%20foo%20--%3E",
      "%3Cbr%2F%3E%3Chr%3E%3Cbr%3E%3Cp%3E%3C%2Fbr%3E%3C%2Fdiv%3E%3Cdiv%3E%20%3C!--%20foo%20--%3E" );
    ( "<div><![CDATA[<img src=x onerror=mxss(1)]]>",
      "%3Cdiv%3E%3C!%5BCDATA%5B%3Cimg%20src=x%20onerror=mxss(1)%5D%5D%3E",
      "%3Cdiv%3E%3C!%5BCDATA%5B%3Cimg%20src%3Dx%20onerror%3Dmxss(1)%5D%5D%3E" );
    ( "<select><template><style><!--</style><a rel=\"--></style></template></select><img id=a src onerror=alert(1)>\">",
      "%3Cselect%3E%3Ctemplate%3E%3Cstyle%3E%3C!--%3C/style%3E%3Ca%20rel=%22--%3E%3C/style%3E%3C/template%3E%3C/select%3E%3Cimg%20id=a%20src%20onerror=alert(1)%3E%22%3E",
      "%3Cselect%3E%3Ctemplate%3E%3Cstyle%3E%3C!--%3C%2Fstyle%3E%3Ca%20rel%3D%22--%3E%3C%2Fstyle%3E%3C%2Ftemplate%3E%3C%2Fselect%3E%3Cimg%20id%3Da%20src%20onerror%3Dalert(1)%3E%22%3E"
    );
    ( "<style title='<ul>  <![CDATA[<!--<dfn><tr foo=<option><img src=x onerror=mxss(1)></option>></dfn>--!>]]> \
       '</ul>'></path> '",
      "%3Cstyle%20title='%3Cul%3E%20%20%3C!%5BCDATA%5B%3C!--%3Cdfn%3E%3Ctr%20foo=%3Coption%3E%3Cimg%20src=x%20onerror=mxss(1)%3E%3C/option%3E%3E%3C/dfn%3E--!%3E%5D%5D%3E%20'%3C/ul%3E'%3E%3C/path%3E%20'",
      "%3Cstyle%20title%3D'%3Cul%3E%20%20%3C!%5BCDATA%5B%3C!--%3Cdfn%3E%3Ctr%20foo%3D%3Coption%3E%3Cimg%20src%3Dx%20onerror%3Dmxss(1)%3E%3C%2Foption%3E%3E%3C%2Fdfn%3E--!%3E%5D%5D%3E%20'%3C%2Ful%3E'%3E%3C%2Fpath%3E%20'"
    );
    ( " %3Cstyle%20data-foo=\"%3Cdl%20title='%3Coption%3E%3Cdesc%3E%3C/div%3E%3Cdiv%20title=%3C/select%3E%3",
      "%20%253Cstyle%2520data-foo=%22%253Cdl%2520title='%253Coption%253E%253Cdesc%253E%253C/div%253E%253Cdiv%2520title=%253C/select%253E%253",
      "%20%253Cstyle%2520data-foo%3D%22%253Cdl%2520title%3D'%253Coption%253E%253Cdesc%253E%253C%2Fdiv%253E%253Cdiv%2520title%3D%253C%2Fselect%253E%253"
    );
    ( "]]><plaintext id='<img src=x onerror=mxss(1)>\">",
      "%5D%5D%3E%3Cplaintext%20id='%3Cimg%20src=x%20onerror=mxss(1)%3E%22%3E",
      "%5D%5D%3E%3Cplaintext%20id%3D'%3Cimg%20src%3Dx%20onerror%3Dmxss(1)%3E%22%3E" );
  ]
