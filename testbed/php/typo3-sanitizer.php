<?php
include 'sanitizer.php';

use sanitizer\typo3\PrintingVisitor;
use TYPO3\HtmlSanitizer\Behavior;
use TYPO3\HtmlSanitizer\Sanitizer;
use TYPO3\HtmlSanitizer\Visitor\CommonVisitor;
use TYPO3\HtmlSanitizer\Builder\CommonBuilder;

require_once 'vendor/autoload.php';

$commonAttrs = [
    new Behavior\Attr('id'),
    new Behavior\Attr('class'),
    new Behavior\Attr('title'),
    new Behavior\Attr('data-', Behavior\Attr::NAME_PREFIX),
];
$hrefAttr = (new Behavior\Attr('href'))
    ->addValues(new Behavior\RegExpAttrValue('#^https?://#'));

// attention: only `Behavior` implementation uses immutability
// (invoking `withFlags()` or `withTags()` returns new instance)
$behavior = (new Behavior())
    ->withFlags(Behavior::ENCODE_INVALID_TAG)
    ->withTags(
        (new Behavior\Tag('style', Behavior\Tag::ALLOW_CHILDREN)),
        (new Behavior\Tag('math', Behavior\Tag::ALLOW_CHILDREN)),
        (new Behavior\Tag('div', Behavior\Tag::ALLOW_CHILDREN))
            ->addAttrs(...$commonAttrs),
        (new Behavior\Tag('form', Behavior\Tag::ALLOW_CHILDREN)),
        (new Behavior\Tag('textarea', Behavior\Tag::ALLOW_CHILDREN))
            ->addAttrs(...$commonAttrs),
        (new Behavior\Tag('input', Behavior\Tag::ALLOW_CHILDREN))
            ->addAttrs(...$commonAttrs),
        (new Behavior\Tag('span', Behavior\Tag::ALLOW_CHILDREN)),
        (new Behavior\Tag('select', Behavior\Tag::ALLOW_CHILDREN)),
        (new Behavior\Tag('keygen', Behavior\Tag::ALLOW_CHILDREN)),
        (new Behavior\Tag('svg', Behavior\Tag::ALLOW_CHILDREN)),
        (new Behavior\Tag('mglyph', Behavior\Tag::ALLOW_CHILDREN)),
        (new Behavior\Tag('noscript', Behavior\Tag::ALLOW_CHILDREN)),
        #(new Behavior\Tag('a', Behavior\Tag::ALLOW_CHILDREN))
        #    ->addAttrs($hrefAttr, ...$commonAttrs),
        #(new Behavior\Tag('br')),
        #(new Behavior\Tag('p'))
    );

$pv = new PrintingVisitor();
$visitors = [$pv,new CommonVisitor($behavior)];
$sanitizer = new Sanitizer(...$visitors);
$common = new CommonBuilder();


$html = <<< EOH
<style></br><svg></br>/*<noscript><img src=x onerror=alert(1)></noscript>*/<style></svg></keygen>
EOH;
$html2 = <<< EOH
<keygen></br><svg></br>/*<noscript><img src=x onerror=mxss(1)></noscript>*/<style></svg></keygen>
EOH;
$html3 = <<< EOH
<svg title=<p><textarea></textarea><math></p></style><form>*/<style></select><img src=x onerror=mxss(1)></form></mglyph></textarea>*/</p>"><span>
EOH;
$html4 = <<< EOH
<mglyph><textarea></textarea>/*</style><svg><input data-foo=<div></br><noscript></p><mglyph><img src=x onerror=mxss(1)></mglyph></noscript></div>'><br></span></svg>"<math>*/</mglyph>
EOH;
$html5 = <<< EOH
<math><style><img src=x onerror=mxss(1)>
EOH;

$html6 = <<< EOH
<select><keygen><img src=x onerror=mxss(17)></select>
EOH;

$html7 = <<< EOH
<!--<a foo=--!>'*/<img src=x onerror=mxss(1)><math><!--<a>">
EOH;
$html8 = <<< EOH
<![CDATA[<math><img src=x onerror=mxss(1)></math>*/]]>
EOH;
$payloads = [ $html, $html2, $html3, $html4, $html5, $html6, $html7, $html8 ];
foreach($payloads as $payload) {
echo "Payload: " . $payload . "\n";
echo "Common sanitizer: \t";
  echo $common->build()->sanitize($payload);
  echo "\n";
echo "Lax sanitizer: \t";
$result = \sanitizer\Sanitizers["typo3-lax"]->sanitize($payload);
echo sprintf("%s\n%s", $result->get_clean(), $result->get_serialized());
  echo "\n";
echo "Custom sanitizer: \t";
  echo $sanitizer->sanitize($payload) . "\n";
  echo $pv->get_content() . "\n";
  $pv->reset();
  echo "\n";
}
