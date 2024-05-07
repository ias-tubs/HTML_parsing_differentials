<?php namespace sanitizer\typo3;

use sanitizer\SanitizerResult;
use TYPO3\HtmlSanitizer\Behavior;
use TYPO3\HtmlSanitizer\Sanitizer;
use TYPO3\HtmlSanitizer\Visitor\CommonVisitor;
use TYPO3\HtmlSanitizer\Builder\CommonBuilder;

require_once 'vendor/autoload.php';

class Typo3Lax {

  public string $name = 'typo3-lax';
  private Sanitizer $sanitizer;
  private PrintingVisitor $printingVisitor;

  public function __construct() {
    $this->printingVisitor = new PrintingVisitor();
    $commonAttrs = [
      new Behavior\Attr('id'),
      new Behavior\Attr('class'),
      new Behavior\Attr('title'),
      new Behavior\Attr('foo'),
      new Behavior\Attr('name'),
      new Behavior\Attr('data-', Behavior\Attr::NAME_PREFIX),
    ];
    $basic_tags = [

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
    ];
    $tags = array();
    foreach($basic_tags as $basic_tag) {
      $tags[] = (new Behavior\Tag($basic_tag, Behavior\Tag::ALLOW_CHILDREN))->addAttrs(...$commonAttrs);
    }
    $behavior = (new Behavior())
    ->withFlags(Behavior::ENCODE_INVALID_TAG)
    ->withTags(
          (new Behavior\Tag('annotation-xml', Behavior\Tag::ALLOW_CHILDREN))
            ->addAttrs(new Behavior\Attr('encoding'), ...$commonAttrs),
        ...$tags
        );

    $visitors = [$this->printingVisitor, new CommonVisitor($behavior)];
    $this->sanitizer = new Sanitizer(...$visitors);
  }

  public function sanitize($input): SanitizerResult
  {
      $clean = $this->sanitizer->sanitize($input);
      $serialized = $this->printingVisitor->get_content();
      return new SanitizerResult($clean, $serialized);
  }
}

