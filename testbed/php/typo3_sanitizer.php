<?php namespace sanitizer\typo3;

use sanitizer\SanitizerResult;
use TYPO3\HtmlSanitizer\Behavior;
use TYPO3\HtmlSanitizer\Sanitizer;
use TYPO3\HtmlSanitizer\Visitor\CommonVisitor;
use TYPO3\HtmlSanitizer\Builder\CommonBuilder;
include 'printing_visitor.php';


require_once 'vendor/autoload.php';
class Builder extends CommonBuilder {

    public function behavior(): Behavior
    {
        return $this->createBehavior();
    }
}
class Typo3 {

  public string $name = 'typo3';
  private Sanitizer $sanitizer;
  private PrintingVisitor $printingVisitor;
  public function __construct() {
    $this->printingVisitor = new PrintingVisitor();
    $behavior = (new Builder())->behavior();
    $visitors = [$this->printingVisitor,new CommonVisitor($behavior)];
    $this->sanitizer = new Sanitizer(...$visitors);
  }

  public function sanitize($input): SanitizerResult
  {
    $clean = $this->sanitizer->sanitize($input);
    $serialized = $this->printingVisitor->get_content();
    return new SanitizerResult($clean, $serialized);
  }
}

