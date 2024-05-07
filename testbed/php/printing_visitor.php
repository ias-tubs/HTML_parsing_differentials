<?php namespace sanitizer\typo3;
use TYPO3\HtmlSanitizer\Visitor\AbstractVisitor;
use TYPO3\HtmlSanitizer\Context;
require_once 'vendor/autoload.php';

class PrintingVisitor extends AbstractVisitor
{
    private int $done;
    private string $content;
    public function beforeTraverse(Context $context): void
    {
        $this->done = 0;
        $this->content = '';
    }

    private function fix(string $content): string {
        return str_replace(array("\\", '"'), array("\\\\", "\\\""), $content);
    }
    private function formatText(\DOMNode $text): string {
        $content = $text->nodeValue;
        $content = $this->fix($content);
        return sprintf('(#text "%s")', $content);
    }

    private function formatComment(\DOMNode $text): string {
        $content = $text->nodeValue;
        $content = $this->fix($content);
        return sprintf('(#comment "%s")', $content);
    }
    private function formatCDATA(\DOMNode $text): string {
        $content = $text->nodeValue;
        $content = $this->fix($content);
        return sprintf('(#cdata "%s")', $content);
    }

    private function formatElement(\DOMNode $elem): string {
        $tagName = $this->fix($elem->nodeName);

        $attrs = array();
        foreach($elem->attributes as $attribute) {

            $attrs[] = sprintf('(#attr "%s" "%s")', $this->fix($attribute->nodeName), $this->fix($attribute->nodeValue));
        }
        $attribute_values = implode(', ', $attrs);

        $children = array();
        foreach($elem->childNodes as $child_node) {
            $children[] = $this->formatNode($child_node);
        }
        $children_values = implode(', ', $children);
        return sprintf('(#tag "%s" [%s] [%s])', $tagName, $attribute_values, $children_values);
    }
    private function formatNode(\DOMNode $node): string {
        return match ($node->nodeType) {
            XML_ELEMENT_NODE => $this->formatElement($node),
            XML_TEXT_NODE => $this->formatText($node),
            XML_COMMENT_NODE => $this->formatComment($node),
            XML_CDATA_SECTION_NODE => $this->formatCDATA($node),
            default => '',
        };
    }
    private function dump(\DOMNode $node): string
    {
        assert($node->nodeName === '#document-fragment', 'has to be fragment');
        $children = array();
        foreach ($node->childNodes as $childNode) {
            $children[] = $this->formatNode($childNode);
        }
        return sprintf('(#document-fragment [%s])', implode(', ', $children));
    }
    public function enterNode(\DOMNode $node): ?\DOMNode
    {
        if($this->done == 1) {
            return $node;
        }
        $top = $node;
        while($top->parentNode !== NULL) {
            $top = $top->parentNode;
        }
        $this->content = $this->dump($top);
        $this->done = 1;
        return $node;
    }
    public function get_content():string {
        return $this->content;
    }

    public function reset(): void {
        $this->done = 0;
        $this->content = '';
    }
}
