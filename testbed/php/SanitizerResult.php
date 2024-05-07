<?php

namespace sanitizer;

class SanitizerResult
{
    public function __construct(protected string $clean, protected ?string $serialized) {
    }

    public function get_clean(): string {
        return $this->clean;
    }

    public function get_serialized(): ?string {
        return $this->serialized;
    }
}