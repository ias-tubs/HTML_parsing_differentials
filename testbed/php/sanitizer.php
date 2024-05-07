<?php namespace sanitizer;

include 'typo3_sanitizer.php';
include 'typo3_sanitizer_lax.php';

const Sanitizers = array(
  'typo3' => new \sanitizer\typo3\Typo3(),
  'typo3-lax' => new \sanitizer\typo3\Typo3Lax()
);

