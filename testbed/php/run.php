<?php
include 'db.php';
include 'sanitizer.php';
#set_error_handler(function($errno, $errstr, $errfile, $errline) {
#    // error was suppressed with the @-operator
#    if (0 === error_reporting()) {
#        return false;
#    }
#
#    throw new ErrorException($errstr, 0, $errno, $errfile, $errline);
#});
function main(): void
{
  $short_options = 's:';
  $long_options = ['sanitizer:'];
  $options = getopt($short_options, $long_options);

  if(isset($options["s"]) || isset($options["sanitizer"])) {
    $sanitizer_name = $options['s'] ?? $options['sanitizer'];
  } else {
    $sanitizer_name = 'typo3';
  }
  $dbh = db\db_connect();
  $san = db\get_sanitizer($dbh, $sanitizer_name);
  #print $san->fmt() . "\n";
  while(true) {
    $batch = db\get_next_batch($dbh, $san->get_id());
    if (count($batch) === 0) {
      print "Waiting for new fragments to sanitize\n";
      sleep(600);
      continue;
    }
    foreach ($batch as $fragment) {

      $sanitized = '';
      $errored = false;
      $error_message = '';
      try {
        $sanitized = sanitizer\Sanitizers[$sanitizer_name]->sanitize($fragment->payload);
      } catch(Exception $ex) {
        $errored = true;
        $error_message = $ex->__toString();
        print $fragment->payload . ' caused an error: ' . $error_message . "\n";
      }
      #print $fragment->fmt() . ' -> ' . $sanitized . "\n";
      db\add_sanitizer_result($dbh, $fragment->id, $san->get_id(), $sanitized, $errored, $error_message, $fragment->ts_id);
      #print sanitizer\Sanitizers['symfony']->sanitize($fragment->payload) . "\n";
    }

  }
}
main();
