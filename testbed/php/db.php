<?php namespace db;

class Fragment {

    public int $id;
    public int $ts_id;
    public string $payload;

    public function fmt(): string
    {
      return $this->id . ' [' . $this->ts_id . ']: ' . $this->payload;
    }
}

class Sanitizer {
    public function __construct(protected int $id, protected string $name) {
    }

    public function get_id(): int
    {
      return $this->id;
    }
    public function get_name(): string
    {
      return $this->name;
    }

    public function fmt(): string
    {
      return $this->id . ': ' . $this->name;
    }
}

function get_next_batch($dbh, $sanitizer_id) {
  $sth = $dbh->prepare('select g.id, ts.id as ts_id, g.payload from to_sanitize ts join generations g on g.id = ts.gen_id where ts.sanitizer_id = ? and ts.sanitized_id is null limit 25000');
  $sth->execute(array($sanitizer_id));
  return $sth->fetchAll(\PDO::FETCH_CLASS, "db\Fragment");
}

function get_sanitizer($dbh, $name): Sanitizer
{
  $sthq = $dbh->prepare('SELECT id from sanitizers where name = ?');
  $sthq->execute(array($name));
  $ret = $sthq->fetch();
  return new Sanitizer($ret['id'], $name);
}

function add_sanitizer_result($dbh, $generation_id, $sanitizer_id, $result, $errored, $error_message, $ts_id): void
{
  $done = !str_contains($result->get_clean(), 'mxss') ? 1 : 0;

  try {
    $sth = $dbh->prepare('call insert_sanitized(?, ?, ?, ?, ?, ?, ?, ?)');
    $sth->execute(array($generation_id, $sanitizer_id, $result->get_serialized(), $result->get_clean(),  $errored ? 1 : 0, $error_message, $done, $ts_id));
  } catch (\PDOException $e) {
    print 'Error!: ' . $e->getMessage();
  }
}

/*function print_browsers($dbh): void
{
  foreach($dbh->query('SELECT * from browsers') as $row) {
    print_r($row['id'] . ': ' . $row['name'] . "\n");
  }
}*/

function db_connect() {
  try {
    $user = 'mxssy';
    $pass = 'allyourmutationsarebelongtous';
    return new \PDO('pgsql:host=database;port=5432;dbname=mxssy', $user, $pass);
  } catch (\PDOException $e) {
    print 'Error!: ' . $e->getMessage();
    die();
  }
}

