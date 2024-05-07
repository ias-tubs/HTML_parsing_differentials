const { Pool } = require('pg')

let pool = undefined;

async function get_payloads(db) {
  const res = await db.query(`SELECT id,hash,payload FROM generations ORDER BY id`)
  return res.rows;
}
async function get_sanitizer_map(db) {
  const res = await db.query(`SELECT id,name FROM sanitizers`);
  let map = new Map();
  for(const row of res.rows) {
    map.set(row.id, row.name);
  }
  return map;
}
async function get_sanitizer(db, name) {
  const res = await db.query(`SELECT id,name FROM sanitizers where name = $1`, [name]);
  return res.rows[0];
}
async function get_payloads_not_considered(db, browser_id) {
  let query =
  ` select s.gen_id, s.id as sanitized_id, s.output as payload, te.mode_id, te.id as teid, s.sanitizer_id from to_evaluate as te  TABLESAMPLE bernoulli(3) join sanitized s on te.sanitized_id = s.id where te.browser_id = $1 and te.eval_id is null limit 500`
;
  const res = await db.query(query, [browser_id]);
  return res.rows;
}


async function get_payloads_to_sanitize(db, sanitizer_id) {
  let query = `select ts.id as tsid, g.id, g.payload from to_sanitize ts join generations g on g.id = ts.gen_id where ts.sanitizer_id = $1 and ts.sanitized_id is null LIMIT 25000`;
  const res = await db.query(query, [sanitizer_id]);
  return res.rows;
}


async function insert_sanitize_result(db, result) {
  let output = result.output;
  let done = output.indexOf("mxss") === -1 ? 1 : 0;
  let errored = result.errored ? 1 : 0;
  await db.query(`call insert_sanitized($1, $2, $3, $4,  $5, $6, $7, $8)`, [ result.id, result.sanitizer_id, result.serialized, output, errored, result.error_message, done, result.tsid]);
}
async function get_browser_id(db, name) {
  await db.query("insert into browsers (name) values ($1) ON CONFLICT DO NOTHING;", [name]);
  let res = await db.query("SELECT id from browsers where name = $1", [name]);
  // console.log(`${name}: ${res.rows[0].id}`);
  return res.rows[0].id;
}

async function get_sanitizer_id(db, name) {
  await db.query("insert into sanitizers (name) values ($1) ON CONFLICT DO NOTHING;", [name]);
  let res = await db.query("SELECT id from sanitizers where name = $1", [name]);
  // console.log(`${name}: ${res.rows[0].id}`);
  return {id: res.rows[0].id, name: name };
}


async function eval_already_exists(db, teid) {
  const res = await db.query(`SELECT e.eval_id is not null as ex from to_evaluate e where e.id = $1 `, [teid]);
  return res.rows[0] !== undefined && res.rows[0].ex === 'true';
}

async function insert_eval_result(db, result, teid) {
  let status = 0;
  if(result.error) {
    status = -1;
  }
 let res = await db.query(`INSERT INTO evaluations (gen_id, sanitized_id, browser_id, sanitizer_id, mode_id, error_message, executed, result, serialized, status) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)  RETURNING id`, [ result.id, result.sanitized_id, result.browser_id, result.sanitizer_id, result.mode_id, result.error_message, result.executed ? 1 : 0, result.output, result.serialized, status]);
	if(res.rows[0] !== undefined) {
    await db.query(`UPDATE to_evaluate set eval_id = $1 where id = $2`, [res.rows[0].id, teid]);
    return true;
	}
  return false;
}

async function connect() {
 pool = new Pool({
  user: 'mxssy',
  host: '127.0.0.1',
  database: 'mxssy',
  password: 'allyourmutationsarebelongtous',
  port: 5432,
})
  let db = await pool.connect();
  return db;
}

async function close(connection) {
    await connection.release();
    await pool.end();
}

module.exports = {
  get_payloads,
  eval_already_exists,
  get_payloads_not_considered,
  get_payloads_to_sanitize,
  get_sanitizer_id,
  get_sanitizer,
  get_sanitizer_map,
  get_browser_id,
  insert_eval_result,
  insert_sanitize_result,
  connect,
  close
}
