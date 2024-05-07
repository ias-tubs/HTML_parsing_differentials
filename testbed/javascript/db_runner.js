const argv = require("minimist")(process.argv.slice(2));
const common = require("./common");
const db = require("./db");

(async function() {
  let connection = await db.connect();
  while(true) {
    const res = await connection.query(`delete from to_sanitize where to_sanitize.sanitized_id is not null;`);

    const rese = await connection.query(`delete from to_evaluate where eval_id is not null;`);
    console.log(`Deleted: ${res.rowCount} from to_sanitize and ${rese.rowCount} from to_evaluate`);
    await common.sleep(900000);
  }
})();

