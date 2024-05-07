const argv = require("minimist")(process.argv.slice(2));
const common = require("./common");
const db = require("./db");
const sanitizers = require("./sanitizers.js");

(async function() {
  const sanitizer_choice = argv["sanitizer"] === undefined ? "vanilla" : argv["sanitizer"];

  let sanitizer = sanitizers.sanitizers[sanitizer_choice];
  let connection = await db.connect();
  let san = await db.get_sanitizer_id(connection, sanitizer.name);
  let sanitizer_id = san.id;

  console.log(`Sanitizing with ${sanitizer.name}`);

  while(true) {

    let payloads = await db.get_payloads_to_sanitize(connection, sanitizer_id);
    if(payloads.length === 0) {
      console.log("Waiting for new payloads!");
      await common.sleep(60000);
      continue;
    }
    for(let payload of payloads) {
      let result = {
        id: payload.id,
        tsid: payload.tsid,
        output: "",
        sanitizer_id: sanitizer_id,
        serialized: '',
        errored: false,
        error_message: "",
      };
      try {
        let r = sanitizer.sanitize(payload.payload);
        result.output = r.clean;
        result.serialized = r.serialized;
      }catch(err) {
        result.errored = true;
        result.error_message = err.message;
      }
      await db.insert_sanitize_result(connection, result);
    }

  }

})();
