const argv = require("minimist")(process.argv.slice(2));
const browsers = require("./browsers.js");
const {main} = require("./pw-validator.js");
const db = require("./db");
const common = require("./common.js");

(async function() {
  const browser_choice = argv["browser"] === undefined ? "chromium" : argv["browser"];

  await common.sleep(500);
  let connection = await db.connect();
  let browser = browsers.browsers[browser_choice];
  console.log(`Running tests with ${browser.name}`);

  await main(connection, browser);

  await db.close(connection);
})();
