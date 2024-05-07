const common = require("./common");
const db = require("./db");
const chalk = require("chalk");
const { performance } = require('node:perf_hooks');


function print_stats(values, name) {
  let min = common.min(values);
  let max = common.max(values);
  let median = common.median(values);
  let average = common.average(values);
  console.log(`${name}: Processed ${values.length} samples with median ${pp_timespan(median)} and average ${pp_timespan(average)}. Min: ${pp_timespan(min)}, Max: ${pp_timespan(max)}.`);
}
let modes_map = new Map();
async function get_modes(db) {
  let mode_map = new Map();
  mode_map.set('innerHTML', innerHTML);
  mode_map.set('document_write', doc_write);
  // mode_map.set('set_content', set_content);
  let modes = [];
  let res = await db.query("select id, name from modes");
  for(let row of res.rows) {
    let func = mode_map.get(row.name);
    let o = {id: row.id, name: row.name, func: func };
    modes.push(o);
    modes_map.set(row.id, o);
  }
  // console.log(`${name}: ${res.rows[0].id}`);
  return modes;
}

const print_script = common.read_file("snippets/print_dom_tree.js");

async function doc_write(page, payload) {
  await page.evaluate(`document.open();document.write("<!DOCTYPE html><html lang='en'><body>${common.fix_payload(payload)}");document.close();`);
}

async function innerHTML(page, payload) {
  await page.setContent(`<!DOCTYPE html><html lang="en"><body></body></html>`);
  await page.evaluate(`
    document.body.innerHTML="${common.fix_payload(payload)}";
    `);
}

async function set_content(page, payload) {
  await page.setContent(`<!DOCTYPE html><html lang="en"><body>${payload}`);
}

async function eval_payload_write(context, result, payload) {
  return await eval_payload(context, result, payload, doc_write, "document_write");
}

async function eval_payload_innerhtml(context, result, payload) {
  return await eval_payload(context, result, payload, innerHTML, "innerHTML");
}

async function eval_payload_set_content(context, result, payload) {
  return await eval_payload(context, result, payload, set_content, "set_content");
}


var state = {
  exec_id: -1,
  val: -1,
  executed: false,
  set_val: function(v) { this.val = v; },
  reset: function() { this.val = -1; this.executed = false; this.exec_id = -1; },
  execute: function() { this.executed = true; this.exec_id = this.val; }
};
async function setup_context(context) {
  await context.exposeBinding("mxss", async function(params, msg) {
    state.execute();
  });
}

async function eval_payload(page, result, payload, method, method_name) {

  try {
    state.reset();
    let inner = payload.payload;
    await page.addScriptTag({
      content: print_script
    });
    state.set_val(payload.teid);
    try {

      await method(page, inner)
      await common.sleep(75);
    } catch(err) {
      console.log(`Error for ${result.id}: ${err.message}`);
      if(state.executed && state.exec_id === payload.teid ) {
        result.executed = true;
      }
      result.error = true;
      result.error_message = err.message;
      return result;
    }
    if(state.executed) {
      if(state.exec_id === payload.teid ) {

        result.executed = true;
      } else {
        console.log(`Prevented FP for ${result.id}`);
      }
    }


    let content = await page.content();
    result.output = content;
    result.serialized = await page.evaluate(`
      print_tree(document);
    `);



    return result;
  } finally {
    state.reset();
  }
}
async function main(connection, used_browser) {
  let modes = await get_modes(connection);
  let sanitizer_map = await db.get_sanitizer_map(connection);
  let browser_id = await db.get_browser_id(connection, used_browser.name);
  let browser = await used_browser.browser.launch()
  let context = await browser.newContext({
    ignoreHTTPSErrors: true
  });

  await context.exposeBinding("mxss", async function(params, msg) {
    state.execute();
  });
  while(true) {

    let ts_pre_db = performance.now();
    let payloads = await db.get_payloads_not_considered(connection, browser_id);
    if(payloads.length === 0) {
      console.log(`Waiting for new payloads to evaluate`);
      await common.sleep(60000);
      continue;
    }
    const page = await context.newPage();
    let ts_post_db = performance.now();
    let delta_db = ts_post_db-ts_pre_db;
    let total_payloads = payloads.length;
    let wasted_work = 0;
    let skipped_work = 0;
    let useful_work = 0;
    let ts_pre_analysis = performance.now();
    for(const payload of payloads) {
      let mode = modes_map.get(Number(payload.mode_id));
      let result = {
        id: payload.gen_id,
        mode_id: payload.mode_id,
        sanitizer_id: payload.sanitizer_id,
        sanitized_id: payload.sanitized_id,
        browser_id: browser_id,
        output: "",
        executed: false,
      };
      if(await db.eval_already_exists(connection, payload.teid)) {
        skipped_work++;
      } else {
        result = await eval_payload(page, result, payload, mode.func, mode.name)
        // console.log(result);
        // console.log(JSON.stringify(result, null, 2));
        let useful = await db.insert_eval_result(connection, result, payload.teid);
        if(useful) {
          useful_work++;
        } else {
          wasted_work++;
        }
        if(useful && result.executed) {
          let sanitizer_name = sanitizer_map.get(result.sanitizer_id);
          if(sanitizer_name !== 'no-sanitizer') {
            console.log(`Code exec with from '${sanitizer_name}'/'${mode.name}' for ${payload.gen_id}: ${payload.payload}`);
          }
        }
      }
    }
    await page.close();
    let ts_post_analysis = performance.now();
    let total_ts = ts_post_analysis-ts_pre_analysis;
    let ts_pp = total_ts/payloads.length;
    let useful_perc = common.percentage_of(useful_work, total_payloads);
    let wasted_perc = common.percentage_of(wasted_work, total_payloads);
    console.log(`Took ${common.pp_timespan(total_ts)} to run ${payloads.length} samples (${common.pp_timespan(ts_pp)} per Sample). DB access took ${common.pp_timespan(delta_db)}. ${useful_perc.toFixed(2)}% moved shit forward and ${wasted_perc.toFixed(2)}% wasted energy.`);
  }

  await context.close();
  await browser.close();
}

module.exports = {
  main,
  eval_payload,
  eval_payload_write,
  eval_payload_innerhtml,
  eval_payload_set_content,
  setup_context
}
