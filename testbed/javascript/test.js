const fs = require('fs');
const common = require("./common");
const sanitizers = require("./sanitizers.js");
const sanitizer_names = [
  "dompurify",
  "dompurifyLax",
  "dompurifyCurrentForceBody",
  "dompurifyLaxForceBodyCurrent",
  "sanitizehtmlCurr",
  "sanitizehtmlCurrLax",
  "caja",
  "caja2"
];


let payloads = [
`<div><noscript><a title='foo&apos; name=<span> <path data-foo="foo" data-foo=xyz><iframe data-foo=xyz><ul/><pre><noframes>--><math><img src=x onerror=mxss(1)></noframes></pre></mglyph>--></iframe></path> </span><!--<xmp foo='xyz'/>  name=&grave;foo>></dfn></noframes> <textarea/></a>]]>`,
`<img src=x onerror=f()>`,
`<textarea><script>f()</script>`,
`<svg><iframe><img src=x onerror=f()>`,
`<svg><style>&lt;img src=x onerror=f()&gt;<wbr>`,
`<svg><style>&lt;img src=x onerror=f()&gt;<area>`,
`<svg><style>&lt;img src=x onerror=f()&gt;<keygen>`


];

// const allFileContents = fs.readFileSync('payloads5.txt', 'utf-8');
// allFileContents.split(/\r?\n/).forEach(line =>  {
//   payloads.push(line);
// });
for (let payload of payloads) {
  if(payload === "") continue;
  // console.log(payload);
    console.log("\n\n");
  console.log(`Trying: ${payload}`);
  for (let sname of sanitizer_names) {

    let sanitizer = sanitizers.sanitizers[sname];

    let result = sanitizer.sanitize(payload);
    console.log(`\t${sanitizer.name}: ${result.clean}`);
    console.log(`\t\t${result.serialized}`);
  }
}


