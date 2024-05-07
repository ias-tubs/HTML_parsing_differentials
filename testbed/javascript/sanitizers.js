const common = require("./common");
const caja = require('sanitizer');
const sanitizeHtmlCurr = require('sanitizehtml-curr');
const createDOMPurify = require('dompurify');
const createDOMPurifyCurr = require('dompurify-curr');
const jsdom  = require('jsdom');
const jsdom20 = require('jsdom20');
const jsdom22 = require('jsdom22');
const caja2 = require('google-caja-sanitizer')

let ATTRS = [
  'id',
  'title',
  'name',
  'data-foo',
  'foo',
  'encoding'
];
let SAFE_TAGS = [

  "div",
  "span",
  "title",
  "form",
  "dfn",
  "header",
  "p",
  "br",
  "a",
  "table",
  "td",
  "tr",
  "colgroup",
  "svg",
  "foreignobject",
  "desc",
  "path",
  "math",
  "mtext",
  "mglyph",
  "mi",
  "mo",
  "mn",
  "ms",
  "annotation-xml",
  "noscript",
  "select",
  "input",
  "textarea",
  "keygen",
  "xmp",
  "noembed",
  "listing",
  "li",
  "ul",
  "pre",
  "var",
  "dl",
  "dt",
  "iframe",
  "noframes",
  "frameset",
  "plaintext",
  "font",
  "option",
  "object",
  "embed"
];
let TAGS = [

  "div",
  "span",
  "title",
  "form",
  "dfn",
  "header",
  "p",
  "br",
  "a",
  "style",
  "table",
  "td",
  "tr",
  "colgroup",
  "svg",
  "foreignobject",
  "desc",
  "path",
  "math",
  "mtext",
  "mglyph",
  "mi",
  "mo",
  "mn",
  "ms",
  "annotation-xml",
  "noscript",
  "select",
  "input",
  "textarea",
  "keygen",
  "xmp",
  "noembed",
  "listing",
  "li",
  "ul",
  "pre",
  "var",
  "dl",
  "dt",
  "iframe",
  "noframes",
  "frameset",
  "plaintext",
  "font",
  "option",
  "object",
  "embed"
];

const sanitizers = {
  caja: {
    name: "caja (node)",
    sanitize: function(x) {
      let caja_attribute_converter = function(attrs) {
          let attributes = [];
          for (let i = 0, n = attrs.length; i < n; i += 2) {
              attributes.push({
                  name: attrs[i],
                  value: attrs[i+1]
              })
          }
          return attributes;
      }
      let emitter = new common.Emitter(caja_attribute_converter);
      let sanitized = caja.sanitize(x, emitter);
      let serialized = common.print_elem(emitter.get_root());
      // console.log(serialized);
      return {clean: sanitized, serialized: serialized };
    }
  },
    caja2: {
        name: "caja2 (node)",
        sanitize: function(x) {
            let caja_attribute_converter = function(attrs) {
                let attributes = [];
                for (let i = 0, n = attrs.length; i < n; i += 2) {
                    attributes.push({
                        name: attrs[i],
                        value: attrs[i+1]
                    })
                }
                return attributes;
            }
            let emitter = new common.Emitter(caja_attribute_converter);
            let sanitized = caja2.sanitize(x, emitter);
            let serialized = common.print_elem(emitter.get_root());
            // console.log(serialized);
            return {clean: sanitized, serialized: serialized };
        }
    },
  dompurify: {
    name: "dompurify (node)",
    sanitize: function(x) {
      const window = new jsdom.JSDOM('').window;
      const DOMPurify = createDOMPurify(window);
    let serialized = undefined;
      DOMPurify.addHook('beforeSanitizeElements', function(node) {
        DOMPurify.removeHook('beforeSanitizeElements');
        serialized = common.print_tree(node.ownerDocument);
      });
      let clean =  DOMPurify.sanitize(x, { FORCE_BODY: false });
    if(serialized === undefined) {
        serialized = `(#document [(#tag "html" [] [(#tag "head" [] []), (#tag "body" [] [(#text "${common.fix_payload(x)}")])])])`;
    }
    // console.log(`{|${serialized}|};`);
      return { clean: clean, serialized: serialized };
    }
  },
  dompurifyLax: {
    name: "dompurify (lax, node)",
    sanitize: function(x) {
      const window = new jsdom.JSDOM('').window;
      const DOMPurify = createDOMPurify(window);
    let serialized = undefined;
      DOMPurify.addHook('beforeSanitizeElements', function(node) {
        DOMPurify.removeHook('beforeSanitizeElements');
        serialized = common.print_tree(node.ownerDocument);
      });
      let clean =  DOMPurify.sanitize(x, { FORCE_BODY: false, ADD_TAGS: TAGS });
    if(serialized === undefined) {
        serialized = `(#document [(#tag "html" [] [(#tag "head" [] []), (#tag "body" [] [(#text "${common.fix_payload(x)}")])])])`;
    }
    // console.log(`{|${serialized}|};`);
      return { clean: clean, serialized: serialized };
    }
  },
  dompurify20: {
    name: "dompurify (node20)",
    sanitize: function(x) {
      const window = new jsdom20.JSDOM('').window;
      const DOMPurify = createDOMPurify(window);
    let serialized = undefined;
    DOMPurify.addHook('beforeSanitizeElements', function(node) {
      DOMPurify.removeHook('beforeSanitizeElements');
      serialized = common.print_tree(node.ownerDocument);
  });
    let clean =  DOMPurify.sanitize(x, { FORCE_BODY: false });
    if(serialized === undefined) {
        serialized = `(#document [(#tag "html" [] [(#tag "head" [] []), (#tag "body" [] [(#text "${common.fix_payload(x)}")])])])`;
    }
    // console.log(`{|${serialized}|};`);
    return { clean: clean, serialized: serialized };
    }
  },
  dompurifyLaxForceBodyCurrent: {
    name: "dompurify (lax, current, forceBody, node22)",
    sanitize: function(x) {
      const window = new jsdom22.JSDOM('').window;
      const DOMPurify = createDOMPurifyCurr(window);
    let serialized = undefined;
    DOMPurify.addHook('beforeSanitizeElements', function(node) {
      DOMPurify.removeHook('beforeSanitizeElements');
      serialized = common.print_tree(node.ownerDocument);
  });
    let clean =  DOMPurify.sanitize(x, { ADD_TAGS: TAGS , ADD_ATTR: ATTRS, FORCE_BODY: true });
    if(serialized === undefined) {
        serialized = `(#document [(#tag "html" [] [(#tag "head" [] []), (#tag "body" [] [(#text "${common.fix_payload(x)}")])])])`;
    }
    // console.log(`{|${serialized}|};`);
    return { clean: clean, serialized: serialized };
    }
  },
  dompurifyCurrentForceBody: {
    name: "dompurify (current, forceBody, node22)",
    sanitize: function(x) {
      const window = new jsdom22.JSDOM('').window;
      const DOMPurify = createDOMPurifyCurr(window);
    let serialized = undefined;
    DOMPurify.addHook('beforeSanitizeElements', function(node) {
      DOMPurify.removeHook('beforeSanitizeElements');
      serialized = common.print_tree(node.ownerDocument);
  });
    let clean =  DOMPurify.sanitize(x, { FORCE_BODY: true });
    if(serialized === undefined) {
        serialized = `(#document [(#tag "html" [] [(#tag "head" [] []), (#tag "body" [] [(#text "${common.fix_payload(x)}")])])])`;
    }
    // console.log(serialized);
    // console.log(`{|${serialized}|};`);
    return { clean: clean, serialized: serialized };
    }
  },
  vanilla: {
    name: "no-sanitizer",
    sanitize: function(x) { return { clean: x, serialized: undefined }; }
  },
  sanitizehtmlCurr: {
    name: "sanitize-html (curr, node)",
    sanitize: function(x) {
    let emitter = new common.Emitter(function(attrs) {
        let attributes = [];
        for (const [key, value] of Object.entries(attrs)) {
            //console.log(`${key}: ${value}`);
            attributes.push({name: key, value: value});
        }
        return attributes;
    });

    let clean = sanitizeHtmlCurr(x, {
        emitter: emitter
    });
    let serialized = common.print_elem(emitter.get_root());
    // console.log(`{|${serialized}|};`);
    return { clean: clean, serialized: serialized};
  },
  },
  sanitizehtmlCurrLax: {
    name: "sanitize-html (curr, lax, node)",
    sanitize: function(x) {
    let emitter = new common.Emitter(function(attrs) {
        let attributes = [];
        for (const [key, value] of Object.entries(attrs)) {
            //console.log(`${key}: ${value}`);
            attributes.push({name: key, value: value});
        }
        return attributes;
    });

    let clean = sanitizeHtmlCurr(x, {
        emitter: emitter,
        allowedAttributes: {
          '*': ATTRS
        },
      allowedTags: sanitizeHtmlCurr.defaults.allowedTags.concat(SAFE_TAGS)
    });
    let serialized = common.print_elem(emitter.get_root());
    // console.log(`{|${serialized}|};`);
    return { clean: clean, serialized: serialized};
  },
  }
};

module.exports = {
  sanitizers
}
