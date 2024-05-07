const fs = require("fs");
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function fix_payload(payload) {
  payload = payload.replace(/\\/g, "\\\\");
  payload = payload.replace(/"/g, "\\\"");
  payload = payload.replace(/[\n\r]/g, "");
  return payload;
}

read_file = function (file) {
    return fs.readFileSync(__dirname + "/" + file, "utf-8");
};

function Emitter(retrieve_attributes)
{

    let makeTag = function(name, attributes, parent) {
        return {nodeName: name, attributes: attributes, childNodes: [], nodeType: 1, parent: parent };
    };
    let makeText = function(text, parent) {
        return {nodeValue: text, nodeType: 3, parent: parent };
    };
    let makeComment = function(text, parent) {
        return {nodeValue: text, nodeType: 8, parent: parent };
    };
    let makeRoot = function() {
        return { nodeType: 1, attributes: [], nodeName: "fragment", childNodes: []};
    };
    this.root = makeRoot();
    this.current = this.root;
    this.start = function(tagname, attrs) {
        let attributes = retrieve_attributes(attrs);
        let tag = makeTag(tagname, attributes, this.current);
        this.current.childNodes.push(tag);
        this.current = tag;
        //console.log(`begin: ${tagname}: ${JSON.stringify(attributes)}`)

    };
    this.end = function(tagname) {
        if(this.current.nodeName === tagname) {
            this.current = this.current.parent;
        }
        //console.log(`end: ${tagname}`)
    };
    this.pcdata = function(text) {
        let node = makeText(text, this.current);
        this.current.childNodes.push(node);
        //console.log(`pcdata: ${text}`)
    };
    this.cdata = function (text) {
        let node = makeText(text, this.current);
        this.current.childNodes.push(node);
        //console.log(`cdata: ${text}`)
    };
    this.rcdata = function(text) {
        let node = makeText(text, this.current);
        this.current.childNodes.push(node);
        //console.log(`rcdata: ${text}`)
    };
    this.comment =  function(text) {
        let node = makeComment(text, this.current);
        this.current.childNodes.push(node);
        //console.log(`comment: ${text}`)
    }

    this.get_root = function() {
        let fix = function(node) {
            switch(node.nodeType) {
                case 1:
                    let in_text = false;
                    let buffer = "";
                    let children = [];
                    for(let i=0; i < node.childNodes.length; i++) {
                        let child = node.childNodes[i];
                        switch(child.nodeType) {
                            case 3: // text
                                if(in_text) {
                                    buffer += child.nodeValue;
                                } else {
                                    buffer = child.nodeValue;
                                    in_text = true;
                                }
                                break;
                            default:
                                if(in_text) {
                                    children.push(makeText(buffer))
                                    buffer = "";
                                    in_text = false;
                                }
                                children.push(fix(child));
                                break;
                        }
                    }
                    if(in_text) {
                        children.push(makeText(buffer))
                        buffer = "";
                        in_text = false;
                    }
                    node.childNodes = children;
                    return node;
                default:
                    return node;
            }
        }
        this.root = fix(this.root);
        return this.root;
    }
};

function print_attr(a) {
  return `(#attr "${fix_payload(a.name)}" "${fix_payload(a.value)}")`
}
function print_document(e) {
    let children = [];
    for (let c of e.childNodes) {
        children.push(print_tree(c));
    }
    let childrens = `[${children.join(', ')}]`;

    return `(#document ${childrens})`
}
function print_fragment(e) {
    let children = [];
    for (let c of e.childNodes) {
        children.push(print_tree(c));
    }
    let childrens = `[${children.join(', ')}]`;

    return `(#document-fragment ${childrens})`
}
function print_elem(e) {
  let attrValues = "";
  let attrs = [];
  if (e.attributes !== undefined) {
    for (let a of e.attributes) {
      attrs.push(print_attr(a));
    }
  }
  attrValues = `[${attrs.join(', ')}]`;
  let children = [];
  if(e.nodeName.toLowerCase() === "template" && e.content) {
      children.push(print_tree(e.content));
  } else {
      for (let c of e.childNodes) {
          children.push(print_tree(c));
      }
  }
  let childrens = `[${children.join(', ')}]`;

  return `(#tag "${fix_payload(e.nodeName.toLowerCase())}" ${attrValues} ${childrens})`
}
function print_text(e) {
  let val = e.nodeValue;
  return `(#text "${fix_payload(val)}")`;
}
function print_comment(e) {
  return `(#comment "${fix_payload(e.nodeValue)}")`;
}
function print_tree(e) {
  switch(e.nodeType) {
    case 1:
      return print_elem(e);
    case 11:
      return print_fragment(e);
    case 3:
      return print_text(e);
    case 9:
      return print_document(e);
    case 8:
      return print_comment(e);
    default:
      console.log(`Error: ${e} has nodeType: ${e.nodeType}`);
      break;
  }
}
function median(numbers) {
  const sorted = Array.from(numbers).sort((a, b) => a - b);
  const middle = Math.floor(sorted.length / 2);

  if (sorted.length % 2 === 0) {
    return (sorted[middle - 1] + sorted[middle]) / 2;
  }

  return sorted[middle];
}

function average(numbers) {
  let sum = 0;
  for(const number of numbers) {
    sum += number;
  }
  return sum / numbers.length;
}

function percentage_of(a, b) {
  return a/(b/100);
}

function min(iter) {
  let m = Number.MAX_SAFE_INTEGER;
  for(let v of iter) {
    if(v < m) {
      m = v;
    }
  }
  return m;
}

function max(iter) {
  let m = Number.MIN_SAFE_INTEGER;
  for(let v of iter) {
    if(v > m) {
      m = v;
    }
  }
  return m;
}

function format_timespan(ts) {
    let unit = "ms";
    if(ts >= 1000) {
        ts /= 1000;
        unit = "s";
    } else {
        return { value: ts, unit: unit };
    }
    if(ts >= 60) {
        ts /= 60;
        unit = "min";
    } else {
        return { value: ts, unit: unit };
    }
    if(ts >= 60) {
        ts /= 60;
        unit = "h";
    } else {
        return { value: ts, unit: unit };
    }
    if(ts >= 24) {
        ts /= 24;
        unit = "d";
    }
    return { value: ts, unit: unit };
}
function pp_timespan(ts) {
    let f = format_timespan(ts);
    return `${f.value.toFixed(2)}${f.unit}`;
}

module.exports = {
  sleep,
  read_file,
  fix_payload,
  Emitter,
  print_tree,
  print_elem,
  min,
  max,
  percentage_of,
  median,
  average,
  format_timespan,
  pp_timespan
}

