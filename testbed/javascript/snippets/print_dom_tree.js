function fix(s) {
  return s.replace(/\\/g, "\\\\").replace(/"/g, "\\\"");
}
function print_attr(a) {
  return `(#attr "${fix(a.name)}" "${fix(a.value)}")`
}
function print_document_type(e) {
  return `(#document_type ${e.name})`;
}
function print_document(e) {
  let children = [];
  for (let c of e.childNodes) {
    children.push(print_tree(c));
  }
  let childrens = `[${children.join(', ')}]`;
  return `(#document ${childrens})`
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
  for (let c of e.childNodes) {
    children.push(print_tree(c));
  }
  let childrens = `[${children.join(', ')}]`;
  return `(#tag "${fix(e.nodeName)}" ${attrValues} ${childrens})`
}
function print_text(e) {
  let val = e.nodeValue;
  return `(#text "${fix(val)}")`;
}
function print_comment(e) {
  return `(#comment "${fix(e.nodeValue)}")`;
}
function print_tree(e) {
  switch(e.nodeType) {
    case 1:
      return print_elem(e);
    case 3:
      return print_text(e);
    case 9:
      return print_document(e);
    case 10:
      return print_document_type(e);
    case 8:
      return print_comment(e);
    default:
      console.log(`Error: ${e} has nodeType: ${e.nodeType}`);
      break;
  }
}
