const { chromium, firefox, webkit } = require("playwright");
let browsers = {
  firefox: {
    name: "firefox",
    browser: firefox
  },
  chromium: {
    name: "chromium",
    browser: chromium
  },
  webkit : {
    name: "webkit",
    browser: webkit
  }
};

module.exports = {
  browsers
}
